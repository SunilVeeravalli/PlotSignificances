# This function calculates and plots significances on bar chart
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

install.packages("tidyverse")
library(tidyverse)


plotSignificances <- function(dataset, error.bars, type, alternative, conf.level, title, subtitle, xlabel, ylabel, legend.title) {
    
    # dataset:
        # should be a dataframe with two columns: 
            # first column should be a categorical variable 
            # second column should be a continuous variable
    
    # error.bars:
        # specify "sd" to plot standard deviation
        # specify "sem" to plot standard error of the mean
    
    # type:
        # specify "t.test" or "wilcox.test"
    
    # alternative:
        # specify "two.sided" or "greater" or "less"
    
    # conf.level:
        # specify a value for confidence level between 0 and 1
    
    # title:
        # specify the chart title enclosed in double quotes
    
    # subtitle:
        # specify the subtitle enclosed in double quotes
    
    # xlabel:
        # specify a title to X-axis enclosed in double quotes
    
    # ylabel:
        # specify a title to Y-axis enclosed in double quotes
    
    # legend.title:
        # specify a title to the legend enclosed in double quotes
    
    
    # Check for errors in the arguments provided by the user    
    error.in.error.bars <- length(intersect(error.bars, c("sd", "sem"))) == 0
    if(error.in.error.bars) {
        cat('\nError in error.bars argument. \nPlease use either "sd" or "sem"')
    }
    
    error.in.type <- length(intersect(type, c("t.test", "wilcox.test"))) == 0
    if(error.in.type) {
        cat('\nError in type argument. \nPlease use either "t.test" or "wilcox.test"')
    }
    
    error.in.alternative <- length(intersect(alternative, c("two.sided", "greater", "less"))) == 0
    if(error.in.alternative) {
        cat('\nError in alternative argument. \nPlease use either "two.sided" or "greater" or "less"')
    }
    
    error.in.conf.level <- conf.level < 0 | conf.level > 1
    if(error.in.conf.level) {
        cat('\nError in conf.level argument. \nPlease use a value between 0 and 1')
    }
    
    # If found an error in the arguments, will throw and error. Otherwise, runs rest of the code.
    if(any(error.in.error.bars, error.in.type, error.in.alternative, error.in.conf.level) == 1) {
        cat('\nPlease try again by providing correct arguments')
    } else {
    
        # Changing the column names for this code to work
        names(dataset) <- c("variable", "values")
        
        # Changing dataset to a dataframe if it is not
        dataset <- as.data.frame(dataset)
        
        # Removing all rows that have a value of NA
        dataset <- na.omit(dataset)
        
        if(error.bars == "sem"){
            data.to.plot <- dataset %>%
                group_by(variable) %>%
                summarise(Average = mean(values), error.bars = sd(values)/sqrt(length(values)))
        } else {
            data.to.plot <- dataset %>%
                group_by(variable) %>%
                summarise(Average = mean(values), error.bars = sd(values))
        }
        
        # Converting a long dataset to a wide dataset
        dataset.wide <- dataset %>%
            mutate(temp.column = 1:nrow(dataset)) %>%
            spread(key = variable, value = values) %>%
            select(-temp.column)
        
        # Creating a table to store p values calculated from either t.test or wilcox.test
        pvalues <- data.frame()
        
        n.cols <- ncol(dataset.wide)
        for(a in 1:n.cols) {
            for(b in a:n.cols) {
                if(type == "wilcox.test") {
                    test <- wilcox.test(dataset.wide[,a], dataset.wide[,b], 
                                        alternative = alternative, 
                                        conf.level = conf.level)
                    pvalues[a,b] <- test$p.value
                } else {
                    test <- t.test(dataset.wide[,a], dataset.wide[,b], 
                                   alternative = alternative, 
                                   conf.level = conf.level)
                    pvalues[a,b] <- test$p.value
                }
            }
        }
        
        colnames(pvalues) <- colnames(dataset.wide)
        pvalues <- round(pvalues, 4)
        
        
        # Creating table for asterisks computed from pvalues table
        asterisks <- pvalues
        for(a in 1:ncol(pvalues)) { 
            for(b in 1:nrow(pvalues)) {
                if(!is.na(pvalues[b,a])) {
                    if(pvalues[b,a] > 0.05 & pvalues[b,a] <= 0.1) {
                        asterisks[b,a] <- "."
                    } else if(pvalues[b,a] > 0.01 & pvalues[b,a] <= 0.05) {
                        asterisks[b,a] <- "*"
                    } else if(pvalues[b,a] > 0.001 & pvalues[b,a] <= 0.01) {
                        asterisks[b,a] <- "**"
                    } else if(pvalues[b,a] > 0.0001 & pvalues[b,a] <= 0.001) {
                        asterisks[b,a] <- "***"
                    } else if(pvalues[b,a] <= 0.0001) {
                        asterisks[b,a] <- "****"
                    } else {
                        asterisks[b,a] <- NA_character_
                    }
                } else {
                    asterisks[b,a] <- NA_character_
                }
                
            }
        }
        
        
        # Since the segments (connect lines on which pvalues and asterisks will be mentioned)
        # should be above the Average + (sd or sem), finding the max value above which the segments
        # should start
        avg.sem <- data.to.plot %>%
            mutate(Max = Average + error.bars) %>%
            mutate(Max = max(Max)) %>%
            select(Max) %>%
            unique() %>%
            .[[1]]
        
        
        max.table <- pvalues
        max.table[,] <- avg.sem
        max.table[is.na(pvalues)] <- NA
        for(a in 1:ncol(max.table)) {
            for(b in 1:nrow(max.table)) {
                if(a == b) {
                    max.table[a,b] <- max.table[a,b]
                } else {
                    max.table[a,b] <- ifelse(is.na(asterisks[a,b]), NA, max.table[a,b])
                }
            }
        }
        
        
        # I will be using annotate() of ggplot to add lines and asterisks on the chart.
        # For this, I need to provide positional parameters. Therefore, creating the following tables.
        value.to.increase.by <- avg.sem * (7/100)
        
        y.table <- as.matrix(max.table)
        no.of.col <- ncol(y.table)
        for(a in 1:nrow(y.table)) {
            if(a < no.of.col) {
                for(b in (a+1):ncol(y.table)) {
                    if(!is.na(y.table[a,b])){
                        if(a == 1) {
                            y.table[a,b] <- max(y.table[1, 1:b], na.rm = TRUE) + value.to.increase.by
                        } else {
                            x <- as.vector(y.table[1, 1:a])
                            y <- as.vector(y.table[a, 1:(b-1)])
                            z <- as.vector(y.table[1:(a-1), (a+1):no.of.col])
                            c <- setdiff(x, union(y, z))
                            if(length(c) > 0) {
                                y.table[a,b] <- c[1]
                            } else {
                                y.table[a,b] <- max(y, z, na.rm = TRUE) + value.to.increase.by
                            }
                        }
                    } else {
                        y.table[a,b] <- NA
                    }
                }
            } else {
                break
            }
        }
        
        
        segment.x = pvalues %>% 
            row()
        segment.x <- segment.x + 0.05
        
        
        segment.xend = pvalues %>%
            col()
        segment.xend <- segment.xend - 0.05
        
        
        segment.drop.min <- y.table - (y.table[1,1] * (2/100))
        
        label.xposition <- segment.xend
        for(a in 1:nrow(segment.xend)) {
            for(b in 1:ncol(segment.xend)) {
                label.xposition[a,b] <- mean(c(segment.xend[a,a], segment.xend[a,b]))
            }
        }
        
        label.yposition <- y.table + (y.table[1,1] * (2/100))
        
        # Creating annotations() to add to the ggplot
        all.annotations <- list()
        for(a in 1:nrow(asterisks)) {
            for(b in 1:ncol(asterisks)) {
                if(!is.na(asterisks[a,b])) {
                    annotation1 <-  annotate('segment', 
                                             x = segment.x[a,b], 
                                             xend = segment.xend[a,b], 
                                             y = y.table[a,b], 
                                             yend = y.table[a,b]) 
                    annotation2 <-  annotate('segment', 
                                             x = segment.x[a,b], 
                                             xend = segment.x[a,b], 
                                             y = segment.drop.min[a,b], 
                                             yend = y.table[a,b]) 
                    annotation3 <-  annotate('segment', 
                                             x = segment.xend[a,b], 
                                             xend = segment.xend[a,b], 
                                             y = segment.drop.min[a,b], 
                                             yend = y.table[a,b]) 
                    annotation4 <- annotate('text', 
                                            label = paste(pvalues[a,b], asterisks[a,b], sep = ""), 
                                            x = label.xposition[a,b], 
                                            y = label.yposition[a,b])
                    all.annotations <- list(all.annotations, 
                                            annotation1, 
                                            annotation2, 
                                            annotation3, 
                                            annotation4)
                }
            } 
        }
        
        
        # Creating ggplot with addition of the annotations created from the above code
        plot <- ggplot() +
            theme_classic() +
            geom_bar(data = data.to.plot, 
                     mapping = aes(x = variable, y = Average, fill = variable), 
                     stat = "identity", 
                     position = "dodge") + 
            geom_errorbar(data = data.to.plot, 
                          mapping = aes(x = variable, ymin = Average - error.bars, ymax = Average + error.bars), 
                          width = 0.1) +
            # theme(axis.title.x = element_blank(), 
            #       axis.text.x = element_blank(), 
            #       axis.ticks.x = element_blank()) + 
            scale_fill_discrete(guide = guide_legend(title = legend.title)) +
            labs(title = title,
                 subtitle = subtitle,
                 x = xlabel,
                 y = ylabel) +
            all.annotations
        
        plot
        
    }
}




    

    
    

    
    

    
    

