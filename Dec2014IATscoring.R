## This should be the algorithm specified by Greenwald.

library(dplyr)
library(ggplot2)

## get directory
x <- readline("State desired Directory (no quotation marks, please!): ")

getscores <- function(x, exclude = c("Log")) {
    
    files <- list.files(x)
    files <- files[!(files %in% exclude)] # grab filenames, minus excluded entries
    
    df <- data.frame()
    
    for (i in 1:length(files)) {
        
        path <- paste(x, files[i], sep = "/") # build file path
                
        subdat <- read.csv(path)
        
        ## Get block means
        
        means <- subdat %>%
                    filter(Block == 3 | Block == 4 | Block == 6 | Block == 7) %>%
                    group_by(Block) %>%
                    filter(RT < 10000 & Correct == "True") %>% 
                    summarize(mean(RT))
        
        ## Pooled standard deviation for Block 3 & Block 6
        
        sd3and6 <- subdat %>%
                    filter(Block == 3 | Block == 6) %>%
                    filter(RT < 10000) %>%
                    summarize(sd(RT))
        
        ## Pooled standard deviation for Block 4 & Block 7
        
        sd4and7 <- subdat %>%
                    filter(Block == 4 | Block == 7) %>%
                    filter(RT < 10000) %>%
                    summarize(sd(RT))
        
        cleaned <- subdat %>%
                    filter(Block == 3 | Block == 4 | Block == 6 | Block == 7) %>%
                    filter(RT < 10000)
        
        ## Store cleaned data
        
        cleaned[10, 3] <- means[3,2]
        
        means2 <- cleaned %>%
                    filter(Block == 3 | Block == 4 | Block == 6 | Block == 7) %>%
                    group_by(Block) %>%
                    filter(RT < 10000 & Correct == "True") %>% 
                    summarize(mean(RT))
        
        ## Get subject score: positive values indicate bias towards association in 
        ## Block 7, while negative values indicate bias towards association in
        ## Block 4.
        
        b6minusb3 <- (means2[3, 2] - means2[1, 2]) / sd3and6
        b7minusb4 <- (means2[4, 2] - means2[2, 2]) / sd4and7
        
        score <- (b6minusb3 + b7minusb4) / 2
        
        newline <- data.frame(files[i], score[1,1])
        df <- rbind(df, newline)
    }
    
    ## Save output to working directory
    
    names(df) <- c("ID", "score")
    write.csv(df, "greenwaldscoresEEGIAT.csv")
}

getscores(x)
