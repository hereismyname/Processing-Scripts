
library(dplyr)
setwd("./DotProbe")

## initialize empty data frames, grab list of files

files <- list.files()
readingtimes <- data.frame()
transformeddata <- data.frame()

for (file in files) {
    
    data <- tbl_df(read.csv(file))
    
    subjectreading <- data %>%
        select(subject,block, HIVparagraph, FLUparagraph, RTP1, RTP2)
    
    ## Store paragraph reading times for each subject by block
    subjectreading <- subjectreading[c(1, 65, 129, 193, 257),]
    
    ## bind each subject's reading times to larger data frame
    readingtimes <- rbind(readingtimes, subjectreading)
    
    
    ## transform initial RT into milliseconds
    ## subset to include only correct trials > 200ms, then log transform the RT
    data <- data %>%
        select(subject, block, trial, isCorrect, RTC) %>%
        mutate(RTmilli = RTC * 1000) %>%
        filter(isCorrect == 1) %>%
        filter(RTmilli > 200 ) %>%
        mutate(ProbeResp_lnrt = log(RTmilli))
    
    ## log transform data that excludes trials +/- 2.5 standard deviations from mean
    data2.5SD <- data %>%
        filter(RTmilli < (mean(RTmilli) + (sd(RTmilli) * 2.5)) & 
                   RTmilli > (mean(RTmilli) - (sd(RTmilli) * 2.5))) %>%
        mutate(ProbeResp_lnrt2.5SD = log(RTmilli))
    
    ## log transform data that excludes trials +/- 3 standard deviations from mean
    data3SD <- data %>%
        filter(RTmilli < (mean(RTmilli) + (sd(RTmilli) * 3)) & 
                   RTmilli > (mean(RTmilli) - (sd(RTmilli) * 3))) %>%
        mutate(ProbeResp_lnrt3SD = log(RTmilli))
    
    ## store means of each log transformed column in a new data frame, bind it
    ## on each loop
    subjectdata <- data.frame()
    subjectdata[1, 'ID'] <- data[1,1]
    subjectdata[1, 'probe_lnrt'] <- mean(data[,7])
    subjectdata[1,'probe_lnrt2.5SD'] <- mean(data2.5SD$ProbeResp_lnrt2.5SD)
    subjectdata[1, 'probe_lnrt3SD'] <- mean(data3SD$ProbeResp_lnrt3SD)
    
    transformeddata <- rbind(transformeddata, subjectdata)
    
}

## create the output files

write.csv(transformeddata, file = "FA2014 Dot Probe RT means & Transformations.csv")
write.csv(readingtimes, file = "FA2014 Dot Probe-- Paragraph Reading Times.csv")

setwd("C:/Users/Andrew/Documents/R")
rm(file, files, subjectdata, subjectreading, data, data2.5SD, data3SD,
   readingtimes, transformeddata)