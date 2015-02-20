library(dplyr)

x <- readline("State desired Directory (no quotation marks, please!): ")


getprobeRTs <- function(directory, RTfile, readfile, exclude = c("log")) {
    ## initialize empty data frames, grab list of files
    
    files <- list.files(directory)
    files <- files[!(files %in% exclude)] # grab filenames, minus excluded entries
    
    readingtimes <- data.frame()
    transformeddata <- data.frame()
    
    readingtimes <- data.frame()
    transformeddata <- data.frame()
    
    for (i in 1:length(files)) {
        
        path <- paste(x, files[i], sep = "/") # build file path
        
        data <- tbl_df(read.csv(path))
        subjectreading <- data %>%
            select(subject,block, HIVparagraph, FLUparagraph, RTP1, RTP2)
        
        ## Store paragraph reading times for each subject by block
        subjectreading <- subjectreading[c(1, 65, 129, 193, 257),]
        
        ## bind each subject's reading times to larger data frame
        readingtimes <- rbind(readingtimes, subjectreading)
        
        if ("isCongruent" %in% names(data)) {
        
            ## transform initial RT into milliseconds
            ## subset to include only correct trials > 200ms, then log transform the RT
            data <- data %>%
                select(subject, block, trial, isCorrect, cue, isCongruent, RTC) %>%
                mutate(RTmilli = RTC * 1000) %>%
                filter(isCorrect == 1) %>%
                filter(RTmilli > 200 ) %>%
                mutate(ProbeResp_lnrt = log(RTmilli))
            
            #### log transform data that excludes trials +/- 2.5 standard deviations from mean
            data2.5SD <- data %>%
                filter(RTmilli < (mean(RTmilli) + (sd(RTmilli) * 2.5)) & 
                           RTmilli > (mean(RTmilli) - (sd(RTmilli) * 2.5))) %>%
                mutate(ProbeResp_lnrt2.5SD = log(RTmilli))
            
            #### log transform data that excludes trials +/- 3 standard deviations from mean
            data3SD <- data %>%
                filter(RTmilli < (mean(RTmilli) + (sd(RTmilli) * 3)) & 
                           RTmilli > (mean(RTmilli) - (sd(RTmilli) * 3))) %>%
                mutate(ProbeResp_lnrt3SD = log(RTmilli))
            
            ## store means of each log transformed column in a new data frame, bind it
            ## on each loop
            
            #### get means for trial types
            
            hivcongmean <- data %>%
                filter(cue == "HIV") %>%
                filter(isCongruent == 1) %>%
                summarize(mean(ProbeResp_lnrt))
            
            hivnotcongmean <- data %>%
                filter(cue == "HIV") %>%
                filter(isCongruent == 0) %>%
                summarize(mean(ProbeResp_lnrt))
            
            flucongmean <- data %>%
                filter(cue == "FLU") %>%
                filter(isCongruent == 1) %>%
                summarize(mean(ProbeResp_lnrt))
            
            flunotcongmean <- data %>%
                filter(cue == "FLU") %>%
                filter(isCongruent == 0) %>%
                summarize(mean(ProbeResp_lnrt))
            
            hivcongmean2.5sd <- data2.5SD %>%
                filter(cue == "HIV") %>%
                filter(isCongruent == 1) %>%
                summarize(mean(ProbeResp_lnrt2.5SD))
            
            hivnotcongmean2.5sd <- data2.5SD %>%
                filter(cue == "HIV") %>%
                filter(isCongruent == 0) %>%
                summarize(mean(ProbeResp_lnrt2.5SD))
            
            flucongmean2.5sd <- data2.5SD %>%
                filter(cue == "FLU") %>%
                filter(isCongruent == 1) %>%
                summarize(mean(ProbeResp_lnrt2.5SD))
            
            flunotcongmean2.5sd <- data2.5SD %>%
                filter(cue == "FLU") %>%
                filter(isCongruent == 0) %>%
                summarize(mean(ProbeResp_lnrt2.5SD))
            
            hivcongmean3sd <- data3SD %>%
                filter(cue == "HIV") %>%
                filter(isCongruent == 1) %>%
                summarize(mean(ProbeResp_lnrt3SD))
            
            hivnotcongmean3sd <- data3SD %>%
                filter(cue == "HIV") %>%
                filter(isCongruent == 0) %>%
                summarize(mean(ProbeResp_lnrt3SD))
            
            flucongmean3sd <- data3SD %>%
                filter(cue == "FLU") %>%
                filter(isCongruent == 1) %>%
                summarize(mean(ProbeResp_lnrt3SD))
            
            flunotcongmean3sd <- data3SD %>%
                filter(cue == "FLU") %>%
                filter(isCongruent == 0) %>%
                summarize(mean(ProbeResp_lnrt3SD))
            
            subjectdata <- data.frame(id = data[[1,1]],
                                      hivcongmean = hivcongmean[[1,1]],
                                      hivnotcongmean = hivnotcongmean[[1,1]],
                                      flucongmean = flucongmean[[1,1]],
                                      flunotcongmean = flunotcongmean[[1,1]],
                                      hivcongmean2.5sd = hivcongmean2.5sd[[1,1]],
                                      hivnotcongmean2.5sd = hivnotcongmean2.5sd[[1,1]],
                                      flucongmean2.5sd = flucongmean2.5sd[[1,1]],
                                      flunotcongmean2.5sd = flunotcongmean2.5sd[[1,1]],
                                      hivcongmean3sd = hivcongmean3sd[[1,1]],
                                      hivnotcongmean3sd = hivnotcongmean3sd[[1,1]],
                                      flucongmean3sd = flucongmean3sd[[1,1]],
                                      flunotcongmean3sd = flunotcongmean3sd[[1,1]]
                                      )
            
            transformeddata <- rbind(transformeddata, subjectdata)
            
    } else {
        text <- paste("No congruency column(s) in file ", data$subject[i], "...") ## derp, should've done this from the start
        message(text)
        }
    }
    
    ## create the output files
    
    write.csv(transformeddata, file = RTfile, row.names = FALSE)
    write.csv(readingtimes, file = readfile, row.names = FALSE)
}
