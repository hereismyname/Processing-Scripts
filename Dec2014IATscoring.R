## This should be the algorithm specified by Greenwald.
## Gather block mean RTs

means <- sub211 %>%
            filter(Block == 3 | Block == 4 | Block == 6 | Block == 7) %>%
            group_by(Block) %>%
            filter(RT < 10000 & Correct == "True") %>% 
            summarize(mean(RT))

## Pooled standard deviation for Block 3 & Block 6

sd3and6 <- sub211 %>%
            filter(Block == 3 | Block == 6) %>%
            filter(RT < 10000) %>%
            summarize(sd(RT))

## Pooled standard deviation for Block 4 & Block 7

sd4and7 <- sub211 %>%
            filter(Block == 4 | Block == 7) %>%
            filter(RT < 10000) %>%
            summarize(sd(RT))

cleaned <- sub211 %>%
            filter(Block == 3 | Block == 4 | Block == 6 | Block == 7) %>%
            filter(RT < 10000)

# cleaned[grep("False", cleaned$Correct),]

cleaned[10, 3] <- means[3,2]

means2 <- cleaned %>%
            filter(Block == 3 | Block == 4 | Block == 6 | Block == 7) %>%
            group_by(Block) %>%
            filter(RT < 10000 & Correct == "True") %>% 
            summarize(mean(RT))

b6minusb3 <- (means2[3, 2] - means2[1, 2]) / sd3and6
b7minusb4 <- (means2[4, 2] - means2[2, 2]) / sd4and7

(b6minusb3 + b7minusb4) / 2