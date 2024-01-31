library(tidyverse)
library(readxl)
library(diptest) 
library(LaplacesDemon)
library(moments)


getwd()
setwd("/path")
as1_data <- read_csv("survey2003.csv")
as1_data

###check column height
table(as1_data$height)
#step1 - clean
as1_data$height <- as.numeric(as1_data$height)
ggplot(as1_data, aes(height)) + geom_histogram()
as1_data$height[as1_data$height < 100]<- NA
as1_data$height[as1_data$height > 220]<- NA
table(as1_data$height, useNA = "always")
#step 2 - visualize
ggplot(as1_data, aes(height)) + geom_histogram()

#step 3a - unimodal/bimodal/multimodal or flat
#unimodal
dip.test(as1_data$height)
is.unimodal(as1_data$height)
is.bimodal(as1_data$height)
is.multimodal(as1_data$height)

#step 3b - symmetric/left-skewed/right-skewed
skewness(as1_data$height)
kurtosis(as1_data$height)

mean(as1_data$height, na.rm = TRUE)
sd(as1_data$height, na.rm = TRUE)
median(as1_data$height, na.rm = TRUE)

#step 3c - five-number summary
min.rt <- min(as1_data$height, na.rm = TRUE)
RT <- sort(as1_data$height)
lower.rt <- RT[1:round(length(RT)/2)] 
lower.h.rt <- median(lower.rt)
median.rt <- median(as1_data$height, na.rm = TRUE)
RT <- sort(as1_data$height)
upper.rt <- RT[round((length(RT)/2)+1):length(RT)] 
upper.h.rt <- median(upper.rt)
max.rt <- max(as1_data$height, na.rm = TRUE)

fivenumber <- cbind(min.rt, lower.h.rt,
                    median.rt, upper.h.rt,
                    max.rt)
colnames(fivenumber) <- c("Min", "Lower-hinge",
                          "Median", "Upper-hinge", "Max")

fivenumber
fivenum(as1_data$height)

#step 3d - missing values
sum(is.na(as1_data$height))
which(is.na(as1_data$height)) 

###check column weight
#step 1 - clean
table(as1_data$weight)
as1_data$weight <- as.numeric(as1_data$weight)
as1_data
as1_data$weight[as1_data$weight < 30]<- NA
as1_data$weight[as1_data$weight > 100]<- NA
table(as1_data$weight, useNA = "always") 
#step 2 - visualize
ggplot(as1_data, aes(weight)) + geom_histogram()

#step 3a - unimodal/bimodal/multimodal or flat
#unimodal
dip.test(as1_data$weight)
is.unimodal(as1_data$weight)
is.bimodal(as1_data$weight)
is.multimodal(as1_data$weight)

#step 3b - symmetric/left-skewed/right-skewed
skewness(as1_data$weight)
kurtosis(as1_data$weight)

mean(as1_data$weight, na.rm = TRUE)
sd(as1_data$weight, na.rm = TRUE)
median(as1_data$weight, na.rm = TRUE)

#step 3c - five-number summary
min.rt1 <- min(as1_data$weight, na.rm = TRUE)
RT1 <- sort(as1_data$weight)
lower.rt1 <- RT1[1:round(length(RT1)/2)] 
lower.h.rt1 <- median(lower.rt1)
median.rt1 <- median(as1_data$weight, na.rm = TRUE)
RT1 <- sort(as1_data$weight)
upper.rt1 <- RT1[round((length(RT1)/2)+1):length(RT1)] 
upper.h.rt1 <- median(upper.rt1)
max.rt1 <- max(as1_data$weight, na.rm = TRUE)

fivenumber <- cbind(min.rt1, lower.h.rt1,
                    median.rt1, upper.h.rt1,
                    max.rt1)
colnames(fivenumber) <- c("Min", "Lower-hinge",
                          "Median", "Upper-hinge", "Max")

fivenumber
fivenum(as1_data$weight)

#step 3d - missing values
sum(is.na(as1_data$weight))
which(is.na(as1_data$weight)) 


### check column favourite genre
#step 1- clean
table(as1_data$favourite_genre, useNA = "always") 
as1_data$favourite_genre[as1_data$favourite_genre == "prefer not to answer"] <- NA

as1_data$favourite_genre <-fct_recode(as1_data$favourite_genre,
                             action = "actin", 
                             comedy = "comdy",
                             comedy = "Comedy",
                             thriller = "Thriler", 
                             thriller = "thrller")

table(as1_data$favourite_genre, useNA = "always") 
#step 2 - viusalize 
ggplot(as1_data, aes(favourite_genre)) + geom_bar()

#step 4a - most common category
tt <- table(as1_data$favourite_genre)
names(tt[which.max(tt)])
#step 4b - least common category
names(tt[which.min(tt)])
#step 4c - frequency table
table(as1_data$favourite_genre)
#step 4d - missing values
sum(is.na(as1_data$favourite_genre))
which(is.na(as1_data$favourite_genre)) 

### check column sleep_hr
#step 1 - clean
table(as1_data$sleep_hr, useNA = "always") 
as1_data$sleep_hr[as1_data$sleep_hr > 20]<- NA
table(as1_data$sleep_hr, useNA = "always") 
#step 2 - visualize
ggplot(as1_data, aes(sleep_hr)) + geom_histogram()
#step 3a - unimodal/bimodal/multimodal or flat
#unimodal
dip.test(as1_data$sleep_hr)
is.unimodal(as1_data$sleep_hr)
is.bimodal(as1_data$sleep_hr)
is.multimodal(as1_data$sleep_hr)

#step 3b - symmetric/left-skewed/right-skewed
skewness(as1_data$sleep_hr)
kurtosis(as1_data$sleep_hr)

mean(as1_data$sleep_hr, na.rm = TRUE)
sd(as1_data$sleep_hr, na.rm = TRUE)
median(as1_data$sleep_hr, na.rm = TRUE)

#step 3c - five-number summary
min.rt2 <- min(as1_data$sleep_hr, na.rm = TRUE)
RT2 <- sort(as1_data$sleep_hr)
lower.rt2 <- RT2[1:round(length(RT2)/2)] 
lower.h.rt2 <- median(lower.rt2)
median.rt2 <- median(as1_data$sleep_hr, na.rm = TRUE)
RT2 <- sort(as1_data$sleep_hr)
upper.rt2 <- RT2[round((length(RT2)/2)+1):length(RT2)] 
upper.h.rt2 <- median(upper.rt2)
max.rt2 <- max(as1_data$sleep_hr, na.rm = TRUE)

fivenumber2 <- cbind(min.rt2, lower.h.rt2,
                    median.rt2, upper.h.rt2,
                    max.rt2)
colnames(fivenumber2) <- c("Min", "Lower-hinge",
                          "Median", "Upper-hinge", "Max")

fivenumber2
fivenum(as1_data$sleep_hr)

#step 3d - missing values
sum(is.na(as1_data$sleep_hr))
which(is.na(as1_data$sleep_hr)) 

### check column TV_hr
#step 1 - clean
table(as1_data$TV_hr, useNA = "always") 
as1_data$TV_hr[as1_data$TV_hr < 0]<- NA
#step 2 - visualize
ggplot(as1_data, aes(TV_hr)) + geom_histogram()
#step 3a - unimodal/bimodal/multimodal or flat
#unimodal
dip.test(as1_data$TV_hr)
is.unimodal(as1_data$TV_hr)
is.bimodal(as1_data$TV_hr)
is.multimodal(as1_data$TV_hr)

#step 3b - symmetric/left-skewed/right-skewed
skewness(as1_data$TV_hr)
kurtosis(as1_data$TV_hr)

mean(as1_data$TV_hr, na.rm = TRUE)
sd(as1_data$TV_hr, na.rm = TRUE)
median(as1_data$TV_hr, na.rm = TRUE)

#step 3c - five-number summary
min.rt3 <- min(as1_data$TV_hr, na.rm = TRUE)
RT3 <- sort(as1_data$TV_hr)
lower.rt3 <- RT3[1:round(length(RT3)/2)] 
lower.h.rt3 <- median(lower.rt3)
median.rt3 <- median(as1_data$TV_hr, na.rm = TRUE)
RT3 <- sort(as1_data$TV_hr)
upper.rt3 <- RT3[round((length(RT3)/2)+1):length(RT3)] 
upper.h.rt3 <- median(upper.rt3)
max.rt3 <- max(as1_data$TV_hr, na.rm = TRUE)

fivenumber3 <- cbind(min.rt3, lower.h.rt3,
                     median.rt3, upper.h.rt3,
                     max.rt3)
colnames(fivenumber3) <- c("Min", "Lower-hinge",
                           "Median", "Upper-hinge", "Max")

fivenumber3
fivenum(as1_data$TV_hr)

#step 3d - missing values
sum(is.na(as1_data$sleep_hr))
which(is.na(as1_data$sleep_hr)) 

### check column sex
#step 1 - clean
table(as1_data$sex, useNA = "always") 
as1_data$sex <-fct_recode(as1_data$sex,
                          Female = "F", 
                          Male = "M")
table(as1_data$sex, useNA = "always") 
#step 2 - visualize
ggplot(as1_data, aes(sex)) + geom_bar()

#step 4a - most common category
tt1 <- table(as1_data$sex)
names(tt1[which.max(tt1)])
#step 4b - least common category
names(tt1[which.min(tt1)])
#step 4c - frequency table
table(as1_data$sex)
#step 4d - missing values
sum(is.na(as1_data$sex))
which(is.na(as1_data$sex)) 

summary(as1_data)

