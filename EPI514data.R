

rm(list = ls())
# install.packages("survey")
# install.packages("Tmisc")
# library("survey")
# library("epiR")
# library("Tmisc")
library(tidyverse)
library(haven)
setwd("/Users/NicoleBriggs 1/Documents/MPH Coursework/SpringQ2022/epi514")
dataDir <- "/Users/NicoleBriggs 1/Documents/MPH Coursework/SpringQ2022/epi514"

dataraw <- read_xpt("LLCP2019.XPT ")
write.csv(dataraw, paste0(dataDir, "LLCP2019.XPT"), row.names = FALSE)
dataraw <- read.csv(paste0(dataDir, "LLCP2019.XPT"))

library(epiR)
#Sample Size/Power
#number exposed: 25985
#number unexposed: 78563

epi.ssxsectn(
  pdexp1= NA, 
  pdexp0 = .55, #expected prevalence in unexposed 
  n = 210000, #sample size 
  power = 0.8, 
  r= 25985/ 78563, #ratio e/u
)




data <- dataraw[, c("_PSU", "_LLCPWT", "_AGE_G" )]
names(data) #view variable names 
summary(data) #check for weird values 

## data cleaning/recoding  
#sex variable 
data$X_SEX[data$X_SEX==9] <- NA
data$sexFac <- factor(data$X_SEX, 
                      levels = 1:2, 
                      labels = c("Male", "Female"))
data$male[data$X_SEX==2] <- 0
data$male[data$X_SEX==1] <- 1


