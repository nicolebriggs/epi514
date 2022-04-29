### EPI 514 Research Project ###

#Sample Size/Power
#number exposed: 25985
#number unexposed: 78563
library(epiR)
epi.ssxsectn(
  pdexp1= NA, 
  pdexp0 = .55, #expected prevalence in unexposed 
  n = 210000, #sample size 
  power = 0.8, 
  r= 25985/ 78563, #ratio e/u
)


rm(list = ls())
# install.packages("survey")
# install.packages("Tmisc")
# library("survey")
# library("epiR")
# library("Tmisc")
library(tidyverse)
library(haven)
setwd("/Users/NicoleBriggs 1/Documents/MPH Coursework/SpringQ2022/epi514") #nicole
dataDir <- "/Users/NicoleBriggs 1/Documents/MPH Coursework/SpringQ2022/epi514" #nicole 
# setwd("~/Desktop/epi514/") #caitlin 
# dataDir <- "~/Desktop/epi514/" #caitlin 

#dataraw <- read_xpt("LLCP2019.XPT ") #just do once for file conversion 
#write.csv(dataraw, paste0(dataDir, "LLCP2019.csv"), row.names = FALSE) #save as csv 
dataCSV <- read.csv(paste0(dataDir, "LLCP2019.csv")) #run from here down after conversion 

# trimming the dataset 
data <- dataCSV[, c("X_STATE", "X_PSU", "X_STSTR", "X_LLCPWT",
                    "ACEDIVRC", "SEXVAR", 
                    "X_AGEG5YR", "X_AGE80", "X_AGE_G",
                    "INCOME2", "X_INCOMG", "EDUCA", "X_EDUCAG", 
                    "X_HISPANC", "X_RACE", "X_RACEGR3",
                    "FLUSHOT7")]
# key variables for analysis: ACEDIVRC, FLUSHOT7, X_AGE_G, X_RACE or X_RACEGR3?, 
# SEXVAR, X_INCOMG, X_EDUCAG
names(data)
summary(data)

## data cleaning/recoding  
# exposure divorce
data$divorce = data$ACEDIVRC
data$divorce[data$divorce==7 | data$divorce==9] <- NA
data$divorce[data$divorce==1] <- 1
data$divorce[data$divorce==2] <- 0
# outcome flu shot 
data$vaccinated = data$FLUSHOT7
data$vaccinated[data$FLUSHOT7==7 | data$FLUSHOT7==9] <- NA
data$vaccinated[data$FLUSHOT7==1] <- 1
data$vaccinated[data$FLUSHOT7==2] <- 0
# sex 
data$sex = data$X_SEX
data$sex[data$sex==9] <- NA
data$sexFac <- factor(data$X_SEX, 
                      levels = 1:2, 
                      labels = c("Male", "Female"))
data$male[data$X_SEX==2] <- 0
data$male[data$X_SEX==1] <- 1
# age 
data$age5yrFac = data$X_AGE_G
data$age5yrFac[data$age5yrFac==14] <- NA
data$age5yrFac <- factor(data$age5yrFac, levels = 1:13,
                         labels = c("18-24", "25-29", "30-34",
                                    "35-39", "40-44", "45-49",
                                    "50-54", "55-59", "60-64",
                                    "65-69", "70-74", "75-79", "80+"))
# race/ethnicity 
data$raceFac = data$X_RACE
data$raceFac[data$raceFac==9] <- NA 
data$raceFac <- factor(data$raceFac, levels = 1:8,
                       labels = c("White non-Hispanic", "Black non-Hispanic",
                                  "AI/AN non-Hispanic", "Asian non-Hispanic",
                                  "Native Hawaiian non-Hispanic",
                                  "Other race non-Hispanic", "Multiracial non-Hispanic",
                                  "Hispanic"))
# income
data$income = data$INCOME2
data$income[data$income==77 | data$income==99] <- NA
data$incomeFac <- factor(data$income, levels = 1:9,
                         labels = c("<$10,000", "$10,000 - $14,999", 
                         "$15,000- $19,999", "$20,000-$24,999", 
                         "$25,000-$34,999", "$35,000 - $49,999", 
                         "$50,00 - $74,999", "$75,000+"))
# education 
data$education = data$X_EDUCAG
data$education[data$education==9] <- NA 
data$educationFac <- factor(data$education, levels = 1:4,
                            "some high school", 
                            "graduated high school", 
                            "some college", 
                            "graduated college")

# when data cleaning is done, save clean dataset: 
#write.csv(data, paste0(dataDir, "epi514dataClean.csv"), row.names = FALSE) #save as csv

#nicole is testing - can delete 