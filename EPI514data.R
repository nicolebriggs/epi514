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
library("survey")
library("epiR")
library("Tmisc")
library(tidyverse)
library(haven)
#setwd("/Users/NicoleBriggs 1/Documents/MPH Coursework/SpringQ2022/epi514/") #nicole
#dataDir <- "/Users/NicoleBriggs 1/Documents/MPH Coursework/SpringQ2022/epi514/" #nicole 
# setwd("~/Desktop/epi514/") #caitlin 
# dataDir <- "~/Desktop/epi514/" #caitlin 
#setwd("/Users/winnieyeung/Documents/SCHOOL/GRADSCHOOL/EPI514/")
#dataDir<- "/Users/winnieyeung/Documents/SCHOOL/GRADSCHOOL/EPI514/"

#dataraw <- read_xpt("LLCP2019.XPT ") #just do once for file conversion 
#write.csv(dataraw, paste0(dataDir, "LLCP2019.csv"), row.names = FALSE) #save as csv 
#dataCSV <- read.csv(paste0(dataDir, "LLCP2019.csv")) #run from here down after conversion 

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
data$divorce[data$divorce==7 | data$divorce==8| data$divorce==9] <- NA
data$divorce[data$divorce==1] <- 1
data$divorce[data$divorce==2] <- 0
data$divorceFac <- factor(data$ACEDIVRC, 
                      levels = 1:2, 
                      labels = c("Experienced parental divorce/separation", "Did not experience parental divorce/separation"))

# outcome flu shot 
data$vaccinated = data$FLUSHOT7
data$vaccinated[data$FLUSHOT7==7 | data$FLUSHOT7==9] <- NA
data$vaccinated[data$FLUSHOT7==1] <- 1
data$vaccinated[data$FLUSHOT7==2] <- 0
data$vaccinatedFac <- factor(data$FLUSHOT7, 
                       levels = 1:2, 
                       labels = c("Yes", "No"))

#Variable to sum up missing (for discussion section)
data$missing_new[data$ACEDIVRC == 7 | data$ACEDIVRC == 8 |data$ACEDIVRC == 9 | data$FLUSHOT7 == 7 | data$FLUSHOT7 == 9]<-1
table(data$missing_new)


#excluding people who don't have data for vaccination
#remove rows with NA in specific columns of data frame
data <- data[complete.cases(data[ , c('vaccinated')]), ]


# sex 
data$sex = data$SEXVAR
data$sex[data$sex==9] <- NA
data$sexFac <- factor(data$SEXVAR, 
                      levels = 1:2, 
                      labels = c("Male", "Female"))
data$male[data$SEXVAR==2] <- 0
data$male[data$SEXVAR==1] <- 1

# age 5 year categories 
data$age5yrFac = data$X_AGEG5YR
data$age5yrFac[data$age5yrFac==14] <- NA
data$age5yrFac <- factor(data$age5yrFac, levels = 1:13,
                         labels = c("18-24", "25-29", "30-34",
                                    "35-39", "40-44", "45-49",
                                    "50-54", "55-59", "60-64",
                                    "65-69", "70-74", "75-79", "80+"))

#age 10 year categories
data$ageFac = data$X_AGE_G
data$ageFac[data$ageFac==14] <- NA
data$ageFac <- factor(data$ageFac, levels = 1:6,
                         labels = c("18-24", "25-34",
                                    "35-44", "45-54", "55-64",
                                    "65 or older"))
# race/ethnicity 
data$raceFac = data$X_RACE
data$raceFac[data$raceFac==9] <- NA 
data$raceFac[data$raceFac==1] <- 1
data$raceFac[data$raceFac==2 ]<- 2
data$raceFac[data$raceFac==3 | data$raceFac==4 | data$raceFac==5 |data$raceFac==6 | data$raceFac==7] <-3
data$raceFac[data$raceFac==8] <- 4
data$raceFac <- factor(data$raceFac, levels = 1:4,
                         labels = c("White", "Black",
                                    "Other", "Hispanic"))
with(data, table(raceFac, X_RACE))

# income
data$income = data$INCOME2
data$income[data$income==77 | data$income==99] <- NA
data$incomeFac <- factor(data$income, levels = 1:8,
                         labels = c("<$10,000", "$10,000 - $14,999", 
                                    "$15,000 - $19,999", "$20,000 - $24,999", 
                                    "$25,000 - $34,999", "$35,000 - $49,999", 
                                    "$50,000 - $74,999", "$75,000+"))
# education 
data$education = data$X_EDUCAG
data$education[data$education==9] <- NA 
data$educationFac <- factor(data$education, levels = 1:4,
                            labels = c("Did not graduate high school", 
                                       "Graduated high school", 
                                       "Attended college", 
                                       "Graduated college"))

# when data cleaning is done, save clean dataset: 
#CSV: write.csv(data, paste0(dataDir, "epi514dataClean.csv"), row.names = FALSE) #save as csv 

saveRDS(data,file="epi514_brfss2019Clean.rds")
#data <- read.csv(paste0(dataDir, "epi514dataClean.csv")) #run from here down after cleaning
data <- readRDS(file="epi514_brfss2019Clean.rds")#run from here down after cleaning

# table 1 
#install.packages("table1")
library(table1)

label(data$ageFac) <- "Age (years)"
label(data$sexFac) <- "Sex"
label(data$raceFac) <- "Race/Ethnicity"
label(data$educationFac) <- "Highest Level of Education Completed"
label(data$incomeFac) <- "Annual Household Income"

#divorce variable recoded so that experienced divorce is on left side of table (experienced = 0, not experienced = 1)
data$divorcetable = data$ACEDIVRC
data$divorcetable[data$divorce==7 | data$divorce==8| data$divorce==9] <- NA
data$divorcetable[data$divorce==1] <- 0
data$divorcetable[data$divorce==2] <- 1
data$divorcetable <- factor(data$ACEDIVRC, 
                            levels = 1:2, 
                            labels = c("Experienced parental divorce/separation", "Did not experience parental divorce/separation"))

#creating table in R
table1(~ ageFac + sexFac + raceFac + educationFac + incomeFac | divorcetable, data=data, overall="Total")
#table1(~ ageFac + sexFac + raceFac + educationFac + incomeFac | divorce, data=data, render.missing=NULL, render.categorical="FREQ (PCTnoNA%)", overall="Total")
# second option also removes NAs from % calculations 

#install.packages("survey")
library("survey")
#install.packages("gtsummary")
library("gtsummary")
#install.packages("dplyr")
library("dplyr") 
#didn't use this:
(results <- survey::svydesign(~ 1, data = data, weights = ~ X_LLCPWT) %>%
    tbl_svysummary(
      by = divorcetable,
      include = c(ageFac, sexFac, raceFac, educationFac, incomeFac),
      statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"), 
      missing = "always", missing_text = "Missing"
    ))

# getting weighted %s including missing values 
#(survey <- survey::svydesign(~ 1, data = data, weights = ~ X_LLCPWT))
#prop.table(svytable(~ageFac+divorcetable, design=survey, na.action=na.pass, exclude = NULL, addNA=T),margin=2)*100
#prop.table(svytable(~sexFac+ divorcetable, design=survey, na.action=na.pass, exclude = NULL, addNA=T),margin=2)*100
#prop.table(svytable(~raceFac+ divorcetable, design=survey, na.action=na.pass, exclude = NULL, addNA=T),margin=2)*100
#prop.table(svytable(~educationFac+ divorcetable, design=survey, na.action=na.pass, exclude = NULL, addNA=T),margin=2)*100
#prop.table(svytable(~incomeFac+ divorcetable, design=survey, na.action=na.pass, exclude = NULL, addNA=T),margin=2)*100

# weighted %s without missing values 
prop.table(svytable(~ageFac+ divorcetable, design=survey),margin=2)*100
prop.table(svytable(~sexFac+ divorcetable, design=survey),margin=2)*100
prop.table(svytable(~raceFac+ divorcetable, design=survey),margin=2)*100
prop.table(svytable(~educationFac+ divorcetable, design=survey),margin=2)*100
prop.table(svytable(~incomeFac+ divorcetable, design=survey),margin=2)*100


# raw missing %s 
prop.table(with(data, table(raceFac, divorcetable, useNA = "ifany")), margin=2)*100
prop.table(with(data, table(educationFac, divorcetable, useNA = "ifany")), margin=2)*100
prop.table(with(data, table(incomeFac, divorcetable, useNA = "ifany")), margin=2)*100

#Table 2
#unadjusted
library(epiR)

unadjusted <- with(data, table(divorce, vaccinated))
epi.2by2(unadjusted)

## Assessing confounding 
# Evaluate: age, sex, race, education, and income

# age
strat_age <- with(data,
                  table(divorce, vaccinated, ageFac))
# sex
strat_sex <- with(data,
                  table(divorce, vaccinated, sexFac))
# race/ethnicity
strat_race <- with(data,
                   table(divorce, vaccinated, raceFac))
# education 
strat_education <- with(data,
                        table(divorce, vaccinated, educationFac))
# income
strat_income <- with(data,
                     table(divorce, vaccinated, incomeFac))
# run confounder analysis
epi.2by2(strat_age) 
epi.2by2(strat_sex) 
epi.2by2(strat_race) 
epi.2by2(strat_education) 
epi.2by2(strat_income) 

## Effect modification
# Evaluate: age, sex, race, education, and income

# age
strat_age_1 <- with(subset(data, ageFac == "18-24"),
                    table(divorce, vaccinated))
strat_age_2 <- with(subset(data, ageFac == "25-34"),
                    table(divorce, vaccinated))
strat_age_3 <- with(subset(data, ageFac == "35-44"),
                    table(divorce, vaccinated))
strat_age_4 <- with(subset(data, ageFac == "45-54"),
                    table(divorce, vaccinated))
strat_age_5 <- with(subset(data, ageFac == "55-64"),
                    table(divorce, vaccinated))
strat_age_6 <- with(subset(data, ageFac == "65 or older"),
                    table(divorce, vaccinated))
# age EM analysis
epi.2by2(strat_age_1) 
epi.2by2(strat_age_2) 
epi.2by2(strat_age_3) 
epi.2by2(strat_age_4) 
epi.2by2(strat_age_5) 
epi.2by2(strat_age_6) 

# # sex 
# strat_sex_1 <- with(subset(data, sexFac == "Male"),
#                     table(divorce, vaccinated))
# strat_sex_2 <- with(subset(data, sexFac == "Female"),
#                     table(divorce, vaccinated))
# # sex EM analysis
# epi.2by2(strat_sex_1) 
# epi.2by2(strat_sex_2) 
# 
# # race/ethnicity
# strat_race_1 <- with(subset(data, raceFac == "White"),
#                      table(divorce, vaccinated))
# strat_race_2 <- with(subset(data, raceFac == "Black"),
#                      table(divorce, vaccinated))
# strat_race_3 <- with(subset(data, raceFac == "American Indian/Alaska Native"),
#                      table(divorce, vaccinated))
# strat_race_4 <- with(subset(data, raceFac == "Asian"),
#                      table(divorce, vaccinated))
# strat_race_5 <- with(subset(data, raceFac == "Native Hawaiian/Pacific Islander"),
#                      table(divorce, vaccinated))
# strat_race_6 <- with(subset(data, raceFac == "Other"),
#                      table(divorce, vaccinated))
# strat_race_7 <- with(subset(data, raceFac == "Multiracial"),
#                      table(divorce, vaccinated))
# strat_race_8 <- with(subset(data, raceFac == "Hispanic"),
#                      table(divorce, vaccinated))
# # race/ethnicity EM analysis
# epi.2by2(strat_race_1)
# epi.2by2(strat_race_2) 
# epi.2by2(strat_race_3) 
# epi.2by2(strat_race_4) 
# epi.2by2(strat_race_5) 
# epi.2by2(strat_race_6) 
# epi.2by2(strat_race_7)
# epi.2by2(strat_race_8) 
# 
# # education  
# strat_education_1 <- with(subset(data, educationFac == "Did not graduate high school"),
#                           table(divorce, vaccinated))
# strat_education_2 <- with(subset(data, educationFac == "Graduated high school"),
#                           table(divorce, vaccinated))
# strat_education_3 <- with(subset(data, educationFac == "Attended college"),
#                           table(divorce, vaccinated))
# strat_education_4 <- with(subset(data, educationFac == "Graduated college"),
#                           table(divorce, vaccinated))
# # education EM analysis
# epi.2by2(strat_education_1)
# epi.2by2(strat_education_2) 
# epi.2by2(strat_education_3) 
# epi.2by2(strat_education_4) 
# 
# # income 
# strat_income_1 <- with(subset(data, incomeFac == "<$10,000"),
#                        table(divorce, vaccinated))
# strat_income_2 <- with(subset(data, incomeFac == "$10,000 - $14,999"),
#                        table(divorce, vaccinated))
# strat_income_3 <- with(subset(data, incomeFac == "$15,000 - $19,999"),
#                        table(divorce, vaccinated))
# strat_income_4 <- with(subset(data, incomeFac == "$20,000 - $24,999"),
#                        table(divorce, vaccinated))
# strat_income_5 <- with(subset(data, incomeFac == "$25,000 - $34,999"),
#                        table(divorce, vaccinated))
# strat_income_6 <- with(subset(data, incomeFac == "$35,000 - $49,999"),
#                        table(divorce, vaccinated))
# strat_income_7 <- with(subset(data, incomeFac == "$50,000 - $74,999"),
#                        table(divorce, vaccinated))
# strat_income_8 <- with(subset(data, incomeFac == "$75,000+"),
#                        table(divorce, vaccinated))
# # income EM analysis
# epi.2by2(strat_income_1)
# epi.2by2(strat_income_2) 
# epi.2by2(strat_income_3) 
# epi.2by2(strat_income_4) 
# epi.2by2(strat_income_5) 
# epi.2by2(strat_income_6) 
# epi.2by2(strat_income_7)
# epi.2by2(strat_income_8) 

##adjusted

#create one variable for confounders
data$confounders <- case_when(!is.na(data$ageFac)&
                                !is.na(data$sexFac) &
                                !is.na(data$educationFac) &
                                !is.na(data$raceFac) &
                                !is.na(data$incomeFac)~
                                paste0(data$ageFac, "_", data$sexFac, "_", data$incomeFac, "_", data$raceFac, "_", data$incomeFac))

#with(data, table(confounders, ageFac))

adjusted_pr<- with(data, table(divorce, vaccinated, confounders))
epi.2by2(adjusted_pr) #adjusted PR value

#Adjusted effect modification
data$em <- case_when(!is.na(data$sexFac) &
                                !is.na(data$educationFac) &
                                !is.na(data$raceFac) &
                                !is.na(data$incomeFac)~
                                paste0(data$sexFac, "_", data$incomeFac, "_", data$raceFac, "_", data$incomeFac))

strat_age_1_em <- with(subset(data, ageFac == "18-24"),
                       table(divorce, vaccinated, em))
epi.2by2(strat_age_1_em)

strat_age_2_em <- with(subset(data, ageFac == "25-34"),
                       table(divorce, vaccinated, em))
epi.2by2(strat_age_2_em) 

strat_age_3_em <- with(subset(data, ageFac == "35-44"),
                       table(divorce, vaccinated, em))
epi.2by2(strat_age_3_em) 

strat_age_4_em <- with(subset(data, ageFac == "45-54"),
                       table(divorce, vaccinated, em))
epi.2by2(strat_age_4_em) 

strat_age_5_em <- with(subset(data, ageFac == "55-64"),
                       table(divorce, vaccinated, em))
epi.2by2(strat_age_5_em) 

strat_age_6_em <- with(subset(data, ageFac == "65 or older"),
                       table(divorce, vaccinated, em))
epi.2by2(strat_age_6_em) 

#Table 3 N count
n <- with(data,
     table(divorce, ageFac))

#Figure 1
library("ggplot2")

boxLabels = c("18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65+")

df <- data.frame(yAxis = length(boxLabels):1,
                 boxOdds = 
                   c(0.84, 
                     0.85, 0.88, 0.91, 0.94, 0.95),
                 boxCILow = 
                   c(0.77, 0.79, 0.84, 0.86,
                     0.90, 0.93),
                 boxCIHigh = 
                   c(0.93, 0.90, 0.93, 0.95, 
                     0.97, 0.97))

(p <- ggplot(df, aes(x = boxOdds, y = boxLabels)) +
    geom_vline(aes(xintercept = 1), size = .25, linetype = 'solid') +
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = 
                     .2, color = 'gray50') +
    geom_point(size = 3.5, color = 'darkorchid4') +
    theme_classic(base_size = 20) +
#    theme(panel.grid.minor = element_blank()) +
    ylab('Age Group (years)') +
    xlab('Prevalence Ratio'))


