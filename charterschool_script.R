
# load packages
library(readxl)
library(dplyr)
library(tidyverse)
library(glmnet)
library(ggplot2)
library(lubridate)
library(readxl)
library(MASS)
library('e1071') 
library('quantmod')
library(Metrics)

set.seed(123)


# read raw dataset
data <- read_excel("Trinity_BATDATA_ 2022.xlsx", sheet = "Data")

# raw dataset shows we're working with 168,234 rows and 18 columns; we also have 233927 NA values
data[data=="NULL"] <- NA
sum(is.na(data)) 
summary(data)
str(data)

# Basic analysis of each variable
summary(data$Academic_Year)
sum(is.na(data$Academic_Year))
table(data$Academic_Year)
prop.table(table(data$Academic_Year))

summary(data$Region)
sum(is.na(data$Region))
table(data$Region)
prop.table(table(data$Region))


summary(data$School)
sum(is.na(data$School))
table(data$School)
prop.table(table(data$School))
length(unique(data$School))


summary(data$Student_ID_State)
sum(is.na(data$Student_ID_State))
table(data$Student_ID_State)
prop.table(table(data$Student_ID_State))
length(unique(data$Student_ID_State))

summary(data$Student_ID_Local)
sum(is.na(data$Student_ID_Local))
table(data$Student_ID_Local)
prop.table(table(data$Student_ID_Local))
length(unique(data$Student_ID_Local))

summary(data$Grade)
sum(is.na(data$Grade))
table(data$Grade)
prop.table(table(data$Grade))

summary(data$`Race/Ethnicity`)
sum(is.na(data$`Race/Ethnicity`))
table(data$`Race/Ethnicity`)
prop.table(table(data$`Race/Ethnicity`))

summary(data$Gender)
sum(is.na(data$Gender))
table(data$Gender)
prop.table(table(data$Gender))

summary(data$ED)
sum(is.na(data$ED))
table(data$ED)
prop.table(table(data$ED))

summary(data$At_Risk)
sum(is.na(data$At_Risk))
table(data$At_Risk)
prop.table(table(data$At_Risk))

summary(data$LEP)
sum(is.na(data$LEP))
table(data$LEP)
prop.table(table(data$LEP))

summary(data$SPED)
sum(is.na(data$SPED))
table(data$SPED)
prop.table(table(data$SPED))

summary(data$Subject)
sum(is.na(data$Subject))
table(data$Subject)
prop.table(table(data$Subject))

summary(data$`Fall Quartile`)
sum(is.na(data$`Fall Quartile`))
table(data$`Fall Quartile`)
prop.table(table(data$`Fall Quartile`))

summary(data$`Fall Percentile`)
sum(is.na(data$`Fall Percentile`))
table(data$`Fall Percentile`)
prop.table(table(data$`Fall Percentile`))

summary(data$`Fall RIT`)
sum(is.na(data$`Fall RIT`))
table(data$`Fall RIT`)
prop.table(table(data$`Fall RIT`))

summary(data$`Fall - Fall Typical Growth Target`)
sum(is.na(data$`Fall - Fall Typical Growth Target`))
table(data$`Fall - Fall Typical Growth Target`)
prop.table(table(data$`Fall - Fall Typical Growth Target`))

summary(data$`Met F-F Typical Growth Target`)
sum(is.na(data$`Met F-F Typical Growth Target`))
table(data$`Met F-F Typical Growth Target`)
prop.table(table(data$`Met F-F Typical Growth Target`))
# str(data)


# data$`Fall RIT` <- as.numeric(data$`Fall RIT`)

#  data %>%
#    group_by(Grade) %>%
#    summarise_at(vars(`Fall RIT`),na.rm=TRUE, list(name = mean))

# ggplot(data, aes(Grade, data$`Fall RIT`)) + geom_point()



#### Data cleaning

# Checking if there are any duplicates
count(data[duplicated(data) == TRUE, ]) #great, no duplicates

# determine which columns to delete
data <- data %>% dplyr::select(-School)
data <- data %>% dplyr::select(-Student_ID_Local)
data <- data %>% dplyr::select(-'Fall Quartile')
data <- data %>% dplyr::select(-At_Risk)

# Academic Year
data$Academic_Year <- substring(data$Academic_Year,3)
data  

# Grade (remove preK and high schoolers)
data <- data[!(data$Grade == "P4" | data$Grade == "9" | data$Grade == "10" | data$Grade == "11" | data$Grade == "12"),]

# Create new column to indicate whether RIT score is actual or predicted
sum(is.na(data$`Fall RIT`))
data$OriginalRITScore <- ifelse(is.na(data$`Fall RIT`)==TRUE, 0, 1)
table(data$OriginalRITScore)

# Create new column with identifier for each entry
data$entry <- seq.int(nrow(data))


#### main problem  
# Fall Percentile
# Fall RIT
# Fall Typical Growth Target
data$`Fall Percentile` <- as.numeric(data$`Fall Percentile`)
data$`Fall - Fall Typical Growth Target` <- as.numeric(data$`Fall - Fall Typical Growth Target`)
table(data$Grade)
data$Grade <- factor(data$Grade, levels = c("KG", "1", "2", "3","4","5", "6", "7","8"))
table(data$Subject)

MathData <-data[data$Subject == "Mathematics",]
table(MathData$Grade)

MathDataKG <- MathData[MathData$Grade == "KG", ]
MathData1 <- MathData[MathData$Grade == "1", ]
MathData2 <- MathData[MathData$Grade == "2", ]
MathData3 <- MathData[MathData$Grade == "3", ]
MathData4 <- MathData[MathData$Grade == "4", ]
MathData5 <- MathData[MathData$Grade == "5", ]
MathData6 <- MathData[MathData$Grade == "6", ]
MathData7 <- MathData[MathData$Grade == "7", ]
MathData8 <- MathData[MathData$Grade == "8", ]

sum(is.na(MathDataKG$`Fall - Fall Typical Growth Target`))
MathDataKG <- MathDataKG %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
MathDataKGmiss <- MathDataKG[is.na(MathDataKG$`Fall - Fall Typical Growth Target`)==TRUE,]

sum(is.na(MathData1$`Fall - Fall Typical Growth Target`))
MathData1 <- MathData1 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
MathData1[is.na(MathData1$`Fall - Fall Typical Growth Target`)==TRUE,]
MathData1$`Fall - Fall Typical Growth Target` <- ifelse(is.na(MathData1$`Fall - Fall Typical Growth Target`) == TRUE, 155, MathData1$`Fall - Fall Typical Growth Target`)

sum(is.na(MathData2$`Fall - Fall Typical Growth Target`))
MathData2 <- MathData2 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
MathData2miss <- MathData2[is.na(MathData2$`Fall - Fall Typical Growth Target`)==TRUE,]
MathData2$`Fall - Fall Typical Growth Target` <- ifelse(MathData2$`Fall Percentile` == 26, 170, MathData2$`Fall - Fall Typical Growth Target`)
MathData2$`Fall - Fall Typical Growth Target` <- ifelse(MathData2$`Fall Percentile` == 31, 174, MathData2$`Fall - Fall Typical Growth Target`)
MathData2$`Fall - Fall Typical Growth Target` <- ifelse(MathData2$`Fall Percentile` == 48, 177.5, MathData2$`Fall - Fall Typical Growth Target`)
MathData2$`Fall - Fall Typical Growth Target` <- ifelse(MathData2$`Fall Percentile` == 51, 178, MathData2$`Fall - Fall Typical Growth Target`)
MathData2$`Fall - Fall Typical Growth Target` <- ifelse(MathData2$`Fall Percentile` == 57, 180, MathData2$`Fall - Fall Typical Growth Target`)
MathData2$`Fall - Fall Typical Growth Target` <- ifelse(MathData2$`Fall Percentile` == 60, 181, MathData2$`Fall - Fall Typical Growth Target`)
MathData2$`Fall - Fall Typical Growth Target` <- ifelse(MathData2$`Fall Percentile` == 63, 182, MathData2$`Fall - Fall Typical Growth Target`)
MathData2$`Fall - Fall Typical Growth Target` <- ifelse(MathData2$`Fall Percentile` == 66, 183, MathData2$`Fall - Fall Typical Growth Target`)
MathData2$`Fall - Fall Typical Growth Target` <- ifelse(MathData2$`Fall Percentile` == 83, 188, MathData2$`Fall - Fall Typical Growth Target`)


sum(is.na(MathData3$`Fall - Fall Typical Growth Target`))
MathData3 <- MathData3 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))

sum(is.na(MathData4$`Fall - Fall Typical Growth Target`))
MathData4 <- MathData4 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
MathData4miss <- MathData4[is.na(MathData4$`Fall - Fall Typical Growth Target`)==TRUE,]
MathData4$`Fall - Fall Typical Growth Target` <- ifelse(MathData4$`Fall Percentile` == 15, 190, MathData4$`Fall - Fall Typical Growth Target`)
MathData4$`Fall - Fall Typical Growth Target` <- ifelse(MathData4$`Fall Percentile` == 41, 198, MathData4$`Fall - Fall Typical Growth Target`)
MathData4$`Fall - Fall Typical Growth Target` <- ifelse(MathData4$`Fall Percentile` == 58, 203, MathData4$`Fall - Fall Typical Growth Target`)
MathData4$`Fall - Fall Typical Growth Target` <- ifelse(MathData4$`Fall Percentile` == 61, 205, MathData4$`Fall - Fall Typical Growth Target`)
MathData4$`Fall - Fall Typical Growth Target` <- ifelse(MathData4$`Fall Percentile` == 69, 207, MathData4$`Fall - Fall Typical Growth Target`)


sum(is.na(MathData5$`Fall - Fall Typical Growth Target`))
MathData5 <- MathData5 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
MathData5miss <- MathData5[is.na(MathData5$`Fall - Fall Typical Growth Target`)==TRUE,]
MathData5$`Fall - Fall Typical Growth Target` <- ifelse(MathData5$`Fall Percentile` == 11, 200, MathData5$`Fall - Fall Typical Growth Target`)
MathData5$`Fall - Fall Typical Growth Target` <- ifelse(MathData5$`Fall Percentile` == 17, 202, MathData5$`Fall - Fall Typical Growth Target`)
MathData5$`Fall - Fall Typical Growth Target` <- ifelse(MathData5$`Fall Percentile` == 40, 210, MathData5$`Fall - Fall Typical Growth Target`)
MathData5$`Fall - Fall Typical Growth Target` <- ifelse(MathData5$`Fall Percentile` == 45, 214, MathData5$`Fall - Fall Typical Growth Target`)
MathData5$`Fall - Fall Typical Growth Target` <- ifelse(MathData5$`Fall Percentile` == 56, 217, MathData5$`Fall - Fall Typical Growth Target`)
MathData5$`Fall - Fall Typical Growth Target` <- ifelse(MathData5$`Fall Percentile` == 59, 218, MathData5$`Fall - Fall Typical Growth Target`)
MathData5$`Fall - Fall Typical Growth Target` <- ifelse(MathData5$`Fall Percentile` == 64, 219, MathData5$`Fall - Fall Typical Growth Target`)



sum(is.na(MathData6$`Fall - Fall Typical Growth Target`))
MathData6 <- MathData6 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))

sum(is.na(MathData7$`Fall - Fall Typical Growth Target`))
MathData7 <- MathData7 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
MathData7miss <- MathData7[is.na(MathData7$`Fall - Fall Typical Growth Target`)==TRUE,]
MathData7$`Fall - Fall Typical Growth Target` <- ifelse(MathData7$`Fall Percentile` == 25, 215, MathData7$`Fall - Fall Typical Growth Target`)
MathData7$`Fall - Fall Typical Growth Target` <- ifelse(MathData7$`Fall Percentile` == 29, 216, MathData7$`Fall - Fall Typical Growth Target`)
MathData7$`Fall - Fall Typical Growth Target` <- ifelse(MathData7$`Fall Percentile` == 31, 216, MathData7$`Fall - Fall Typical Growth Target`)
MathData7$`Fall - Fall Typical Growth Target` <- ifelse(MathData7$`Fall Percentile` == 42, 219, MathData7$`Fall - Fall Typical Growth Target`)
MathData7$`Fall - Fall Typical Growth Target` <- ifelse(MathData7$`Fall Percentile` == 70, 230, MathData7$`Fall - Fall Typical Growth Target`)
MathData7$`Fall - Fall Typical Growth Target` <- ifelse(MathData7$`Fall Percentile` == 76, 231, MathData7$`Fall - Fall Typical Growth Target`)
MathData7$`Fall - Fall Typical Growth Target` <- ifelse(MathData7$`Fall Percentile` == 78, 232, MathData7$`Fall - Fall Typical Growth Target`)


sum(is.na(MathData8$`Fall - Fall Typical Growth Target`))
MathData8 <- MathData8 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
MathData8miss <- MathData8[is.na(MathData8$`Fall - Fall Typical Growth Target`)==TRUE,]
MathData8$`Fall - Fall Typical Growth Target` <- ifelse(MathData8$`Fall Percentile` == 35, 218.5, MathData8$`Fall - Fall Typical Growth Target`)
MathData8$`Fall - Fall Typical Growth Target` <- ifelse(MathData8$`Fall Percentile` == 37, 222, MathData8$`Fall - Fall Typical Growth Target`)
MathData8$`Fall - Fall Typical Growth Target` <- ifelse(MathData8$`Fall Percentile` == 39, 223, MathData8$`Fall - Fall Typical Growth Target`)
MathData8$`Fall - Fall Typical Growth Target` <- ifelse(MathData8$`Fall Percentile` == 41, 223, MathData8$`Fall - Fall Typical Growth Target`)
MathData8$`Fall - Fall Typical Growth Target` <- ifelse(MathData8$`Fall Percentile` == 57, 232, MathData8$`Fall - Fall Typical Growth Target`)



ReadingData <-data[data$Subject == "Reading",]
table(ReadingData$Grade)

ReadingDataKG <- ReadingData[ReadingData$Grade == "KG", ]
ReadingData1 <- ReadingData[ReadingData$Grade == "1", ]
ReadingData2 <- ReadingData[ReadingData$Grade == "2", ]
ReadingData3 <- ReadingData[ReadingData$Grade == "3", ]
ReadingData4 <- ReadingData[ReadingData$Grade == "4", ]
ReadingData5 <- ReadingData[ReadingData$Grade == "5", ]
ReadingData6 <- ReadingData[ReadingData$Grade == "6", ]
ReadingData7 <- ReadingData[ReadingData$Grade == "7", ]
ReadingData8 <- ReadingData[ReadingData$Grade == "8", ]

sum(is.na(ReadingDataKG$`Fall - Fall Typical Growth Target`))
ReadingDataKG <- ReadingDataKG %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))


sum(is.na(ReadingData1$`Fall - Fall Typical Growth Target`))
ReadingData1 <- ReadingData1 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))

sum(is.na(ReadingData2$`Fall - Fall Typical Growth Target`))
ReadingData2 <- ReadingData2 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
ReadingData2miss <- ReadingData2[is.na(ReadingData2$`Fall - Fall Typical Growth Target`)==TRUE,]
ReadingData2$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData2$`Fall Percentile` == 26, 171, ReadingData2$`Fall - Fall Typical Growth Target`)
ReadingData2$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData2$`Fall Percentile` == 37, 174, ReadingData2$`Fall - Fall Typical Growth Target`)
ReadingData2$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData2$`Fall Percentile` == 42, 175, ReadingData2$`Fall - Fall Typical Growth Target`)
ReadingData2$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData2$`Fall Percentile` == 47, 178, ReadingData2$`Fall - Fall Typical Growth Target`)


sum(is.na(ReadingData3$`Fall - Fall Typical Growth Target`))
ReadingData3 <- ReadingData3 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))

sum(is.na(ReadingData4$`Fall - Fall Typical Growth Target`))
ReadingData4 <- ReadingData4 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
ReadingData4miss <- ReadingData4[is.na(ReadingData4$`Fall - Fall Typical Growth Target`)==TRUE,]
ReadingData4$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData4$`Fall Percentile` == 49, 198, ReadingData4$`Fall - Fall Typical Growth Target`)
ReadingData4$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData4$`Fall Percentile` == 54, 200, ReadingData4$`Fall - Fall Typical Growth Target`)
ReadingData4$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData4$`Fall Percentile` == 59, 201, ReadingData4$`Fall - Fall Typical Growth Target`)
ReadingData4$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData4$`Fall Percentile` == 64, 202, ReadingData4$`Fall - Fall Typical Growth Target`)
ReadingData4$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData4$`Fall Percentile` == 84, 211, ReadingData4$`Fall - Fall Typical Growth Target`)


sum(is.na(ReadingData5$`Fall - Fall Typical Growth Target`))
ReadingData5 <- ReadingData5 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
ReadingData5miss <- ReadingData5[is.na(ReadingData5$`Fall - Fall Typical Growth Target`)==TRUE,]
ReadingData5$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData5$`Fall Percentile` == 23, 197, ReadingData5$`Fall - Fall Typical Growth Target`)
ReadingData5$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData5$`Fall Percentile` == 25, 199, ReadingData5$`Fall - Fall Typical Growth Target`)
ReadingData5$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData5$`Fall Percentile` == 45, 205, ReadingData5$`Fall - Fall Typical Growth Target`)
ReadingData5$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData5$`Fall Percentile` == 47, 206, ReadingData5$`Fall - Fall Typical Growth Target`)
ReadingData5$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData5$`Fall Percentile` == 55, 209, ReadingData5$`Fall - Fall Typical Growth Target`)
ReadingData5$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData5$`Fall Percentile` == 85, 220, ReadingData5$`Fall - Fall Typical Growth Target`)


sum(is.na(ReadingData6$`Fall - Fall Typical Growth Target`))
ReadingData6 <- ReadingData6 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))

sum(is.na(ReadingData7$`Fall - Fall Typical Growth Target`))
ReadingData7 <- ReadingData7 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
ReadingData7miss <- ReadingData7[is.na(ReadingData7$`Fall - Fall Typical Growth Target`)==TRUE,]
ReadingData7$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData7$`Fall Percentile` == 22, 207, ReadingData7$`Fall - Fall Typical Growth Target`)
ReadingData7$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData7$`Fall Percentile` == 26, 209, ReadingData7$`Fall - Fall Typical Growth Target`)
ReadingData7$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData7$`Fall Percentile` == 28, 210, ReadingData7$`Fall - Fall Typical Growth Target`)
ReadingData7$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData7$`Fall Percentile` == 58, 218, ReadingData7$`Fall - Fall Typical Growth Target`)
ReadingData7$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData7$`Fall Percentile` == 60, 219, ReadingData7$`Fall - Fall Typical Growth Target`)
ReadingData7$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData7$`Fall Percentile` == 63, 220, ReadingData7$`Fall - Fall Typical Growth Target`)
ReadingData7$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData7$`Fall Percentile` == 82, 226, ReadingData7$`Fall - Fall Typical Growth Target`)


sum(is.na(ReadingData8$`Fall - Fall Typical Growth Target`))
ReadingData8 <- ReadingData8 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
ReadingData8miss <- ReadingData8[is.na(ReadingData8$`Fall - Fall Typical Growth Target`)==TRUE,]
ReadingData8$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData8$`Fall Percentile` == 25, 210, ReadingData8$`Fall - Fall Typical Growth Target`)
ReadingData8$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData8$`Fall Percentile` == 31, 212, ReadingData8$`Fall - Fall Typical Growth Target`)
ReadingData8$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData8$`Fall Percentile` == 33, 213, ReadingData8$`Fall - Fall Typical Growth Target`)
ReadingData8$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData8$`Fall Percentile` == 40, 216, ReadingData8$`Fall - Fall Typical Growth Target`)
ReadingData8$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData8$`Fall Percentile` == 53, 218, ReadingData8$`Fall - Fall Typical Growth Target`)
ReadingData8$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData8$`Fall Percentile` == 58, 221, ReadingData8$`Fall - Fall Typical Growth Target`)
ReadingData8$`Fall - Fall Typical Growth Target` <- ifelse(ReadingData8$`Fall Percentile` == 65, 223, ReadingData8$`Fall - Fall Typical Growth Target`)



ScienceData <-data[data$Subject == "Science",]
table(ScienceData$Grade)

ScienceData3 <- ScienceData[ScienceData$Grade == "3", ]
ScienceData4 <- ScienceData[ScienceData$Grade == "4", ]
ScienceData5 <- ScienceData[ScienceData$Grade == "5", ]
ScienceData6 <- ScienceData[ScienceData$Grade == "6", ]
ScienceData7 <- ScienceData[ScienceData$Grade == "7", ]
ScienceData8 <- ScienceData[ScienceData$Grade == "8", ]

sum(is.na(ScienceData3$`Fall - Fall Typical Growth Target`))
table(ScienceData3$`Fall - Fall Typical Growth Target`)
ScienceData3$`Fall - Fall Typical Growth Target`[is.na(ScienceData3$`Fall - Fall Typical Growth Target`)]<-190.5


sum(is.na(ScienceData4$`Fall - Fall Typical Growth Target`))
ScienceData4 <- ScienceData4 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
ScienceData4miss <- ScienceData4[is.na(ScienceData4$`Fall - Fall Typical Growth Target`)==TRUE,]
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 2, 172, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 4, 175, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 5, 178, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 6, 178, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 7, 178, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 9, 185, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 11, 185, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 17, 184, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 22, 187, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 24, 188, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 25, 188, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 30, 190, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 34, 191, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 81, 202, ScienceData4$`Fall - Fall Typical Growth Target`)
ScienceData4$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData4$`Fall Percentile` == 96, 210, ScienceData4$`Fall - Fall Typical Growth Target`)



sum(is.na(ScienceData5$`Fall - Fall Typical Growth Target`))
ScienceData5 <- ScienceData5 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
ScienceData5miss <- ScienceData5[is.na(ScienceData5$`Fall - Fall Typical Growth Target`)==TRUE,]
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 12, 192, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 14, 193, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 16, 194, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 18, 195, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 29, 196, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 31, 197, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 37, 199, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 40, 200, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 44, 200, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 47, 201, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 51, 202, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 55, 202, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 58, 203, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 62, 203, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 65, 203, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 68, 204, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 74, 206, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 83, 209, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 85, 210, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 87, 211, ScienceData5$`Fall - Fall Typical Growth Target`)
ScienceData5$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData5$`Fall Percentile` == 89, 213, ScienceData5$`Fall - Fall Typical Growth Target`)


sum(is.na(ScienceData6$`Fall - Fall Typical Growth Target`))
ScienceData6 <- ScienceData6 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))

sum(is.na(ScienceData7$`Fall - Fall Typical Growth Target`))
ScienceData7 <- ScienceData7 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
ScienceData7miss <- ScienceData7[is.na(ScienceData7$`Fall - Fall Typical Growth Target`)==TRUE,]
ScienceData7$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData7$`Fall Percentile` == 23, 203, ScienceData7$`Fall - Fall Typical Growth Target`)
ScienceData7$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData7$`Fall Percentile` == 34, 205, ScienceData7$`Fall - Fall Typical Growth Target`)
ScienceData7$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData7$`Fall Percentile` == 37, 206, ScienceData7$`Fall - Fall Typical Growth Target`)
ScienceData7$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData7$`Fall Percentile` == 41, 208, ScienceData7$`Fall - Fall Typical Growth Target`)
ScienceData7$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData7$`Fall Percentile` == 47, 209, ScienceData7$`Fall - Fall Typical Growth Target`)
ScienceData7$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData7$`Fall Percentile` == 73, 214, ScienceData7$`Fall - Fall Typical Growth Target`)
ScienceData7$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData7$`Fall Percentile` == 74, 215, ScienceData7$`Fall - Fall Typical Growth Target`)
ScienceData7$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData7$`Fall Percentile` == 78, 216, ScienceData7$`Fall - Fall Typical Growth Target`)
ScienceData7$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData7$`Fall Percentile` == 83, 217, ScienceData7$`Fall - Fall Typical Growth Target`)


sum(is.na(ScienceData8$`Fall - Fall Typical Growth Target`))
ScienceData8 <- ScienceData8 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
ScienceData8miss <- ScienceData8[is.na(ScienceData8$`Fall - Fall Typical Growth Target`)==TRUE,]
ScienceData8$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData8$`Fall Percentile` == 34, 208, ScienceData8$`Fall - Fall Typical Growth Target`)
ScienceData8$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData8$`Fall Percentile` == 37, 209, ScienceData8$`Fall - Fall Typical Growth Target`)
ScienceData8$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData8$`Fall Percentile` == 40, 210, ScienceData8$`Fall - Fall Typical Growth Target`)
ScienceData8$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData8$`Fall Percentile` == 72, 217, ScienceData8$`Fall - Fall Typical Growth Target`)
ScienceData8$`Fall - Fall Typical Growth Target` <- ifelse(ScienceData8$`Fall Percentile` == 77, 219, ScienceData8$`Fall - Fall Typical Growth Target`)


LanguageData <-data[data$Subject == "Language",]
table(LanguageData$Grade)


LanguageData2 <- LanguageData[LanguageData$Grade == "2", ]
LanguageData3 <- LanguageData[LanguageData$Grade == "3", ]
LanguageData4 <- LanguageData[LanguageData$Grade == "4", ]
LanguageData5 <- LanguageData[LanguageData$Grade == "5", ]
LanguageData6 <- LanguageData[LanguageData$Grade == "6", ]
LanguageData7 <- LanguageData[LanguageData$Grade == "7", ]
LanguageData8 <- LanguageData[LanguageData$Grade == "8", ]

sum(is.na(LanguageData3$`Fall - Fall Typical Growth Target`))
LanguageData3 <- LanguageData3 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))

sum(is.na(LanguageData4$`Fall - Fall Typical Growth Target`))
LanguageData4 <- LanguageData4 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
LanguageData4miss <- LanguageData4[is.na(LanguageData4$`Fall - Fall Typical Growth Target`)==TRUE,]
LanguageData4$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData4$`Fall Percentile` == 15, 185, LanguageData4$`Fall - Fall Typical Growth Target`)


sum(is.na(LanguageData5$`Fall - Fall Typical Growth Target`))
LanguageData5 <- LanguageData5 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
LanguageData5miss <- LanguageData5[is.na(LanguageData5$`Fall - Fall Typical Growth Target`)==TRUE,]
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 14, 197, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 17, 198, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 28, 201, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 33, 201, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 53, 205, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 56, 209, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 59, 209, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 61, 210, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 64, 212, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 67, 214, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 69, 215, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 72, 216, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 74, 216, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 76, 217, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 78, 217, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 80, 218, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 82, 219, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 84, 220, LanguageData5$`Fall - Fall Typical Growth Target`)
LanguageData5$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData5$`Fall Percentile` == 86, 221, LanguageData5$`Fall - Fall Typical Growth Target`)


sum(is.na(LanguageData6$`Fall - Fall Typical Growth Target`))
LanguageData6 <- LanguageData6 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))

sum(is.na(LanguageData7$`Fall - Fall Typical Growth Target`))
LanguageData7 <- LanguageData7 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
LanguageData7miss <- LanguageData7[is.na(LanguageData7$`Fall - Fall Typical Growth Target`)==TRUE,]
LanguageData7$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData7$`Fall Percentile` == 24, 208, LanguageData7$`Fall - Fall Typical Growth Target`)
LanguageData7$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData7$`Fall Percentile` == 65, 218, LanguageData7$`Fall - Fall Typical Growth Target`)
LanguageData7$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData7$`Fall Percentile` == 68, 219, LanguageData7$`Fall - Fall Typical Growth Target`)
LanguageData7$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData7$`Fall Percentile` == 70, 221, LanguageData7$`Fall - Fall Typical Growth Target`)
LanguageData7$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData7$`Fall Percentile` == 75, 223, LanguageData7$`Fall - Fall Typical Growth Target`)


sum(is.na(LanguageData8$`Fall - Fall Typical Growth Target`))
LanguageData8 <- LanguageData8 %>% group_by(`Fall Percentile`) %>%
  mutate(`Fall - Fall Typical Growth Target`=ifelse(is.na(`Fall - Fall Typical Growth Target`),median(`Fall - Fall Typical Growth Target`,na.rm=TRUE),`Fall - Fall Typical Growth Target`))
LanguageData8miss <- LanguageData8[is.na(LanguageData8$`Fall - Fall Typical Growth Target`)==TRUE,]
LanguageData8$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData8$`Fall Percentile` == 29, 211, LanguageData8$`Fall - Fall Typical Growth Target`)
LanguageData8$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData8$`Fall Percentile` == 34, 212, LanguageData8$`Fall - Fall Typical Growth Target`)
LanguageData8$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData8$`Fall Percentile` == 37, 213, LanguageData8$`Fall - Fall Typical Growth Target`)
LanguageData8$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData8$`Fall Percentile` == 39, 213, LanguageData8$`Fall - Fall Typical Growth Target`)
LanguageData8$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData8$`Fall Percentile` == 42, 214, LanguageData8$`Fall - Fall Typical Growth Target`)
LanguageData8$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData8$`Fall Percentile` == 45, 214, LanguageData8$`Fall - Fall Typical Growth Target`)
LanguageData8$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData8$`Fall Percentile` == 48, 214, LanguageData8$`Fall - Fall Typical Growth Target`)
LanguageData8$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData8$`Fall Percentile` == 53, 217, LanguageData8$`Fall - Fall Typical Growth Target`)
LanguageData8$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData8$`Fall Percentile` == 56, 218, LanguageData8$`Fall - Fall Typical Growth Target`)
LanguageData8$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData8$`Fall Percentile` == 86, 226, LanguageData8$`Fall - Fall Typical Growth Target`)
LanguageData8$`Fall - Fall Typical Growth Target` <- ifelse(LanguageData8$`Fall Percentile` == 89, 228, LanguageData8$`Fall - Fall Typical Growth Target`)



# Joining by entry
sum(is.na(data$`Fall - Fall Typical Growth Target`))
data$`Fall - Fall Typical Growth Target`[match(MathDataKG$entry, data$entry)] <- MathDataKG$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(MathData1$entry, data$entry)] <- MathData1$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(MathData2$entry, data$entry)] <- MathData2$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(MathData3$entry, data$entry)] <- MathData3$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(MathData4$entry, data$entry)] <- MathData4$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(MathData5$entry, data$entry)] <- MathData5$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(MathData6$entry, data$entry)] <- MathData6$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(MathData7$entry, data$entry)] <- MathData7$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(MathData8$entry, data$entry)] <- MathData8$`Fall - Fall Typical Growth Target`

data$`Fall - Fall Typical Growth Target`[match(ReadingDataKG$entry, data$entry)] <- ReadingDataKG$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ReadingData1$entry, data$entry)] <- ReadingData1$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ReadingData2$entry, data$entry)] <- ReadingData2$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ReadingData3$entry, data$entry)] <- ReadingData3$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ReadingData4$entry, data$entry)] <- ReadingData4$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ReadingData5$entry, data$entry)] <- ReadingData5$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ReadingData6$entry, data$entry)] <- ReadingData6$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ReadingData7$entry, data$entry)] <- ReadingData7$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ReadingData8$entry, data$entry)] <- ReadingData8$`Fall - Fall Typical Growth Target`

data$`Fall - Fall Typical Growth Target`[match(ScienceData3$entry, data$entry)] <- ScienceData3$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ScienceData4$entry, data$entry)] <- ScienceData4$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ScienceData5$entry, data$entry)] <- ScienceData5$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ScienceData6$entry, data$entry)] <- ScienceData6$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ScienceData7$entry, data$entry)] <- ScienceData7$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(ScienceData8$entry, data$entry)] <- ScienceData8$`Fall - Fall Typical Growth Target`

data$`Fall - Fall Typical Growth Target`[match(LanguageData2$entry, data$entry)] <- LanguageData2$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(LanguageData3$entry, data$entry)] <- LanguageData3$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(LanguageData4$entry, data$entry)] <- LanguageData4$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(LanguageData5$entry, data$entry)] <- LanguageData5$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(LanguageData6$entry, data$entry)] <- LanguageData6$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(LanguageData7$entry, data$entry)] <- LanguageData7$`Fall - Fall Typical Growth Target`
data$`Fall - Fall Typical Growth Target`[match(LanguageData8$entry, data$entry)] <- LanguageData8$`Fall - Fall Typical Growth Target`

sum(is.na(data$`Fall - Fall Typical Growth Target`))

# Growth Target Met
sum(is.na(data$`Met F-F Typical Growth Target`))
data$`Met F-F Typical Growth Target` <- ifelse(data$`Fall RIT` >= data$`Fall - Fall Typical Growth Target`, 1, data$`Met F-F Typical Growth Target`)
data$`Met F-F Typical Growth Target` <- ifelse(data$`Fall RIT` < data$`Fall - Fall Typical Growth Target`, 0, data$`Met F-F Typical Growth Target`)

# Create backup data including missing values
databackup <- data

#Delete entries with NA from working dataset
data <- na.omit(data)

# Checks if there are any NAs left
sapply(data[,1:16], function(x) sum(is.na(x)))
sapply(databackup[,1:16], function(x) sum(is.na(x)))


# first change Fall columns to numeric. These columns were originally character types.
str(data)

# Fall columns should be converted into numeric data types
data$`Fall Percentile` <- as.numeric(data$`Fall Percentile`)
data$`Fall RIT` <- as.numeric(data$`Fall RIT`)
data$`Fall - Fall Typical Growth Target` <- as.numeric(data$`Fall - Fall Typical Growth Target`)

str(data)

# Then change other character columns to factor
data <- as.data.frame(unclass(data), stringsAsFactors = TRUE)

str(data)    



### descriptive statistics
summary(data)

# categorical variables (how do I put count and percentages showing proportions above each bar chart? couldn't figure it out)

# Academic Year
ggplot(data = data) + geom_bar(mapping = aes(, x=Academic_Year, fill = Academic_Year)) + theme(legend.position = "none")

# Grade
ggplot(data = data) + geom_bar(mapping = aes(, x=Grade, fill = Grade)) + theme(legend.position = "none")

# Race/Ethnicity
ggplot(data = data) + geom_bar(mapping = aes(, x=Race.Ethnicity, fill = Race.Ethnicity)) + theme(legend.position = "none")

# Gender
ggplot(data = data) + geom_bar(mapping = aes(, x=Gender, fill = Gender)) + theme(legend.position = "none")

# ED
ggplot(data = data) + geom_bar(mapping = aes(, x=ED, fill = ED)) + theme(legend.position = "none")

# LEP
ggplot(data = data) + geom_bar(mapping = aes(, x=LEP, fill = LEP)) + theme(legend.position = "none")

# SPED
ggplot(data = data) + geom_bar(mapping = aes(, x=SPED, fill = SPED)) + theme(legend.position = "none")

# Subject
ggplot(data = data) + geom_bar(mapping = aes(, x=Subject, fill = Subject)) + theme(legend.position = "none")

# Met Typical Growth Target
ggplot(data = data) + geom_bar(mapping = aes(, x=Met.F.F.Typical.Growth.Target, fill = Met.F.F.Typical.Growth.Target)) + theme(legend.position = "none")
summary(data$Met.F.F.Typical.Growth.Target)
sum(is.na(data$Met.F.F.Typical.Growth.Target))
table(data$Met.F.F.Typical.Growth.Target)
prop.table(table(data$Met.F.F.Typical.Growth.Target))

# numeric variables
data %>%
  summarise(mean_Fall.RIT = mean(Fall.RIT, na.rm = TRUE), mean_Typical.Growth.Target = mean(Fall...Fall.Typical.Growth.Target, na.rm = TRUE), sd_Fall.RIT = sd(Fall.RIT, na.rm = TRUE), sd_Typical.Growth.Target = sd(Fall...Fall.Typical.Growth.Target, na.rm = TRUE))





dog<- with(data, table(Grade, `Met.F.F.Typical.Growth.Target`))
write.csv(dog, "dog.csv")

tibbb <- read.csv("dog.csv")

##Overall Grade
dog<- with(data, table(Grade, `Met.F.F.Typical.Growth.Target`))
write.csv(dog, "dog.csv")
GradeGT <- read.csv("dog.csv")
colnames(GradeGT)[1] <- "Grade"
GradeGT$Pass <- GradeGT$X1/(GradeGT$X0+ GradeGT$X1)


##Overall Subject
subpass<- with(data, table(Subject, `Met.F.F.Typical.Growth.Target`))
write.csv(subpass, "subpass.csv")
SubjectGT <- read.csv("subpass.csv")
colnames(SubjectGT)[1] <- "Subject"
SubjectGT$Pass <- SubjectGT$X1/(SubjectGT$X0+ SubjectGT$X1)







# just added a number in front of variable name
LanguageData2 <-data[data$Subject == "Language",]
table(LanguageData$Grade)


LanguageData22 <- LanguageData2[LanguageData2$Grade == "2", ]
LanguageData32<- LanguageData2[LanguageData2$Grade == "3", ]
LanguageData42 <- LanguageData2[LanguageData2$Grade == "4", ]
LanguageData52 <- LanguageData2[LanguageData2$Grade == "5", ]
LanguageData62 <- LanguageData2[LanguageData2$Grade == "6", ]
LanguageData72 <- LanguageData2[LanguageData2$Grade == "7", ]
LanguageData82 <- LanguageData2[LanguageData2$Grade == "8", ]

leveneTest(Fall.RIT~Race.Ethnicity, data=LanguageData22) #0.2734
leveneTest(Fall.RIT~Race.Ethnicity, data=LanguageData32) #0.3041
leveneTest(Fall.RIT~Race.Ethnicity, data=LanguageData42) #0.3221
leveneTest(Fall.RIT~Race.Ethnicity, data=LanguageData52) #0.7524
leveneTest(Fall.RIT~Race.Ethnicity, data=LanguageData62) #0.6195
leveneTest(Fall.RIT~Race.Ethnicity, data=LanguageData72) #0.5103
leveneTest(Fall.RIT~Race.Ethnicity, data=LanguageData82) #0.2671

summary(outputLRA5 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=LanguageData52)) #5.64e-13
summary(outputLRA6 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=LanguageData62)) #<2e-16
summary(outputLRA7 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=LanguageData72)) #0.0104
summary(outputLRA8 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=LanguageData82)) #0.000179



###Comparisons we want to make for grades 5-8 and each subject
# RIT Score vs Academic Year (isaiah)
# RIT Score vs Race / Ethnicity AND Academic Year (rei)
# RIT Score vs Gender AND Academic Year (isaiah)

# list any significant values here (we can reference this to explain any trends for the report)
### LANGUAGE RIT Score vs Race / Ethnicity AND Academic Year
LanguageData2 <-data[data$Subject == "Language",]
table(LanguageData$Grade)

LanguageData52 <- LanguageData2[LanguageData2$Grade == "5", ]
LanguageData62 <- LanguageData2[LanguageData2$Grade == "6", ]
LanguageData72 <- LanguageData2[LanguageData2$Grade == "7", ]
LanguageData82 <- LanguageData2[LanguageData2$Grade == "8", ]

# Levene's test tests for equality of variance. If p value is below 0.05, we CANNOT proceed
# with the anova test
# Language with Race.Ethnicity

leveneTest(Fall.RIT~Race.Ethnicity, data=LanguageData52) #0.7524
leveneTest(Fall.RIT~Race.Ethnicity, data=LanguageData62) #0.6195
leveneTest(Fall.RIT~Race.Ethnicity, data=LanguageData72) #0.5103
leveneTest(Fall.RIT~Race.Ethnicity, data=LanguageData82) #0.2671

#LANGUAGE RIT Score vs Race/Ethnicity AND Academic Year   
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=LanguageData52) #0.006921
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=LanguageData62) #0.02824
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=LanguageData72) #0.05415
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=LanguageData82) #0.06016

summary(outputLRA5 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=LanguageData52)) #5.64e-13
summary(outputLRA6 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=LanguageData62)) #<2e-16
summary(outputLRA7 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=LanguageData72)) #0.0104
summary(outputLRA8 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=LanguageData82)) #0.000179

TukeyHSD(outputLRA5) # done
TukeyHSD(outputLRA6) # done
TukeyHSD(outputLRA7) # done
TukeyHSD(outputLRA8) # done

# There were significant differences (<0.05) when: 
# Asians compared to any other race. asians score higher in general (5,6)
# hispanic/latinx from 2021 to 2020/2019. hispanic/latinx heavily influenced by covid. Their performance progressively gets worse. (5,6)
#opposite for Black/African american. They performed significantly better (5)
# Hispanic/latinx performed worse than black/african americans in 2021 (5)
# Hispanic/latinx performed better than black/african american in 2019 (6)
# no significant values for 7th/8th graders. Generally same trends though

### SCIENCE RIT Score vs Race / Ethnicity AND Academic Year
ScienceData2 <-data[data$Subject == "Science",]
table(ScienceData$Grade)

ScienceData52 <- ScienceData2[ScienceData2$Grade == "5", ]
ScienceData62 <- ScienceData2[ScienceData2$Grade == "6", ]
ScienceData72 <- ScienceData2[ScienceData2$Grade == "7", ]
ScienceData82 <- ScienceData2[ScienceData2$Grade == "8", ]

leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=ScienceData52) #0.1339
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=ScienceData62) #1.65e-05
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=ScienceData72) #0.0001283
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=ScienceData82) #0.02186

summary(outputSRA5 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=ScienceData52)) #2.53e-15
summary(outputSRA6 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=ScienceData62)) #<2e-16
summary(outputSRA7 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=ScienceData72)) #8.26e-05
summary(outputSRA8 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=ScienceData82)) #0.000655

TukeyHSD(outputSRA5) # done
TukeyHSD(outputSRA6) # done
TukeyHSD(outputSRA7) # done
TukeyHSD(outputSRA8) # done

# There were significant differences (<0.05) when: 
# black and asians 2019 (5,6)
# black and asians 2020 (8)
# hispanic/latinx and asians 2019, 2021, 2020 (8)
# two or more races and asians 2021
# white and asians 2021
# hispanic performed worse than black in 2021 (5)
# hispanic performed better than black in 2019 (6)
# hispanic performed worse in 2021 compared to 2020 (6)
# no significant values for 7th graders

### READING RIT Score vs Race / Ethnicity AND Academic Year
ReadingData2 <-data[data$Subject == "Reading",]
table(ReadingData$Grade)


ReadingData52 <- ReadingData2[ReadingData2$Grade == "5", ]
ReadingData62 <- ReadingData2[ReadingData2$Grade == "6", ]
ReadingData72 <- ReadingData2[ReadingData2$Grade == "7", ]
ReadingData82 <- ReadingData2[ReadingData2$Grade == "8", ]   

leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=ReadingData52) #0.01811
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=ReadingData62) #7.184e-05
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=ReadingData72) #3.32e-06
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=ReadingData82) #0.03913

summary(outputRRA5 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=ReadingData52)) #<2e-16
summary(outputRRA6 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=ReadingData62)) #<2e-16
summary(outputRRA7 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=ReadingData72)) #9.11e-09
summary(outputRRA8 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=ReadingData82)) #4.35e-09

TukeyHSD(outputRRA5) # done
TukeyHSD(outputRRA6) # done
TukeyHSD(outputRRA7) # done
TukeyHSD(outputRRA8) # done

# There were significant differences (<0.05) when:
# Black and asians 2019 (5,6), 2020 (6)
# hispanic and asians 2019 (5,6), 2020 (6), 2021 (5,6)
# hispanic did worse in 2021 than 2020 (5,6,7,8)
# hispanic did worse in 2021 than 2019 (6,7,8)
# white and asians 2021 (5)
# hispanic did worse than black in 2021 (5)
#  black did better in 2020 compared to 2019
#  black did better in 2021 compared to 2020 (8)

### MATH RIT Score vs Race / Ethnicity AND Academic Year
MathData2 <-data[data$Subject == "Mathematics",]
table(MathData$Grade)

MathData52 <- MathData2[MathData2$Grade == "5", ]
MathData62 <- MathData2[MathData2$Grade == "6", ]
MathData72 <- MathData2[MathData2$Grade == "7", ]
MathData82 <- MathData2[MathData2$Grade == "8", ]

leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=MathData52) #0.04197
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=MathData62) #0.003069
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=MathData72) #0.006657
leveneTest(Fall.RIT~Race.Ethnicity:Academic_Year, data=MathData82) #7.655e-05

summary(outputMRA5 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=MathData52)) #<2e-16
summary(outputMRA6 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=MathData62)) #<2e-16
summary(outputMRA7 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=MathData72)) #<2e-16
summary(outputMRA8 <- aov(Fall.RIT~Race.Ethnicity:Academic_Year, data=MathData82)) #4.27e-10

TukeyHSD(outputMRA5) # done
TukeyHSD(outputMRA6) # done
TukeyHSD(outputMRA7) # done
TukeyHSD(outputMRA8) # done

# There were significant differences (<0.05) when:
# Asian and American indian 2019 (5)
# black and asian 2019 (5,6,7), 2021 (5,6,7,8), 2020 (6,7,8)
# hispanic and asian 2019 (5,6,7), 2021 (5,6,7,8), 2020 (6,7,8)
# hispanic performed better than black 2019 (5), 2020 (6,7), 2021 (7)
# hispanic performed worse in 2020 compared to 2019 (5)
# hispanic performed worse in 2021 compared to 2019 (5,6)
# hispanic performed worse in 2021 compared to 2020 (5,7)
# two or more and asian 2021 (5,8), 2019 (7), 2020 (8)
# white and asian (5), 2021 (8)





Findings from  #RIT Score vs Academic Year (by subject)

Language

Language Grade 5 Academic Year:  
  
  Grade 5 students had worse Language RIT scores in 2021 than in 2019 (roughly 1.7 point average) and a larger difference between 2021 and 2020 (roughly 2.6). Overall scores were worse in 2021 than in the previous years.


Language Grade 6 Academic Year:  
  
  Grade 6 students had better Language RIT scores in 2020 than 2019; however, they had worse scores in 2021 compared to 2019 and consequently even worse scores when 2021 was compared to 2020.

Language Grade 7 Academic Year:  
  
  There were no significant differences between scores for 7th Graders

Language Grade 8 Academic Year:  
  
  8th Graders had worse scores in 2021 when compared to 2019 ; they also scored lower in 2021 when compared to 2020 by roughly the same amount.


Science

Science Grade 5 Academic Year:  
  
  No significant difference in scored among the years

Science Grade 6 Academic Year:  
  
  6th graders scored higher on average in 2020 than 2019; in 2021 however they scored lower than in 2019--and even lower than 2020.

Science Grade 7 Academic Year:  
  
  No significant difference for 7th graders

Science Grade 8 Academic Year:  
  
  No significant difference for 8th graders


Reading

Reading Grade 5 Academic Year:  
  
  5th graders did worse in 2021 than in 2019; they did worse in 2021 than in 2020 but not as low.

Reading Grade 6 Academic Year:
  
  6th graders performed better in 2020 than in 2019; however they performed worse in 2021 compared to 2019. In 2021 they also performed worse than 2020

Reading Grade 7 Academic Year:
  
  7th graders in 2021 performed worse than they did in 2019 and worse than they did in 2020.

Reading Grade 8 Academic Year:
  
  8th graders in 2021 performed worse than they did in 2019 and worse than they did in 2020.


Math

Math Grade 5 Academic Year:  
  
  5th graders performed worse in 2020 than in 2019; worse in 2021 than in 2020; and performed worse in 2021 than in 2019.

Math Grade 6 Academic Year:  
  
  6th graders performed worse in 2021 than in 2019 and much worse in 2021 than in 2020

Math Grade 7 Academic Year:  
  
  7th graders performed worse in 2021 than in 2019 and much worse in 2021 than in 2020

Math Grade 8 Academic Year:  
  
  8th graders performed worse in 2021 than in 2019; other comparisons did not yield significant results.


Code for RIT vs Academic Year: 
  
  ###ANOVA forAcademic Year#####

##Language

#only one significant, went ahead with anova anyways 
leveneTest(Fall.RIT~Academic_Year, data=LanguageData52)
leveneTest(Fall.RIT~Academic_Year, data=LanguageData62)
leveneTest(Fall.RIT~Academic_Year, data=LanguageData72)
leveneTest(Fall.RIT~Academic_Year, data=LanguageData82)

leveneTest(Fall.RIT~Academic_Year, data=ScienceData52)
leveneTest(Fall.RIT~Academic_Year, data=ScienceData62)
leveneTest(Fall.RIT~Academic_Year, data=ScienceData72)
leveneTest(Fall.RIT~Academic_Year, data=ScienceData82)

leveneTest(Fall.RIT~Academic_Year, data=ReadingData52)
leveneTest(Fall.RIT~Academic_Year, data=ReadingData62)
leveneTest(Fall.RIT~Academic_Year, data=ReadingData72)
leveneTest(Fall.RIT~Academic_Year, data=ReadingData82)

leveneTest(Fall.RIT~Academic_Year, data=MathData52)#0.06952
leveneTest(Fall.RIT~Academic_Year, data=MathData62)
leveneTest(Fall.RIT~Academic_Year, data=MathData72)
leveneTest(Fall.RIT~Academic_Year, data=MathData82)

#Language,Academic Year,Grade 5
summary(outputLA5 <- aov(Fall.RIT~Academic_Year, data=LanguageData52))
TukeyHSD(outputLA5)

#Language,Academic Year,Grade 6
summary(outputLA6 <- aov(Fall.RIT~Academic_Year, data=LanguageData62))
TukeyHSD(outputLA6)

#Language,Academic Year,Grade 7
summary(outputLA7 <- aov(Fall.RIT~Academic_Year, data=LanguageData72))
TukeyHSD(outputLA7)

#Language,Academic Year,Grade 8
summary(outputLA8 <- aov(Fall.RIT~Academic_Year, data=LanguageData82))
TukeyHSD(outputLA8)


#Science

#Science,Academic Year,Grade 5
summary(outputSA5 <- aov(Fall.RIT~Academic_Year, data=ScienceData52))
TukeyHSD(outputSA5)

#Science,Academic Year,Grade 6
summary(outputSA6 <- aov(Fall.RIT~Academic_Year, data=ScienceData62))
TukeyHSD(outputSA6)

#Science,Academic Year,Grade 7
summary(outputSA7 <- aov(Fall.RIT~Academic_Year, data=ScienceData72))
TukeyHSD(outputSA7)

#Science,Academic Year,Grade 8
summary(outputSA8 <- aov(Fall.RIT~Academic_Year, data=ScienceData82))
TukeyHSD(outputSA8)


#Reading


#Reading,Academic Year,Grade 5
summary(outputRA5 <- aov(Fall.RIT~Academic_Year, data=ReadingData52))
TukeyHSD(outputRA5)

#Reading,Academic Year,Grade 6
summary(outputRA6 <- aov(Fall.RIT~Academic_Year, data=ReadingData62))
TukeyHSD(outputRA6)

#Reading,Academic Year,Grade 7
summary(outputRA7 <- aov(Fall.RIT~Academic_Year, data=ReadingData72))
TukeyHSD(outputRA7)

#Reading,Academic Year,Grade 8
summary(outputRA8 <- aov(Fall.RIT~Academic_Year, data=ReadingData82))
TukeyHSD(outputRA8)


#Math

#Math,Academic Year,Grade 5
summary(outputMA5 <- aov(Fall.RIT~Academic_Year, data=MathData52))
TukeyHSD(outputMA5)

#Math,Academic Year,Grade 6
summary(outputMA6 <- aov(Fall.RIT~Academic_Year, data=MathData62))
TukeyHSD(outputMA6)

#Math,Academic Year,Grade 7
summary(outputMA7 <- aov(Fall.RIT~Academic_Year, data=MathData72))
TukeyHSD(outputMA7)

#Math,Academic Year,Grade 8
summary(outputMA8 <- aov(Fall.RIT~Academic_Year, data=MathData82))
TukeyHSD(outputMA8)


Findings from RIT Score vs Gender and Academic Year (by subject)


Gender: Academic_Year


Language

Language Grade 5 Gender and Academic Year:
  
  5th grade Males performed worse than females in 2019 by a  language RIT point difference of roughly 6.5

5th grade Males performed worse than females in 2020 by a  language RIT point difference of roughly 4

5th grade Females in 2021 performed worse than females in 2019 on the language test

5th grade Females in 2021 performed worse than females in 2020 on the language test

Males had lower RIT scores than females in 2021 but only by roughly 3.5 points.

Males scored lower in 2021 than they did in 2020




Language Grade 6 Gender and Academic Year:
  
  
  Males and Females had of 9 point RIT language difference between in 2019; Females did better.

Females did worse in 2021 than 2019

Males did better in 2020 than in 2019

Males did worse than females in 2020 by only 5 points this time

Females did worse in 2021 than in 2020

Males did worse in 2021 than in 2020

Males did roughly 5.5 points worse in 2021



Language Grade 7 Gender and Academic Year:
  
  
  Males did worse than females in 2019

Males did worse than females in 2020 (by a worse margin)

Males did worse than than females in 2021



Language Grade 8 Gender and Academic Year:
  
  
  Males did worse than females in 2019

Males did worse in 2021 than 2020

Males idd worse in 2021 than females in 2021 by roughly 7 points



Science

Science Grade 5 Gender and Academic Year:
  
  No statistically significant differences


Science Grade 6 Gender and Academic Year:
  
  Females did better in 2020 than 2019

Females did worse in 2021 than in 2019

Females did worse in 2021 than in 2020

Males did worse in 2021 than in 2020


Science Grade 7 Gender and Academic Year:
  Nothing is significant


Science Grade 8 Gender and Academic Year:
  Nothing is significant


Reading

Reading Gender Academic Year Grade 5

Males performed worse than girls in 2019 by a magnitude of 4

Females in 2021 did 4 RIT points worse than females in 2019

Males in 2021 did roughly 3 RIT points worse than in 2019

Females did worse in 2021 than 2020

Males in 2021 did worse than females in 2021 by magnitude of 3


Reading Gender Academic Year Grade 6

Males performed worse than girls in 2019 by a magnitude of 5

Females in 2021 did 3.4 RIT points worse than in 2019

Males did 4.5 points worse than females in 2020

Females did 4.7 points worse in 2021 than in 2020

Males in 2021 did 3 points worse than in 2020

Males in 2021 did 3 points worse than females in 2021


Reading Gender Academic Year Grade 7


Males in 2019 did 3.7 worse than females in 2019

Males in 2021 did 4 points worse than in 2019

Males in 2020 did 5.8 points worse than females in 2020

Females in 2021 did 2 points worse than they did 2020

Males did 2 points worse in 2021 than in 2020

Males in 2021 were 5.7 points worse than females in 2021


Reading Gender Academic Year Grade 8

Males in 2019 were 4.7 points worse than females in 2019

Males in 2021 did 3.8 points worse than they did in 2019

Males in 2020 did 4 pints worse than females in 2020

Males in 2021 did 4 points worse than in 2020

Males in 2021 did 6.5 points worse than females in 2021


Math

Math Gender Academic Year Grade 5

Females in 2020 did worse than females in 2019 by 2 points

Females in 2021 did 3.5 points worse than they did in 2019

Males did 3 points worse in 2021  than they did in 2019



Math Gender Academic Year Grade 6

Males scored 1.7 points lower in 2019 compared to females

Females scored 3.6 points lower in 2021 than they did in 2019

Males scored 3.17 points lower in 2021 compared to 2019

Females scored 3.7 points lower in 2021 than in 2020

Males performed 4 pints worse in 2021 than in 2020


Math Gender Academic Year Grade 7

Females did 2.4 points worse in 2021 than in 2019

Males did 4.2 points worse in 2021 than in 2019

Males did 2.6 points worse in 2020 than in 2020

Females did 4 points worse in 2021 than they did in 2020

Males did 3.7 points worse in 2021 than they did in 2020


Math Gender Academic Year Grade 8

Males did 2.7 points worse in 2021 than they did in 2019


Code:
  
  ###ANOVA for Gender:Academic Year#####


#all signficant
leveneTest(Fall.RIT~Gender:Academic_Year, data=LanguageData52)
leveneTest(Fall.RIT~Gender:Academic_Year, data=LanguageData62)
leveneTest(Fall.RIT~Gender:Academic_Year, data=LanguageData72)
leveneTest(Fall.RIT~Gender:Academic_Year, data=LanguageData82)

leveneTest(Fall.RIT~Gender:Academic_Year, data=ScienceData52)
leveneTest(Fall.RIT~Gender:Academic_Year, data=ScienceData62)
leveneTest(Fall.RIT~Gender:Academic_Year, data=ScienceData72)
leveneTest(Fall.RIT~Gender:Academic_Year, data=ScienceData82)

leveneTest(Fall.RIT~Gender:Academic_Year, data=ReadingData52)
leveneTest(Fall.RIT~Gender:Academic_Year, data=ReadingData62)
leveneTest(Fall.RIT~Gender:Academic_Year, data=ReadingData72)
leveneTest(Fall.RIT~Gender:Academic_Year, data=ReadingData82)

leveneTest(Fall.RIT~Gender:Academic_Year, data=MathData52)
leveneTest(Fall.RIT~Gender:Academic_Year, data=MathData62)
leveneTest(Fall.RIT~Gender:Academic_Year, data=MathData72)
leveneTest(Fall.RIT~Gender:Academic_Year, data=MathData82)


#Language

#Language,Academic Year,Grade 5
summary(outputLGA5 <- aov(Fall.RIT~Gender:Academic_Year, data=LanguageData52))
TukeyHSD(outputLGA5)

#Language,Academic Year,Grade 6
summary(outputLGA6 <- aov(Fall.RIT~Gender:Academic_Year, data=LanguageData62))
TukeyHSD(outputLGA6)

#Language,Academic Year,Grade 7
summary(outputLGA7 <- aov(Fall.RIT~Gender:Academic_Year, data=LanguageData72))
TukeyHSD(outputLGA7)

#Language,Academic Year,Grade 8
summary(outputLGA8 <- aov(Fall.RIT~Gender:Academic_Year, data=LanguageData82))
TukeyHSD(outputLGA8)


#Science

#Science ,Academic Year,Grade 5
summary(outputSGA5 <- aov(Fall.RIT~Gender:Academic_Year, data=ScienceData52))
TukeyHSD(outputSGA5)

#Science,Academic Year,Grade 6
summary(outputSGA6 <- aov(Fall.RIT~Gender:Academic_Year, data=ScienceData62))
TukeyHSD(outputSGA6)

#Science,Academic Year,Grade 7
summary(outputSGA7 <- aov(Fall.RIT~Gender:Academic_Year, data=ScienceData72))
TukeyHSD(outputSGA7)

#Science,Academic Year,Grade 8
summary(outputSGA8 <- aov(Fall.RIT~Gender:Academic_Year, data=ScienceData82))
TukeyHSD(outputSGA8)


#Reading

#Reading,Academic Year,Grade 5
summary(outputRGA5 <- aov(Fall.RIT~Gender:Academic_Year, data=ReadingData52))
TukeyHSD(outputRGA5)

#Reading,Academic Year,Grade 6
summary(outputRGA6 <- aov(Fall.RIT~Gender:Academic_Year, data=ReadingData62))
TukeyHSD(outputRGA6)

#Reading,Academic Year,Grade 7
summary(outputRGA7 <- aov(Fall.RIT~Gender:Academic_Year, data=ReadingData72))
TukeyHSD(outputRGA7)

#Reading,Academic Year,Grade 8
summary(outputRGA8 <- aov(Fall.RIT~Gender:Academic_Year, data=ReadingData82))
TukeyHSD(outputRGA8)


#Math

#Math,Academic Year,Grade 5
summary(outputMGA5 <- aov(Fall.RIT~Gender:Academic_Year, data=MathData52))
TukeyHSD(outputMGA5)

#Math,Academic Year,Grade 6
summary(outputMGA6 <- aov(Fall.RIT~Gender:Academic_Year, data=MathData62))
TukeyHSD(outputMGA6)

#Math,Academic Year,Grade 7
summary(outputMGA7 <- aov(Fall.RIT~Gender:Academic_Year, data=MathData72))
TukeyHSD(outputMGA7)

#Math,Academic Year,Grade 8
summary(outputMGA8 <- aov(Fall.RIT~Gender:Academic_Year, data=MathData82))
TukeyHSD(outputMGA8)




data19 <- data[data$Academic_Year == "19",]
data20 <- data[data$Academic_Year == "20",]
data21 <- data[data$Academic_Year == "21",]


dog<- with(data19, table(Grade, `Met.F.F.Typical.Growth.Target`))
write.csv(dog, "dog.csv")
GradeGT19 <- read.csv("dog.csv")
colnames(GradeGT19)[1] <- "Grade"
GradeGT19$Pass <- GradeGT19$X1/(GradeGT19$X0+ GradeGT19$X1)

dog<- with(data20, table(Grade, `Met.F.F.Typical.Growth.Target`))
write.csv(dog, "dog.csv")
GradeGT20 <- read.csv("dog.csv")
colnames(GradeGT20)[1] <- "Grade"
GradeGT20$Pass <- GradeGT20$X1/(GradeGT20$X0+ GradeGT20$X1)

dog<- with(data21, table(Grade, `Met.F.F.Typical.Growth.Target`))
write.csv(dog, "dog.csv")
GradeGT21 <- read.csv("dog.csv")
colnames(GradeGT21)[1] <- "Grade"
GradeGT21$Pass <- GradeGT21$X1/(GradeGT21$X0+ GradeGT21$X1)


subpass<- with(data19, table(Subject, `Met.F.F.Typical.Growth.Target`))
write.csv(subpass, "subpass.csv")
SubjectGT19 <- read.csv("subpass.csv")
colnames(SubjectGT19)[1] <- "Subject"
SubjectGT19$Pass <- SubjectGT19$X1/(SubjectGT19$X0+ SubjectGT19$X1)

subpass<- with(data20, table(Subject, `Met.F.F.Typical.Growth.Target`))
write.csv(subpass, "subpass.csv")
SubjectGT20 <- read.csv("subpass.csv")
colnames(SubjectGT20)[1] <- "Subject"
SubjectGT20$Pass <- SubjectGT20$X1/(SubjectGT20$X0+ SubjectGT20$X1)

subpass<- with(data21, table(Subject, `Met.F.F.Typical.Growth.Target`))
write.csv(subpass, "subpass.csv")
SubjectGT21 <- read.csv("subpass.csv")
colnames(SubjectGT21)[1] <- "Subject"
SubjectGT21$Pass <- SubjectGT21$X1/(SubjectGT21$X0+ SubjectGT21$X1)

spass <- data.frame("Subject" = SubjectGT19$Subject,
                    "2019" = SubjectGT19$Pass,
                    "2020" = SubjectGT20$Pass,
                    "2021" = SubjectGT21$Pass)

gpass <- data.frame("Subject" = GradeGT19$Grade,
                    "2019" = GradeGT19$Pass,
                    "2020" = GradeGT20$Pass,
                    "2021" = GradeGT21$Pass)




gpass2 <- data.frame(t(gpass[-1]))
colnames(gpass2) <- gpass[, 1]

gpass2$Year <- seq(2019:2021)
gpass2$Year <- ifelse(gpass2$Year == 1, 2019, 
                      ifelse(gpass2$Year == 2, 2020, 
                             2021))
gpass2$`1`

gradeplot <- ggplot(data = gpass2, aes(x= Year))
gradeplot <- gradeplot + geom_line(aes(y = KG), color = "blue")
gradeplot <- gradeplot + geom_point(aes(y = KG), color = "blue")
gradeplot <- gradeplot + geom_line(aes(y = `1`), color = "orange")
gradeplot <- gradeplot + geom_point(aes(y = `1`), color = "orange")
gradeplot <- gradeplot + geom_line(aes(y = `2`), color = "red")
gradeplot <- gradeplot + geom_point(aes(y = `2`), color = "red")
gradeplot <- gradeplot + geom_line(aes(y = `3`), color = "yellow")
gradeplot <- gradeplot + geom_point(aes(y = `3`), color = "yellow")
gradeplot <- gradeplot + geom_line(aes(y = `4`), color = "green")
gradeplot <- gradeplot + geom_point(aes(y = `4`), color = "green")
gradeplot <- gradeplot + geom_line(aes(y = `5`), color = "maroon")
gradeplot <- gradeplot + geom_point(aes(y = `5`), color = "maroon")
gradeplot <- gradeplot + geom_line(aes(y = `6`), color = "purple")
gradeplot <- gradeplot + geom_point(aes(y = `6`), color = "purple")
gradeplot <- gradeplot + geom_line(aes(y = `7`), color = "navy")
gradeplot <- gradeplot + geom_point(aes(y = `7`), color = "navy")
gradeplot <- gradeplot + geom_line(aes(y = `8`), color = "pink")
gradeplot <- gradeplot + geom_point(aes(y = `8`), color = "pink")
gradeplot <- gradeplot + theme_classic()
gradeplot <- gradeplot + scale_color_manual(name = "variable",
                                            labels = c("KG", "1", "2", "3", "4", "5", "6", "7", "8"),
                                            values = c("blue", "orange", "red", "yellow", "green", "maroon", "purple", "navy", "pink"))

gradeplot


