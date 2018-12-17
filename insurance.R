install.packages("readr")
install.packages("tidyverse")
install.packages("leaps")

library(readr)
library(tidyverse)
library(leaps)
library(plotly)

#goal of this 

#49% of females of insurance holders
#51% of males of insurance holders

#Dataset: Description of columns
#age: age of primary beneficiary
#sex: insurance contractor gender, female, male
#bmi: Body mass index, providing an understanding of body, weights that are relatively high or low relative to height, 
#objective index of body weight (kg / m ^ 2) using the ratio of height to weight, ideally 18.5 to 24.9
#children: Number of children covered by health insurance / Number of dependents
#smoker: Smoking
#region: the beneficiary's residential area in the US, northeast, southeast, southwest, northwest.
#charges: Individual medical costs billed by health insurance


#set working directory
setwd("D:/Projects/DataScience/Insurance")

#Read csv file.
insurance<-read_csv("insurance.csv")

datatable(insurance, colnames = c('Age','Sex','BMI','Children','Smoker','Region','Charges'))

#summary of dataset
summary(insurance)

#convert male and female to 1 and 0. Convert smoker value yes and no to 1 and 0.
insurance$male<-ifelse(insurance$sex=='male',1,0)
insurance$smoker<-ifelse(insurance$smoker=='yes',1,0)

#make linear regression
summary(lm(charges~male+factor(age)+region+smoker+factor(children), insurance))

#relationship between sex and bmi: male tend to be unhealthier
ggplot(insurance, aes(x = sex, fill = bmi))+
  geom_density(alpha=0.5)

#Is age impact on bmi?
ggplot(insurance, aes(x=charges, fill=sex))+
  geom_density(alpha=0.5)

