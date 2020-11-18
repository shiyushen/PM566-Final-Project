library(data.table)
library(tidyverse)
library(dplyr)
library(plotly)
library(DT)
library(knitr)
library(kableExtra)

#import data
covid <- fread("/Users/sherryshen/Desktop/COVID19-data.csv")

#remove 3 descriptive variables
covid <- covid[,-c(4,19,20)] 

#impute missing value
#1
which(is.na(covid$`reporting date`))
covid[which(is.na(covid$`reporting date`)),] #all other attribute are NA
new = covid[-262,] #delete it

sum(is.na(new$`reporting date`))

#3
mean_age = round(mean(covid$age,na.rm=T))
new[, age2 :=age]

for (i in 1:1084) {
  if (is.na(new$age[i]==T)){new$age2[i]=mean_age}
  else {new$age2[i]=new$age[i]}
}

sum(is.na(new$age2)) #check if there are NA

#4 Missing values in "hosp visit date" will be imputed by it's "reporting date"; 
new[,c(3,8,10,11,12)]<-lapply(new[,c(3,8,10,11,12)],as.Date)

new$hosp_visit_date[is.na(new$hosp_visit_date)] <- 
  new$`reporting date`[is.na(new$hosp_visit_date)]

sum(is.na(new$hosp_visit_date))

#5 hosp-symonset

library(lubridate)
library(zoo)

new1=new[-(which(is.na(new$symptom_onset))),]
dim(new1)
hos_sym_diff=days(rep(0,563))
hos_sym_diff=as.Date(new1$hosp_visit_date)-as.Date(new1$symptom_onset)
mean_diff1=round(mean(hos_sym_diff)) #mean diff is 4 
new$symptom_onset[is.na(new$symptom_onset)] <- 
  as.Date(new$hosp_visit_date[is.na(new$symptom_onset)])+days(4)

sum(is.na(new$symptom_onset))


#6 Missing value in "exposure start data" will be imputed by the average of the difference between "exposure start data" and "symptom onset date"; 

new2=new[-(which(is.na(new$exposure_start))),]
dim(new2)
sym_exps_diff=days(rep(0,128))
sym_exps_diff=as.Date(new2$symptom_onset)-as.Date(new2$exposure_start)
mean_diff2=round(mean(sym_exps_diff)) #mean diff is 12
new$exposure_start[is.na(new$exposure_start)] <- 
  as.Date(new$symptom_onset[is.na(new$exposure_start)])-days(12)

sum(is.na(new$exposure_start))


#7 Missing value in "exposure end data" will be imputed by "hosp visit data".

new$exposure_end[is.na(new$exposure_end)] <- 
  as.Date(new$hosp_visit_date[is.na(new$exposure_end)])

sum(is.na(new$exposure_end))


#8 Missing value in "symptoms" will be imputed by the most frequent symptoms. 
library(stringr)

sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "fever"))
sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "cough"))
sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "throat pain"))
sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "difficult in breathing"))
sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "chills"))
sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "joint pain"))
sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "runny nose"))
sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "abdominal pain"))
sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "diarrhea"))
sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "loss of appetite"))
sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "malaise"))
sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "pneumonia"))
sum(str_count(new$symptom[is.na(new$symptom)==F], pattern = "headache"))

new$symptom[is.na(new$symptom)] <- c("fever")
sum(is.na(new$symptom))


# Plot of number of new cases vs date outside of China


#remove data that do not have number of case record
newcase_data=new[-(which(is.na(new$case_in_country))),c(2,3,5)]
dim(newcase_data)

#compute new cases for each country
a=as.data.frame(table(newcase_data$`reporting date`))

a %>% 
  plot_ly(x = ~Var1, y = ~Freq, type = "scatter", mode = "lines") %>%
  layout(xaxis = list(title="Date"), yaxis = list(title="Number of cases"), title = "Number of new cases outside of China")



new %>% plot_ly(y = ~exposure_start, type = "box", name = "Exposure Start Date") %>% 
  add_trace(y = ~symptom_onset, type = "box", name = "Symptom_onset Date")




b=as.data.frame(table(newcase_data$country))
b %>%
  group_by(Var1) %>%
  plot_ly(labels = ~ Var1, values = ~Freq, textposition = "inside") %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Donus Chatrt of total numbe cases outside of China")


