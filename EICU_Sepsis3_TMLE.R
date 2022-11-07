require(dplyr)
library("tmle")
library("psych")
library(tidyverse)
library(table1)
raw_data <- read.csv("Eicu_Sepsis3_Raw.csv", header=TRUE, stringsAsFactors=TRUE)
sepsis_data <- raw_data %>%
  mutate(Male = ifelse(gender == "Male",1,0))%>%
  mutate(age_int = age)%>%
  mutate(age_cat = ifelse(age_int >= 18 & age_int <= 34, "18 - 34",
                          ifelse(age_int >= 35 & age_int <= 44, "35 - 44",
                                 ifelse(age_int >= 45 & age_int <= 64, "45 - 64",
                                        ifelse(age_int >= 65 & age_int <= 74, "65 - 74",
                                               ifelse(age_int >= 75 & age_int <= 84, "75 - 84", "85 and above"))))))%>%
  mutate(vp =vasopressor)%>%
  mutate(renal = rrt)%>%
  mutate(white = ifelse(ethnicity == "Caucasian", "WHITE", "Non-WHITE"))%>%
  mutate(cci_int = Charlson)%>%
  mutate(cci_cat = ifelse(cci_int >= 0 & cci_int <= 5, "0 - 5",
                          ifelse(cci_int >= 6 & cci_int <= 10, "6 - 10",
                                 ifelse(cci_int >= 11 & cci_int <= 15, "11 - 15","16 and above"))))%>%
  mutate(death_int = ifelse(hospitaldischargestatus == "Expired" | hospitaldischargelocation == "Death" ,1, 0))%>%
  mutate(sofa_int=sofa_admit)%>%
  mutate(sofa_cat = ifelse(sofa_int >= 0 & sofa_int <= 5, "0 - 5",
                           ifelse(sofa_int >= 6 & sofa_int <= 10, "6 - 10",
                                  ifelse(sofa_int >= 11 & sofa_int <= 15, "11 - 15", "16 and above"))))
table1(~ as.factor(gender) + as.factor(Male)+as.factor(vent)+as.factor(renal)
       +as.factor(vp)+cci_int+cci_cat+ as.factor(death_int) + sofa_int + sofa_cat
       | white, data=sepsis_data, render.missing=NULL,topclass="Rtable1-grid Rtable1-shade Rtable1-times")

