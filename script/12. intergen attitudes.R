library(tidyverse)
library(writexl)
library(lavaan)
library(apaTables)
source("script/functions/extractREG.R")

# 1. create longitudinal from waves 9, 11, and 13 ####
db9<- readRDS("data/clean/wave9.rds")
db10<- readRDS("data/clean/wave10.rds")

db9<- db9 %>% select(id, sex.w9,age.w9, adm.w9, riv.w9)
db10<- db10 %>% select(id, intgen.sup.att.w10)


dblong<- inner_join(db9,db10, by = "id")

# correlations in longitudinal data ####
apa.cor.table(dblong%>%select(-id), "output/tables/cor_intergen attitudes.doc")

#regressions across waves ####
reg.w9.par <- lm("intgen.sup.att.w10 ~ adm.w9+riv.w9",data = dblong)
results <-extract_model(reg.w9.par,  "w9",  "par") 

results

write_xlsx(results,"output/tables/intergen attitudes regressions.xlsx")
