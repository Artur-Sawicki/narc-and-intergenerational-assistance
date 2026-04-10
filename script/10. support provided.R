library(tidyverse)
library(writexl)
library(lavaan)
library(apaTables)
source("script/functions/extractREG.R")

# 1. create longitudinal from waves 9, 11, and 13 ####
db9<- readRDS("data/clean/wave9.rds")
db12<- readRDS("data/clean/wave12.rds")
db10<- readRDS("data/clean/wave10.rds")
db11<- readRDS("data/clean/wave11.rds")

db9<- db9 %>% select(id, sex.w9,age.w9, adm.w9, riv.w9)
db11<- db11 %>% select(id, sex.w11,age.w11,adm.w11, riv.w11)
db12<- db12 %>% select(id, par.sup.prov.w12,moth.sup.prov.w12,fath.sup.prov.w12)
db10<- db10 %>% select(id, par.sup.prov.w10,moth.sup.prov.w10,fath.sup.prov.w10)


dblong<- inner_join(db9,db11, by = "id")%>%inner_join(db12, by="id")%>%inner_join(db10, by="id")

# correlations in longitudinal data ####
apa.cor.table(dblong%>%select(-id), "output/tables/cor_support provided.doc")

#regressions across waves ####
reg.w9.par <- lm("par.sup.prov.w10 ~ adm.w9+riv.w9",data = dblong)
reg.w9.moth <- lm("moth.sup.prov.w10 ~ adm.w9+riv.w9",data = dblong)
reg.w9.fath <- lm("fath.sup.prov.w10 ~ adm.w9+riv.w9",data = dblong)

reg.w11.par <- lm("par.sup.prov.w12 ~ adm.w11+riv.w11",data = dblong)
reg.w11.moth <- lm("moth.sup.prov.w12 ~ adm.w11+riv.w11",data = dblong)
reg.w11.fath <- lm("fath.sup.prov.w12 ~ adm.w11+riv.w11",data = dblong)


results <- bind_rows(
  extract_model(reg.w9.par,  "w9",  "par"),
  extract_model(reg.w9.moth, "w9",  "moth"),
  extract_model(reg.w9.fath, "w9",  "fath"),
  
  extract_model(reg.w11.par,  "w11",  "par"),
  extract_model(reg.w11.moth, "w11",  "moth"),
  extract_model(reg.w11.fath, "w11",  "fath")
) 

results

write_xlsx(results,"output/tables/support provided regressions.xlsx")

#regressions across waves - gender moderation ####
dblong$sex.w9 <- haven::as_factor(dblong$sex.w9, levels = "labels")
dblong$sex.w9 <- sub("^-?\\d+\\s+", "", dblong$sex.w9)

table(dblong$sex.w9)

reg.w9.par <- lm("par.sup.prov.w10 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.moth <- lm("moth.sup.prov.w10 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.fath <- lm("fath.sup.prov.w10 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)

reg.w11.par <- lm("par.sup.prov.w12 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.moth <- lm("moth.sup.prov.w12 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.fath <- lm("fath.sup.prov.w12 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)


results <- bind_rows(
  extract_model(reg.w9.par,  "w9",  "par"),
  extract_model(reg.w9.moth, "w9",  "moth"),
  extract_model(reg.w9.fath, "w9",  "fath"),
  
  extract_model(reg.w11.par,  "w11", "par"),
  extract_model(reg.w11.moth, "w11", "moth"),
  extract_model(reg.w11.fath, "w11", "fath"),
  
) 

results

write_xlsx(results,"output/tables/support provided interactions.xlsx")