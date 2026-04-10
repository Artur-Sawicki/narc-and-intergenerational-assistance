library(tidyverse)
library(writexl)
library(lavaan)
library(apaTables)
source("script/functions/extractREG.R")

# 1. create longitudinal from waves 9, 11, and 13 ####
db9<- readRDS("data/clean/wave9.rds")
db11<- readRDS("data/clean/wave11.rds")

db9<- db9 %>% select(id, sex.w9,age.w9, adm.w9, riv.w9, par.amb.w9, moth.amb.w9, fath.amb.w9)
db11<- db11 %>% select(id, sex.w11,age.w11,adm.w11, riv.w11, par.amb.w11, moth.amb.w11, fath.amb.w11)

dblong<- inner_join(db9,db11, by = "id")

# correlations in longitudinal data ####
apa.cor.table(dblong%>%select(-id), "output/tables/cor_ambivalence.doc")

#regressions across waves ####
reg.w9.par <- lm("par.amb.w9 ~ adm.w9+riv.w9",data = dblong)
reg.w9.moth <- lm("moth.amb.w9 ~ adm.w9+riv.w9",data = dblong)
reg.w9.fath <- lm("fath.amb.w9 ~ adm.w9+riv.w9",data = dblong)

reg.w11.par <- lm("par.amb.w11 ~ adm.w11+riv.w11",data = dblong)
reg.w11.moth <- lm("moth.amb.w11 ~ adm.w11+riv.w11",data = dblong)
reg.w11.fath <- lm("fath.amb.w11 ~ adm.w11+riv.w11",data = dblong)


results <- bind_rows(
  extract_model(reg.w9.par,  "w9",  "par"),
  extract_model(reg.w9.moth, "w9",  "moth"),
  extract_model(reg.w9.fath, "w9",  "fath"),
  
  extract_model(reg.w11.par,  "w11", "par"),
  extract_model(reg.w11.moth, "w11", "moth"),
  extract_model(reg.w11.fath, "w11", "fath"),
) 

results

write_xlsx(results,"output/tables/ambivalence regressions.xlsx")

#regressions across waves - gender moderation ####
dblong$sex.w9 <- haven::as_factor(dblong$sex.w9, levels = "labels")
dblong$sex.w9 <- sub("^-?\\d+\\s+", "", dblong$sex.w9)

table(dblong$sex.w9)

reg.w9.par <- lm("par.amb.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.moth <- lm("moth.amb.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.fath <- lm("fath.amb.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)

reg.w11.par <- lm("par.amb.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.moth <- lm("moth.amb.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.fath <- lm("fath.amb.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)


results <- bind_rows(
  extract_model(reg.w9.par,  "w9",  "par"),
  extract_model(reg.w9.moth, "w9",  "moth"),
  extract_model(reg.w9.fath, "w9",  "fath"),
  
  extract_model(reg.w11.par,  "w11", "par"),
  extract_model(reg.w11.moth, "w11", "moth"),
  extract_model(reg.w11.fath, "w11", "fath"),
  
) 

results

write_xlsx(results,"output/tables/ambivalence interactions.xlsx")