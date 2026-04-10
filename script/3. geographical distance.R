library(tidyverse)
library(writexl)
library(lavaan)
library(apaTables)
source("script/functions/extractREG.R")

# 1. create longitudinal from waves 9, 11, and 13 ####
db9<- readRDS("data/clean/wave9.rds")
db11<- readRDS("data/clean/wave11.rds")
db13<- readRDS("data/clean/wave13.rds")

db9<- db9 %>% select(id, sex.w9,age.w9, adm.w9, riv.w9, par.geo.w9, moth.geo.w9, fath.geo.w9)
db11<- db11 %>% select(id, sex.w11,age.w11,adm.w11, riv.w11, par.geo.w11, moth.geo.w11, fath.geo.w11)
db13<- db13 %>% select(id, sex.w13,age.w13, adm.w13, riv.w13,par.geo.w13, moth.geo.w13, fath.geo.w13)

dblong<- inner_join(db9,db11, by = "id")%>%inner_join(db13, by="id")

# correlations in longitudinal data ####
apa.cor.table(dblong%>%select(-id), "output/tables/cor_geo_distance.doc")

#regressions across waves ####
reg.w9.par <- lm("par.geo.w9 ~ adm.w9+riv.w9",data = dblong)
reg.w9.moth <- lm("moth.geo.w9 ~ adm.w9+riv.w9",data = dblong)
reg.w9.fath <- lm("fath.geo.w9 ~ adm.w9+riv.w9",data = dblong)

reg.w11.par <- lm("par.geo.w11 ~ adm.w11+riv.w11",data = dblong)
reg.w11.moth <- lm("moth.geo.w11 ~ adm.w11+riv.w11",data = dblong)
reg.w11.fath <- lm("fath.geo.w11 ~ adm.w11+riv.w11",data = dblong)

reg.w13.par <- lm("par.geo.w13 ~ adm.w13+riv.w13",data = dblong)
reg.w13.moth <- lm("moth.geo.w13 ~ adm.w13+riv.w13",data = dblong)
reg.w13.fath <- lm("fath.geo.w13 ~ adm.w13+riv.w13",data = dblong)

results <- bind_rows(
  extract_model(reg.w9.par,  "w9",  "par"),
  extract_model(reg.w9.moth, "w9",  "moth"),
  extract_model(reg.w9.fath, "w9",  "fath"),
  
  extract_model(reg.w11.par,  "w11", "par"),
  extract_model(reg.w11.moth, "w11", "moth"),
  extract_model(reg.w11.fath, "w11", "fath"),
  
  extract_model(reg.w13.par,  "w13", "par"),
  extract_model(reg.w13.moth, "w13", "moth"),
  extract_model(reg.w13.fath, "w13", "fath")
) 

results

write_xlsx(results,"output/tables/geo distance regressions.xlsx")
#regressions across waves - gender moderation ####
dblong$sex.w9 <- haven::as_factor(dblong$sex.w9, levels = "labels")
dblong$sex.w9 <- sub("^-?\\d+\\s+", "", dblong$sex.w9)

table(dblong$sex.w9)

reg.w9.par <- lm("par.geo.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.moth <- lm("moth.geo.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.fath <- lm("fath.geo.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)

reg.w11.par <- lm("par.geo.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.moth <- lm("moth.geo.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.fath <- lm("fath.geo.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)

reg.w13.par <- lm("par.geo.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)
reg.w13.moth <- lm("moth.geo.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)
reg.w13.fath <- lm("fath.geo.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)

results <- bind_rows(
  extract_model(reg.w9.par,  "w9",  "par"),
  extract_model(reg.w9.moth, "w9",  "moth"),
  extract_model(reg.w9.fath, "w9",  "fath"),
  
  extract_model(reg.w11.par,  "w11", "par"),
  extract_model(reg.w11.moth, "w11", "moth"),
  extract_model(reg.w11.fath, "w11", "fath"),
  
  extract_model(reg.w13.par,  "w13", "par"),
  extract_model(reg.w13.moth, "w13", "moth"),
  extract_model(reg.w13.fath, "w13", "fath")
) 

results

write_xlsx(results,"output/tables/geo distance interactions.xlsx")
