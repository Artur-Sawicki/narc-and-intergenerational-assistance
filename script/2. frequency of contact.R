library(tidyverse)
library(writexl)
library(lavaan)
library(apaTables)
source("script/functions/extractREG.R")

# 1. create longitudinal from waves 9, 11, and 13 ####
db9<- readRDS("data/clean/wave9.rds")
db11<- readRDS("data/clean/wave11.rds")
db13<- readRDS("data/clean/wave13.rds")

db9<- db9 %>% select(id, sex.w9,age.w9, adm.w9, riv.w9, par.con.frq.w9, moth.con.frq.w9, fath.con.frq.w9)
db11<- db11 %>% select(id, sex.w11,age.w11,adm.w11, riv.w11, par.con.frq.w11, moth.con.frq.w11, fath.con.frq.w11)
db13<- db13 %>% select(id, sex.w13,age.w13, adm.w13, riv.w13,par.con.frq.w13, moth.con.frq.w13, fath.con.frq.w13)

dblong<- inner_join(db9,db11, by = "id")%>%inner_join(db13, by="id")

# correlations in longitudinal data ####
apa.cor.table(dblong%>%select(-id), "output/tables/cor_frrequency of contact.doc")

#regressions across waves ####
reg.w9.par <- lm("par.con.frq.w9 ~ adm.w9+riv.w9",data = dblong)
reg.w9.moth <- lm("moth.con.frq.w9 ~ adm.w9+riv.w9",data = dblong)
reg.w9.fath <- lm("fath.con.frq.w9 ~ adm.w9+riv.w9",data = dblong)

reg.w11.par <- lm("par.con.frq.w11 ~ adm.w11+riv.w11",data = dblong)
reg.w11.moth <- lm("moth.con.frq.w11 ~ adm.w11+riv.w11",data = dblong)
reg.w11.fath <- lm("fath.con.frq.w11 ~ adm.w11+riv.w11",data = dblong)

reg.w13.par <- lm("par.con.frq.w13 ~ adm.w13+riv.w13",data = dblong)
reg.w13.moth <- lm("moth.con.frq.w13 ~ adm.w13+riv.w13",data = dblong)
reg.w13.fath <- lm("fath.con.frq.w13 ~ adm.w13+riv.w13",data = dblong)

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

write_xlsx(results,"output/tables/frequency of contact regressions.xlsx")

#regressions across waves - gender moderation ####
dblong$sex.w9 <- haven::as_factor(dblong$sex.w9, levels = "labels")
dblong$sex.w9 <- sub("^-?\\d+\\s+", "", dblong$sex.w9)

table(dblong$sex.w9)

reg.w9.par <- lm("par.con.frq.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.moth <- lm("moth.con.frq.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.fath <- lm("fath.con.frq.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)

reg.w11.par <- lm("par.con.frq.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.moth <- lm("moth.con.frq.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.fath <- lm("fath.con.frq.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)

reg.w13.par <- lm("par.con.frq.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)
reg.w13.moth <- lm("moth.con.frq.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)
reg.w13.fath <- lm("fath.con.frq.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)

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

write_xlsx(results,"output/tables/frequency of contact interactions.xlsx")


# CLPM ####
## MOTHER ####
m.frq.clpm.mod <- "adm.w11 ~ a*adm.w9 + b*riv.w9 + c*moth.con.frq.w9
                adm.w13 ~ a*adm.w11 + b*riv.w11 + c*moth.con.frq.w11
                
                riv.w11 ~ d*adm.w9 + e*riv.w9 + f*moth.con.frq.w9
                riv.w13 ~ d*adm.w11 + e*riv.w11 + f*moth.con.frq.w11
                
                moth.con.frq.w11 ~ g*adm.w9 + h*riv.w9 + i*moth.con.frq.w9
                moth.con.frq.w13 ~ g*adm.w11 + h*riv.w11 + i*moth.con.frq.w11
                
                adm.w9 ~~ riv.w9 + moth.con.frq.w9
                adm.w11 ~~ riv.w11 + moth.con.frq.w11
                adm.w13 ~~ riv.w13 + moth.con.frq.w13
                
                riv.w11 ~~ moth.con.frq.w11
                riv.w9 ~~ moth.con.frq.w9
"
m.frq.clpm.fit <- sem(m.frq.clpm.mod, data = dblong, missing = "FIML", estimator = "MLR")
summary(m.frq.clpm.fit, fit.measures=T, standardized=T,rsquare=T)
moth.clpm<-standardizedSolution(m.frq.clpm.fit)%>%as.data.frame()

## FATHER ####
f.frq.clpm.mod <- "adm.w11 ~ a*adm.w9 + b*riv.w9 + c*moth.con.frq.w9
                adm.w13 ~ a*adm.w11 + b*riv.w11 + c*moth.con.frq.w11
                
                riv.w11 ~ d*adm.w9 + e*riv.w9 + f*moth.con.frq.w9
                riv.w13 ~ d*adm.w11 + e*riv.w11 + f*moth.con.frq.w11
                
                moth.con.frq.w11 ~ g*adm.w9 + h*riv.w9 + i*moth.con.frq.w9
                moth.con.frq.w13 ~ g*adm.w11 + h*riv.w11 + i*moth.con.frq.w11
                
                adm.w9 ~~ riv.w9 + moth.con.frq.w9
                adm.w11 ~~ riv.w11 + moth.con.frq.w11
                adm.w13 ~~ riv.w13 + moth.con.frq.w13
                
                riv.w11 ~~ moth.con.frq.w11
                riv.w9 ~~ moth.con.frq.w9
"
f.frq.clpm.fit <- sem(f.frq.clpm.mod, data = dblong, missing = "FIML", estimator = "MLR")
summary(f.frq.clpm.fit, fit.measures=T, standardized=T,rsquare=T)
fath.clpm<-standardizedSolution(f.frq.clpm.fit)%>%as.data.frame()

# random-intercept cross-lag panel model - correlation ####
## MOTHER ####
m.frq.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.frq =~ 1*moth.con.frq.w9 + 1*moth.con.frq.w11 + 1*moth.con.frq.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wmoth.con.frq.w9 =~ 1*moth.con.frq.w9
  wmoth.con.frq.w11 =~ 1*moth.con.frq.w11
  wmoth.con.frq.w13 =~ 1*moth.con.frq.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wmoth.con.frq.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wmoth.con.frq.w9
  wmoth.con.frq.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wmoth.con.frq.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wmoth.con.frq.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wmoth.con.frq.w11
  wmoth.con.frq.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wmoth.con.frq.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wmoth.con.frq.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wmoth.con.frq.w13
  wriv.w11 ~~ cov3*wmoth.con.frq.w11
  wriv.w13 ~~ cov3*wmoth.con.frq.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wmoth.con.frq.w9 
  wriv.w9 ~~ wmoth.con.frq.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.frq ~~ RI.frq
  RIadm ~~ RIriv + RI.frq
  RIriv ~~ RI.frq
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wmoth.con.frq.w9 ~~ wmoth.con.frq.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wmoth.con.frq.w11 ~~ vse*wmoth.con.frq.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wmoth.con.frq.w13 ~~ vse*wmoth.con.frq.w13

'
m.frq.riclpm.fit <- lavaan(m.frq.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T) 
summary(m.frq.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)
moth.cor<-standardizedSolution(m.frq.riclpm.fit)%>%as.data.frame()

## FATHER ####
f.frq.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.frq =~ 1*fath.con.frq.w9 + 1*fath.con.frq.w11 + 1*fath.con.frq.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wfath.con.frq.w9 =~ 1*fath.con.frq.w9
  wfath.con.frq.w11 =~ 1*fath.con.frq.w11
  wfath.con.frq.w13 =~ 1*fath.con.frq.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wfath.con.frq.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wfath.con.frq.w9
  wfath.con.frq.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wfath.con.frq.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wfath.con.frq.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wfath.con.frq.w11
  wfath.con.frq.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wfath.con.frq.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wfath.con.frq.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wfath.con.frq.w13
  wriv.w11 ~~ cov3*wfath.con.frq.w11
  wriv.w13 ~~ cov3*wfath.con.frq.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wfath.con.frq.w9 
  wriv.w9 ~~ wfath.con.frq.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.frq ~~ RI.frq
  RIadm ~~ RIriv + RI.frq
  RIriv ~~ RI.frq
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wfath.con.frq.w9 ~~ wfath.con.frq.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wfath.con.frq.w11 ~~ vse*wfath.con.frq.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wfath.con.frq.w13 ~~ vse*wfath.con.frq.w13

'
f.frq.riclpm.fit <- lavaan(f.frq.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T) 
summary(f.frq.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)
fath.cor<-standardizedSolution(m.frq.riclpm.fit)%>%as.data.frame()




# random-intercept cross-lag panel model - regression ####
## MOTHER ####
m.frq.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.frq =~ 1*moth.con.frq.w9 + 1*moth.con.frq.w11 + 1*moth.con.frq.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wmoth.con.frq.w9 =~ 1*moth.con.frq.w9
  wmoth.con.frq.w11 =~ 1*moth.con.frq.w11
  wmoth.con.frq.w13 =~ 1*moth.con.frq.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wmoth.con.frq.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wmoth.con.frq.w9
  wmoth.con.frq.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wmoth.con.frq.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wmoth.con.frq.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wmoth.con.frq.w11
  wmoth.con.frq.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wmoth.con.frq.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wmoth.con.frq.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wmoth.con.frq.w13
  wriv.w11 ~~ cov3*wmoth.con.frq.w11
  wriv.w13 ~~ cov3*wmoth.con.frq.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wmoth.con.frq.w9 
  wriv.w9 ~~ wmoth.con.frq.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.frq ~~ RI.frq
  RI.frq ~ RIadm + RIriv 
  RIriv ~~ RIadm
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wmoth.con.frq.w9 ~~ wmoth.con.frq.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wmoth.con.frq.w11 ~~ vse*wmoth.con.frq.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wmoth.con.frq.w13 ~~ vse*wmoth.con.frq.w13

'
m.frq.riclpm.fit <- lavaan(m.frq.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T) 
summary(m.frq.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)
moth.reg<-standardizedSolution(m.frq.riclpm.fit)%>%as.data.frame()

## FATHER ####
f.frq.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.frq =~ 1*fath.con.frq.w9 + 1*fath.con.frq.w11 + 1*fath.con.frq.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wfath.con.frq.w9 =~ 1*fath.con.frq.w9
  wfath.con.frq.w11 =~ 1*fath.con.frq.w11
  wfath.con.frq.w13 =~ 1*fath.con.frq.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wfath.con.frq.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wfath.con.frq.w9
  wfath.con.frq.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wfath.con.frq.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wfath.con.frq.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wfath.con.frq.w11
  wfath.con.frq.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wfath.con.frq.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wfath.con.frq.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wfath.con.frq.w13
  wriv.w11 ~~ cov3*wfath.con.frq.w11
  wriv.w13 ~~ cov3*wfath.con.frq.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wfath.con.frq.w9 
  wriv.w9 ~~ wfath.con.frq.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.frq ~~ RI.frq
  RI.frq ~ RIadm + RIriv 
  RIriv ~~ RIadm
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wfath.con.frq.w9 ~~ wfath.con.frq.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wfath.con.frq.w11 ~~ vse*wfath.con.frq.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wfath.con.frq.w13 ~~ vse*wfath.con.frq.w13

'
f.frq.riclpm.fit <- lavaan(f.frq.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T) 
summary(f.frq.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)
fath.reg<-standardizedSolution(f.frq.riclpm.fit)%>%as.data.frame()

# RI-CLPM plus gender

# export ####
write_xlsx(list(
  mother.clpm = moth.clpm, father.clpm = fath.clpm,
  mother.cor = moth.cor,father.cor = fath.cor, 
  mother.reg = moth.reg,father.reg = fath.reg), "output/tables/RICLPM frequency of contact.xlsx")


