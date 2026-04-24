library(tidyverse)
library(writexl)
library(lavaan)
library(apaTables)
source("script/functions/extractREG.R")

# 1. create longitudinal from waves 9, 11, and 13 ####
db9<- readRDS("data/clean/wave9.rds")
db11<- readRDS("data/clean/wave11.rds")
db13<- readRDS("data/clean/wave13.rds")

db9<- db9 %>% select(id, sex.w9,age.w9, adm.w9, riv.w9, par.intim.w9, moth.intim.w9, fath.intim.w9)
db11<- db11 %>% select(id, sex.w11,age.w11,adm.w11, riv.w11, par.intim.w11, moth.intim.w11, fath.intim.w11)
db13<- db13 %>% select(id, sex.w13,age.w13, adm.w13, riv.w13,par.intim.w13, moth.intim.w13, fath.intim.w13)

dblong<- inner_join(db9,db11, by = "id")%>%inner_join(db13, by="id")

# correlations in longitudinal data ####
apa.cor.table(dblong%>%select(-id), "output/tables/cor_intimacy.doc")

#regressions across waves ####
reg.w9.par <- lm("par.intim.w9 ~ adm.w9+riv.w9",data = dblong)
reg.w9.moth <- lm("moth.intim.w9 ~ adm.w9+riv.w9",data = dblong)
reg.w9.fath <- lm("fath.intim.w9 ~ adm.w9+riv.w9",data = dblong)

reg.w11.par <- lm("par.intim.w11 ~ adm.w11+riv.w11",data = dblong)
reg.w11.moth <- lm("moth.intim.w11 ~ adm.w11+riv.w11",data = dblong)
reg.w11.fath <- lm("fath.intim.w11 ~ adm.w11+riv.w11",data = dblong)

reg.w13.par <- lm("par.intim.w13 ~ adm.w13+riv.w13",data = dblong)
reg.w13.moth <- lm("moth.intim.w13 ~ adm.w13+riv.w13",data = dblong)
reg.w13.fath <- lm("fath.intim.w13 ~ adm.w13+riv.w13",data = dblong)

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

write_xlsx(results,"output/tables/intimacy regressions.xlsx")

#regressions across waves - gender moderation ####
dblong$sex.w9 <- haven::as_factor(dblong$sex.w9, levels = "labels")
dblong$sex.w9 <- sub("^-?\\d+\\s+", "", dblong$sex.w9)

table(dblong$sex.w9)

reg.w9.par <- lm("par.intim.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.moth <- lm("moth.intim.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.fath <- lm("fath.intim.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)

reg.w11.par <- lm("par.intim.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.moth <- lm("moth.intim.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.fath <- lm("fath.intim.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)

reg.w13.par <- lm("par.intim.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)
reg.w13.moth <- lm("moth.intim.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)
reg.w13.fath <- lm("fath.intim.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)

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

write_xlsx(results,"output/tables/intimacy interactions.xlsx")
# CLPM ####
## MOTHER ####
m.intim.clpm.mod <- "adm.w11 ~ a*adm.w9 + b*riv.w9 + c*moth.intim.w9
                adm.w13 ~ a*adm.w11 + b*riv.w11 + c*moth.intim.w11
                
                riv.w11 ~ d*adm.w9 + e*riv.w9 + f*moth.intim.w9
                riv.w13 ~ d*adm.w11 + e*riv.w11 + f*moth.intim.w11
                
                moth.intim.w11 ~ g*adm.w9 + h*riv.w9 + i*moth.intim.w9
                moth.intim.w13 ~ g*adm.w11 + h*riv.w11 + i*moth.intim.w11
                
                adm.w9 ~~ riv.w9 + moth.intim.w9
                adm.w11 ~~ riv.w11 + moth.intim.w11
                adm.w13 ~~ riv.w13 + moth.intim.w13
                
                riv.w11 ~~ moth.intim.w11
                riv.w9 ~~ moth.intim.w9
"
m.intim.clpm.fit <- sem(m.intim.clpm.mod, data = dblong, missing = "FIML", estimator = "MLR")
summary(m.intim.clpm.fit, fit.measures=T, standardized=T,rsquare=T)
moth.clpm<-standardizedSolution(m.intim.clpm.fit)%>%as.data.frame()

## FATHER ####
f.intim.clpm.mod <- "adm.w11 ~ a*adm.w9 + b*riv.w9 + c*moth.intim.w9
                adm.w13 ~ a*adm.w11 + b*riv.w11 + c*moth.intim.w11
                
                riv.w11 ~ d*adm.w9 + e*riv.w9 + f*moth.intim.w9
                riv.w13 ~ d*adm.w11 + e*riv.w11 + f*moth.intim.w11
                
                moth.intim.w11 ~ g*adm.w9 + h*riv.w9 + i*moth.intim.w9
                moth.intim.w13 ~ g*adm.w11 + h*riv.w11 + i*moth.intim.w11
                
                adm.w9 ~~ riv.w9 + moth.intim.w9
                adm.w11 ~~ riv.w11 + moth.intim.w11
                adm.w13 ~~ riv.w13 + moth.intim.w13
                
                riv.w11 ~~ moth.intim.w11
                riv.w9 ~~ moth.intim.w9
"
f.intim.clpm.fit <- sem(f.intim.clpm.mod, data = dblong, missing = "FIML", estimator = "MLR")
summary(f.intim.clpm.fit, fit.measures=T, standardized=T,rsquare=T)
fath.clpm<-standardizedSolution(f.intim.clpm.fit)%>%as.data.frame()

# random-intercept cross-lag panel model - correlation ####
## MOTHER ####
m.intim.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.intim =~ 1*moth.intim.w9 + 1*moth.intim.w11 + 1*moth.intim.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wmoth.intim.w9 =~ 1*moth.intim.w9
  wmoth.intim.w11 =~ 1*moth.intim.w11
  wmoth.intim.w13 =~ 1*moth.intim.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wmoth.intim.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wmoth.intim.w9
  wmoth.intim.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wmoth.intim.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wmoth.intim.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wmoth.intim.w11
  wmoth.intim.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wmoth.intim.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wmoth.intim.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wmoth.intim.w13
  wriv.w11 ~~ cov3*wmoth.intim.w11
  wriv.w13 ~~ cov3*wmoth.intim.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wmoth.intim.w9 
  wriv.w9 ~~ wmoth.intim.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.intim ~~ RI.intim
  RIadm ~~ RIriv + RI.intim
  RIriv ~~ RI.intim
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wmoth.intim.w9 ~~ wmoth.intim.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wmoth.intim.w11 ~~ vse*wmoth.intim.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wmoth.intim.w13 ~~ vse*wmoth.intim.w13

'
m.intim.riclpm.fit <- lavaan(m.intim.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T) 
summary(m.intim.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)
moth.cor<-standardizedSolution(m.intim.riclpm.fit)%>%as.data.frame()

## FATHER ####
f.intim.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.intim =~ 1*fath.intim.w9 + 1*fath.intim.w11 + 1*fath.intim.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wfath.intim.w9 =~ 1*fath.intim.w9
  wfath.intim.w11 =~ 1*fath.intim.w11
  wfath.intim.w13 =~ 1*fath.intim.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wfath.intim.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wfath.intim.w9
  wfath.intim.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wfath.intim.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wfath.intim.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wfath.intim.w11
  wfath.intim.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wfath.intim.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wfath.intim.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wfath.intim.w13
  wriv.w11 ~~ cov3*wfath.intim.w11
  wriv.w13 ~~ cov3*wfath.intim.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wfath.intim.w9 
  wriv.w9 ~~ wfath.intim.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.intim ~~ RI.intim
  RIadm ~~ RIriv + RI.intim
  RIriv ~~ RI.intim
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wfath.intim.w9 ~~ wfath.intim.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wfath.intim.w11 ~~ vse*wfath.intim.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wfath.intim.w13 ~~ vse*wfath.intim.w13

'
f.intim.riclpm.fit <- lavaan(f.intim.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T) 
summary(f.intim.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)
fath.cor<-standardizedSolution(m.intim.riclpm.fit)%>%as.data.frame()




# random-intercept cross-lag panel model - regression ####
## MOTHER ####
m.intim.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.intim =~ 1*moth.intim.w9 + 1*moth.intim.w11 + 1*moth.intim.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wmoth.intim.w9 =~ 1*moth.intim.w9
  wmoth.intim.w11 =~ 1*moth.intim.w11
  wmoth.intim.w13 =~ 1*moth.intim.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wmoth.intim.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wmoth.intim.w9
  wmoth.intim.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wmoth.intim.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wmoth.intim.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wmoth.intim.w11
  wmoth.intim.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wmoth.intim.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wmoth.intim.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wmoth.intim.w13
  wriv.w11 ~~ cov3*wmoth.intim.w11
  wriv.w13 ~~ cov3*wmoth.intim.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wmoth.intim.w9 
  wriv.w9 ~~ wmoth.intim.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.intim ~~ RI.intim
  RI.intim ~ RIadm + RIriv 
  RIriv ~~ RIadm
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wmoth.intim.w9 ~~ wmoth.intim.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wmoth.intim.w11 ~~ vse*wmoth.intim.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wmoth.intim.w13 ~~ vse*wmoth.intim.w13

'
m.intim.riclpm.fit <- lavaan(m.intim.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T) 
summary(m.intim.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)
moth.reg<-standardizedSolution(m.intim.riclpm.fit)%>%as.data.frame()

## FATHER ####
f.intim.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.intim =~ 1*fath.intim.w9 + 1*fath.intim.w11 + 1*fath.intim.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wfath.intim.w9 =~ 1*fath.intim.w9
  wfath.intim.w11 =~ 1*fath.intim.w11
  wfath.intim.w13 =~ 1*fath.intim.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wfath.intim.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wfath.intim.w9
  wfath.intim.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wfath.intim.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wfath.intim.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wfath.intim.w11
  wfath.intim.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wfath.intim.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wfath.intim.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wfath.intim.w13
  wriv.w11 ~~ cov3*wfath.intim.w11
  wriv.w13 ~~ cov3*wfath.intim.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wfath.intim.w9 
  wriv.w9 ~~ wfath.intim.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.intim ~~ RI.intim
  RI.intim ~ RIadm + RIriv 
  RIriv ~~ RIadm
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wfath.intim.w9 ~~ wfath.intim.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wfath.intim.w11 ~~ vse*wfath.intim.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wfath.intim.w13 ~~ vse*wfath.intim.w13

'
f.intim.riclpm.fit <- lavaan(f.intim.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T) 
summary(f.intim.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)
fath.reg<-standardizedSolution(f.intim.riclpm.fit)%>%as.data.frame()



# export ####
write_xlsx(list(
  mother.clpm = moth.clpm, father.clpm = fath.clpm,
  mother.cor = moth.cor,father.cor = fath.cor, 
  mother.reg = moth.reg,father.reg = fath.reg), "output/tables/RICLPM intimacy.xlsx")

