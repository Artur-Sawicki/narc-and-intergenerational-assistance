library(tidyverse)
library(writexl)
library(lavaan)
library(apaTables)
source("script/functions/extractREG.R")

# 1. create longitudinal from waves 9, 11, and 13 ####
db9<- readRDS("data/clean/wave9.rds")
db11<- readRDS("data/clean/wave11.rds")
db13<- readRDS("data/clean/wave13.rds")

db9<- db9 %>% select(id, sex.w9,age.w9, adm.w9, riv.w9, par.conf.w9, moth.conf.w9, fath.conf.w9)
db11<- db11 %>% select(id, sex.w11,age.w11,adm.w11, riv.w11, par.conf.w11, moth.conf.w11, fath.conf.w11)
db13<- db13 %>% select(id, sex.w13,age.w13, adm.w13, riv.w13,par.conf.w13, moth.conf.w13, fath.conf.w13)

dblong<- inner_join(db9,db11, by = "id")%>%inner_join(db13, by="id")

# correlations in longitudinal data ####
apa.cor.table(dblong%>%select(-id), "output/tables/cor_conflict.doc")

#regressions across waves ####
reg.w9.par <- lm("par.conf.w9 ~ adm.w9+riv.w9",data = dblong)
reg.w9.moth <- lm("moth.conf.w9 ~ adm.w9+riv.w9",data = dblong)
reg.w9.fath <- lm("fath.conf.w9 ~ adm.w9+riv.w9",data = dblong)

reg.w11.par <- lm("par.conf.w11 ~ adm.w11+riv.w11",data = dblong)
reg.w11.moth <- lm("moth.conf.w11 ~ adm.w11+riv.w11",data = dblong)
reg.w11.fath <- lm("fath.conf.w11 ~ adm.w11+riv.w11",data = dblong)

reg.w13.par <- lm("par.conf.w13 ~ adm.w13+riv.w13",data = dblong)
reg.w13.moth <- lm("moth.conf.w13 ~ adm.w13+riv.w13",data = dblong)
reg.w13.fath <- lm("fath.conf.w13 ~ adm.w13+riv.w13",data = dblong)

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

write_xlsx(results,"output/tables/conflict regressions.xlsx")

#regressions across waves - gender moderation ####
dblong$sex.w9 <- haven::as_factor(dblong$sex.w9, levels = "labels")
dblong$sex.w9 <- sub("^-?\\d+\\s+", "", dblong$sex.w9)

table(dblong$sex.w9)

reg.w9.par <- lm("par.conf.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.moth <- lm("moth.conf.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.fath <- lm("fath.conf.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)

reg.w11.par <- lm("par.conf.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.moth <- lm("moth.conf.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.fath <- lm("fath.conf.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)

reg.w13.par <- lm("par.conf.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)
reg.w13.moth <- lm("moth.conf.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)
reg.w13.fath <- lm("fath.conf.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)

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

write_xlsx(results,"output/tables/conflict interactions.xlsx")

# CLPM ####
## MOTHER ####
m.conf.clpm.mod <- "adm.w11 ~ a*adm.w9 + b*riv.w9 + c*moth.conf.w9
                adm.w13 ~ a*adm.w11 + b*riv.w11 + c*moth.conf.w11
                
                riv.w11 ~ d*adm.w9 + e*riv.w9 + f*moth.conf.w9
                riv.w13 ~ d*adm.w11 + e*riv.w11 + f*moth.conf.w11
                
                moth.conf.w11 ~ g*adm.w9 + h*riv.w9 + i*moth.conf.w9
                moth.conf.w13 ~ g*adm.w11 + h*riv.w11 + i*moth.conf.w11
                
                adm.w9 ~~ riv.w9 + moth.conf.w9
                adm.w11 ~~ riv.w11 + moth.conf.w11
                adm.w13 ~~ riv.w13 + moth.conf.w13
                
                riv.w11 ~~ moth.conf.w11
                riv.w9 ~~ moth.conf.w9
"
m.conf.clpm.fit <- sem(m.conf.clpm.mod, data = dblong, missing = "FIML", estimator = "MLR")
summary(m.conf.clpm.fit, fit.measures=T, standardized=T,rsquare=T)

moth.clpm<-parameterEstimates(m.conf.clpm.fit) %>% as.data.frame()%>%left_join(
  standardizedSolution(m.conf.clpm.fit)%>%as.data.frame()%>%
    select(lhs, op, rhs, est.std),by = c("lhs", "op", "rhs"),suffix = c(".unstd", ".std"))

moth.clpm.fit <- fitMeasures(m.conf.clpm.fit, 
                             c("chisq.scaled", "df.scaled","cfi.scaled","rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled","srmr")) %>%
  as.data.frame()%>%  tibble::rownames_to_column("index")

## FATHER ####
f.conf.clpm.mod <- "adm.w11 ~ a*adm.w9 + b*riv.w9 + c*fath.conf.w9
                adm.w13 ~ a*adm.w11 + b*riv.w11 + c*fath.conf.w11
                
                riv.w11 ~ d*adm.w9 + e*riv.w9 + f*fath.conf.w9
                riv.w13 ~ d*adm.w11 + e*riv.w11 + f*fath.conf.w11
                
                fath.conf.w11 ~ g*adm.w9 + h*riv.w9 + i*fath.conf.w9
                fath.conf.w13 ~ g*adm.w11 + h*riv.w11 + i*fath.conf.w11
                
                adm.w9 ~~ riv.w9 + fath.conf.w9
                adm.w11 ~~ riv.w11 + fath.conf.w11
                adm.w13 ~~ riv.w13 + fath.conf.w13
                
                riv.w11 ~~ fath.conf.w11
                riv.w9 ~~ fath.conf.w9
"
f.conf.clpm.fit <- sem(f.conf.clpm.mod, data = dblong, missing = "FIML", estimator = "MLR")
summary(f.conf.clpm.fit, fit.measures=T, standardized=T,rsquare=T)

fath.clpm<-parameterEstimates(f.conf.clpm.fit) %>% as.data.frame()%>%left_join(
  standardizedSolution(f.conf.clpm.fit)%>%as.data.frame()%>%
    select(lhs, op, rhs, est.std),by = c("lhs", "op", "rhs"),suffix = c(".unstd", ".std"))

fath.clpm.fit <- fitMeasures(f.conf.clpm.fit, 
                             c("chisq.scaled", "df.scaled","cfi.scaled","rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled","srmr")) %>%
  as.data.frame()%>%  tibble::rownames_to_column("index")

# random-intercept cross-lag panel model - correlation ####
## MOTHER ####
m.conf.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.conf =~ 1*moth.conf.w9 + 1*moth.conf.w11 + 1*moth.conf.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wmoth.conf.w9 =~ 1*moth.conf.w9
  wmoth.conf.w11 =~ 1*moth.conf.w11
  wmoth.conf.w13 =~ 1*moth.conf.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wmoth.conf.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wmoth.conf.w9
  wmoth.conf.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wmoth.conf.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wmoth.conf.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wmoth.conf.w11
  wmoth.conf.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wmoth.conf.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wmoth.conf.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wmoth.conf.w13
  wriv.w11 ~~ cov3*wmoth.conf.w11
  wriv.w13 ~~ cov3*wmoth.conf.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wmoth.conf.w9 
  wriv.w9 ~~ wmoth.conf.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.conf ~~ RI.conf
  RIadm ~~ RIriv + RI.conf
  RIriv ~~ RI.conf
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wmoth.conf.w9 ~~ wmoth.conf.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wmoth.conf.w11 ~~ vse*wmoth.conf.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wmoth.conf.w13 ~~ vse*wmoth.conf.w13

'
m.conf.riclpm.fit <- lavaan(m.conf.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T, estimator = "MLR") 
summary(m.conf.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)


moth.cor<-parameterEstimates(m.conf.riclpm.fit) %>% as.data.frame()%>%left_join(
  standardizedSolution(m.conf.riclpm.fit)%>%as.data.frame()%>%
    select(lhs, op, rhs, est.std),by = c("lhs", "op", "rhs"),suffix = c(".unstd", ".std"))

moth.cor.fit <- fitMeasures(m.conf.riclpm.fit, 
                            c("chisq.scaled", "df.scaled","cfi.scaled","rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled","srmr")) %>%
  as.data.frame()%>%  tibble::rownames_to_column("index")

## FATHER ####
f.conf.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.conf =~ 1*fath.conf.w9 + 1*fath.conf.w11 + 1*fath.conf.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wfath.conf.w9 =~ 1*fath.conf.w9
  wfath.conf.w11 =~ 1*fath.conf.w11
  wfath.conf.w13 =~ 1*fath.conf.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wfath.conf.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wfath.conf.w9
  wfath.conf.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wfath.conf.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wfath.conf.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wfath.conf.w11
  wfath.conf.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wfath.conf.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wfath.conf.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wfath.conf.w13
  wriv.w11 ~~ cov3*wfath.conf.w11
  wriv.w13 ~~ cov3*wfath.conf.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wfath.conf.w9 
  wriv.w9 ~~ wfath.conf.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.conf ~~ RI.conf
  RIadm ~~ RIriv + RI.conf
  RIriv ~~ RI.conf
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wfath.conf.w9 ~~ wfath.conf.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wfath.conf.w11 ~~ vse*wfath.conf.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wfath.conf.w13 ~~ vse*wfath.conf.w13

'
f.conf.riclpm.fit <- lavaan(f.conf.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T, estimator = "MLR") 
summary(f.conf.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)

fath.cor<-parameterEstimates(f.conf.riclpm.fit) %>% as.data.frame()%>%left_join(
  standardizedSolution(f.conf.riclpm.fit)%>%as.data.frame()%>%
    select(lhs, op, rhs, est.std),by = c("lhs", "op", "rhs"),suffix = c(".unstd", ".std"))

fath.cor.fit <- fitMeasures(f.conf.riclpm.fit, 
                            c("chisq.scaled", "df.scaled","cfi.scaled","rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled","srmr")) %>%
  as.data.frame()%>%  tibble::rownames_to_column("index")




# random-intercept cross-lag panel model - regression ####
## MOTHER ####
m.conf.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.conf =~ 1*moth.conf.w9 + 1*moth.conf.w11 + 1*moth.conf.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wmoth.conf.w9 =~ 1*moth.conf.w9
  wmoth.conf.w11 =~ 1*moth.conf.w11
  wmoth.conf.w13 =~ 1*moth.conf.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wmoth.conf.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wmoth.conf.w9
  wmoth.conf.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wmoth.conf.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wmoth.conf.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wmoth.conf.w11
  wmoth.conf.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wmoth.conf.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wmoth.conf.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wmoth.conf.w13
  wriv.w11 ~~ cov3*wmoth.conf.w11
  wriv.w13 ~~ cov3*wmoth.conf.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wmoth.conf.w9 
  wriv.w9 ~~ wmoth.conf.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.conf ~~ RI.conf
  RI.conf ~ RIadm + RIriv 
  RIriv ~~ RIadm
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wmoth.conf.w9 ~~ wmoth.conf.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wmoth.conf.w11 ~~ vse*wmoth.conf.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wmoth.conf.w13 ~~ vse*wmoth.conf.w13

'
m.conf.riclpm.fit <- lavaan(m.conf.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T, estimator = "MLR") 
summary(m.conf.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)

moth.reg<-parameterEstimates(m.conf.riclpm.fit) %>% as.data.frame()%>%left_join(
  standardizedSolution(m.conf.riclpm.fit)%>%as.data.frame()%>%
    select(lhs, op, rhs, est.std),by = c("lhs", "op", "rhs"),suffix = c(".unstd", ".std"))

moth.reg.fit <- fitMeasures(m.conf.riclpm.fit, 
                            c("chisq.scaled", "df.scaled","cfi.scaled","rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled","srmr")) %>%
  as.data.frame()%>%  tibble::rownames_to_column("index")

## FATHER ####
f.conf.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.conf =~ 1*fath.conf.w9 + 1*fath.conf.w11 + 1*fath.conf.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wfath.conf.w9 =~ 1*fath.conf.w9
  wfath.conf.w11 =~ 1*fath.conf.w11
  wfath.conf.w13 =~ 1*fath.conf.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wfath.conf.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wfath.conf.w9
  wfath.conf.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wfath.conf.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wfath.conf.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wfath.conf.w11
  wfath.conf.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wfath.conf.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wfath.conf.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wfath.conf.w13
  wriv.w11 ~~ cov3*wfath.conf.w11
  wriv.w13 ~~ cov3*wfath.conf.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wfath.conf.w9 
  wriv.w9 ~~ wfath.conf.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.conf ~~ RI.conf
  RI.conf ~ RIadm + RIriv 
  RIriv ~~ RIadm
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wfath.conf.w9 ~~ wfath.conf.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wfath.conf.w11 ~~ vse*wfath.conf.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wfath.conf.w13 ~~ vse*wfath.conf.w13

'
f.conf.riclpm.fit <- lavaan(f.conf.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T, estimator = "MLR") 
summary(f.conf.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)

fath.reg<-parameterEstimates(f.conf.riclpm.fit) %>% as.data.frame()%>%left_join(
  standardizedSolution(f.conf.riclpm.fit)%>%as.data.frame()%>%
    select(lhs, op, rhs, est.std),by = c("lhs", "op", "rhs"),suffix = c(".unstd", ".std"))

fath.reg.fit <- fitMeasures(m.conf.riclpm.fit, 
                            c("chisq.scaled", "df.scaled","cfi.scaled","rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled","srmr")) %>%
  as.data.frame()%>%  tibble::rownames_to_column("index")



# export ####
write_xlsx(list(
  mother.clpm = moth.clpm, mother.clpm.fit = moth.clpm.fit, 
  father.clpm = fath.clpm, father.clpm.fit = fath.clpm.fit,
  mother.cor = moth.cor, mother.cor.fit = moth.cor.fit,
  father.cor = fath.cor, father.cor.fit = fath.cor.fit,
  mother.reg = moth.reg, mother.reg.fit = moth.reg.fit,
  father.reg = fath.reg, father.reg.fit = fath.reg.fit), 
  "output/tables/RICLPM conflict.xlsx")


