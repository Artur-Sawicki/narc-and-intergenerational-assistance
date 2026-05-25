library(tidyverse)
library(writexl)
library(lavaan)
library(apaTables)
library(sjPlot)
source("script/functions/extractREG.R")

# 1. create longitudinal from waves 9, 11, and 13 ####
db9<- readRDS("data/clean/wave9.rds")
db11<- readRDS("data/clean/wave11.rds")
db13<- readRDS("data/clean/wave13.rds")

db9<- db9 %>% select(id, sex.w9,age.w9, adm.w9, riv.w9, par.emo.close.w9, moth.emo.close.w9, fath.emo.close.w9)
db11<- db11 %>% select(id, sex.w11,age.w11,adm.w11, riv.w11, par.emo.close.w11, moth.emo.close.w11, fath.emo.close.w11)
db13<- db13 %>% select(id, sex.w13,age.w13, adm.w13, riv.w13,par.emo.close.w13, moth.emo.close.w13, fath.emo.close.w13)

dblong<- inner_join(db9,db11, by = "id")%>%inner_join(db13, by="id")

# correlations in longitudinal data ####
apa.cor.table(dblong%>%select(-id), "output/tables/cor_emo_close.doc")

#regressions across waves ####
reg.w9.par <- lm("par.emo.close.w9 ~ adm.w9+riv.w9",data = dblong)
reg.w9.moth <- lm("moth.emo.close.w9 ~ adm.w9+riv.w9",data = dblong)
reg.w9.fath <- lm("fath.emo.close.w9 ~ adm.w9+riv.w9",data = dblong)

reg.w11.par <- lm("par.emo.close.w11 ~ adm.w11+riv.w11",data = dblong)
reg.w11.moth <- lm("moth.emo.close.w11 ~ adm.w11+riv.w11",data = dblong)
reg.w11.fath <- lm("fath.emo.close.w11 ~ adm.w11+riv.w11",data = dblong)

reg.w13.par <- lm("par.emo.close.w13 ~ adm.w13+riv.w13",data = dblong)
reg.w13.moth <- lm("moth.emo.close.w13 ~ adm.w13+riv.w13",data = dblong)
reg.w13.fath <- lm("fath.emo.close.w13 ~ adm.w13+riv.w13",data = dblong)

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

write_xlsx(results,"output/tables/emotional close regressions.xlsx")

#regressions across waves - gender moderation ####
dblong$sex.w9 <- haven::as_factor(dblong$sex.w9, levels = "labels")
dblong$sex.w9 <- sub("^-?\\d+\\s+", "", dblong$sex.w9)

table(dblong$sex.w9)

reg.w9.par <- lm("par.emo.close.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.moth <- lm("moth.emo.close.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)
reg.w9.fath <- lm("fath.emo.close.w9 ~ sex.w9*(adm.w9+riv.w9)",data = dblong)

reg.w11.par <- lm("par.emo.close.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.moth <- lm("moth.emo.close.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)
reg.w11.fath <- lm("fath.emo.close.w11 ~ sex.w9*(adm.w11+riv.w11)",data = dblong)

reg.w13.par <- lm("par.emo.close.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)
reg.w13.moth <- lm("moth.emo.close.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)
reg.w13.fath <- lm("fath.emo.close.w13 ~ sex.w9*(adm.w13+riv.w13)",data = dblong)

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

write_xlsx(results,"output/tables/emotional closeness interactions.xlsx")

# interaction plots ####
plot_model(reg.w13.fath, type = "pred", terms = c("riv.w13","sex.w9"))
plot_model(reg.w11.fath, type = "pred", terms = c("riv.w11","sex.w9"))
plot_model(reg.w9.fath, type = "pred", terms = c("riv.w9","sex.w9"))

# CLPM ####
## MOTHER ####
m.emo.close.clpm.mod <- "adm.w11 ~ a*adm.w9 + b*riv.w9 + c*moth.emo.close.w9
                adm.w13 ~ a*adm.w11 + b*riv.w11 + c*moth.emo.close.w11
                
                riv.w11 ~ d*adm.w9 + e*riv.w9 + f*moth.emo.close.w9
                riv.w13 ~ d*adm.w11 + e*riv.w11 + f*moth.emo.close.w11
                
                moth.emo.close.w11 ~ g*adm.w9 + h*riv.w9 + i*moth.emo.close.w9
                moth.emo.close.w13 ~ g*adm.w11 + h*riv.w11 + i*moth.emo.close.w11
                
                adm.w9 ~~ riv.w9 + moth.emo.close.w9
                adm.w11 ~~ riv.w11 + moth.emo.close.w11
                adm.w13 ~~ riv.w13 + moth.emo.close.w13
                
                riv.w11 ~~ moth.emo.close.w11
                riv.w9 ~~ moth.emo.close.w9
"

m.emo.close.clpm.fit <- sem(m.emo.close.clpm.mod, data = dblong, missing = "FIML", estimator = "MLR")
summary(m.emo.close.clpm.fit, fit.measures=T, standardized=T,rsquare=T)

moth.clpm<-parameterEstimates(m.emo.close.clpm.fit) %>% as.data.frame()%>%left_join(
  standardizedSolution(m.emo.close.clpm.fit)%>%as.data.frame()%>%
    select(lhs, op, rhs, est.std),by = c("lhs", "op", "rhs"),suffix = c(".unstd", ".std"))

moth.clpm.fit <- fitMeasures(m.emo.close.clpm.fit, 
                             c("chisq.scaled", "df.scaled","cfi.scaled","rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled","srmr")) %>%
  as.data.frame()%>%  tibble::rownames_to_column("index")
## FATHER ####
f.emo.close.clpm.mod <- "adm.w11 ~ a*adm.w9 + b*riv.w9 + c*fath.emo.close.w9
                adm.w13 ~ a*adm.w11 + b*riv.w11 + c*fath.emo.close.w11
                
                riv.w11 ~ d*adm.w9 + e*riv.w9 + f*fath.emo.close.w9
                riv.w13 ~ d*adm.w11 + e*riv.w11 + f*fath.emo.close.w11
                
                fath.emo.close.w11 ~ g*adm.w9 + h*riv.w9 + i*fath.emo.close.w9
                fath.emo.close.w13 ~ g*adm.w11 + h*riv.w11 + i*fath.emo.close.w11
                
                adm.w9 ~~ riv.w9 + fath.emo.close.w9
                adm.w11 ~~ riv.w11 + fath.emo.close.w11
                adm.w13 ~~ riv.w13 + fath.emo.close.w13
                
                riv.w11 ~~ fath.emo.close.w11
                riv.w9 ~~ fath.emo.close.w9
"
f.emo.close.clpm.fit <- sem(f.emo.close.clpm.mod, data = dblong, missing = "FIML", estimator = "MLR")
summary(f.emo.close.clpm.fit, fit.measures=T, standardized=T,rsquare=T)

fath.clpm<-parameterEstimates(f.emo.close.clpm.fit) %>% as.data.frame()%>%left_join(
  standardizedSolution(f.emo.close.clpm.fit)%>%as.data.frame()%>%
    select(lhs, op, rhs, est.std),by = c("lhs", "op", "rhs"),suffix = c(".unstd", ".std"))

fath.clpm.fit <- fitMeasures(f.emo.close.clpm.fit, 
                             c("chisq.scaled", "df.scaled","cfi.scaled","rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled","srmr")) %>%
  as.data.frame()%>%  tibble::rownames_to_column("index")

# random-intercept cross-lag panel model - correlation ####
## MOTHER ####
m.emo.close.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.emo.close =~ 1*moth.emo.close.w9 + 1*moth.emo.close.w11 + 1*moth.emo.close.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wmoth.emo.close.w9 =~ 1*moth.emo.close.w9
  wmoth.emo.close.w11 =~ 1*moth.emo.close.w11
  wmoth.emo.close.w13 =~ 1*moth.emo.close.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wmoth.emo.close.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wmoth.emo.close.w9
  wmoth.emo.close.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wmoth.emo.close.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wmoth.emo.close.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wmoth.emo.close.w11
  wmoth.emo.close.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wmoth.emo.close.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wmoth.emo.close.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wmoth.emo.close.w13
  wriv.w11 ~~ cov3*wmoth.emo.close.w11
  wriv.w13 ~~ cov3*wmoth.emo.close.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wmoth.emo.close.w9 
  wriv.w9 ~~ wmoth.emo.close.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.emo.close ~~ RI.emo.close
  RIadm ~~ RIriv + RI.emo.close
  RIriv ~~ RI.emo.close
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wmoth.emo.close.w9 ~~ wmoth.emo.close.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wmoth.emo.close.w11 ~~ vse*wmoth.emo.close.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wmoth.emo.close.w13 ~~ vse*wmoth.emo.close.w13

'
m.emo.close.riclpm.fit <- lavaan(m.emo.close.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T, estimator = "MLR") 
summary(m.emo.close.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)


moth.cor<-parameterEstimates(m.emo.close.riclpm.fit) %>% as.data.frame()%>%left_join(
  standardizedSolution(m.emo.close.riclpm.fit)%>%as.data.frame()%>%
    select(lhs, op, rhs, est.std),by = c("lhs", "op", "rhs"),suffix = c(".unstd", ".std"))

moth.cor.fit <- fitMeasures(m.emo.close.riclpm.fit, 
                            c("chisq.scaled", "df.scaled","cfi.scaled","rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled","srmr")) %>%
  as.data.frame()%>%  tibble::rownames_to_column("index")


## FATHER ####
f.emo.close.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.emo.close =~ 1*fath.emo.close.w9 + 1*fath.emo.close.w11 + 1*fath.emo.close.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wfath.emo.close.w9 =~ 1*fath.emo.close.w9
  wfath.emo.close.w11 =~ 1*fath.emo.close.w11
  wfath.emo.close.w13 =~ 1*fath.emo.close.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wfath.emo.close.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wfath.emo.close.w9
  wfath.emo.close.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wfath.emo.close.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wfath.emo.close.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wfath.emo.close.w11
  wfath.emo.close.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wfath.emo.close.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wfath.emo.close.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wfath.emo.close.w13
  wriv.w11 ~~ cov3*wfath.emo.close.w11
  wriv.w13 ~~ cov3*wfath.emo.close.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wfath.emo.close.w9 
  wriv.w9 ~~ wfath.emo.close.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.emo.close ~~ RI.emo.close
  RIadm ~~ RIriv + RI.emo.close
  RIriv ~~ RI.emo.close
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wfath.emo.close.w9 ~~ wfath.emo.close.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wfath.emo.close.w11 ~~ vse*wfath.emo.close.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wfath.emo.close.w13 ~~ vse*wfath.emo.close.w13

'
f.emo.close.riclpm.fit <- lavaan(f.emo.close.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T, estimator = "MLR") 
summary(f.emo.close.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)

fath.cor<-parameterEstimates(f.emo.close.riclpm.fit) %>% as.data.frame()%>%left_join(
  standardizedSolution(f.emo.close.riclpm.fit)%>%as.data.frame()%>%
    select(lhs, op, rhs, est.std),by = c("lhs", "op", "rhs"),suffix = c(".unstd", ".std"))

fath.cor.fit <- fitMeasures(f.emo.close.riclpm.fit, 
                            c("chisq.scaled", "df.scaled","cfi.scaled","rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled","srmr")) %>%
  as.data.frame()%>%  tibble::rownames_to_column("index")




# random-intercept cross-lag panel model - regression ####
## MOTHER ####
m.emo.close.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.emo.close =~ 1*moth.emo.close.w9 + 1*moth.emo.close.w11 + 1*moth.emo.close.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wmoth.emo.close.w9 =~ 1*moth.emo.close.w9
  wmoth.emo.close.w11 =~ 1*moth.emo.close.w11
  wmoth.emo.close.w13 =~ 1*moth.emo.close.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wmoth.emo.close.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wmoth.emo.close.w9
  wmoth.emo.close.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wmoth.emo.close.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wmoth.emo.close.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wmoth.emo.close.w11
  wmoth.emo.close.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wmoth.emo.close.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wmoth.emo.close.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wmoth.emo.close.w13
  wriv.w11 ~~ cov3*wmoth.emo.close.w11
  wriv.w13 ~~ cov3*wmoth.emo.close.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wmoth.emo.close.w9 
  wriv.w9 ~~ wmoth.emo.close.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.emo.close ~~ RI.emo.close
  RI.emo.close ~ RIadm + RIriv 
  RIriv ~~ RIadm
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wmoth.emo.close.w9 ~~ wmoth.emo.close.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wmoth.emo.close.w11 ~~ vse*wmoth.emo.close.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wmoth.emo.close.w13 ~~ vse*wmoth.emo.close.w13

'
m.emo.close.riclpm.fit <- lavaan(m.emo.close.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T, estimator = "MLR") 
summary(m.emo.close.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)

moth.reg<-parameterEstimates(m.emo.close.riclpm.fit) %>% as.data.frame()%>%left_join(
  standardizedSolution(m.emo.close.riclpm.fit)%>%as.data.frame()%>%
    select(lhs, op, rhs, est.std),by = c("lhs", "op", "rhs"),suffix = c(".unstd", ".std"))

moth.reg.fit <- fitMeasures(m.emo.close.riclpm.fit, 
                            c("chisq.scaled", "df.scaled","cfi.scaled","rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled","srmr")) %>%
  as.data.frame()%>%  tibble::rownames_to_column("index")

## FATHER ####
f.emo.close.riclpm.mod <- '
  # between-person
  RIadm =~ 1*adm.w9 + 1*adm.w11 + 1*adm.w13
  RIriv =~ 1*riv.w9 + 1*riv.w11 + 1*riv.w13 
  RI.emo.close =~ 1*fath.emo.close.w9 + 1*fath.emo.close.w11 + 1*fath.emo.close.w13 
  
  # within-person
  wadm.w9 =~ 1*adm.w9
  wadm.w11 =~ 1*adm.w11
  wadm.w13 =~ 1*adm.w13 

  wriv.w9 =~ 1*riv.w9
  wriv.w11 =~ 1*riv.w11
  wriv.w13 =~ 1*riv.w13
  
  wfath.emo.close.w9 =~ 1*fath.emo.close.w9
  wfath.emo.close.w11 =~ 1*fath.emo.close.w11
  wfath.emo.close.w13 =~ 1*fath.emo.close.w13
  
  # lagged
  wadm.w11 ~ a*wadm.w9 + b*wriv.w9 + e*wfath.emo.close.w9
  wriv.w11 ~ c*wadm.w9 + d*wriv.w9 + f*wfath.emo.close.w9
  wfath.emo.close.w11 ~ g*wadm.w9 + h*wriv.w9 + i*wfath.emo.close.w9
  
  wadm.w13 ~ a*wadm.w11 + b*wriv.w11 + e*wfath.emo.close.w11
  wriv.w13 ~ c*wadm.w11 + d*wriv.w11 + f*wfath.emo.close.w11
  wfath.emo.close.w13 ~ g*wadm.w11 + h*wriv.w11 + i*wfath.emo.close.w11

  # fluctuations
  wadm.w11 ~~ cov1*wriv.w11 + cov2*wfath.emo.close.w11
  wadm.w13 ~~ cov1*wriv.w13 + cov2*wfath.emo.close.w13
  wriv.w11 ~~ cov3*wfath.emo.close.w11
  wriv.w13 ~~ cov3*wfath.emo.close.w13
  
  # first wave cor
  wadm.w9 ~~ wriv.w9 + wfath.emo.close.w9 
  wriv.w9 ~~ wfath.emo.close.w9 
  
  # relationships on between-person level (cor or reg, to choose) 
  RIadm ~~ RIadm
  RIriv ~~ RIriv
  RI.emo.close ~~ RI.emo.close
  RI.emo.close ~ RIadm + RIriv 
  RIriv ~~ RIadm
  
  # within-person var
  wadm.w9 ~~ wadm.w9 # var
  wriv.w9 ~~ wriv.w9 
  wfath.emo.close.w9 ~~ wfath.emo.close.w9 
  
  wadm.w11 ~~ vadm*wadm.w11 # residual var
  wriv.w11 ~~ vriv*wriv.w11
  wfath.emo.close.w11 ~~ vse*wfath.emo.close.w11
  
  wadm.w13 ~~ vadm*wadm.w13 
  wriv.w13 ~~ vriv*wriv.w13
  wfath.emo.close.w13 ~~ vse*wfath.emo.close.w13

'
f.emo.close.riclpm.fit <- lavaan(f.emo.close.riclpm.mod,data = dblong, meanstructure = T,  int.ov.free = T, estimator = "MLR") 
summary(f.emo.close.riclpm.fit, fit.measures=T, standardized=T,rsquare=T)

fath.reg<-parameterEstimates(f.emo.close.riclpm.fit) %>% as.data.frame()%>%left_join(
  standardizedSolution(f.emo.close.riclpm.fit)%>%as.data.frame()%>%
    select(lhs, op, rhs, est.std),by = c("lhs", "op", "rhs"),suffix = c(".unstd", ".std"))

fath.reg.fit <- fitMeasures(m.emo.close.riclpm.fit, 
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
  "output/tables/RICLPM emotional closeness.xlsx")


