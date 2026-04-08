library(tidyverse)
library(apaTables)
library(psych)
library(writexl)

# 1. load data - waves 9, 11, and 13 ####
db9<- readRDS("data/clean/wave9.rds")
db11<- readRDS("data/clean/wave11.rds")
db13<- readRDS("data/clean/wave13.rds")

# 2. select variables ####
db9<- db9 %>% select(sex.w9,age.w9, adm.w9, riv.w9, 
                    par.geo.w9, moth.geo.w9, fath.geo.w9,
                    par.con.frq.w9, moth.con.frq.w9, fath.con.frq.w9,
                    p.moth.con.frq.w9, p.fath.con.frq.w9,
                    par.emo.close.w9, moth.emo.close.w9, fath.emo.close.w9,
                    par.conf.w9, moth.conf.w9, fath.conf.w9,
                    par.intim.w9, moth.intim.w9, fath.intim.w9,
                    par.amb.w9, moth.amb.w9, fath.amb.w9)

db11<- db11 %>% select(sex.w11,age.w11,adm.w11, riv.w11, 
                     par.geo.w11, moth.geo.w11, fath.geo.w11,
                     par.con.frq.w11, moth.con.frq.w11, fath.con.frq.w11,
                     p.moth.con.frq.w11, p.fath.con.frq.w11,
                     par.emo.close.w11, moth.emo.close.w11, fath.emo.close.w11,
                     par.conf.w11, moth.conf.w11, fath.conf.w11,
                     par.intim.w11, moth.intim.w11, fath.intim.w11,
                     par.amb.w11, moth.amb.w11, fath.amb.w11)

db13<- db13 %>% select(sex.w13,age.w13, adm.w13, riv.w13, 
                       par.geo.w13, moth.geo.w13, fath.geo.w13,
                       par.con.frq.w13, moth.con.frq.w13, fath.con.frq.w13,
                       p.moth.con.frq.w13, p.fath.con.frq.w13,
                       par.emo.close.w13, moth.emo.close.w13, fath.emo.close.w13,
                       par.conf.w13, moth.conf.w13, fath.conf.w13,
                       par.intim.w13, moth.intim.w13, fath.intim.w13)


#3. descriptives ####

tab <- bind_rows(
  as.data.frame(table(db9$sex.w9))  %>% mutate(wave = "w9"),
  as.data.frame(table(db11$sex.w11)) %>% mutate(wave = "w11"),
  as.data.frame(table(db13$sex.w13)) %>% mutate(wave = "w13")
) %>%
  rename(sex = Var1, n = Freq) %>%
  pivot_wider(names_from = wave, values_from = n) %>%
  mutate(sex = recode(as.numeric(as.character(sex)), `1` = "men",`2` = "women"))

tab

desc9 <- describe(db9%>%select(-sex.w9))%>%as.data.frame()%>%rownames_to_column(var = "variable")
desc11 <- describe(db11%>%select(-sex.w11))%>%as.data.frame()%>%rownames_to_column(var = "variable")
desc13 <- describe(db13%>%select(-sex.w13))%>%as.data.frame()%>%rownames_to_column(var = "variable")

# export ####
write_xlsx(list(wave9 = desc9,wave11=desc11,wave13=desc13, gender = tab), "output/tables/basicdesc.xlsx")
#4. export correlations within samples ####
apa.cor.table(db9, "output/tables/db9cor.doc")
apa.cor.table(db11, "output/tables/db11cor.doc")
apa.cor.table(db13, "output/tables/db13cor.doc")

