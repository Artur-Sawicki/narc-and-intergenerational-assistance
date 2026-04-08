library(tidyverse)
library(haven)

# load the data ####
d8<- read_sav("data/raw/anchor8.sav")
d9<- read_sav("data/raw/anchor9.sav")
d10<- read_sav("data/raw/anchor10.sav")
d11<- read_sav("data/raw/anchor11.sav")

d12a<- read_sav("data/raw/anchor12_CAPI.sav")
d12b<- read_sav("data/raw/anchor12_CATI.sav")
d12<- bind_rows(d12a,d12b,.id="metoda") #combine wave 12 to one dataset
rm(d12a,d12b)

d13a<- read_sav("data/raw/anchor13_CAPI.sav")
d13b<- read_sav("data/raw/anchor13_CATI.sav")
d13<- bind_rows(d13a,d13b,.id="metoda") #combine wave 13 to one dataset
rm(d13a,d13b)

biopar<- read_sav("data/raw/bioparent.sav")

# select varaibles ####
d8 <- d8 %>% select(id, 
                    igr43p1,igr46p1,igr43p3,igr46p3, #parental admiration
                    igr44p1,igr48p1,igr44p3,igr48p3, #parental dominance
)

d9<-d9%>%select(id,sex_gen,age, #anchor demografic
                per8i7,per8i9,per8i12,#riv
                per8i8,per8i10,per8i11,#adm
                igr41p1,igr41p3, # geo distance
                igr39p1,igr39p3, # freq of contact-anchor
                igr40p1,igr40p3, # emotional closeness
                igr45p1,igr47p1,igr45p3,igr47p3, #conflict
                igr42p1,igr49p1,igr42p3,igr49p3, #intimacy
                igr98p1,igr99p1,igr98p3,igr99p3 #ambivalence
)


d10<- d10 %>% select(id,
                     val2i2,val2i5,val2i6, # attitudes on intgen support,
                     igr56p1,igr56p3, #support provided
                     igr63p1,igr63p3, #support received
                     igr43p1,igr46p1,igr43p3,igr46p3, #parental admiration
                     igr44p1,igr48p1,igr44p3,igr48p3, #parental dominance
                     igr98p1,igr99p1,igr98p3,igr99p3 #ambivalence
)

d11<-d11%>%select(id,sex_gen,age, #anchor demografic
                  per8i7,per8i9,per8i12,#riv
                  per8i8,per8i10,per8i11,#adm
                  igr41p1,igr41p3, #geo distance
                  igr39p1,igr39p3, # freq of contact-anchor
                  igr40p1,igr40p3, # emotional closeness
                  igr45p1,igr47p1,igr45p3,igr47p3, #conflict
                  igr42p1,igr49p1,igr42p3,igr49p3, #intimacy
                  igr98p1,igr99p1,igr98p3,igr99p3 #ambivalence
)

d12<- d12 %>% select(id,
                     igr56p1,igr56p3, #support provided
                     igr63p1,igr63p3, #support received
                     igr43p1,igr46p1,igr43p3,igr46p3, #parental admiration
                     igr44p1,igr48p1,igr44p3,igr48p3, #parental dominance
)

d13<-d13%>%select(id,sex_gen,age, #anchor demografic
                  per8i7,per8i9,per8i12,#riv
                  per8i8,per8i10,per8i11,#adm
                  igr41p1,igr41p3, #geo distance
                  igr39p1,igr39p3, # freq of contact-anchor
                  igr40p1,igr40p3, # emotional closeness
                  igr45p1,igr47p1,igr45p3,igr47p3, #conflict
                  igr42p1,igr49p1,igr42p3,igr49p3, #intimacy
)





# total scores and renaming ####

d8<- d8 %>% rowwise%>% mutate(moth.adm.w8 = mean(c(igr43p1, igr46p1),na.rm=T),
                              fath.adm.w8 = mean(c(igr43p3, igr46p3),na.rm=T),
                              par.adm.w8 = mean(c(igr43p1, igr46p1, igr43p3, igr46p3),na.rm=T),
                              moth.dom.w8 = mean(c(igr44p1, igr48p1),na.rm=T),
                              fath.dom.w8 = mean(c(igr44p3, igr48p3),na.rm=T),
                              par.dom.w8 = mean(c(igr44p1, igr48p1, igr44p3, igr48p3),na.rm=T)
                              )
d9<- d9 %>% rowwise%>% mutate(riv.w9 = mean(c(per8i7,per8i9,per8i12),na.rm=T),
                              adm.w9 = mean(c(per8i8,per8i10,per8i11),na.rm=T),
                              par.geo.w9 = mean(c(igr41p1,igr41p3),na.rm=T),
                              par.con.frq.w9 = mean(c(igr39p1,igr39p3),na.rm=T),
                              par.emo.close.w9 = mean(c(igr40p1,igr40p3),na.rm=T),
                              moth.conf.w9 = mean(c(igr45p1,igr47p1),na.rm=T),
                              fath.conf.w9 = mean(c(igr45p3,igr47p3),na.rm=T),
                              par.conf.w9 = mean(c(igr45p1,igr47p1,igr45p3,igr47p3),na.rm=T),
                              moth.intim.w9 = mean(c(igr42p1,igr49p1),na.rm=T),
                              fath.intim.w9 = mean(c(igr42p3,igr49p3),na.rm=T),
                              par.intim.w9 = mean(c(igr42p1,igr49p1,igr42p3,igr49p3),na.rm=T),
                              moth.amb.w9 = mean(c(igr98p1, igr99p1),na.rm=T),
                              fath.amb.w9 = mean(c(igr98p3, igr99p3),na.rm=T),
                              par.amb.w9 = mean(c(igr98p1, igr99p1, igr98p3, igr99p3),na.rm=T))%>%
                       rename(sex.w9 = sex_gen,
                              age.w9 = age,
                              moth.geo.w9 = igr41p1,
                              fath.geo.w9 = igr41p3,
                              moth.con.frq.w9 = igr39p1,
                              fath.con.frq.w9 = igr39p3,
                              moth.emo.close.w9 = igr40p1,
                              fath.emo.close.w9 = igr40p3)


d10<-d10%>%rowwise%>% mutate(moth.adm.w10 = mean(c(igr43p1, igr46p1),na.rm=T),
                          fath.adm.w10 = mean(c(igr43p3, igr46p3),na.rm=T),
                          par.adm.w10 = mean(c(igr43p1, igr46p1, igr43p3, igr46p3),na.rm=T),
                          moth.dom.w10 = mean(c(igr44p1, igr48p1),na.rm=T),
                          fath.dom.w10 = mean(c(igr44p3, igr48p3),na.rm=T),
                          par.dom.w10 = mean(c(igr44p1, igr48p1, igr44p3, igr48p3),na.rm=T),
                          moth.amb.w10 = mean(c(igr98p1, igr99p1),na.rm=T),
                          fath.amb.w10 = mean(c(igr98p3, igr99p3),na.rm=T),
                          par.amb.w10 = mean(c(igr98p1, igr99p1, igr98p3, igr99p3),na.rm=T),
                          par.sup.prov.w10 = mean(c(igr56p1,igr56p3),na.rm=T),
                          par.sup.rec.w10 = mean(c(igr63p1,igr63p3),na.rm=T),
                          intgen.sup.att.w10 = mean(c(val2i2,val2i5,val2i6),na.rm=T))%>%
                    rename(moth.sup.prov.w10 = igr56p1,
                           fath.sup.prov.w10 = igr56p3,
                           moth.sup.rec.w10 = igr63p1,
                           fath.sup.rec.w10 = igr63p3)

d11<- d11 %>% rowwise%>% mutate(riv.w11 = mean(c(per8i7,per8i9,per8i12),na.rm=T),
                                adm.w11 = mean(c(per8i8,per8i10,per8i11),na.rm=T),
                                par.geo.w11 = mean(c(igr41p1,igr41p3),na.rm=T),
                                par.con.frq.w11 = mean(c(igr39p1,igr39p3),na.rm=T),
                                par.emo.close.w11 = mean(c(igr40p1,igr40p3),na.rm=T),
                                moth.conf.w11 = mean(c(igr45p1,igr47p1),na.rm=T),
                                fath.conf.w11 = mean(c(igr45p3,igr47p3),na.rm=T),
                                par.conf.w11 = mean(c(igr45p1,igr47p1,igr45p3,igr47p3),na.rm=T),
                                moth.intim.w11 = mean(c(igr42p1,igr49p1),na.rm=T),
                                fath.intim.w11 = mean(c(igr42p3,igr49p3),na.rm=T),
                                par.intim.w11 = mean(c(igr42p1,igr49p1,igr42p3,igr49p3),na.rm=T),
                                moth.amb.w11 = mean(c(igr98p1, igr99p1),na.rm=T),
                                fath.amb.w11 = mean(c(igr98p3, igr99p3),na.rm=T),
                                par.amb.w11 = mean(c(igr98p1, igr99p1, igr98p3, igr99p3),na.rm=T))%>%
  rename(sex.w11 = sex_gen,
         age.w11 = age,
         moth.geo.w11 = igr41p1,
         fath.geo.w11 = igr41p3,
         moth.con.frq.w11 = igr39p1,
         fath.con.frq.w11 = igr39p3,
         moth.emo.close.w11 = igr40p1,
         fath.emo.close.w11 = igr40p3)


d12<-d12%>%rowwise%>% mutate(moth.adm.w12 = mean(c(igr43p1, igr46p1),na.rm=T),
                             fath.adm.w12 = mean(c(igr43p3, igr46p3),na.rm=T),
                             par.adm.w12 = mean(c(igr43p1, igr46p1, igr43p3, igr46p3),na.rm=T),
                             moth.dom.w12 = mean(c(igr44p1, igr48p1),na.rm=T),
                             fath.dom.w12 = mean(c(igr44p3, igr48p3),na.rm=T),
                             par.dom.w12 = mean(c(igr44p1, igr48p1, igr44p3, igr48p3),na.rm=T),
                             par.sup.prov.w12 = mean(c(igr56p1,igr56p3),na.rm=T),
                             par.sup.rec.w12 = mean(c(igr63p1,igr63p3),na.rm=T))%>%
  rename(moth.sup.prov.w12 = igr56p1,
         fath.sup.prov.w12 = igr56p3,
         moth.sup.rec.w12 = igr63p1,
         fath.sup.rec.w12 = igr63p3)


d13<- d13 %>% rowwise%>% mutate(riv.w13 = mean(c(per8i7,per8i9,per8i12),na.rm=T),
                                adm.w13 = mean(c(per8i8,per8i10,per8i11),na.rm=T),
                                par.geo.w13 = mean(c(igr41p1,igr41p3),na.rm=T),
                                par.con.frq.w13 = mean(c(igr39p1,igr39p3),na.rm=T),
                                par.emo.close.w13 = mean(c(igr40p1,igr40p3),na.rm=T),
                                moth.conf.w13 = mean(c(igr45p1,igr47p1),na.rm=T),
                                fath.conf.w13 = mean(c(igr45p3,igr47p3),na.rm=T),
                                par.conf.w13 = mean(c(igr45p1,igr47p1,igr45p3,igr47p3),na.rm=T),
                                moth.intim.w13 = mean(c(igr42p1,igr49p1),na.rm=T),
                                fath.intim.w13 = mean(c(igr42p3,igr49p3),na.rm=T),
                                par.intim.w13 = mean(c(igr42p1,igr49p1,igr42p3,igr49p3),na.rm=T))%>%
  rename(sex.w13 = sex_gen,
         age.w13 = age,
         moth.geo.w13 = igr41p1,
         fath.geo.w13 = igr41p3,
         moth.con.frq.w13 = igr39p1,
         fath.con.frq.w13 = igr39p3,
         moth.emo.close.w13 = igr40p1,
         fath.emo.close.w13 = igr40p3)

biopar <- biopar%>%select(id, partype,contactw9,contactw11,contactw13)%>%
  rename(bio.con.frq.w9 = contactw9,bio.con.frq.w11 = contactw11,bio.con.frq.w13 = contactw13)%>%
  filter(partype==1|partype==2)
biopar_wide <- biopar %>%  
  pivot_wider(id_cols = id,names_from = partype,  values_from = starts_with("bio.con.frq"),names_glue = "{.value}_p{partype}")

names(biopar_wide) <- c("id",
                        "p.moth.con.frq.w9",  "p.fath.con.frq.w9",
                        "p.moth.con.frq.w11", "p.fath.con.frq.w11", 
                        "p.moth.con.frq.w13", "p.fath.con.frq.w13")


# reliabilities ####

library(psych)
library(purrr)


# named list: scale -> items
scales <- list(
  # W8
  moth.adm.w8 = c("igr43p1","igr46p1"),
  fath.adm.w8 = c("igr43p3","igr46p3"),
  par.adm.w8  = c("igr43p1","igr46p1","igr43p3","igr46p3"),
  moth.dom.w8 = c("igr44p1","igr48p1"),
  fath.dom.w8 = c("igr44p3","igr48p3"),
  par.dom.w8  = c("igr44p1","igr48p1","igr44p3","igr48p3"),
  
  # W9
  riv.w9 = c("per8i7","per8i9","per8i12"),
  adm.w9 = c("per8i8","per8i10","per8i11"),
  par.geo.w9 = c("igr41p1","igr41p3"),
  par.con.frq.w9 = c("igr39p1","igr39p3"),
  par.emo.close.w9 = c("igr40p1","igr40p3"),
  moth.conf.w9 = c("igr45p1","igr47p1"),
  fath.conf.w9 = c("igr45p3","igr47p3"),
  par.conf.w9 = c("igr45p1","igr47p1","igr45p3","igr47p3"),
  moth.intim.w9 = c("igr42p1","igr49p1"),
  fath.intim.w9 = c("igr42p3","igr49p3"),
  par.intim.w9 = c("igr42p1","igr49p1","igr42p3","igr49p3"),
  moth.amb.w9 = c("igr98p1","igr99p1"),
  fath.amb.w9 = c("igr98p3","igr99p3"),
  par.amb.w9 = c("igr98p1","igr99p1","igr98p3","igr99p3"),
  
  # W10
  moth.adm.w10 = c("igr43p1","igr46p1"),
  fath.adm.w10 = c("igr43p3","igr46p3"),
  par.adm.w10 = c("igr43p1","igr46p1","igr43p3","igr46p3"),
  moth.dom.w10 = c("igr44p1","igr48p1"),
  fath.dom.w10 = c("igr44p3","igr48p3"),
  par.dom.w10 = c("igr44p1","igr48p1","igr44p3","igr48p3"),
  moth.amb.w10 = c("igr98p1","igr99p1"),
  fath.amb.w10 = c("igr98p3","igr99p3"),
  par.amb.w10 = c("igr98p1","igr99p1","igr98p3","igr99p3"),
  par.sup.prov.w10 = c("igr56p1","igr56p3"),
  par.sub.rec.w10 = c("igr63p1","igr63p3"),
  intgen.sup.att.w10 = c("val2i2","val2i5","val2i6"),
  
  # W11
  riv.w11 = c("per8i7","per8i9","per8i12"),
  adm.w11 = c("per8i8","per8i10","per8i11"),
  par.geo.w11 = c("igr41p1","igr41p3"),
  par.con.frq.w11 = c("igr39p1","igr39p3"),
  par.emo.close.w11 = c("igr40p1","igr40p3"),
  moth.conf.w11 = c("igr45p1","igr47p1"),
  fath.conf.w11 = c("igr45p3","igr47p3"),
  par.conf.w11 = c("igr45p1","igr47p1","igr45p3","igr47p3"),
  moth.intim.w11 = c("igr42p1","igr49p1"),
  fath.intim.w11 = c("igr42p3","igr49p3"),
  par.intim.w11 = c("igr42p1","igr49p1","igr42p3","igr49p3"),
  moth.amb.w11 = c("igr98p1","igr99p1"),
  fath.amb.w11 = c("igr98p3","igr99p3"),
  par.amb.w11 = c("igr98p1","igr99p1","igr98p3","igr99p3"),
  
  # W12
  moth.adm.w12 = c("igr43p1","igr46p1"),
  fath.adm.w12 = c("igr43p3","igr46p3"),
  par.adm.w12 = c("igr43p1","igr46p1","igr43p3","igr46p3"),
  moth.dom.w12 = c("igr44p1","igr48p1"),
  fath.dom.w12 = c("igr44p3","igr48p3"),
  par.dom.w12 = c("igr44p1","igr48p1","igr44p3","igr48p3"),
  par.sup.prov.w12 = c("igr56p1","igr56p3"),
  par.sub.rec.w12 = c("igr63p1","igr63p3"),
  
  # W13
  riv.w13 = c("per8i7","per8i9","per8i12"),
  adm.w13 = c("per8i8","per8i10","per8i11"),
  par.geo.w13 = c("igr41p1","igr41p3"),
  par.con.frq.w13 = c("igr39p1","igr39p3"),
  par.emo.close.w13 = c("igr40p1","igr40p3"),
  moth.conf.w13 = c("igr45p1","igr47p1"),
  fath.conf.w13 = c("igr45p3","igr47p3"),
  par.conf.w13 = c("igr45p1","igr47p1","igr45p3","igr47p3"),
  moth.intim.w13 = c("igr42p1","igr49p1"),
  fath.intim.w13 = c("igr42p3","igr49p3"),
  par.intim.w13 = c("igr42p1","igr49p1","igr42p3","igr49p3")
)

data_list <- list(
  w8 = d8,
  w9 = d9,
  w10 = d10,
  w11 = d11,
  w12 = d12,
  w13 = d13
)

get_alpha <- function(data, items){
  # keep only existing columns (important if some missing)
  items <- items[items %in% names(data)]
  if(length(items) < 2) return(NA)
  psych::alpha(data[, items], warnings = FALSE)$total$raw_alpha
}

alpha_table <- imap_dfr(scales, function(items, scale_name){
  
  # detect wave from name (e.g., ".w9")
  wave <- sub(".*\\.(w[0-9]+)$", "\\1", scale_name)
  
  data <- data_list[[wave]]
  
  tibble(
    scale = scale_name,
    wave = wave,
    n_items = length(items),
    alpha = get_alpha(data, items)
  )
})

#export ####

write_xlsx(alpha_table, "output/tables/alphas.xlsx")

# export clean datasets ####
d8<- d8 %>%select(id,moth.adm.w8:par.dom.w8)
d9 <- d9%>%select(id,sex.w9,age.w9,moth.geo.w9:fath.emo.close.w9,riv.w9: par.amb.w9)
d10<- d10 %>%select(id,moth.sup.prov.w10:fath.sup.rec.w10,moth.adm.w10:intgen.sup.att.w10)
d11 <- d11%>%select(id,sex.w11,age.w11,moth.geo.w11:fath.emo.close.w11,riv.w11: par.amb.w11)
d12 <- d12%>%select(id,moth.sup.prov.w12:fath.sup.rec.w12,moth.adm.w12:par.sup.rec.w12)
d13 <- d13%>%select(id,sex.w13,age.w13,moth.geo.w13:fath.emo.close.w13,riv.w13: par.intim.w13)

bio9<- biopar_wide%>%select(id, p.moth.con.frq.w9,p.fath.con.frq.w9)
db9<- left_join(d9,bio9,by="id")

bio11<- biopar_wide%>%select(id, p.moth.con.frq.w11,p.fath.con.frq.w11)
db11<- left_join(d11,bio11,by="id")

bio13<- biopar_wide%>%select(id, p.moth.con.frq.w13,p.fath.con.frq.w13)
db13<- left_join(d13,bio13,by="id")


saveRDS(d8, "data/clean/wave8.rds")
saveRDS(db9, "data/clean/wave9.rds")
saveRDS(d10, "data/clean/wave10.rds")
saveRDS(db11, "data/clean/wave11.rds")
saveRDS(d12, "data/clean/wave12.rds")
saveRDS(db13, "data/clean/wave13.rds")
