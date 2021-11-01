library(data.table);library(magrittr);library(parallel);library(fst);library(stats);library(imputeTS);library(readxl)

#=setwd("/home/js/ShinyApps/chi-hun.kim/UKbiobank-KCH")
bd <- fst::read_fst("/home/heejooko/ShinyApps/UKbiobank/ukb47038.fst", as.data.table = T)

a<-data.table()

# 4 Instances
# _0	Initial assessment visit (2006-2010) at which participants were recruited and consent given
# _1	First repeat assessment visit (2012-13)
# _2	Imaging visit (2014+)
# _3	First repeat imaging visit (2019+)

#Population characteristics------------------------------------------------------------------
a$ID<-bd$f.eid
a$age<-bd$f.21022.0.0 #age at recruitment [years]
a$sex<-ifelse(bd$f.31.0.0==0,"F","M")
a$townsend_deprivation_index<-bd$f.189.0.0
a$visit_date_0<-bd$f.53.0.0
a$visit_date_1<-bd$f.53.1.0
a$visit_date_2<-bd$f.53.2.0
a$visit_date_3<-bd$f.53.3.0

#Summed MET minutes per week for all activity 22040 없다.
# a$MET_activity<-bd$f.22040.0.0 

#Smoking
# -3	Prefer not to answer / 0	Never / 1	Previous / 2	Current
a$smoking_status_0<-bd$f.20116.0.0
a$smoking_status_1<-bd$f.20116.1.0
a$smoking_status_2<-bd$f.20116.2.0
a$smoking_status_3<-bd$f.20116.3.0

a$smoking_stop_age<-bd$f.22507.0.0

a$smoking_packyears_0<-bd$f.20161.0.0
a$smoking_packyears_1<-bd$f.20161.1.0
a$smoking_packyears_2<-bd$f.20161.2.0
a$smoking_packyears_3<-bd$f.20161.3.0

#Alcohol
# -3	Prefer not to answer / 0	Never / 1	Previous / 2	Current
a$alcohol_status_0<-bd$f.20117.0.0
a$alcohol_status_1<-bd$f.20117.1.0
a$alcohol_status_2<-bd$f.20117.2.0
a$alcohol_status_3<-bd$f.20117.3.0

# 1	Daily or almost daily / 2	Three or four times a week / 3	Once or twice a week / 4	One to three times a month / 5	Special occasions only / 6	Never / -3	Prefer not to answer
a$alcohol_frequency_0<-bd$f.1558.0.0
a$alcohol_frequency_1<-bd$f.1558.1.0
a$alcohol_frequency_2<-bd$f.1558.2.0
a$alcohol_frequency_3<-bd$f.1558.3.0

# -818	Prefer not to answer / -121	Do not know / 0	No / 1	Yes : ever addicted to alcohol
a$alcohol_addiction<-bd$f.20406.0.0

#Exposures------------------------------------------------------------------
#DM
#Non-cancer illness, self-reported
myCol<-colnames(bd)[grep("f.20002.0.",colnames(bd))]
a$noncancer_illness_self_0<-bd[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$noncancer_illness_self_0<-lapply(a$noncancer_illness_self_0,function(x){gsub("_NA","",x)}) %>% unlist

myCol<-colnames(bd)[grep("f.20002.1.",colnames(bd))]
a$noncancer_illness_self_1<-bd[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$noncancer_illness_self_1<-lapply(a$noncancer_illness_self_1,function(x){gsub("_NA","",x)}) %>% unlist

myCol<-colnames(bd)[grep("f.20002.2.",colnames(bd))]
a$noncancer_illness_self_2<-bd[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$noncancer_illness_self_2<-lapply(a$noncancer_illness_self_2,function(x){gsub("_NA","",x)}) %>% unlist

myCol<-colnames(bd)[grep("f.20002.3.",colnames(bd))]
a$noncancer_illness_self_3<-bd[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$noncancer_illness_self_3<-lapply(a$noncancer_illness_self_3,function(x){gsub("_NA","",x)}) %>% unlist

a$DM_self_0<-ifelse(grepl(pattern='1220|1221|1222|1223',x=a$noncancer_illness_self_0),1,0)
a$DM_self_1<-ifelse(grepl(pattern='1220|1221|1222|1223',x=a$noncancer_illness_self_1),1,0)
a$DM_self_2<-ifelse(grepl(pattern='1220|1221|1222|1223',x=a$noncancer_illness_self_2),1,0)
a$DM_self_3<-ifelse(grepl(pattern='1220|1221|1222|1223',x=a$noncancer_illness_self_3),1,0)

a$DM_diagnosed_0<-ifelse(bd$f.2443.0.0==1,1,ifelse(bd$f.2443.0.0==0,0,NA))
a$DM_diagnosed_1<-ifelse(bd$f.2443.1.0==1,1,ifelse(bd$f.2443.1.0==0,0,NA))
a$DM_diagnosed_2<-ifelse(bd$f.2443.2.0==1,1,ifelse(bd$f.2443.2.0==0,0,NA))
a$DM_diagnosed_3<-ifelse(bd$f.2443.3.0==1,1,ifelse(bd$f.2443.3.0==0,0,NA))

a$gestational_DM_0<-ifelse(bd$f.4041.0.0==1,1,ifelse(bd$f.4041.0.0==0,0,NA))
a$gestational_DM_1<-ifelse(bd$f.4041.1.0==1,1,ifelse(bd$f.4041.1.0==0,0,NA))
a$gestational_DM_2<-ifelse(bd$f.4041.2.0==1,1,ifelse(bd$f.4041.2.0==0,0,NA))
a$gestational_DM_3<-ifelse(bd$f.4041.3.0==1,1,ifelse(bd$f.4041.3.0==0,0,NA))

a$age_DM_diagnosed_0<-ifelse(bd$f.2976.0.0 %in% c(-1,-3),NA,bd$f.2976.0.0)
a$age_DM_diagnosed_1<-ifelse(bd$f.2976.1.0 %in% c(-1,-3),NA,bd$f.2976.1.0)
a$age_DM_diagnosed_2<-ifelse(bd$f.2976.2.0 %in% c(-1,-3),NA,bd$f.2976.2.0)
a$age_DM_diagnosed_3<-ifelse(bd$f.2976.3.0 %in% c(-1,-3),NA,bd$f.2976.3.0)

#f.2986 없음
# a$insulin_within_1y_diagnosed_DM_0<-ifelse(bd$f.2986.0.0 %in% c(-1,-3),NA,bd$f.2986.0.0)
# a$insulin_within_1y_diagnosed_DM_1<-ifelse(bd$f.2986.1.0 %in% c(-1,-3),NA,bd$f.2986.1.0)
# a$insulin_within_1y_diagnosed_DM_2<-ifelse(bd$f.2986.2.0 %in% c(-1,-3),NA,bd$f.2986.2.0)
# a$insulin_within_1y_diagnosed_DM_3<-ifelse(bd$f.2986.3.0 %in% c(-1,-3),NA,bd$f.2986.3.0)

#[mmol/L]
a$glucose_0<-bd$f.30740.0.0
a$glucose_1<-bd$f.30740.1.0

#[mmol/mol]
a$HbA1c_0<-bd$f.30750.0.0
a$HbA1c_1<-bd$f.30750.1.0

#fasting time 74가 없다
# a$fasting_time_0<-bd$f.74.0.0
# a$fasting_time_1<-bd$f.74.1.0

#BP
#[mmHg]
myCol<-colnames(bd)[grep("f.4080.0.",colnames(bd))]
a$sysBP_automated_0<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(bd)[grep("f.93.0.",colnames(bd))]
a$sysBP_manual_0<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(bd)[grep("f.4080.1.",colnames(bd))]
a$sysBP_automated_1<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(bd)[grep("f.93.1.",colnames(bd))]
a$sysBP_manual_1<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(bd)[grep("f.4080.2.",colnames(bd))]
a$sysBP_automated_2<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(bd)[grep("f.93.2.",colnames(bd))]
a$sysBP_manual_2<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(bd)[grep("f.4080.3.",colnames(bd))]
a$sysBP_automated_3<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(bd)[grep("f.93.3.",colnames(bd))]
a$sysBP_manual_3<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]

a$sysBP_0<-ifelse(is.na(a$sysBP_automated_0),a$sysBP_manual_0,a$sysBP_automated_0)
a$sysBP_1<-ifelse(is.na(a$sysBP_automated_1),a$sysBP_manual_1,a$sysBP_automated_1)
a$sysBP_2<-ifelse(is.na(a$sysBP_automated_2),a$sysBP_manual_2,a$sysBP_automated_2)
a$sysBP_3<-ifelse(is.na(a$sysBP_automated_3),a$sysBP_manual_3,a$sysBP_automated_3)

myCol<-colnames(bd)[grep("f.4079.0.",colnames(bd))]
a$diaBP_automated_0<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(bd)[grep("f.94.0.",colnames(bd))]
a$diaBP_manual_0<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(bd)[grep("f.4079.1.",colnames(bd))]
a$diaBP_automated_1<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(bd)[grep("f.94.1.",colnames(bd))]
a$diaBP_manual_1<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(bd)[grep("f.4079.2.",colnames(bd))]
a$diaBP_automated_2<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(bd)[grep("f.94.2.",colnames(bd))]
a$diaBP_manual_2<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(bd)[grep("f.4079.3.",colnames(bd))]
a$diaBP_automated_3<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(bd)[grep("f.94.3.",colnames(bd))]
a$diaBP_manual_3<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]

a$diaBP_0<-ifelse(is.na(a$diaBP_automated_0),a$diaBP_manual_0,a$diaBP_automated_0)
a$diaBP_1<-ifelse(is.na(a$diaBP_automated_1),a$diaBP_manual_1,a$diaBP_automated_1)
a$diaBP_2<-ifelse(is.na(a$diaBP_automated_2),a$diaBP_manual_2,a$diaBP_automated_2)
a$diaBP_3<-ifelse(is.na(a$diaBP_automated_3),a$diaBP_manual_3,a$diaBP_automated_3)

#Abdominal obesity
#[cm]
a$WC_0<-bd$f.48.0.0
a$WC_1<-bd$f.48.1.0
a$WC_2<-bd$f.48.2.0
a$WC_3<-bd$f.48.3.0

#Lipidic profile
#[mmol/L]
a$TG_0<-bd$f.30870.0.0
a$TG_1<-bd$f.30870.1.0

a$HDL_0<-bd$f.30760.0.0
a$HDL_1<-bd$f.30760.1.0

#Insulin
#serum insulin 변수 못 찾음

#Medication
# 1	Cholesterol lowering medication / 2	Blood pressure medication / 3	Insulin / -7	None of the above / -1	Do not know / -3	Prefer not to answer
temp<-paste0(bd$f.6177.0.0," ",bd$f.6177.0.1," ",bd$f.6177.0.2)
a$cholesterol_medication_0<-ifelse(grepl("1",temp),"1","0")
a$BP_medication_0<-ifelse(grepl("2",temp),"1","0")
a$insulin_medication_0<-ifelse(grepl("3",temp),"1","0")

temp<-paste0(bd$f.6177.1.0," ",bd$f.6177.1.1," ",bd$f.6177.1.2)
a$cholesterol_medication_1<-ifelse(grepl("1",temp),"1","0")
a$BP_medication_1<-ifelse(grepl("2",temp),"1","0")
a$insulin_medication_1<-ifelse(grepl("3",temp),"1","0")

temp<-paste0(bd$f.6177.2.0," ",bd$f.6177.2.1," ",bd$f.6177.2.2)
a$cholesterol_medication_2<-ifelse(grepl("1",temp),"1","0")
a$BP_medication_2<-ifelse(grepl("2",temp),"1","0")
a$insulin_medication_2<-ifelse(grepl("3",temp),"1","0")

temp<-paste0(bd$f.6177.3.0," ",bd$f.6177.3.1," ",bd$f.6177.3.2)
a$cholesterol_medication_3<-ifelse(grepl("1",temp),"1","0")
a$BP_medication_3<-ifelse(grepl("2",temp),"1","0")
a$insulin_medication_3<-ifelse(grepl("3",temp),"1","0")

#MetS Diagnostic Criteria

a$DM_0<-ifelse(a$DM_diagnosed_0==1 | a$insulin_medication_0==1,1,0)
a$DM_1<-ifelse(a$DM_diagnosed_1==1 | a$insulin_medication_1==1,1,0)
a$DM_2<-ifelse(a$DM_diagnosed_2==1 | a$insulin_medication_2==1,1,0)
a$DM_3<-ifelse(a$DM_diagnosed_3==1 | a$insulin_medication_3==1,1,0)


a$MetS_NCEPATPIII_count_0<-as.integer(a$DM_0)+
                            as.integer(a$sysBP_0>=130 | a$diaBP_0>=85 | a$BP_medication_0==1)+
                            as.integer((a$sex=="F" & a$WC_0>=88) | (a$sex=="M" & a$WC_0>=102))+
                            as.integer(a$TG_0>=1.7 & ((a$sex=="M" & a$HDL_0<1.03) | (a$sex=="F" & a$HDL_0<1.29)))
a$MetS_NCEPATPIII_count_1<-as.integer(a$DM_1)+
                            as.integer(a$sysBP_1>=130 | a$diaBP_1>=85 | a$BP_medication_1==1)+
                            as.integer((a$sex=="F" & a$WC_1>=88) | (a$sex=="M" & a$WC_1>=102))+
                            as.integer(a$TG_1>=1.7 & ((a$sex=="M" & a$HDL_1<1.03) | (a$sex=="F" & a$HDL_1<1.29)))
a$MetS_NCEPATPIII_0<-ifelse(a$MetS_NCEPATPIII_count_0>=3,1,0)
a$MetS_NCEPATPIII_1<-ifelse(a$MetS_NCEPATPIII_count_1>=3,1,0)

a$MetS_IDF_count_0<-as.integer(a$DM_0)+
                    as.integer(a$sysBP_0>=130 | a$diaBP_0>=85 | a$BP_medication_0==1)+
                    as.integer((a$sex=="F" & a$WC_0>=80) | (a$sex=="M" & a$WC_0>=94))+
                    as.integer((a$TG_0>=1.7 & ((a$sex=="M" & a$HDL_0<1.03) | (a$sex=="F" & a$HDL_0<1.29))) | (a$cholesterol_medication_0==1))
a$MetS_IDF_count_1<-as.integer(a$DM_1)+
                    as.integer(a$sysBP_1>=130 | a$diaBP_1>=85 | a$BP_medication_1==1)+
                    as.integer((a$sex=="F" & a$WC_1>=80) | (a$sex=="M" & a$WC_1>=94))+
                    as.integer((a$TG_1>=1.7 & ((a$sex=="M" & a$HDL_1<1.03) | (a$sex=="F" & a$HDL_1<1.29))) | (a$cholesterol_medication_1==1))

criteria_c<-((a$sex=="F" & a$WC_0>=80) | (a$sex=="M" & a$WC_0>=94))
criteria_count_abd<-as.integer(a$DM_0)+
                    as.integer(a$sysBP_0>=130 | a$diaBP_0>=85 | a$BP_medication_0==1)+
                    as.integer((a$TG_0>=1.7 & ((a$sex=="M" & a$HDL_0<1.03) | (a$sex=="F" & a$HDL_0<1.29))) | (a$cholesterol_medication_0==1))
a$MetS_IDF_0<-ifelse(is.na(criteria_c) | is.na(criteria_count_abd),NA,ifelse(criteria_c & criteria_count_abd>=2,1,0))

criteria_c<-((a$sex=="F" & a$WC_1>=80) | (a$sex=="M" & a$WC_1>=94))
criteria_count_abd<-as.integer(a$DM_1)+
                    as.integer(a$sysBP_1>=130 | a$diaBP_1>=85 | a$BP_medication_1==1)+
                    as.integer((a$TG_1>=1.7 & ((a$sex=="M" & a$HDL_1<1.03) | (a$sex=="F" & a$HDL_1<1.29))) | (a$cholesterol_medication_1==1))
a$MetS_IDF_1<-ifelse(is.na(criteria_c) | is.na(criteria_count_abd),NA,ifelse(criteria_c & criteria_count_abd>=2,1,0))


sapply(c("MetS_NCEPATPIII_count_0","MetS_NCEPATPIII_count_1","MetS_IDF_count_0","MetS_IDF_count_1"),
       function(v){a[[v]] %>% as.factor %>% summary})

sapply(c("MetS_NCEPATPIII_0","MetS_NCEPATPIII_1","MetS_IDF_0","MetS_IDF_1"),
       function(v){a[[v]] %>% as.factor %>% summary})


#Biomarkers
#IGF-1 [nmol/L]
a$IGF1_0<-bd$f.30770.0.0
a$IGF1_1<-bd$f.30770.1.0

#SHBG [nmol/L]
a$SHBG_0<-bd$f.30830.0.0
a$SHBG_1<-bd$f.30830.1.0


#Other past medical conditions------------------------------------------------------------------
a$angina_self_0<-ifelse(grepl(pattern='1074',x=a$noncancer_illness_self_0),1,0)
a$angina_self_1<-ifelse(grepl(pattern='1074',x=a$noncancer_illness_self_1),1,0)
a$angina_self_2<-ifelse(grepl(pattern='1074',x=a$noncancer_illness_self_2),1,0)
a$angina_self_3<-ifelse(grepl(pattern='1074',x=a$noncancer_illness_self_3),1,0)

a$heartattack_or_MI_self_0<-ifelse(grepl(pattern='1075',x=a$noncancer_illness_self_0),1,0)
a$heartattack_or_MI_self_1<-ifelse(grepl(pattern='1075',x=a$noncancer_illness_self_1),1,0)
a$heartattack_or_MI_self_2<-ifelse(grepl(pattern='1075',x=a$noncancer_illness_self_2),1,0)
a$heartattack_or_MI_self_3<-ifelse(grepl(pattern='1075',x=a$noncancer_illness_self_3),1,0)

a$ischaemicstroke_self_0<-ifelse(grepl(pattern='1583',x=a$noncancer_illness_self_0),1,0)
a$ischaemicstroke_self_1<-ifelse(grepl(pattern='1583',x=a$noncancer_illness_self_1),1,0)
a$ischaemicstroke_self_2<-ifelse(grepl(pattern='1583',x=a$noncancer_illness_self_2),1,0)
a$ischaemicstroke_self_3<-ifelse(grepl(pattern='1583',x=a$noncancer_illness_self_3),1,0)

a$tia_self_0<-ifelse(grepl(pattern='1082',x=a$noncancer_illness_self_0),1,0)
a$tia_self_1<-ifelse(grepl(pattern='1082',x=a$noncancer_illness_self_1),1,0)
a$tia_self_2<-ifelse(grepl(pattern='1082',x=a$noncancer_illness_self_2),1,0)
a$tia_self_3<-ifelse(grepl(pattern='1082',x=a$noncancer_illness_self_3),1,0)

a$depression_self_0<-ifelse(grepl(pattern='1286',x=a$noncancer_illness_self_0),1,0)
a$depression_self_1<-ifelse(grepl(pattern='1286',x=a$noncancer_illness_self_1),1,0)
a$depression_self_2<-ifelse(grepl(pattern='1286',x=a$noncancer_illness_self_2),1,0)
a$depression_self_3<-ifelse(grepl(pattern='1286',x=a$noncancer_illness_self_3),1,0)

a$schizophrenia_self_0<-ifelse(grepl(pattern='1289',x=a$noncancer_illness_self_0),1,0)
a$schizophrenia_self_1<-ifelse(grepl(pattern='1289',x=a$noncancer_illness_self_1),1,0)
a$schizophrenia_self_2<-ifelse(grepl(pattern='1289',x=a$noncancer_illness_self_2),1,0)
a$schizophrenia_self_3<-ifelse(grepl(pattern='1289',x=a$noncancer_illness_self_3),1,0)

a$maniabiopolar_self_0<-ifelse(grepl(pattern='1291',x=a$noncancer_illness_self_0),1,0)
a$maniabiopolar_self_1<-ifelse(grepl(pattern='1291',x=a$noncancer_illness_self_1),1,0)
a$maniabiopolar_self_2<-ifelse(grepl(pattern='1291',x=a$noncancer_illness_self_2),1,0)
a$maniabiopolar_self_3<-ifelse(grepl(pattern='1291',x=a$noncancer_illness_self_3),1,0)

temp<-paste0(bd$f.6150.0.0," ",bd$f.6150.0.1," ",bd$f.6150.0.2," ",bd$f.6150.0.3)
a$heartattack_diagnosed_0<-ifelse(grepl("1",temp),"1","0")
a$angina_diagnosed_0<-ifelse(grepl("2",temp),"1","0")
a$stroke_diagnosed_0<-ifelse(grepl("3",temp),"1","0")
a$HT_diagnosed_0<-ifelse(grepl("4",temp),"1","0")

temp<-paste0(bd$f.6150.1.0," ",bd$f.6150.1.1," ",bd$f.6150.1.2," ",bd$f.6150.1.3)
a$heartattack_diagnosed_1<-ifelse(grepl("1",temp),"1","0")
a$angina_diagnosed_1<-ifelse(grepl("2",temp),"1","0")
a$stroke_diagnosed_1<-ifelse(grepl("3",temp),"1","0")
a$HT_diagnosed_1<-ifelse(grepl("4",temp),"1","0")

temp<-paste0(bd$f.6150.2.0," ",bd$f.6150.2.1," ",bd$f.6150.2.2," ",bd$f.6150.2.3)
a$heartattack_diagnosed_2<-ifelse(grepl("1",temp),"1","0")
a$angina_diagnosed_2<-ifelse(grepl("2",temp),"1","0")
a$stroke_diagnosed_2<-ifelse(grepl("3",temp),"1","0")
a$HT_diagnosed_2<-ifelse(grepl("4",temp),"1","0")

#-1 : do not know, -3 : prefer not to answer
a$age_heartattack_diagnosed_0<-ifelse(bd$f.3894.0.0 %in% c(-1,-3),NA,bd$f.3894.0.0)
a$age_heartattack_diagnosed_1<-ifelse(bd$f.3894.1.0 %in% c(-1,-3),NA,bd$f.3894.1.0)
a$age_heartattack_diagnosed_2<-ifelse(bd$f.3894.2.0 %in% c(-1,-3),NA,bd$f.3894.2.0)
a$age_heartattack_diagnosed_3<-ifelse(bd$f.3894.3.0 %in% c(-1,-3),NA,bd$f.3894.3.0)

a$age_angina_diagnosed_0<-ifelse(bd$f.3627.0.0 %in% c(-1,-3),NA,bd$f.3627.0.0)
a$age_angina_diagnosed_1<-ifelse(bd$f.3627.1.0 %in% c(-1,-3),NA,bd$f.3627.1.0)
a$age_angina_diagnosed_2<-ifelse(bd$f.3627.2.0 %in% c(-1,-3),NA,bd$f.3627.2.0)
a$age_angina_diagnosed_3<-ifelse(bd$f.3627.3.0 %in% c(-1,-3),NA,bd$f.3627.3.0)

a$age_stroke_diagnosed_0<-ifelse(bd$f.4056.0.0 %in% c(-1,-3),NA,bd$f.4056.0.0)
a$age_stroke_diagnosed_1<-ifelse(bd$f.4056.1.0 %in% c(-1,-3),NA,bd$f.4056.1.0)
a$age_stroke_diagnosed_2<-ifelse(bd$f.4056.2.0 %in% c(-1,-3),NA,bd$f.4056.2.0)
a$age_stroke_diagnosed_3<-ifelse(bd$f.4056.3.0 %in% c(-1,-3),NA,bd$f.4056.3.0)

a$age_HT_diagnosed_0<-ifelse(bd$f.2966.0.0 %in% c(-1,-3),NA,bd$f.2966.0.0)
a$age_HT_diagnosed_1<-ifelse(bd$f.2966.1.0 %in% c(-1,-3),NA,bd$f.2966.1.0)
a$age_HT_diagnosed_2<-ifelse(bd$f.2966.2.0 %in% c(-1,-3),NA,bd$f.2966.2.0)
a$age_HT_diagnosed_3<-ifelse(bd$f.2966.3.0 %in% c(-1,-3),NA,bd$f.2966.3.0)

#Cognitive function tests (baseline/f.u)------------------------------------------------------------------
# data-coding 498 : 0	Completed / 1	Abandoned / 2	Completed with pause

#Fluid intelligence / reasoning
# baseline
# 20016	Fluid intelligence score
# 20128	Number of fluid intelligence questions attempted within time limit
# fu
# 20242	Fluid intelligence completion status
# 20135	When fluid intelligence test completed
# 20191	Fluid intelligence score
# 20192	Number of fluid intelligence questions attempted within time limit
names(bd)[grep("20016|20128|20242|20135|20191|20192",names(bd))]


#Trail making
# baseline
# 6348	Duration to complete numeric path (trail #1)
# 6350	Duration to complete alphanumeric path (trail #2)
# 6349	Total errors traversing numeric path (trail #1)
# 6351	Total errors traversing alphanumeric path (trail #2)
# fu
# 20246	Trail making completion status
# 20136	When trail making test completed
# 20156	Duration to complete numeric path (trail #1)
# 20157	Duration to complete alphanumeric path (trail #2)
# 20247	Total errors traversing numeric path (trail #1)
# 20248	Total errors traversing alphanumeric path (trail #2)
names(bd)[grep("6348|6350|6349|6351|20246|20136|20156|20157|20247|20248",names(bd))]

#Symbol digit substitution
# baseline
# 23323	Number of symbol digit matches attempted
# 23324	Number of symbol digit matches made correctly
# fu
# 20245	Symbol digit completion status
# 20137	When symbol digit substitution test completed
# 20159	Number of symbol digit matches made correctly
# 20195	Number of symbol digit matches attempted
names(bd)[grep("22323|22324|20245|20137|20159|20195",names(bd))]

#Pairs matching
# baseline
# 398	Number of correct matches in round
# 399	Number of incorrect matches in round
# 400	Time to complete round
# fu
# 20244	Pairs matching completion status
# 20134	When pairs test completed
# 20131	Number of correct matches in round
# 20132	Number of incorrect matches in round
# 20133	Time to complete round
names(bd)[grep("f.398.0.|f.399.0.|f.400.0.|20244|20134|20131|20132|20133",names(bd))]

#Numeric memory
# baseline
# 4282	Maximum digits remembered correctly
# fu
# 20138	When numeric memory test completed
# 20240	Maximum digits remembered correctly
names(bd)[grep("4282,20138,20240",names(bd))]

#Brain MRI------------------------------------------------------------------

# 107 Diffusion brain MRI
# 134 dMRI skeleton	432
# 135 dMRI weighted means	243

# 110 T1 structural brain MRI	26
# 1101 Regional grey matter volumes (FAST)	139
# 1102 Subcortical volumes (FIRST)	14
# 190 Freesurfer ASEG	99
# 195 Freesurfer BA exvivo	84
# 197 Freesurfer a2009s	444
# 196 Freesurfer DKT	186
# 194 Freesurfer desikan gw	70
# 193 Freesurfer desikan pial	66
# 192 Freesurfer desikan white	202
# 191 Freesurfer subsegmentation	121

categorynames<-c("dMRI_skeleton","dMRI_weighted_means",
                 "T1_Regional_grey_matter_volumes_FAST","T1_Subcortical_volumes_FIRST",
                 "T1_Freesurfer_ASEG","T1_Freesurfer_BA_exvivo","T1_Freesurfer_a2009s","T1_Freesurfer_DKT",
                 "T1_Freesurfer_desikan_gw","T1_Freesurfer_desikan_pial","T1_Freesurfer_desikan_white","T1_Freesurfer_subsegmentation")

mrivars <- excel_sheets("brain mri variables.xlsx") %>% 
                lapply(function(x){read_excel("brain mri variables.xlsx",sheet=x)})

#5:12 데이터 누락있음. 원래는 1:12까지 all.
for(i in 1:4){
  for(j in 2:3){
    mri_set_fieldcodes<-paste0("f.",mrivars[[i]]$Field_ID,".",j,".0")
    mri_set_colnames<-paste0(categorynames[i],"_",mrivars[[i]]$Description,"_",j)
    
    mri_set<-sapply(mri_set_fieldcodes,function(v){bd[[v]]})
    mri_set<-as.data.table(mri_set)
    colnames(mri_set)<-mri_set_colnames
    
    leftlocations<-grep("left",mri_set_colnames)
    avgcols<-sapply(leftlocations,
                    function(ll){
                      rl<-grep(gsub("left","right",mri_set_colnames[ll]),mri_set_colnames)
                      temp_avgcol<-(mri_set[[ll]]+mri_set[[rl]])/2
                      temp_avgcol
                    })
    colnames(avgcols)<-gsub("left","avg",mri_set_colnames[leftlocations])
    
    mri_set<-cbind(mri_set,avgcols)
    
    a<-cbind(a,mri_set) 
  }
}

#Outcomes------------------------------------------------------------------
#Health related outcomes - Algorithmically defined outcomes

a$dementia_outcome<-ifelse(is.na(bd$f.42018.0.0),0,1) #all cause dementia
a$dementia_outcome_date<-bd$f.42018.0.0

a$parkinson_outcome<-ifelse(is.na(bd$f.42032.0.0),0,1)
a$parkinson_outcome_date<-bd$f.42032.0.0

a$asthma_outcome<-ifelse(is.na(bd$f.42014.0.0),0,1)
a$asthma_outcome_date<-bd$f.42014.0.0

a$COPD_outcome<-ifelse(is.na(bd$f.42016.0.0),0,1)
a$COPD_outcome_date<-bd$f.42016.0.0

a$endstage_renal_disease_outcome<-ifelse(is.na(bd$f.42026.0.0),0,1)
a$endstage_renal_disease_outcome_date<-bd$f.42026.0.0

a$motor_neuron_disease_outcome<-ifelse(is.na(bd$f.42028.0.0),0,1)
a$motor_neuron_disease_outcome_date<-bd$f.42028.0.0  

a$MI_outcome<-ifelse(is.na(bd$f.42000.0.0),0,1)
a$MI_outcome_date<-bd$f.42000.0.0

a$stroke_outcome<-ifelse(is.na(bd$f.42006.0.0),0,1)
a$stroke_outcome_date<-bd$f.42006.0.0


#Death

a$death_date<-bd$f.40000.0.0

days <- sapply(c("dementia", "parkinson", "asthma", "COPD", "endstage_renal_disease", "motor_neuron", "MI", "stroke"),
               function(v){
                 as.integer(ifelse(!is.na(a[[paste0(v, "_outcome_date")]]),a[[paste0(v, "_outcome_date")]],
                                   ifelse(!is.na(a[["death_date"]]),a[["death_date"]],as.IDate("2019-03-31")))) - as.integer(a[["visit_date_0"]])
                }) %>% do.call(cbind, .)
colnames(days) <- paste0(colnames(days), "_day")


#----------------------------------------------------------------------------------

out<-cbind(a[,-c("ID",
                 "sysBP_automated_0","sysBP_manual_0","sysBP_automated_1","sysBP_manual_1","sysBP_automated_2","sysBP_manual_2","sysBP_automated_3","sysBP_manual_3",
                 "diaBP_automated_0","diaBP_manual_0","diaBP_automated_1","diaBP_manual_1","diaBP_automated_2","diaBP_manual_2","diaBP_automated_3","diaBP_manual_3")],
           days)

factor_vars<-c("sex", "smoking_status_0","smoking_status_1","smoking_status_2","smoking_status_3",
               "alcohol_status_0","alcohol_status_1","alcohol_status_2","alcohol_status_3",
               "alcohol_addiction","DM_self_0","DM_self_1","DM_self_2","DM_self_3",
               "DM_diagnosed_0","DM_diagnosed_1","DM_diagnosed_2","DM_diagnosed_3",
               "gestational_DM_0","gestational_DM_1","gestational_DM_2","gestational_DM_3",
               "cholesterol_medication_0","BP_medication_0","insulin_medication_0",
               "cholesterol_medication_1","BP_medication_1","insulin_medication_1",
               "cholesterol_medication_2","BP_medication_2","insulin_medication_2",
               "cholesterol_medication_3","BP_medication_3","insulin_medication_3",
               "DM_0","DM_1","DM_2","DM_3",
               "MetS_NCEPATPIII_0","MetS_NCEPATPIII_1","MetS_IDF_0","MetS_IDF_1",
               "angina_self_0","angina_self_1","angina_self_2","angina_self_3",
               "heartattack_or_MI_self_0","heartattack_or_MI_self_1","heartattack_or_MI_self_2","heartattack_or_MI_self_3",
               "ischaemicstroke_self_0","ischaemicstroke_self_1","ischaemicstroke_self_2","ischaemicstroke_self_3",
               "tia_self_0","tia_self_1","tia_self_2","tia_self_3",
               "depression_self_0","depression_self_1","depression_self_2","depression_self_3",
               "schizophrenia_self_0","schizophrenia_self_1","schizophrenia_self_2","schizophrenia_self_3",
               "maniabiopolar_self_0","maniabiopolar_self_1","maniabiopolar_self_2","maniabiopolar_self_3",
               "heartattack_diagnosed_0","angina_diagnosed_0","stroke_diagnosed_0","HT_diagnosed_0",
               "heartattack_diagnosed_1","angina_diagnosed_1","stroke_diagnosed_1","HT_diagnosed_1",
               "heartattack_diagnosed_2","angina_diagnosed_2","stroke_diagnosed_2","HT_diagnosed_2",
               "dementia_outcome","parkinson_outcome","asthma_outcome","COPD_outcome","endstage_renal_disease_outcome","motor_neuron_disease_outcome","MI_outcome","stroke_outcome")
out[,(factor_vars):=lapply(.SD,as.factor),.SDcols=factor_vars]

out.label <- jstable::mk.lev(a)

# label.main[variable == " ", `:=`(var_label = " ", val_label = c("","",""))]

# saveRDS(list(data = out, label = out.label), "data.RDS")
