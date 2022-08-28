library(data.table);library(magrittr);library(parallel);library(fst);library(stats);library(imputeTS);library(readxl)
setDTthreads(0)
setwd("/home/heejooko/ShinyApps/UKbiobank")
# setwd("/home/js/UKbiobank/UKbiobank2022")
# list.files(path=".", pattern=NULL, all.files=FALSE, full.names=FALSE)

mydata <- fread("ukb49960.tab", header = T, sep = "\t", quote = "")
#mydata <-read.delim("ukb49960.tab", header = TRUE, sep = "\t", quote = "")
#mydata <- as.data.table(mydata)

# bd <- fst::read_fst("/home/heejooko/ShinyApps/UKbiobank/ukb47038.fst", as.data.table = T)

a<-data.table()

# 4 Instances
# _0	Initial assessment visit (2006-2010) at which participants were recruited and consent given
# _1	First repeat assessment visit (2012-13)
# _2	Imaging visit (2014+)
# _3	First repeat imaging visit (2019+)

#Population characteristics------------------------------------------------------------------
a$ID<-mydata$f.eid
a$age<-mydata$f.21022.0.0 #age at recruitment [years]
a$sex<-ifelse(mydata$f.31.0.0==0,"F","M")
a$townsend_deprivation_index<-mydata$f.189.0.0
a$visit_date_0<-mydata$f.53.0.0
a$visit_date_1<-mydata$f.53.1.0
a$visit_date_2<-mydata$f.53.2.0
a$visit_date_3<-mydata$f.53.3.0

#BMI [kg/m2]
a$bmi_0<-mydata$f.21001.0.0
a$bmi_1<-mydata$f.21001.1.0
a$bmi_2<-mydata$f.21001.2.0
a$bmi_3<-mydata$f.21001.3.0

#ethnicity
# 1	White	1	Top
# 1001	British	1001	1
# 2001	White and Black Caribbean	2001	2
# 3001	Indian	3001	3
# 4001	Caribbean	4001	4
# 2	Mixed	2	Top
# 1002	Irish	1002	1
# 2002	White and Black African	2002	2
# 3002	Pakistani	3002	3
# 4002	African	4002	4
# 3	Asian or Asian British	3	Top
# 1003	Any other white background	1003	1
# 2003	White and Asian	2003	2
# 3003	Bangladeshi	3003	3
# 4003	Any other Black background	4003	4
# 4	Black or Black British	4	Top
# 2004	Any other mixed background	2004	2
# 3004	Any other Asian background	3004	3
# 5	Chinese	5	Top
# 6	Other ethnic group	6	Top
# -1->9999	Do not know	-1	Top
# -3->9997	Prefer not to answer	-3	Top

a$ethnicity<-mydata$f.21000.0.0 %% 10000
a$ethnicity_group<-a$ethnicity %% 1000

#Summed MET minutes per week for all activity 22040 없어서 walking, moderate, vigorous 더해서 구함
#[minutes/week]

# a$MET_activity<-mydata$f.22037.0.0+mydata$f.22038.0.0+mydata$f.22039.0.0
a$MET_activity<-mydata$f.22040.0.0

#Smoking
# -3->9	Prefer not to answer / 0	Never / 1	Previous / 2	Current
a$smoking_status_0<-ifelse(mydata$f.20116.0.0==-3,9,mydata$f.20116.0.0)
a$smoking_status_1<-ifelse(mydata$f.20116.1.0==-3,9,mydata$f.20116.1.0)
a$smoking_status_2<-ifelse(mydata$f.20116.2.0==-3,9,mydata$f.20116.2.0)
a$smoking_status_3<-ifelse(mydata$f.20116.3.0==-3,9,mydata$f.20116.3.0)

a$smoking_stop_age<-mydata$f.22507.0.0

a$smoking_packyears_0<-mydata$f.20161.0.0
a$smoking_packyears_1<-mydata$f.20161.1.0
a$smoking_packyears_2<-mydata$f.20161.2.0
a$smoking_packyears_3<-mydata$f.20161.3.0

#Alcohol
# -3->9	Prefer not to answer / 0	Never / 1	Previous / 2	Current
a$alcohol_status_0<-ifelse(mydata$f.20117.0.0==-3,9,mydata$f.20117.0.0)
a$alcohol_status_1<-ifelse(mydata$f.20117.1.0==-3,9,mydata$f.20117.1.0)
a$alcohol_status_2<-ifelse(mydata$f.20117.2.0==-3,9,mydata$f.20117.2.0)
a$alcohol_status_3<-ifelse(mydata$f.20117.3.0==-3,9,mydata$f.20117.3.0)

# 1	Daily or almost daily / 2	Three or four times a week / 3	Once or twice a week / 4	One to three times a month / 5	Special occasions only / 6	Never / -3->9	Prefer not to answer
a$alcohol_frequency_0<-ifelse(mydata$f.1558.0.0==-3,9,mydata$f.1558.0.0)
a$alcohol_frequency_1<-ifelse(mydata$f.1558.1.0==-3,9,mydata$f.1558.1.0)
a$alcohol_frequency_2<-ifelse(mydata$f.1558.2.0==-3,9,mydata$f.1558.2.0)
a$alcohol_frequency_3<-ifelse(mydata$f.1558.3.0==-3,9,mydata$f.1558.3.0)

# -818->8	Prefer not to answer / -121->9	Do not know / 0	No / 1	Yes : ever addicted to alcohol
a$alcohol_addiction<-ifelse(mydata$f.20406.0.0==-818,8,
                            ifelse(mydata$f.20406.0.0==-121,9,mydata$f.20406.0.0))

#Exposures------------------------------------------------------------------
#DM
#Non-cancer illness, self-reported
myCol<-colnames(mydata)[grep("f.20002.0.",colnames(mydata))]
a$noncancer_illness_self_0<-mydata[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$noncancer_illness_self_0<-lapply(a$noncancer_illness_self_0,function(x){gsub("_NA","",x)}) %>% unlist

myCol<-colnames(mydata)[grep("f.20002.1.",colnames(mydata))]
a$noncancer_illness_self_1<-mydata[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$noncancer_illness_self_1<-lapply(a$noncancer_illness_self_1,function(x){gsub("_NA","",x)}) %>% unlist

myCol<-colnames(mydata)[grep("f.20002.2.",colnames(mydata))]
a$noncancer_illness_self_2<-mydata[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$noncancer_illness_self_2<-lapply(a$noncancer_illness_self_2,function(x){gsub("_NA","",x)}) %>% unlist

myCol<-colnames(mydata)[grep("f.20002.3.",colnames(mydata))]
a$noncancer_illness_self_3<-mydata[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$noncancer_illness_self_3<-lapply(a$noncancer_illness_self_3,function(x){gsub("_NA","",x)}) %>% unlist

a$DM_self_0<-ifelse(grepl(pattern='1220|1221|1222|1223',x=a$noncancer_illness_self_0),1,0)
a$DM_self_1<-ifelse(grepl(pattern='1220|1221|1222|1223',x=a$noncancer_illness_self_1),1,0)
a$DM_self_2<-ifelse(grepl(pattern='1220|1221|1222|1223',x=a$noncancer_illness_self_2),1,0)
a$DM_self_3<-ifelse(grepl(pattern='1220|1221|1222|1223',x=a$noncancer_illness_self_3),1,0)

a$DM_diagnosed_0<-ifelse(mydata$f.2443.0.0==1,1,ifelse(mydata$f.2443.0.0==0,0,NA))
a$DM_diagnosed_1<-ifelse(mydata$f.2443.1.0==1,1,ifelse(mydata$f.2443.1.0==0,0,NA))
a$DM_diagnosed_2<-ifelse(mydata$f.2443.2.0==1,1,ifelse(mydata$f.2443.2.0==0,0,NA))
a$DM_diagnosed_3<-ifelse(mydata$f.2443.3.0==1,1,ifelse(mydata$f.2443.3.0==0,0,NA))

a$gestational_DM_0<-ifelse(mydata$f.4041.0.0==1,1,ifelse(mydata$f.4041.0.0==0,0,NA))
a$gestational_DM_1<-ifelse(mydata$f.4041.1.0==1,1,ifelse(mydata$f.4041.1.0==0,0,NA))
a$gestational_DM_2<-ifelse(mydata$f.4041.2.0==1,1,ifelse(mydata$f.4041.2.0==0,0,NA))
a$gestational_DM_3<-ifelse(mydata$f.4041.3.0==1,1,ifelse(mydata$f.4041.3.0==0,0,NA))

a$age_DM_diagnosed_0<-ifelse(mydata$f.2976.0.0 %in% c(-1,-3),NA,mydata$f.2976.0.0)
a$age_DM_diagnosed_1<-ifelse(mydata$f.2976.1.0 %in% c(-1,-3),NA,mydata$f.2976.1.0)
a$age_DM_diagnosed_2<-ifelse(mydata$f.2976.2.0 %in% c(-1,-3),NA,mydata$f.2976.2.0)
a$age_DM_diagnosed_3<-ifelse(mydata$f.2976.3.0 %in% c(-1,-3),NA,mydata$f.2976.3.0)

# f.2986 누락되었던 것 추가
a$insulin_within_1y_diagnosed_DM_0<-ifelse(mydata$f.2986.0.0 %in% c(-1,-3),NA,mydata$f.2986.0.0)
a$insulin_within_1y_diagnosed_DM_1<-ifelse(mydata$f.2986.1.0 %in% c(-1,-3),NA,mydata$f.2986.1.0)
a$insulin_within_1y_diagnosed_DM_2<-ifelse(mydata$f.2986.2.0 %in% c(-1,-3),NA,mydata$f.2986.2.0)
a$insulin_within_1y_diagnosed_DM_3<-ifelse(mydata$f.2986.3.0 %in% c(-1,-3),NA,mydata$f.2986.3.0)

#[mmol/L]
a$glucose_0<-mydata$f.30740.0.0
a$glucose_1<-mydata$f.30740.1.0

#[mmol/mol]
a$HbA1c_0<-mydata$f.30750.0.0
a$HbA1c_1<-mydata$f.30750.1.0

#fasting time f.74 누락되었던 것 추가 
a$fasting_time_0<-mydata$f.74.0.0
a$fasting_time_1<-mydata$f.74.1.0
a$fasting_time_2<-mydata$f.74.2.0
a$fasting_time_3<-mydata$f.74.3.0

#BP
#[mmHg]
myCol<-colnames(mydata)[grep("f.4080.0.",colnames(mydata))]
a$sysBP_automated_0<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.93.0.",colnames(mydata))]
a$sysBP_manual_0<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(mydata)[grep("f.4080.1.",colnames(mydata))]
a$sysBP_automated_1<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.93.1.",colnames(mydata))]
a$sysBP_manual_1<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(mydata)[grep("f.4080.2.",colnames(mydata))]
a$sysBP_automated_2<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.93.2.",colnames(mydata))]
a$sysBP_manual_2<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(mydata)[grep("f.4080.3.",colnames(mydata))]
a$sysBP_automated_3<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.93.3.",colnames(mydata))]
a$sysBP_manual_3<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]

a$sysBP_0<-ifelse(is.na(a$sysBP_manual_0),a$sysBP_automated_0,a$sysBP_manual_0)
a$sysBP_1<-ifelse(is.na(a$sysBP_manual_1),a$sysBP_automated_1,a$sysBP_manual_1)
a$sysBP_2<-ifelse(is.na(a$sysBP_manual_2),a$sysBP_automated_2,a$sysBP_manual_2)
a$sysBP_3<-ifelse(is.na(a$sysBP_manual_3),a$sysBP_automated_3,a$sysBP_manual_3)

myCol<-colnames(mydata)[grep("f.4079.0.",colnames(mydata))]
a$diaBP_automated_0<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.94.0.",colnames(mydata))]
a$diaBP_manual_0<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(mydata)[grep("f.4079.1.",colnames(mydata))]
a$diaBP_automated_1<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.94.1.",colnames(mydata))]
a$diaBP_manual_1<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(mydata)[grep("f.4079.2.",colnames(mydata))]
a$diaBP_automated_2<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.94.2.",colnames(mydata))]
a$diaBP_manual_2<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(mydata)[grep("f.4079.3.",colnames(mydata))]
a$diaBP_automated_3<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.94.3.",colnames(mydata))]
a$diaBP_manual_3<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]

a$diaBP_0<-ifelse(is.na(a$diaBP_manual_0),a$diaBP_automated_0,a$diaBP_manual_0)
a$diaBP_1<-ifelse(is.na(a$diaBP_manual_1),a$diaBP_automated_1,a$diaBP_manual_1)
a$diaBP_2<-ifelse(is.na(a$diaBP_manual_2),a$diaBP_automated_2,a$diaBP_manual_2)
a$diaBP_3<-ifelse(is.na(a$diaBP_manual_3),a$diaBP_automated_3,a$diaBP_manual_3)

#Abdominal obesity
#[cm]
a$WC_0<-mydata$f.48.0.0
a$WC_1<-mydata$f.48.1.0
a$WC_2<-mydata$f.48.2.0
a$WC_3<-mydata$f.48.3.0

#Lipidic profile
#[mmol/L]
a$TG_0<-mydata$f.30870.0.0
a$TG_1<-mydata$f.30870.1.0

a$HDL_0<-mydata$f.30760.0.0
a$HDL_1<-mydata$f.30760.1.0

#serum insulin 변수 못 찾음

#Medication(self)----------------------------------------------------------
# 1	Cholesterol lowering medication / 2	Blood pressure medication / 3	Insulin / -7	None of the above / -1	Do not know / -3	Prefer not to answer
temp<-paste0(mydata$f.6177.0.0," ",mydata$f.6177.0.1," ",mydata$f.6177.0.2)
a$cholesterol_medication_self_0<-ifelse(grepl("1",temp),"1","0")
a$BP_medication_self_0<-ifelse(grepl("2",temp),"1","0")
a$insulin_medication_self_0<-ifelse(grepl("3",temp),"1","0")

temp<-paste0(mydata$f.6177.1.0," ",mydata$f.6177.1.1," ",mydata$f.6177.1.2)
a$cholesterol_medication_self_1<-ifelse(grepl("1",temp),"1","0")
a$BP_medication_self_1<-ifelse(grepl("2",temp),"1","0")
a$insulin_medication_self_1<-ifelse(grepl("3",temp),"1","0")

temp<-paste0(mydata$f.6177.2.0," ",mydata$f.6177.2.1," ",mydata$f.6177.2.2)
a$cholesterol_medication_self_2<-ifelse(grepl("1",temp),"1","0")
a$BP_medication_self_2<-ifelse(grepl("2",temp),"1","0")
a$insulin_medication_self_2<-ifelse(grepl("3",temp),"1","0")

temp<-paste0(mydata$f.6177.3.0," ",mydata$f.6177.3.1," ",mydata$f.6177.3.2)
a$cholesterol_medication_self_3<-ifelse(grepl("1",temp),"1","0")
a$BP_medication_self_3<-ifelse(grepl("2",temp),"1","0")
a$insulin_medication_self_3<-ifelse(grepl("3",temp),"1","0")

#education------------------------------------------------------------------
#qualifications
# 1	College or University degree
# 2	A levels/AS levels or equivalent
# 3	O levels/GCSEs or equivalent
# 4	CSEs or equivalent
# 5	NVQ or HND or HNC or equivalent
# 6	Other professional qualifications eg: nursing, teaching
# -7	None of the above
# -3	Prefer not to answer

temp<-paste0(mydata$f.6138.0.0," ",mydata$f.6138.0.1," ",mydata$f.6138.0.2," ",mydata$f.6138.0.3," ",mydata$f.6138.0.4," ",mydata$f.6138.0.5)
a$education_college_university_0<-ifelse(grepl("1",temp),1,0)
a$education_A_AS_0<-ifelse(grepl("2",temp),1,0)
a$education_O_GCSEs_0<-ifelse(grepl("3",temp),1,0)
a$education_CSEs_0<-ifelse(grepl("4",temp),1,0)
a$education_NVQ_HND_HNC_0<-ifelse(grepl("5",temp),1,0)
a$education_other_professional_0<-ifelse(grepl("6",temp),1,0)

temp<-paste0(mydata$f.6138.1.0," ",mydata$f.6138.1.1," ",mydata$f.6138.1.2," ",mydata$f.6138.1.3," ",mydata$f.6138.1.4," ",mydata$f.6138.1.5)
a$education_college_university_1<-ifelse(grepl("1",temp),1,0)
a$education_A_AS_1<-ifelse(grepl("2",temp),1,0)
a$education_O_GCSEs_1<-ifelse(grepl("3",temp),1,0)
a$education_CSEs_1<-ifelse(grepl("4",temp),1,0)
a$education_NVQ_HND_HNC_1<-ifelse(grepl("5",temp),1,0)
a$education_other_professional_1<-ifelse(grepl("6",temp),1,0)

temp<-paste0(mydata$f.6138.2.0," ",mydata$f.6138.2.1," ",mydata$f.6138.2.2," ",mydata$f.6138.2.3," ",mydata$f.6138.2.4," ",mydata$f.6138.2.5)
a$education_college_university_2<-ifelse(grepl("1",temp),1,0)
a$education_A_AS_2<-ifelse(grepl("2",temp),1,0)
a$education_O_GCSEs_2<-ifelse(grepl("3",temp),1,0)
a$education_CSEs_2<-ifelse(grepl("4",temp),1,0)
a$education_NVQ_HND_HNC_2<-ifelse(grepl("5",temp),1,0)
a$education_other_professional_2<-ifelse(grepl("6",temp),1,0)

temp<-paste0(mydata$f.6138.3.0," ",mydata$f.6138.3.1," ",mydata$f.6138.3.2," ",mydata$f.6138.3.3," ",mydata$f.6138.3.4," ",mydata$f.6138.3.5)
a$education_college_university_3<-ifelse(grepl("1",temp),1,0)
a$education_A_AS_3<-ifelse(grepl("2",temp),1,0)
a$education_O_GCSEs_3<-ifelse(grepl("3",temp),1,0)
a$education_CSEs_3<-ifelse(grepl("4",temp),1,0)
a$education_NVQ_HND_HNC_3<-ifelse(grepl("5",temp),1,0)
a$education_other_professional_3<-ifelse(grepl("6",temp),1,0)

#Age completed full time education
# -2	Never went to school
# -1	Do not know
# -3	Prefer not to answer

a$education_school_never_0<-ifelse(grepl("-2",mydata$f.845.0.0),1,0)
a$education_age_completed_full_time_education_0<-ifelse(mydata$f.845.0.0<0,NA,mydata$f.845.0.0)

a$education_school_never_1<-ifelse(grepl("-2",mydata$f.845.1.0),1,0)
a$education_age_completed_full_time_education_1<-ifelse(mydata$f.845.0.0<0,NA,mydata$f.845.1.0)

a$education_school_never_2<-ifelse(grepl("-2",mydata$f.845.0.0),1,0)
a$education_age_completed_full_time_education_2<-ifelse(mydata$f.845.0.0<0,NA,mydata$f.845.2.0)


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

temp<-paste0(mydata$f.6150.0.0," ",mydata$f.6150.0.1," ",mydata$f.6150.0.2," ",mydata$f.6150.0.3)
a$heartattack_diagnosed_0<-ifelse(grepl("1",temp),"1","0")
a$angina_diagnosed_0<-ifelse(grepl("2",temp),"1","0")
a$stroke_diagnosed_0<-ifelse(grepl("3",temp),"1","0")
a$HT_diagnosed_0<-ifelse(grepl("4",temp),"1","0")

temp<-paste0(mydata$f.6150.1.0," ",mydata$f.6150.1.1," ",mydata$f.6150.1.2," ",mydata$f.6150.1.3)
a$heartattack_diagnosed_1<-ifelse(grepl("1",temp),"1","0")
a$angina_diagnosed_1<-ifelse(grepl("2",temp),"1","0")
a$stroke_diagnosed_1<-ifelse(grepl("3",temp),"1","0")
a$HT_diagnosed_1<-ifelse(grepl("4",temp),"1","0")

temp<-paste0(mydata$f.6150.2.0," ",mydata$f.6150.2.1," ",mydata$f.6150.2.2," ",mydata$f.6150.2.3)
a$heartattack_diagnosed_2<-ifelse(grepl("1",temp),"1","0")
a$angina_diagnosed_2<-ifelse(grepl("2",temp),"1","0")
a$stroke_diagnosed_2<-ifelse(grepl("3",temp),"1","0")
a$HT_diagnosed_2<-ifelse(grepl("4",temp),"1","0")


#-1 : do not know, -3 : prefer not to answer
a$age_heartattack_diagnosed_0<-ifelse(mydata$f.3894.0.0 %in% c(-1,-3),NA,mydata$f.3894.0.0)
a$age_heartattack_diagnosed_1<-ifelse(mydata$f.3894.1.0 %in% c(-1,-3),NA,mydata$f.3894.1.0)
a$age_heartattack_diagnosed_2<-ifelse(mydata$f.3894.2.0 %in% c(-1,-3),NA,mydata$f.3894.2.0)
a$age_heartattack_diagnosed_3<-ifelse(mydata$f.3894.3.0 %in% c(-1,-3),NA,mydata$f.3894.3.0)

a$age_angina_diagnosed_0<-ifelse(mydata$f.3627.0.0 %in% c(-1,-3),NA,mydata$f.3627.0.0)
a$age_angina_diagnosed_1<-ifelse(mydata$f.3627.1.0 %in% c(-1,-3),NA,mydata$f.3627.1.0)
a$age_angina_diagnosed_2<-ifelse(mydata$f.3627.2.0 %in% c(-1,-3),NA,mydata$f.3627.2.0)
a$age_angina_diagnosed_3<-ifelse(mydata$f.3627.3.0 %in% c(-1,-3),NA,mydata$f.3627.3.0)

a$age_stroke_diagnosed_0<-ifelse(mydata$f.4056.0.0 %in% c(-1,-3),NA,mydata$f.4056.0.0)
a$age_stroke_diagnosed_1<-ifelse(mydata$f.4056.1.0 %in% c(-1,-3),NA,mydata$f.4056.1.0)
a$age_stroke_diagnosed_2<-ifelse(mydata$f.4056.2.0 %in% c(-1,-3),NA,mydata$f.4056.2.0)
a$age_stroke_diagnosed_3<-ifelse(mydata$f.4056.3.0 %in% c(-1,-3),NA,mydata$f.4056.3.0)

a$age_HT_diagnosed_0<-ifelse(mydata$f.2966.0.0 %in% c(-1,-3),NA,mydata$f.2966.0.0)
a$age_HT_diagnosed_1<-ifelse(mydata$f.2966.1.0 %in% c(-1,-3),NA,mydata$f.2966.1.0)
a$age_HT_diagnosed_2<-ifelse(mydata$f.2966.2.0 %in% c(-1,-3),NA,mydata$f.2966.2.0)
a$age_HT_diagnosed_3<-ifelse(mydata$f.2966.3.0 %in% c(-1,-3),NA,mydata$f.2966.3.0)

#Cognitive function tests (baseline/f.u)------------------------------------------------------------------
# data-coding 498 : 0	Completed / 1	Abandoned / 2	Completed with pause

Cog<-data.table()
#Fluid intelligence / reasoning
# baseline
# 20016	Fluid intelligence score
# 20128	Number of fluid intelligence questions attempted within time limit
# fu
# 20242	Fluid intelligence completion status
# 20135	When fluid intelligence test completed
# 20191	Fluid intelligence score
# 20192	Number of fluid intelligence questions attempted within time limit
names(mydata)[grep("20016|20128|20242|20135|20191|20192",names(mydata))]

# Cog 변수를 out에도 넣고 varlist에서 넣기
# factor_var에 넣어야될 것은 넣기

Cog$FI_score_0<-mydata$f.20016.0.0
Cog$FI_score_1<-mydata$f.20016.1.0
Cog$FI_score_2<-mydata$f.20016.2.0
Cog$FI_score_3<-mydata$f.20016.3.0

Cog$FI_qcount_0<-mydata$f.20128.0.0
Cog$FI_qcount_1<-mydata$f.20128.1.0
Cog$FI_qcount_2<-mydata$f.20128.2.0
Cog$FI_qcount_3<-mydata$f.20128.3.0

# 0	Completed / 1	Abandoned / 2	Completed with pause
Cog$FI_fu<-mydata$f.20242.0.0
Cog$FI_fu_day<-as.IDate(mydata$f.20135.0.0)-as.IDate(a[["visit_date_0"]])
Cog$FI_fu_score<-mydata$f.20191.0.0
Cog$FI_fu_qcount<-mydata$f.20192.0.0

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
names(mydata)[grep("6348|6350|6349|6351|20246|20136|20156|20157|20247|20248",names(mydata))]

Cog$TM_num_dur_2<-mydata$f.6348.2.0
Cog$TM_num_dur_3<-mydata$f.6348.3.0

Cog$TM_alpha_dur_2<-mydata$f.6350.2.0
Cog$TM_alpha_dur_3<-mydata$f.6350.3.0

Cog$TM_num_err_2<-mydata$f.6349.2.0
Cog$TM_num_err_3<-mydata$f.6349.3.0

Cog$TM_alpha_err_2<-mydata$f.6351.2.0
Cog$TM_alpha_err_3<-mydata$f.6351.3.0

# 0	Completed / 1	Abandoned / 2	Completed with pause / 3	Timed-out due to inactivity
Cog$TM_fu<-mydata$f.20246.0.0
Cog$TM_fu_day<-as.IDate(mydata$f.20136.0.0)-as.IDate(a[["visit_date_0"]])
Cog$TM_fu_num_dur<-mydata$f.20156.0.0
Cog$TM_fu_alpha_dur<-mydata$f.20157.0.0
Cog$TM_fu_num_err<-mydata$f.20247.0.0
Cog$TM_fu_alpha_err<-mydata$f.20248.0.0

#Symbol digit substitution
# baseline
# 23323	Number of symbol digit matches attempted
# 23324	Number of symbol digit matches made correctly
# fu
# 20245	Symbol digit completion status
# 20137	When symbol digit substitution test completed
# 20159	Number of symbol digit matches made correctly
# 20195	Number of symbol digit matches attempted
# names(mydata)[grep("22323|22324|20245|20137|20159|20195",names(mydata))]

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
names(mydata)[grep("f.398.0.|f.399.0.|f.400.0.|20244|20134|20131|20132|20133",names(mydata))]


myCol<-colnames(mydata)[grep("f.398.0.",colnames(mydata))]
Cog$PM_corr_0<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.398.1.",colnames(mydata))]
Cog$PM_corr_1<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.398.2.",colnames(mydata))]
Cog$PM_corr_2<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.398.3.",colnames(mydata))]
Cog$PM_corr_3<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]


myCol<-colnames(mydata)[grep("f.399.0.",colnames(mydata))]
Cog$PM_err_0<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.399.1.",colnames(mydata))]
Cog$PM_err_1<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.399.2.",colnames(mydata))]
Cog$PM_err_2<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.399.3.",colnames(mydata))]
Cog$PM_err_3<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-c("f.400.0.1","f.400.0.2","f.400.0.3")
Cog$PM_time_0<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-c("f.400.1.1","f.400.1.2","f.400.1.3")
Cog$PM_time_1<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-c("f.400.2.1","f.400.2.2","f.400.2.3")
Cog$PM_time_2<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-c("f.400.3.1","f.400.3.2","f.400.3.3")
Cog$PM_time_3<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]

# 0	Completed / 1	Abandoned / 2	Completed with pause
Cog$PM_fu<-mydata$f.20244.0.0
Cog$PM_fu_day<-as.IDate(mydata$f.20134.0.0)-as.IDate(a[["visit_date_0"]])

myCol<-colnames(mydata)[grep("f.20131.0.",colnames(mydata))]
Cog$PM_fu_corr<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.20132.0.",colnames(mydata))]
Cog$PM_fu_err<-mydata[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(mydata)[grep("f.20133.0.",colnames(mydata))]
Cog$PM_fu_time<-mydata[,..myCol][,lapply(.SD,function(x){ifelse(x==-1,NA,x)}),.SD][,rowMeans(.SD,na.rm=T),]

#Numeric memory
# baseline
# 4282	Maximum digits remembered correctly
# fu
# 20138	When numeric memory test completed
# 20240	Maximum digits remembered correctly
# names(mydata)[grep("4282,20138,20240",names(mydata))]

# Cog$ID<-a$ID

#Brain MRI------------------------------------------------------------------

# Diffusion brain MRI
# 134 dMRI skeleton	432
# 135 dMRI weighted means	243

# T1 structural brain MRI	26
# 1101 Regional grey matter volumes (FAST)	139
# 1102 Subcortical volumes (FIRST)	14

# T1 structural brain MRI
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

#5:12 데이터 누락. 원래는 1:12까지 all. -> 2022 데이터로 해결
for(i in 1:12){
  for(j in 2:3){
    mri_set_fieldcodes<-paste0("f.",mrivars[[i]]$Field_ID,".",j,".0")
    mri_set_colnames<-paste0(categorynames[i],"_",mrivars[[i]]$Description,"_",j)
    
    mri_set<-sapply(mri_set_fieldcodes,function(v){mydata[[v]]})
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




#Biomarkers--------------------------------------------------------------------
#IGF-1 [nmol/L]
a$IGF1_0<-mydata$f.30770.0.0
a$IGF1_1<-mydata$f.30770.1.0

#SHBG [nmol/L]
a$SHBG_0<-mydata$f.30830.0.0
a$SHBG_1<-mydata$f.30830.1.0


#20220821 add vars

a$IPAQ_activity_group<-mydata$f.22032.0.0

a$GPT_0<-mydata$f.30620.0.0
a$GPT_1<-mydata$f.30620.1.0

a$albumin_0<-mydata$f.30600.0.0
a$albumin_1<-mydata$f.30600.1.0

a$ALP_0<-mydata$f.30610.0.0
a$ALP_1<-mydata$f.30610.1.0

a$ApoA_0<-mydata$f.30630.0.0
a$ApoA_1<-mydata$f.30630.1.0

a$ApoB_0<-mydata$f.30640.0.0
a$ApoB_1<-mydata$f.30640.1.0

a$AST_0<-mydata$f.30650.0.0
a$AST_1<-mydata$f.30650.1.0

a$CRP_0<-mydata$f.30710.0.0
a$CRP_1<-mydata$f.30710.1.0

a$Ca_0<-mydata$f.30680.0.0
a$Ca_1<-mydata$f.30680.1.0

a$cholesterol_0<-mydata$f.30690.0.0
a$cholesterol_1<-mydata$f.30690.1.0

a$creatinine_0<-mydata$f.30700.0.0
a$creatinine_1<-mydata$f.30700.1.0

a$CysC_0<-mydata$f.30720.0.0
a$CysC_1<-mydata$f.30720.1.0

a$Dbil_0<-mydata$f.30660.0.0
a$Dbil_1<-mydata$f.30660.1.0

a$GGT_0<-mydata$f.30730.0.0
a$GGT_1<-mydata$f.30730.1.0

a$LDL_0<-mydata$f.30780.0.0
a$LDL_1<-mydata$f.30780.1.0

a$lipoproteinA_0<-mydata$f.30790.0.0
a$lipoproteinA_1<-mydata$f.30790.1.0

a$estradiol_0<-mydata$f.30800.0.0
a$estradiol_1<-mydata$f.30800.1.0

a$phosphate_0<-mydata$f.30810.0.0
a$phosphate_1<-mydata$f.30810.1.0

a$RF_0<-mydata$f.30820.0.0
a$RF_1<-mydata$f.30820.1.0

a$testosterone_0<-mydata$f.30850.0.0
a$testosterone_1<-mydata$f.30850.1.0

a$Tbil_0<-mydata$f.30840.0.0
a$Tbil_1<-mydata$f.30840.1.0

a$tot_protein_0<-mydata$f.30860.0.0
a$tot_protein_1<-mydata$f.30860.1.0

a$urate_0<-mydata$f.30880.0.0
a$urate_1<-mydata$f.30880.1.0

a$urea_0<-mydata$f.30670.0.0
a$urea_1<-mydata$f.30670.1.0

a$vitD_0<-mydata$f.30890.0.0
a$vitD_1<-mydata$f.30890.1.0


# quantile(a$CRP_0,probs = c(0,.25, .33,.5, .67,.75,1),na.rm=T)
a$CRP_cat_0<-ifelse(a$CRP_0<0.65,"<0.65",ifelse(a$CRP_0<=2.75,"0.65 ~ 2.75",ifelse(a$CRP_0>2.75,">2.75",NA)))
a$CRP_cat_1<-ifelse(a$CRP_1<0.65,"<0.65",ifelse(a$CRP_1<=2.75,"0.65 ~ 2.75",ifelse(a$CRP_1>2.75,">2.75",NA)))


#Medications------------------------------------------------------------------

myCol<-colnames(mydata)[grep("f.20003.0.",colnames(mydata))]
a$medication_0<-mydata[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$medication_0<-lapply(a$medication_0,function(x){gsub("_NA","",x)}) %>% unlist

myCol<-colnames(mydata)[grep("f.20003.1.",colnames(mydata))]
a$medication_1<-mydata[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$medication_1<-lapply(a$medication_1,function(x){gsub("_NA","",x)}) %>% unlist

myCol<-colnames(mydata)[grep("f.20003.2.",colnames(mydata))]
a$medication_2<-mydata[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$medication_2<-lapply(a$medication_2,function(x){gsub("_NA","",x)}) %>% unlist

myCol<-colnames(mydata)[grep("f.20003.3.",colnames(mydata))]
a$medication_3<-mydata[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$medication_3<-lapply(a$medication_3,function(x){gsub("_NA","",x)}) %>% unlist

medication_excel <- as.data.table(read_excel("ukb_read_match_20220823.xlsx",sheet="UKBdrugmatch_160216"))

codelist<-list()
#lipid
# LIPID_MODIFYING_AGENTS
# HMG_CoA_reductase_inhibitors
# non_HMG_CoA_reductase_inhibitor_LIPID_MODIFYING_AGENTS
# Fibrates_or_Nicotinic_acid_and_derivatives
# atorvastatin
# fluvastatin
# pravastatin
# rosuvastatin
# simvastatin

codelist$LIPID_MODIFYING_AGENTS<-unique(medication_excel[ATC3tx=="LIPID MODIFYING AGENTS",UKBdrugID,])
codelist$HMG_CoA_reductase_inhibitors<-unique(medication_excel[ATC5chem=="HMG CoA reductase inhibitors",UKBdrugID,])
codelist$non_HMG_CoA_LIPID_MODIFYING_AGENTS<-codelist$LIPID_MODIFYING_AGENTS[!(codelist$LIPID_MODIFYING_AGENTS %in% codelist$HMG_CoA_reductase_inhibitors)]
codelist$atorvastatin<-unique(medication_excel[ATCname=="atorvastatin",UKBdrugID,])
codelist$fluvastatin<-unique(medication_excel[ATCname=="fluvastatin",UKBdrugID,])
codelist$pravastatin<-unique(medication_excel[ATCname=="pravastatin",UKBdrugID,])
codelist$rosuvastatin<-unique(medication_excel[ATCname=="rosuvastatin",UKBdrugID,])
codelist$simvastatin<-unique(medication_excel[ATCname=="simvastatin",UKBdrugID,])


#antithrombotic
# ANTITHROMBOTIC_AGENTS
# Platelet_aggregation_inhibitors_excl.heparin
# clopidogrel
# Non_clopidogrel_ANTITHROMBOTIC_AGENTS

# dipyridamole
# acetylsalicylic acid (aspirin)
# cilostazol
# ticlopidine

codelist$ANTITHROMBOTIC_AGENTS<-unique(medication_excel[ATC3tx=="ANTITHROMBOTIC AGENTS",UKBdrugID,])
codelist$Platelet_aggregation_inhibitors_excl.heparin<-unique(medication_excel[ATC5chem=="Platelet aggregation inhibitors excl. heparin",UKBdrugID,])
codelist$clopidogrel<-unique(medication_excel[ATCname=="clopidogrel",UKBdrugID,])
codelist$non_clopidogrel_ANTITHROMBOTIC_AGENTS<-codelist$ANTITHROMBOTIC_AGENTS[!(codelist$ANTITHROMBOTIC_AGENTS %in% codelist$clopidogrel)]
codelist$acetylsalicylic_acid<-unique(medication_excel[ATCname=="acetylsalicylic acid"|
                                                       grepl("acetylsalicylic acid|aspirin",UKBdrugterm)|
                                                       grepl("acetylsalicylic acid|aspirin",NSFname)|
                                                       grepl("acetylsalicylic acid|aspirin",UKBdrug_trc),UKBdrugID,])
codelist$dipyridamole<-unique(medication_excel[ATCname=="dipyridamole"|grepl("dipyridamole",UKBdrug_trc),UKBdrugID,])
codelist$cilostazol<-unique(medication_excel[UKBdrug_trc=="cilostazol"|UKBdrug_trc=="pletal",UKBdrugID,])
codelist$ticlopidine<-unique(medication_excel[ATCname=="ticlopidine",UKBdrugID,])

#dm
# DRUGS_USED_IN_DIABETES
codelist$DRUGS_USED_IN_DIABETES<-unique(medication_excel[ATC3tx=="DRUGS USED IN DIABETES",UKBdrugID,])

#htn
# HTN_DRUGS
## DIURETICS
## AGENTS ACTING ON THE RENIN-ANGIOTENSIN SYSTEM
## CALCIUM CHANNEL BLOCKERS
## BETA BLOCKING AGENTS
## ANTIHYPERTENSIVES

codelist$HTN_DRUGS<-unique(medication_excel[grepl("DIURETICS|AGENTS ACTING ON THE RENIN-ANGIOTENSIN SYSTEM|CALCIUM CHANNEL BLOCKERS|BETA BLOCKING AGENTS|ANTIHYPERTENSIVES",ATC3tx),UKBdrugID,])


for(i in 1:length(codelist)){
  a[[paste0(names(codelist)[i],"_0")]]<-grepl(paste(unlist(codelist[i]),collapse="|"),a$medication_0) %>% as.integer
  a[[paste0(names(codelist)[i],"_1")]]<-grepl(paste(unlist(codelist[i]),collapse="|"),a$medication_1) %>% as.integer
  a[[paste0(names(codelist)[i],"_2")]]<-grepl(paste(unlist(codelist[i]),collapse="|"),a$medication_2) %>% as.integer
  a[[paste0(names(codelist)[i],"_3")]]<-grepl(paste(unlist(codelist[i]),collapse="|"),a$medication_3) %>% as.integer
}

medication_vars<-a[,.SD,.SDcols=c((ncol(a)-length(codelist)*4+1):ncol(a))] %>% colnames

#MetS Diagnostic Criteria--------------------------------------------------------------------

a$DM_0<-ifelse(a$DM_diagnosed_0==1 | a$insulin_medication_self_0==1,1,0)
a$DM_1<-ifelse(a$DM_diagnosed_1==1 | a$insulin_medication_self_1==1,1,0)
a$DM_2<-ifelse(a$DM_diagnosed_2==1 | a$insulin_medication_self_2==1,1,0)
a$DM_3<-ifelse(a$DM_diagnosed_3==1 | a$insulin_medication_self_3==1,1,0)

#HbA1c: 6.0% == 42mmol/mol
#TG: 150mg/dL == 1.7mmol/L
#HDL: 40mg/dL == 1.03mmol/L
#HDL: 50mg/dL == 1.29mmol/L
a$MetS_Fasting_Glucose_0<-as.integer((a$fasting_time_0>=8 & a$glucose_0>=5.6) |
                                       a$HbA1c_0>=42 | 
                                       a$insulin_medication_self_0==1 |
                                       a$DRUGS_USED_IN_DIABETES_0==1)
a$MetS_High_BP_0<-as.integer((a$sysBP_0>=130 | a$diaBP_0>=85) |
                               a$BP_medication_self_0==1 |
                               a$HTN_DRUGS_0==1)
a$MetS_High_TG_0<-as.integer(a$TG_0>=1.7 |
                               a$non_HMG_CoA_LIPID_MODIFYING_AGENTS_0==1)
a$MetS_Low_HDL_0<-as.integer(((a$sex=="M" & a$HDL_0<1.03) | (a$sex=="F" & a$HDL_0<1.29)) |
                               a$non_HMG_CoA_LIPID_MODIFYING_AGENTS_0==1)
a$MetS_WC_ATP2005_0<-as.integer((a$sex=="F" & a$WC_0>=88) | (a$sex=="M" & a$WC_0>=102))
a$MetS_WC_IDF2009_0<-as.integer((a$sex=="F" & a$WC_0>=80) | (a$sex=="M" & a$WC_0>=94))

a$MetS_ATP2005_count_0<-a$MetS_Fasting_Glucose_0+a$MetS_High_BP_0+a$MetS_High_TG_0+a$MetS_Low_HDL_0+a$MetS_WC_ATP2005_0
a$MetS_ATP2005_0<-as.integer(a$MetS_ATP2005_count_0>=3)

a$MetS_IDF2009_count_0<-a$MetS_Fasting_Glucose_0+a$MetS_High_BP_0+a$MetS_High_TG_0+a$MetS_Low_HDL_0+a$MetS_WC_IDF2009_0
a$MetS_IDF2009_0<-as.integer(a$MetS_WC_IDF2009_0==1 &
                               (a$MetS_Fasting_Glucose_0+a$MetS_High_BP_0+a$MetS_High_TG_0+a$MetS_Low_HDL_0)>=2)


a$MetS_Fasting_Glucose_1<-as.integer((a$fasting_time_1>=8 & a$glucose_1>=5.6) |
                                       a$HbA1c_1>=42 | 
                                       a$insulin_medication_self_1==1 |
                                       a$DRUGS_USED_IN_DIABETES_1==1)
a$MetS_High_BP_1<-as.integer((a$sysBP_1>=130 | a$diaBP_1>=85) |
                               a$BP_medication_self_1==1 |
                               a$HTN_DRUGS_1==1)
a$MetS_High_TG_1<-as.integer(a$TG_1>=1.7 |
                               a$non_HMG_CoA_LIPID_MODIFYING_AGENTS_1==1)
a$MetS_Low_HDL_1<-as.integer(((a$sex=="M" & a$HDL_1<1.03) | (a$sex=="F" & a$HDL_1<1.29)) |
                               a$non_HMG_CoA_LIPID_MODIFYING_AGENTS_1==1)
a$MetS_WC_ATP2005_1<-as.integer((a$sex=="F" & a$WC_1>=88) | (a$sex=="M" & a$WC_1>=102))
a$MetS_WC_IDF2009_1<-as.integer((a$sex=="F" & a$WC_1>=80) | (a$sex=="M" & a$WC_1>=94))

a$MetS_ATP2005_count_1<-a$MetS_Fasting_Glucose_1+a$MetS_High_BP_1+a$MetS_High_TG_1+a$MetS_Low_HDL_1+a$MetS_WC_ATP2005_1
a$MetS_ATP2005_1<-as.integer(a$MetS_ATP2005_count_1>=3)

a$MetS_IDF2009_count_1<-a$MetS_Fasting_Glucose_1+a$MetS_High_BP_1+a$MetS_High_TG_1+a$MetS_Low_HDL_1+a$MetS_WC_IDF2009_1
a$MetS_IDF2009_1<-as.integer(a$MetS_WC_IDF2009_1==1 &
                               (a$MetS_Fasting_Glucose_1+a$MetS_High_BP_1+a$MetS_High_TG_1+a$MetS_Low_HDL_1)>=2)

MetS_vars<-grep("MetS",names(a),value=T)

#Outcomes------------------------------------------------------------------
#Health related outcomes - Algorithmically defined outcomes

#dementia
a$dementia_all_outcome<-ifelse(is.na(mydata$f.42018.0.0),0,1) #all cause dementia
a$dementia_all_outcome_date<-mydata$f.42018.0.0

a$dementia_alzheimer_outcome<-ifelse(is.na(mydata$f.42020.0.0),0,1)
a$dementia_alzheimer_outcome_date<-mydata$f.42020.0.0

a$dementia_vascular_outcome<-ifelse(is.na(mydata$f.42022.0.0),0,1)
a$dementia_vascular_outcome_date<-mydata$f.42022.0.0

a$dementia_frontotemporal_outcome<-ifelse(is.na(mydata$f.42024.0.0),0,1)
a$dementia_frontotemporal_outcome_date<-mydata$f.42024.0.0

#parkinson
a$parkinson_PD_outcome<-ifelse(is.na(mydata$f.42032.0.0),0,1)
a$parkinson_PD_outcome_date<-mydata$f.42032.0.0

a$parkinson_parkinsonism_outcome<-ifelse(is.na(mydata$f.42030.0.0),0,1)
a$parkinson_parkinsonism_outcome_date<-mydata$f.42030.0.0

a$parkinson_progressive_supranuclear_palsy_outcome<-ifelse(is.na(mydata$f.42034.0.0),0,1)
a$parkinson_progressive_supranuclear_palsy_outcome_date<-mydata$f.42034.0.0

a$parkinson_multiple_system_atrophy_outcome<-ifelse(is.na(mydata$f.42036.0.0),0,1)
a$parkinson_multiple_system_atrophy_outcome_date<-mydata$f.42036.0.0

#asthma
a$asthma_outcome<-ifelse(is.na(mydata$f.42014.0.0),0,1)
a$asthma_outcome_date<-mydata$f.42014.0.0

#COPD
a$COPD_outcome<-ifelse(is.na(mydata$f.42016.0.0),0,1)
a$COPD_outcome_date<-mydata$f.42016.0.0

#endstage renal
a$endstage_renal_disease_outcome<-ifelse(is.na(mydata$f.42026.0.0),0,1)
a$endstage_renal_disease_outcome_date<-mydata$f.42026.0.0

#motor neuron
a$motor_neuron_disease_outcome<-ifelse(is.na(mydata$f.42028.0.0),0,1)
a$motor_neuron_disease_outcome_date<-mydata$f.42028.0.0  

#MI
a$MI_all_outcome<-ifelse(is.na(mydata$f.42000.0.0),0,1)
a$MI_all_outcome_date<-mydata$f.42000.0.0

a$MI_STEMI_outcome<-ifelse(is.na(mydata$f.42002.0.0),0,1)
a$MI_STEMI_outcome_date<-mydata$f.42002.0.0

a$MI_NSTEMI_outcome<-ifelse(is.na(mydata$f.42004.0.0),0,1)
a$MI_NSTEMI_outcome_date<-mydata$f.42004.0.0

#stroke
a$stroke_all_outcome<-ifelse(is.na(mydata$f.42006.0.0),0,1)
a$stroke_all_outcome_date<-mydata$f.42006.0.0

a$stroke_ischaemic_outcome<-ifelse(is.na(mydata$f.42008.0.0),0,1)
a$stroke_ischaemic_outcome_date<-mydata$f.42008.0.0

a$stroke_intracerebral_haemorrhage_outcome<-ifelse(is.na(mydata$f.42010.0.0),0,1)
a$stroke_intracerebral_haemorrhage_outcome_date<-mydata$f.42010.0.0

a$stroke_subarachnoid_haemorrhage_outcome<-ifelse(is.na(mydata$f.42012.0.0),0,1)
a$stroke_subarachnoid_haemorrhage_outcome_date<-mydata$f.42012.0.0

#Death

a$death_date<-mydata$f.40000.0.0
a$death<-ifelse(!is.na(a$death_date),1,0)

#date->day conversion
events<-names(a) %>% .[grepl(pattern='_outcome|death',x=.)] %>% .[!grepl("_date",.)]


days <- sapply(events, function(v){as.integer(pmin(a[[paste0(v, "_date")]], a[["death_date"]], as.IDate("2019-03-31"), na.rm = T) - as.IDate(a[["visit_date_0"]]))})

#days <- sapply(events,
#               function(v){
#                 as.integer(ifelse(!is.na(a[[paste0(v, "_date")]]),a[[paste0(v, "_date")]],
#                                   ifelse(!is.na(a[["death_date"]]),a[["death_date"]],as.IDate("2019-03-31")))) - as.integer(as.IDate(a[["visit_date_0"]]))
#               }) 
colnames(days) <- gsub("_outcome", "_day", events)
colnames(days)[colnames(days) == "death"] <- "death_day"

days<-as.data.table(days)
# days$ID<-a$ID


#Exclusion criteria----------------------------------------------------------------------------------
#20002 Non-cancer illness code, self-reported
#dementia_all_outcome 1263
#parkinson_PD_outcome 1262
#motor_neuron_disease_outcome 1259
#MI_all_outcome 1075
#stroke_all_outcome 1081 : used diagnosed history instead

a$prev_dementia<-ifelse(grepl("1263",a$noncancer_illness_self_0),1,0)
a$prev_parkinson<-ifelse(grepl("1262",a$noncancer_illness_self_0),1,0)
a$prev_motor_neuron_disease<-ifelse(grepl("1259",a$noncancer_illness_self_0),1,0)
a$prev_IHD<-ifelse(grepl("1074|1075",a$noncancer_illness_self_0),1,0)
a$prev_stroke<-a$stroke_diagnosed_0

# a[,lapply(.SD,function(x){summary(as.factor(x))}),.SDcols=c("prev_dementia","prev_parkinson","prev_motor_neuron_disease","prev_IHD","prev_stroke")]

#Data Export----------------------------------------------------------------------------------
varlist <- list(
  MetS = MetS_vars,
  Event = c(gsub("_day","_outcome",colnames(days)[1:(ncol(days)-1)]),"death"),
  Time = colnames(days),
  Base = c("age", "sex", "townsend_deprivation_index", paste0("bmi_",0:3),
           "smoking_status_0","smoking_status_1","smoking_status_2","smoking_status_3",
           "smoking_stop_age",paste0("smoking_packyears_",0:3),
           "alcohol_status_0","alcohol_status_1","alcohol_status_2","alcohol_status_3",
           "alcohol_addiction",paste0("alcohol_frequency_",0:3),paste0("noncancer_illness_self_", 0:3),
           "DM_self_0","DM_self_1","DM_self_2","DM_self_3",
           "DM_diagnosed_0","DM_diagnosed_1","DM_diagnosed_2","DM_diagnosed_3",
           "gestational_DM_0","gestational_DM_1","gestational_DM_2","gestational_DM_3",
           paste0("age_DM_diagnosed_", 0:3), paste0("glucose_", 0:1), paste0("HbA1c_", 0:1),
           paste0("sysBP_", 0:3), paste0("diaBP_", 0:3),
           paste0("WC_", 0:3), paste0("TG_", 0:1), paste0("HDL_", 0:1),
           "cholesterol_medication_self_0","BP_medication_self_0","insulin_medication_self_0",
           "cholesterol_medication_self_1","BP_medication_self_1","insulin_medication_self_1",
           "cholesterol_medication_self_2","BP_medication_self_2","insulin_medication_self_2",
           "cholesterol_medication_self_3","BP_medication_self_3","insulin_medication_self_3",
           "DM_0","DM_1","DM_2","DM_3",
           paste0("fasting_time_", 0:3),
           paste0("insulin_within_1y_diagnosed_DM_", 0:3),
           paste0("IGF1_", 0:1), paste0("SHBG_", 0:1),
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
           paste0("age_heartattack_diagnosed_", 0:3), paste0("age_angina_diagnosed_", 0:3), paste0("age_stroke_diagnosed_", 0:3),
           paste0("age_HT_diagnosed_", 0:3),
           paste0("education_college_university_",0:3),
           paste0("education_A_AS_",0:3),
           paste0("education_O_GCSEs_",0:3),
           paste0("education_CSEs_",0:3),
           paste0("education_NVQ_HND_HNC_",0:3),
           paste0("education_other_professional_",0:3),
           paste0("education_school_never_",0:2),
           paste0("education_age_completed_full_time_education_",0:2),
           "ethnicity","ethnicity_group","MET_activity","IPAQ_activity_group",
           "GPT_0","GPT_1","albumin_0","albumin_1",
           "ALP_0","ALP_1","ApoA_0","ApoA_1",
           "ApoB_0","ApoB_1","AST_0","AST_1",
           "CRP_0","CRP_1","Ca_0","Ca_1",
           "cholesterol_0","cholesterol_1","creatinine_0","creatinine_1",
           "CysC_0","CysC_1","Dbil_0","Dbil_1",
           "GGT_0","GGT_1","LDL_0","LDL_1",
           "lipoproteinA_0","lipoproteinA_1","estradiol_0","estradiol_1",
           "phosphate_0","phosphate_1","RF_0","RF_1",
           "testosterone_0","testosterone_1","Tbil_0","Tbil_1",
           "tot_protein_0","tot_protein_1","urate_0","urate_1",                                       
           "urea_0","urea_1","vitD_0","vitD_1","CRP_cat_0","CRP_cat_1",
           "prev_dementia","prev_parkinson","prev_motor_neuron_disease","prev_IHD","prev_stroke"
           ),
  MRI = grep(pattern='dMRI_|T1_', x=names(a), value = T),
  Medication = medication_vars,
  Cognition = colnames(Cog)
)

out <- cbind(a,days,Cog)
out <- out[, .SD, .SDcols = unlist(varlist)]

factor_vars<-c("sex", "smoking_status_0","smoking_status_1","smoking_status_2","smoking_status_3",
               "alcohol_status_0","alcohol_status_1","alcohol_status_2","alcohol_status_3",
               "alcohol_addiction",paste0("alcohol_frequency_",0:3),"DM_self_0","DM_self_1","DM_self_2","DM_self_3",
               "DM_diagnosed_0","DM_diagnosed_1","DM_diagnosed_2","DM_diagnosed_3",
               "gestational_DM_0","gestational_DM_1","gestational_DM_2","gestational_DM_3",
               "cholesterol_medication_self_0","BP_medication_self_0","insulin_medication_self_0",
               "cholesterol_medication_self_1","BP_medication_self_1","insulin_medication_self_1",
               "cholesterol_medication_self_2","BP_medication_self_2","insulin_medication_self_2",
               "cholesterol_medication_self_3","BP_medication_self_3","insulin_medication_self_3",
               "DM_0","DM_1","DM_2","DM_3",
               paste0("insulin_within_1y_diagnosed_DM_", 0:3),
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
               paste0("education_college_university_",0:3),
               paste0("education_A_AS_",0:3),
               paste0("education_O_GCSEs_",0:3),
               paste0("education_CSEs_",0:3),
               paste0("education_NVQ_HND_HNC_",0:3),
               paste0("education_other_professional_",0:3),
               paste0("education_school_never_",0:2),
               events,"ethnicity","ethnicity_group",
               MetS_vars,
               "CRP_cat_0","CRP_cat_1","IPAQ_activity_group",
               "prev_dementia","prev_parkinson","prev_motor_neuron_disease","prev_IHD","prev_stroke",
               medication_vars,
               "FI_fu","TM_fu","PM_fu")
out[,(factor_vars):=lapply(.SD,as.factor),.SDcols=factor_vars]

out.label <- jstable::mk.lev(out)

# label.main[variable == " ", `:=`(var_label = " ", val_label = c("","",""))]

fst::write_fst(out, "data.fst")
saveRDS(list(factor_vars = factor_vars, label= out.label, varlist = varlist), "info.RDS")

#The other study data----------------------------------------------------------------------------------
a$ID<-as.character(a$ID)

vout<-data.table()
vout$ID<-a$ID

vn<-c("IPAQ_activity_group",
      "GPT_0","albumin_0","ALP_0","ApoA_0","ApoB_0","AST_0","CRP_0","Ca_0","cholesterol_0",
      "creatinine_0","CysC_0","Dbil_0","GGT_0","LDL_0","lipoproteinA_0","estradiol_0",
      "phosphate_0","RF_0","testosterone_0","Tbil_0","tot_protein_0","urate_0","urea_0",
      "vitD_0","glucose_0","HbA1c_0","TG_0","HDL_0","IGF1_0","SHBG_0",
      "MET_activity",
      grep("_0",medication_vars,value=T),
      "age","sex","ethnicity_group","education_college_university_0","townsend_deprivation_index",
      "bmi_0","smoking_status_0","alcohol_status_0",
      "dementia_all_outcome","dementia_alzheimer_outcome","dementia_vascular_outcome","dementia_frontotemporal_outcome",
      "parkinson_PD_outcome","parkinson_parkinsonism_outcome","parkinson_progressive_supranuclear_palsy_outcome","parkinson_multiple_system_atrophy_outcome",
      "asthma_outcome","COPD_outcome","endstage_renal_disease_outcome","motor_neuron_disease_outcome","MI_all_outcome","MI_STEMI_outcome","MI_NSTEMI_outcome",
      "stroke_all_outcome","stroke_ischaemic_outcome","stroke_intracerebral_haemorrhage_outcome","stroke_subarachnoid_haemorrhage_outcome",
      "death","dementia_all_day","dementia_alzheimer_day","dementia_vascular_day","dementia_frontotemporal_day","parkinson_PD_day",
      "parkinson_parkinsonism_day","parkinson_progressive_supranuclear_palsy_day","parkinson_multiple_system_atrophy_day","asthma_day","COPD_day",
      "endstage_renal_disease_day","motor_neuron_disease_day","MI_all_day","MI_STEMI_day","MI_NSTEMI_day",
      "stroke_all_day","stroke_ischaemic_day","stroke_intracerebral_haemorrhage_day","stroke_subarachnoid_haemorrhage_day","death_day",
      "prev_dementia","prev_parkinson","prev_motor_neuron_disease","prev_IHD","prev_stroke")

vout<-cbind(vout,out[,.SD,.SDcols=vn])
vout$prev_DM<-a$DM_diagnosed_0
vout$prev_HTN<-a$HT_diagnosed_0

write.csv(vout,"variable_request_20220828.csv")

#mri data colnames----------------------------------------------------------------------------------
# mridt<-data.table()
# mridt$vn<-MRI
# mridt[,ct:=as.factor(ifelse(grepl("dMRI_skeleton",vn),134,
#                   ifelse(grepl("dMRI_weighted_means",vn),135,
#                          ifelse(grepl("T1_Regional_grey_matter_volumes_FAST",vn),1101,
#                                 ifelse(grepl("T1_Subcortical_volumes_FIRST",vn),1102,
#                                        ifelse(grepl("T1_Freesurfer_ASEG",vn),190,
#                                               ifelse(grepl("T1_Freesurfer_BA_exvivo",vn),195,
#                                                      ifelse(grepl("T1_Freesurfer_a2009s",vn),197,
#                                                             ifelse(grepl("T1_Freesurfer_DKT",vn),196,
#                                                                    ifelse(grepl("T1_Freesurfer_desikan_gw",vn),194,
#                                                                           ifelse(grepl("T1_Freesurfer_desikan_pial",vn),193,
#                                                                                        ifelse(grepl("T1_Freesurfer_desikan_white",vn),192,191))))))))))))]
# fwrite(mridt,"mridt.csv")
