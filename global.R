library(data.table);library(magrittr);library(parallel);library(fst);library(stats)

#=setwd("/home/js/ShinyApps/chi-hun.kim/UKbiobank-KCH")
bd <- fst::read_fst("/home/heejooko/ShinyApps/UKbiobank/ukb47038.fst", as.data.table = T)

a<-data.table()
# _0 : initial 2006-2010 / _1 : first repeat assessment visit 2012-2013

#Population characteristics------------------------------------------------------------------
a$ID<-bd$f.eid
a$age<-bd$f.21022.0.0 #age at recruitment [years]
a$sex<-ifelse(bd$f.31.0.0==0,"F","M")
a$townsend_deprivation_index<-bd$f.189.0.0
a$visit_date_0<-bd$f.53.0.0
a$visit_date_1<-bd$f.53.1.0

#Summed MET minutes per week for all activity 22040 없다.
# a$MET_activity<-bd$f.22040.0.0 

a$smoking_status<-bd$f.20116.0.0 # -3	Prefer not to answer / 0	Never / 1	Previous / 2	Current
a$smoking_stop_age<-bd$f.22507.0.0
a$smoking_packyears<-bd$f.20161.0.0

a$alcohol_status<-bd$f.20117.0.0 # -3	Prefer not to answer / 0	Never / 1	Previous / 2	Current
a$alcohol_frequency<-bd$f.1558.0.0 # 1	Daily or almost daily / 2	Three or four times a week / 3	Once or twice a week / 4	One to three times a month / 5	Special occasions only / 6	Never / -3	Prefer not to answer
a$alcohol_addiction<-bd$f.20406.0.0 # -818	Prefer not to answer / -121	Do not know / 0	No / 1	Yes : ever addicted to alcohol

#Exposures------------------------------------------------------------------
#DM
#Non-cancer illness, self-reported
myCol<-colnames(bd)[grep("f.20002.0.",colnames(bd))]
a$noncancer_illness_self<-bd[,..myCol][, do.call(paste, c(.SD, sep = "_")),]
a$noncancer_illness_self<-lapply(a$noncancer_illness_self,function(x){gsub("_NA","",x)}) %>% unlist

a$DM_self<-ifelse(grepl(pattern='1220|1221|1222|1223',x=a$noncancer_illness_self),1,0)
a$DM_diagnosed<-ifelse(bd$f.2443.0.0==1,1,ifelse(bd$f.2443.0.0==0,0,NA))

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

a$sysBP_0<-ifelse(is.na(a$sysBP_automated_0),a$sysBP_manual_0,a$sysBP_automated_0)
a$sysBP_1<-ifelse(is.na(a$sysBP_automated_1),a$sysBP_manual_1,a$sysBP_automated_1)

myCol<-colnames(bd)[grep("f.4079.0.",colnames(bd))]
a$diaBP_automated_0<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(bd)[grep("f.94.0.",colnames(bd))]
a$diaBP_manual_0<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]

myCol<-colnames(bd)[grep("f.4079.1.",colnames(bd))]
a$diaBP_automated_1<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]
myCol<-colnames(bd)[grep("f.94.1.",colnames(bd))]
a$diaBP_manual_1<-bd[,..myCol][,rowMeans(.SD,na.rm=T),]

a$diaBP_0<-ifelse(is.na(a$diaBP_automated_0),a$diaBP_manual_0,a$diaBP_automated_0)
a$diaBP_1<-ifelse(is.na(a$diaBP_automated_1),a$diaBP_manual_1,a$diaBP_automated_1)

#Abdominal obesity
#[cm]
a$WC_0<-bd$f.48.0.0
a$WC_1<-bd$f.48.1.0

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
a$cholesterol_medication_0<-ifelse(bd$f.6177.0.0==1,1,0)
a$BP_medication_0<-ifelse(bd$f.6177.0.0==2,1,0)
a$insulin_medication_0<-ifelse(bd$f.6177.0.0==3,1,0)

a$cholesterol_medication_1<-ifelse(bd$f.6177.1.0==1,1,0)
a$BP_medication_1<-ifelse(bd$f.6177.1.0==2,1,0)
a$insulin_medication_1<-ifelse(bd$f.6177.1.0==3,1,0)


#Biomarkers
#IGF-1 [nmol/L]
a$IGF1_0<-bd$f.30770.0.0
a$IGF1_1<-bd$f.30770.1.0

#SHBG [nmol/L]
a$SHBG_0<-bd$f.30830.0.0
a$SHBG_1<-bd$f.30830.1.0


#Other past medical conditions------------------------------------------------------------------
a$angina<-ifelse(grepl(pattern='1074',x=a$noncancer_illness_self),1,0)
a$heartattack_or_MI<-ifelse(grepl(pattern='1075',x=a$noncancer_illness_self),1,0)

a$ischaemicstroke<-ifelse(grepl(pattern='1583',x=a$noncancer_illness_self),1,0)
a$tia<-ifelse(grepl(pattern='1082',x=a$noncancer_illness_self),1,0)

a$depression<-ifelse(grepl(pattern='1286',x=a$noncancer_illness_self),1,0)
a$schizophrenia<-ifelse(grepl(pattern='1289',x=a$noncancer_illness_self),1,0)
a$maniabiopolar<-ifelse(grepl(pattern='1291',x=a$noncancer_illness_self),1,0)


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


#Outcomes------------------------------------------------------------------
#Health related outcomes - Algorithmically defined outcomes

a$dementia<-ifelse(is.na(bd$f.42018.0.0),0,1) #all cause dementia
a$dementia_date<-bd$f.42018.0.0

a$parkinson<-ifelse(is.na(bd$f.42032.0.0),0,1)
a$parkinson_date<-bd$f.42032.0.0

a$asthma<-ifelse(is.na(bd$f.42014.0.0),0,1)
a$asthma_date<-bd$f.42014.0.0

a$COPD<-ifelse(is.na(bd$f.42016.0.0),0,1)
a$COPD_date<-bd$f.42016.0.0

a$endstage_renal_disease<-ifelse(is.na(bd$f.42026.0.0),0,1)
a$endstage_renal_disease_date<-bd$f.42026.0.0

a$motor_neuron_disease<-ifelse(is.na(bd$f.42028.0.0),0,1)
a$motor_neuron_disease_date<-bd$f.42028.0.0  

a$MI<-ifelse(is.na(bd$f.42000.0.0),0,1)
a$MI_date<-bd$f.42000.0.0

a$stroke<-ifelse(is.na(bd$f.42006.0.0),0,1)
a$stroke_date<-bd$f.42006.0.0


days <- sapply(c("dementia", "parkinson", "asthma", "motor_neuron", "MI", "stroke"), function(v){
  as.integer(pmin(as.IDate("2019-03-31"), a[[paste0(v, "_date")]], na.rm = T) - a[["visit_date_0"]])
}) %>% do.call(cbind, .)
colnames(days) <- paste0(colnames(days), "_day")



#----------------------------------------------------------------------------------

out<-cbind(a[,-c("ID","sysBP_automated_0","sysBP_manual_0","sysBP_automated_1","sysBP_manual_1","diaBP_automated_0","diaBP_manual_0","diaBP_automated_1","diaBP_manual_1")],
           days)

factor_vars<-c("sex", "smoking_status","alcohol_status","alcohol_addiction","DM_self","DM_diagnosed",
               "cholesterol_medication_0","BP_medication_0","insulin_medication_0",
               "cholesterol_medication_1","BP_medication_1","insulin_medication_1",
               "angina","heartattack_or_MI","ischaemicstroke","tia","depression","schizophrenia","maniabiopolar",
               "dementia","parkinson","asthma","COPD","endstage_renal_disease","motor_neuron_disease","MI","stroke")
out[,(factor_vars):=lapply(.SD,as.factor),.SDcols=factor_vars]

out.label <- jstable::mk.lev(a)

# label.main[variable == " ", `:=`(var_label = " ", val_label = c("","",""))]

#saveRDS(list(data = out, label = out.label), "data.RDS")
