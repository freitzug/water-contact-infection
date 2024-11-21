#Author: Fabian Reitzug
#Date: 2022-06-21
#Purpose: prepare hh and clinical data

# Notes: df_techu has five extra exams (were not of final participants)
#this line below: removes obs. with na's in POC-CCA and KK:
# df %<>% filter(!is.na(poc_cca) & !is.na(sma))

source("prep/load_pkgs.R") #load packages

# Read data ---------------------------------------------------------------
df_hh <- read.csv(paste0(path.dbl,"df_hh.csv"))
df_techkk <- read.csv(paste0(path.dbl,"df_techkk.csv"))[-1]
df_techu <- read.csv(paste0(path.dbl,"df_techu.csv"))[-1]

#water contact obs. data
#wc <- read.csv(paste0(path.dbl,"df_water_obs.csv"))


# Source scripts ----------------------------------------------------------
source(paste0(path.git,"funs/20220201_odk_name_cleaner.R"))
source(paste0(path.git,"funs/20220627_clean_lake_activities.R"))
source(paste0(path.git,"funs/20220613_multi_exams.R"))
source(paste0(path.git,"funs/20220629_split_var.R"))

# Main --------------------------------------------------------------------
# -------------------------------------------------------------------------

# 1. Prepare df_hh --------------------------------------------------------

#rename for convenience
df_hh %<>% dplyr::select(!1:3)
names(df_hh) <- odk_clean(df_hh)
df_hh %<>% dplyr::rename(barcode=id_ind)

#filter: study participants
df_hh$age[df_hh$age<5] <- 5 #assign children <5 age 5
df_hh %<>% dplyr::select(barcode,district_name,village_name,age,gender,hh_mem_id,occupation,yearsvillage,
                                                   year_in_village,electricity_in_household,hand_washing,soap_availability,water_availability, #vars different for mayuge
                                                   material_roof,material_walls,material_floor,water_source, toilet_facility,water_safety,#vars different for mayuge
                                                   mem_pregnant, education, school_enrolment, tribe, religion,
                                                   pzq_received,roofmaterial,wallsmaterial,floormaterial,electricity,
                                                   watersource,watersafe,toilet,handwash,water_available,soap_available,n_people,
                                                   pzq_received,symptoms_name,ind_med_care,
                                                   latitude,longitude,
                                                   study_participant,
                                                   owning_home
                                                   )

#water contact per individual [multiply lact with freq of lact]
df_hh$lact_freq <-  rowSums(df_lact[,1:11]*df_lact_trips[,1:11])

#duration per contact [multiply lact with freq of lact]
df_lact_time[,-12][df_lact_time[,-12]=="none"] <- 0
df_lact_time[,-12][df_lact_time[,-12]=="30min_or_less"] <- 0.5
df_lact_time[,-12][df_lact_time[,-12]=="less_30_minutes"] <- 0.5
df_lact_time[,-12][df_lact_time[,-12]=="1hour_or_less"] <- 1
df_lact_time[,-12][df_lact_time[,-12]=="1hour"] <- 1
df_lact_time[,-12][df_lact_time[,-12]== "2hours"] <- 2
df_lact_time[,-12][df_lact_time[,-12]== "3hours"] <- 3
df_lact_time[,-12][df_lact_time[,-12]== "4hours_or_more"] <- 4
df_lact_time[,-12][df_lact_time[,-12]== "not_know"] <- 0

df_hh$lact_dura <-  rowSums(df_lact[,1:11]*df_lact_time[,1:11] %>% mutate_all(as.numeric))

#frequency * duration
df_hh$lact_hrs <-  rowSums(df_lact[,1:11]*df_lact_time[,1:11] %>% mutate_all(as.numeric)*df_lact_trips[,1:11])

#join with df_lact, df_lact_trip, df_lact_time from clean_lake_activities.R script
names(df_lact) <- odk_clean(df_lact)
names(df_lact_time) <- odk_clean(df_lact_time)
names(df_lact_trips) <- odk_clean(df_lact_trips)
names(df_lact_daytime) <- odk_clean(df_lact_daytime)

df_hh %<>% filter(study_participant==1)

#hh_id
df_hh %<>% mutate(hh_id = as.numeric(substr(barcode,1,5)))

#generate hh-level water contact vars
df_lact_hh <- left_join(df_hh %>% dplyr::select(hh_mem_id,hh_id),df_lact) %>% select_if(is.numeric) %>% dplyr::select(!c(nlact_ind,nlact_hh))
df_lact_hh %<>% group_by(hh_id) %>% dplyr::summarise_at(vars(-1),max)
names(df_lact_hh) <- c("hh_id",paste0(names(df_lact_hh)[-1],"_hh"))

df_lact_hh <- left_join(df_hh %>% dplyr::select(hh_mem_id,hh_id),df_lact) %>% select_if(is.numeric) %>% dplyr::select(!c(nlact_ind,nlact_hh))
df_lact_hh %<>% group_by(hh_id) %>% dplyr::summarise_all(max)
names(df_lact_hh) <- c("hh_id",paste0(names(df_lact_hh)[-1],"_hh"))

df_hh %<>% left_join(df_lact,by=c("hh_mem_id")) %>% left_join(df_lact_time,by=c("hh_mem_id")) %>% 
  left_join(df_lact_trips,by=c("hh_mem_id")) %>% left_join(df_lact_daytime,by=c("hh_mem_id")) %>%
  left_join(df_lact_hh,by="hh_id")

#binary var for hh-level lact
df_hh$lact_hh <- ifelse(df_hh$nlact_hh>0,1,0)
#binary var for ind.-level lact
df_hh$lact_ind <- ifelse(df_hh$nlact_ind>0,1,0)

# 2. Prepare df_techkk ----------------------------------------------------

#Method:
# For all ppl with <= 2 readings, take mean across all exams
# For all ppl with > 2 readings take mean weighted by technicians (to account for double readings of same slide by same technician)


#baseline tech exams --------

#kk
names_kk <- odk_clean(df_techkk)
names(df_techkk) <- names_kk

#filter: only exams with techkk complete
df_techkk %<>% filter(included=="yes")

# apply multi.exam function
df_techkksma <- multi.exams(df_techkk,barcode,sma,kk_technician_a,24) %>% dplyr::select(!exam_number)
df_techkkhwa <- multi.exams(df_techkk,barcode,hwa,kk_technician_a,24) %>% dplyr::select(!exam_number)
df_techkkasca <- multi.exams(df_techkk,barcode,asca,kk_technician_a,24) %>% dplyr::select(!exam_number)
df_techkktta <- multi.exams(df_techkk,barcode,tta,kk_technician_a,24) %>% dplyr::select(!exam_number)

#u
names_u <- odk_clean(df_techu)
names(df_techu) <- names_u

#df_techu %<>% filter(included=="yes")  %>% dplyr::select(barcode,poc_cca,microhaematuria)

# 4. Create master df -----------------------------------------------------

df <- left_join(df_hh,df_techkksma,by="barcode") %>% left_join(df_techu,by="barcode")
df <- left_join(df,df_techkkhwa,by="barcode") %>% left_join(df_techkkasca,by="barcode")  %>% left_join(df_techkktta,by="barcode")

# generate helminth infection  variables --------------------

# binary kk variable
df$kk_yes_no <- NA
df$kk_yes_no <- ifelse(df$sma>0,1,0)

# binary hwa variable
df$hwa_yes_no <- NA
df$hwa_yes_no <- ifelse(df$hwa>0,1,0)

# binary tta variable
df$tta_yes_no <- NA
df$tta_yes_no <- ifelse(df$tta>0,1,0)

# binary asca variable
df$asca_yes_no <- NA
df$asca_yes_no <- ifelse(df$asca>0,1,0)


# generate infection categories
df %<>% mutate(kk_intensity = as.factor(case_when(
  sma == 0 ~ 0,
  sma < 100 & sma > 0 ~ 1,
  sma >= 100 & sma < 200  ~ 2,
  sma >= 200 ~ 3
)))

# binary poc_cca variable -- trace as pos
df %<>% mutate(poc_cca_yes_nop = factor(case_when(
  poc_cca == "pos1" | poc_cca == "pos2" | poc_cca == "pos3" | poc_cca == "trace"~ 1,
  poc_cca == "trace" | poc_cca == "negative"  ~ 0)))

# binary poc_cca variable -- trace as neg
df %<>% mutate(poc_cca_yes_nom = factor(case_when(
  poc_cca == "pos1" | poc_cca == "pos2" | poc_cca == "pos3"~ 1,
  poc_cca == "trace" | poc_cca == "negative" | poc_cca == "trace"  ~ 0)))


# gen ordered factor variable for poc_cca
df$poc_cca <- as.factor(df$poc_cca) %>% as.numeric()
df$poc_cca[df$poc_cca==2] <- NA
df$poc_cca[df$poc_cca==6] <- 2 
df$poc_cca <- as.factor(df$poc_cca)

#poc_cca trace
df$trace <- ifelse(df$poc_cca==2,1,0)

#categorical age variables
df %<>% mutate(age_cat = factor(case_when(
  age < 10 ~ 1,
  age > 9 & age < 15 ~2,
  age > 14 & age < 20~ 3,
  age > 19 & age < 25~ 4,
  age > 24 & age < 30~ 5,
  age > 29 & age < 35 ~ 6,
  age > 34 & age < 40 ~ 7,
  age > 39 & age < 45 ~ 8,
  age > 44 & age < 50 ~ 9,
  age > 49 & age < 55 ~ 10,
  age > 54 & age < 60 ~ 11,
  age > 59 & age < 65 ~ 12,
  age > 64 & age < 70 ~ 13,
  age > 69 & age < 75 ~ 14,
  age > 74 & age < 80 ~ 15,
  age > 79 & age < 85 ~ 16,
  age > 84 ~ 17
)))


#remove na's in poc_cca and kk
df %<>% filter(!is.na(poc_cca) & !is.na(sma))
df$adult <- ifelse(df$age<18,"child","adult")

#define microhaematuria variables
df %<>% mutate(mhaem_yes_no = case_when(
  microhaematuria == "trace" | microhaematuria == "1_trace_haemo" | microhaematuria == "negative" ~ 0,
  microhaematuria == "2pos" | microhaematuria == "3pos" | microhaematuria == "4pos" ~ 1
))

df$haem_yes_no <- as.factor(df$mhaem_yes_no)

#gen village id
t <- df %>% group_by(village_name) %>% dplyr::summarise(n = n(), id = first(substr(barcode,1,3)),name = first(village_name))
df$vill_id <- substr(df$barcode,2,3) %>% as.numeric() #village id 
df$hh_id <- substr(df$barcode,1,5) %>% as.factor()

#gender
df$gender_cat <- ifelse(df$gender=="male",0,1) %>% as.factor()

#occupation
df %<>% mutate(occupation_cat = factor(case_when(
  is.na(occupation) ~ 0,
  occupation == "subsistence_farmer" | occupation =="rice_farmer" ~ 1, # farming
  occupation == "fisherman" | occupation =="fishmonger"  ~ 2, #fishing
  occupation == "businessman"~3, #business
  occupation == "other" |  occupation == "carpenter" | occupation == "driver" | occupation == "health_worker" | occupation == "army" |
  occupation == "papyrus_gatherer" | occupation == "school_teacher" ~ 4,
)))


#years in village
df$yearsvillage_hh <- abs(df$yearsvillage)
df$yearsvillage_hh <- ifelse(is.na(df$yearsvillage_hh), df$year_in_village,df$yearsvillage_hh) #mayuge data saved in year_in_village var, merge in
vec <- df %>% group_by(hh_id) %>% dplyr::summarise(m_age = max(age))  #get max hh age
df <- left_join(df,vec,by="hh_id")
df$yearsvillage_hh <- ifelse(df$yearsvillage_hh > 300,df$age,df$yearsvillage_hh) #replace years in village > oldest hh member if implausible value
df$yearsvillage_ind <- df$yearsvillage_hh 
df$yearsvillage_ind <- ifelse(df$yearsvillage_ind > df$age,df$age,df$yearsvillage_ind) #same for ind. years in village
df$yearsoutside_ind <- ifelse(df$age > df$yearsvillage_hh,df$age-df$yearsvillage_hh,0 )
df$yearsvillage_hh_notind <-  df$yearsvillage_hh -df$yearsvillage_ind

#pregnancy
df %<>% mutate(pregnant = if_else(mem_pregnant==hh_mem_id,1,0,missing =0))

#education

df %<>% mutate(education_cat = factor(case_when(
  education == "none" ~0,
  education %in% c("p1","p2","p3","p4","p4","p5","p6","p7") ~1,
  education %in% c("s1","s2","s3","s4","s5","s6","s7") ~2,
  education =="diploma, certificate" ~3,
  education %in% c("some university")  ~4,
  education %in% c("completed university")  ~5
  )))

df %<>% mutate(school_enrolment_cat = if_else(school_enrolment=="yes",1,0,))

#tribe/religion
df$tribe <- as.factor(df$tribe)
df %<>% mutate(religion_cat = factor(case_when(
  religion %in% c("no religion","other") ~ 0,
  religion %in% c("born-again christian","christian") ~1,
  religion %in% c("muslim") ~2))
)

#gen minority tribe

#define mode function
mymode <- function(x) {
  t <- table(x)
  names(t)[ which.max(t) ]
}

vec <- df %>% group_by(vill_id) %>% dplyr::summarise(maj_tribe = mymode(tribe), n=n())  #get max hh age
df <- left_join(df,vec,by="vill_id")

df$maj_tribe_cat <- ifelse(df$tribe==df$maj_tribe,1,0)

#electricity
df$electricity <- ifelse(is.na(df$electricity),df$electricity_in_household,df$electricity) #mayuge data saved in var electricity_in_household
df$electricity_cat <- ifelse(df$electricity=="yes" | df$electricity=="yes_solar_power",1,0)

#building materials
df$roofmaterial <- ifelse(is.na(df$roofmaterial),df$material_roof,df$roofmaterial)
df$roofmaterial <- as.factor(df$roofmaterial)
levels(df$roofmaterial) <- c(1,4,3,2)
df$roofmaterial %<>% as.numeric()
#df$roofmaterial[is.na(df$roofmaterial)] <- 0

df$wallsmaterial <- ifelse(is.na(df$wallsmateria),df$material_walls,df$wallsmaterial)
df$wallsmaterial <- as.factor(df$wallsmaterial)
levels(df$wallsmaterial) <- c(4,3,1,2)
df$wallsmaterial %<>% as.numeric()
#df$wallsmaterial[is.na(df$wallsmaterial)] <- 0

df$floormaterial <- ifelse(is.na(df$floormaterial),df$material_floor,df$floormaterial)
df$floormaterial <- as.factor(df$floormaterial)
levels(df$floormaterial) <- c(4,1,2,3)
df$floormaterial %<>% as.numeric()
#df$floormaterial[is.na(df$floormaterial)] <- 0

#gen home quality score -> count of above vars
df$home_quality_score <- rowSums(df[,c("roofmaterial","wallsmaterial","floormaterial")])


#water source
df$watersource <- ifelse(is.na(df$watersource),df$water_source,df$watersource)
df$openfreshw <- ifelse(df$watersource=="water_from_swamp" | df$watersource=="water_from_lake",1,0)
df$watersource <- as.factor(df$watersource) %>% as.numeric()

df$watersafe <- ifelse(is.na(df$watersafe),df$water_safety,df$watersafe)
#gen one var for each cat
df <- split_var(df,"watersafe")

df$toilet <- ifelse(is.na(df$toilet),df$toilet_facility,df$toilet)
df$toilet_cat <- ifelse(df$toilet=="no",0,1)
df %<>% mutate(toilet = case_when(
  toilet=="no" ~ 0,
  toilet=="bucket_latrine" ~ 1,
  toilet=="composting_toilet"  ~ 2,
  toilet=="covered_latrine_with_privacy"  ~ 3,
  toilet=="flush_toilet" ~ 4,
  toilet=="uncovered_latrine_without_privacy" ~ 5,
  toilet=="other"~ 6))

#water purification 
df %<>% mutate(water_pur = if_else(
  df$watersafe %in% c("no"),0,1 )
)

#handwash and soap
df$handwash <- ifelse(is.na(df$handwash), df$hand_washing,df$handwash)
df$handwash_cat <- ifelse(df$handwash=="fixed_facility",1,0)
df$handwash <- as.factor(df$handwash)
levels(df$handwash) <- c(1,0,2,3); df$handwash <- as.character(df$handwash) %>% as.numeric()
df$soap_available <- ifelse(is.na(df$soap_available),df$soap_availability,df$soap_available)
df$soap_cat <- ifelse(df$soap_available=="soap_available",1,0)
df$water_available <- ifelse(is.na(df$water_available),df$water_availability,df$water_available)
df$water_cat <- ifelse(df$water_available=="water_available" ,1,0)

#pzq
df$pzq_cat <- ifelse(df$pzq_received=="yes",1,0)

#UTI/blood in urine
df %<>% mutate(uti_cat = if_else(str_detect(df$symptoms_name,"urinary_tract_infection")==TRUE,1,0,missing = 0))  
df %<>% mutate(blood_u_cat = if_else(str_detect(df$symptoms_name,"blood_in_urine")==TRUE,1,0,missing = 0))  

df %<>% mutate(hc_visit_cat = if_else(ind_med_care=="government_health_centre",1,0,missing = 0))  

#district
df %<>% mutate(district = case_when(district_name=="mayuge" ~1,
                                    district_name=="buliisa" ~2,
                                    district_name=="pakwach" ~3,
))

#village stats and prevalence

#village stats (SACs)
vill <- df %>% filter(age<15) %>% group_by(village_name) %>% dplyr::summarise(n = n(),
                                                                       prev = length(kk_yes_no[kk_yes_no==1])/n(),
                                                                       prev_heav =length(kk_intensity[kk_intensity==3])/n(),
                                                                       el_php = ifelse(prev_heav<0.01,1,0),
                                                                       prev_cat = ifelse(prev<.10,1,2)
)

vill$prev_cat <- ifelse(vill$prev>.50,3,vill$prev_cat)

df <- left_join(df,vill %>% select(village_name,prev,prev_cat),by="village_name")


