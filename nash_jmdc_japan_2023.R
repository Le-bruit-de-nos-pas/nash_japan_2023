library(tidyverse)
library(data.table)
library(lubridate)
library(randomForest)
library(readxl)
library(missMDA)


# Define continuously enrolled ---------------------------------------

JMDC_cePts <- fread("JMDC_cePts.txt", colClasses = "character")
length(unique(JMDC_cePts$MEMBER_ID)) #3376803
sort(unique(JMDC_cePts$GENDER_OF_MEMBER))

# Age >= 18 years old
# Max Date Should be 202301, 18th birthday for those born after 2005-01
sort(unique(JMDC_cePts$MONTH_AND_YEAR_OF_BIRTH_OF_MEMBER))
min(sort(unique(JMDC_cePts$MONTH_AND_YEAR_OF_BIRTH_OF_MEMBER))) ; max(sort(unique(JMDC_cePts$MONTH_AND_YEAR_OF_BIRTH_OF_MEMBER)))
JMDC_cePts <- JMDC_cePts[JMDC_cePts$MONTH_AND_YEAR_OF_BIRTH_OF_MEMBER<="200501",]
min(sort(unique(JMDC_cePts$MONTH_AND_YEAR_OF_BIRTH_OF_MEMBER))) ; max(sort(unique(JMDC_cePts$MONTH_AND_YEAR_OF_BIRTH_OF_MEMBER)))

# Continuously enrolled up to Jan 2023
min(sort(unique(JMDC_cePts$OBSERVATION_END))) ; max(sort(unique(JMDC_cePts$OBSERVATION_END)))
min(sort(unique(JMDC_cePts$OBSERVATION_START))) ; max(sort(unique(JMDC_cePts$OBSERVATION_START)))

fwrite(JMDC_cePts , "JMDC_cePts_18plus.txt")

# ---------------------------------------

# Project to Japanese Population V1 -------------------------------

Projection_Weights_Japan <- fread("Projection Weights Japan.txt", colClasses = "character")
sum(as.numeric(Projection_Weights_Japan$total_population))
names(Projection_Weights_Japan)[2] <- "GENDER_OF_MEMBER"
Projection_Weights_Japan <- Projection_Weights_Japan %>% select(age, GENDER_OF_MEMBER, total_population)
Projection_Weights_Japan$age <- as.numeric(Projection_Weights_Japan$age)
Projection_Weights_Japan$total_population <- as.numeric(Projection_Weights_Japan$total_population)
sum(Projection_Weights_Japan$total_population)


JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt", colClasses = "character")
JMDC_cePts_18plus <- JMDC_cePts_18plus %>% mutate(GENDER_OF_MEMBER=ifelse(GENDER_OF_MEMBER=="Male", "M", "F"))
JMDC_cePts_18plus <- JMDC_cePts_18plus %>% mutate(YoB = str_sub(MONTH_AND_YEAR_OF_BIRTH_OF_MEMBER, 1L, 4L)) 
JMDC_cePts_18plus <- JMDC_cePts_18plus %>% mutate(MoB = str_sub(MONTH_AND_YEAR_OF_BIRTH_OF_MEMBER, 5L, 6L))
JMDC_cePts_18plus <- JMDC_cePts_18plus %>% mutate(DoB = paste(YoB, MoB, sep="-")) %>% mutate(DoB = paste(DoB, "-01", sep=""))
JMDC_cePts_18plus <- JMDC_cePts_18plus %>% mutate(DoB = as.Date(DoB)) %>% select(-c(YoB, MoB))
min(interval(JMDC_cePts_18plus$DoB, as.Date("2023-01-01")) / years(1))
JMDC_cePts_18plus$age <- interval(JMDC_cePts_18plus$DoB, as.Date("2023-01-01")) / years(1)
JMDC_cePts_18plus$age <- round(JMDC_cePts_18plus$age) 
range(JMDC_cePts_18plus$age)
JMDC_cePts_18plus <- JMDC_cePts_18plus %>% select(-weight)

data.frame(JMDC_cePts_18plus %>% group_by(age) %>% count())

Samples <- JMDC_cePts_18plus %>% group_by(GENDER_OF_MEMBER, age) %>% count()
Samples <- Projection_Weights_Japan %>% left_join(Samples) %>% mutate(weight=total_population/n)
Samples <- Samples %>% select(age, GENDER_OF_MEMBER, weight)
Samples <-Samples %>% drop_na()



Samples %>% rename("Gender"="GENDER_OF_MEMBER") %>%
  ggplot(aes(age, weight, colour=Gender)) +
  geom_line(size=2) +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(18,75,3)) +
    xlab("\n Age (years)") + ylab("Projection Weight \n") +
  scale_colour_manual(values=c("#a52a2a","#0d2b4e")) 
  

JMDC_cePts_18plus <- JMDC_cePts_18plus %>% left_join(Samples)
fwrite(JMDC_cePts_18plus, "JMDC_cePts_18plus.txt")

data.frame(JMDC_cePts_18plus %>% rename("Gender"="GENDER_OF_MEMBER") %>%
  group_by(age, Gender) %>% summarise(n=sum(weight))) %>%
  ggplot(aes(age, n, colour=Gender, fill=Gender)) +
  geom_col(alpha=0.7) +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(18,75,3)) +
    xlab("\n Age (years)") + ylab("Projected Population \n") +
  scale_colour_manual(values=c("#a52a2a","#0d2b4e")) +
      scale_fill_manual(values=c("#a52a2a","#0d2b4e")) 


data.frame(JMDC_cePts_18plus %>% rename("Gender"="GENDER_OF_MEMBER") %>%
  group_by(age, Gender) %>% count()) %>%
  ggplot(aes(age, n, colour=Gender, fill=Gender)) +
  geom_col(alpha=0.7) +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(18,75,3)) +
    xlab("\n Age (years)") + ylab("Available Sample Counts \n") +
  scale_colour_manual(values=c("#a52a2a","#0d2b4e")) +
      scale_fill_manual(values=c("#a52a2a","#0d2b4e")) 
  
# -------------------

# Identify NASH | NAFLD patients among continuously enrolled ------------------------

JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt", colClasses = "character")


NASH_JMDC_Diagnosis_Codes <- fread("NASH_JMDC_Diagnosis_Codes.csv", colClasses = "character")



ce18_NASH_diags <- fread("ce18_NASH_diags.txt", colClasses = "character")

# 0.00260104
NASH_pats <- ce18_NASH_diags %>% filter(STANDARD_DISEASE_CODE=="8843497") %>% select(MEMBER_ID) %>% distinct() 



# 0.07299258
NAFLD_pats <- ce18_NASH_diags %>% filter(STANDARD_DISEASE_CODE=="8850319"|
                                          STANDARD_DISEASE_CODE=="8850318"|
                                          STANDARD_DISEASE_CODE=="5718008") %>% select(MEMBER_ID) %>% distinct() %>% anti_join(NASH_pats)


Fibrosis_pats <- NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Fibrosis") %>% select(standard_disease_code) %>%
  inner_join(ce18_NASH_diags, by=c("standard_disease_code"="STANDARD_DISEASE_CODE")) %>% 
  select(MEMBER_ID) %>% distinct()

Cirrhosis_pats <- NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Cirrhosis") %>% select(standard_disease_code) %>%
  inner_join(ce18_NASH_diags, by=c("standard_disease_code"="STANDARD_DISEASE_CODE")) %>% 
  select(MEMBER_ID) %>% distinct()


NASH_pats # 7510
NASH_pats %>% inner_join(Fibrosis_pats) #137

NASH_pats %>% inner_join(Cirrhosis_pats) #4105

NAFLD_pats$Dx <- "NAFLD"
NASH_pats$Dx <- "NASH"

Dx_pats <- NASH_pats %>% bind_rows(NAFLD_pats)

fwrite(Dx_pats, "NASH_NAFLD_Dx_pats.txt")


#V2 fibrosis cirrhosis

JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt", colClasses = "character")
ce18_NASH_diags <- fread("ce18_NASH_diags.txt", colClasses = "character")

NASH_JMDC_Diagnosis_Codes <- fread("NASH_JMDC_Diagnosis_Codes.csv", colClasses = "character")

# 0.00260104
NASH_pats <- ce18_NASH_diags %>% filter(STANDARD_DISEASE_CODE=="8843497") %>% select(MEMBER_ID) %>% distinct() 

NASH_pats$Dx <- "NASH"

# 0.07299258
NAFLD_pats <- ce18_NASH_diags %>% filter(STANDARD_DISEASE_CODE=="8850319"|
                                          STANDARD_DISEASE_CODE=="8850318"|
                                          STANDARD_DISEASE_CODE=="5718008") %>% select(MEMBER_ID) %>% distinct() %>% anti_join(NASH_pats)

NAFLD_pats$Dx <- "NAFLD"



Fibrosis_pats <- NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Fibrosis") %>% select(standard_disease_code) %>%
  inner_join(ce18_NASH_diags, by=c("standard_disease_code"="STANDARD_DISEASE_CODE")) %>% 
  select(MEMBER_ID) %>% distinct()

Fibrosis_pats$Fibrosis <- "Fibrosis"

Cirrhosis_pats <- NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Cirrhosis") %>% select(standard_disease_code) %>%
  inner_join(ce18_NASH_diags, by=c("standard_disease_code"="STANDARD_DISEASE_CODE")) %>% 
  select(MEMBER_ID) %>% distinct()

Cirrhosis_pats$Cirrhosis <- "Cirrhosis"

NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Fibrosis")  %>% select(standard_disease_code )
NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Cirrhosis")  %>% select(standard_disease_code )


Diagnosis_master <- fread("Diagnosis_master.csv", colClasses = "character")

NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Fibrosis")  %>% select(standard_disease_code ) %>%
  inner_join(Diagnosis_master %>% select(standard_disease_code, standard_disease_name_j, standard_disease_name))


NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Cirrhosis")  %>% select(standard_disease_code ) %>%
  inner_join(Diagnosis_master %>% select(standard_disease_code, standard_disease_name_j, standard_disease_name))





temp <- NASH_pats %>% bind_rows(NAFLD_pats) %>% left_join(Fibrosis_pats) %>% left_join(Cirrhosis_pats) %>% 
  left_join(JMDC_cePts_18plus %>% select(MEMBER_ID, weight))

temp %>%  group_by(Dx, Fibrosis, Cirrhosis) %>% summarise(n=sum(as.numeric(weight)))


temp2 <- NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Fibrosis"|Type=="Cirrhosis")  %>% select(standard_disease_code, icd10_level3_name ) %>%
  inner_join(ce18_NASH_diags, by=c("standard_disease_code"="STANDARD_DISEASE_CODE")) %>%
  select(MEMBER_ID, icd10_level3_name) %>% distinct() %>%
  mutate(Fibrosis=ifelse(grepl("ibrosis", icd10_level3_name), 1,0)) %>%
  mutate(Cirrhosis=ifelse(grepl("irrhosis", icd10_level3_name), 1,0)) %>%
  select(MEMBER_ID, Fibrosis, Cirrhosis) %>% 
  mutate(sum=Fibrosis+Cirrhosis) %>% group_by(MEMBER_ID) %>% filter(sum==max(sum)) %>% slice(1) %>% ungroup() %>% select(-sum)

temp2 <- NASH_pats %>% bind_rows(NAFLD_pats) %>%  left_join(JMDC_cePts_18plus %>% select(MEMBER_ID, weight)) %>% 
  left_join(temp2)

temp2[is.na(temp2)] <- 0 

temp2 %>%  group_by(Dx, Fibrosis, Cirrhosis) %>% summarise(n=sum(as.numeric(weight)))

# ------------------------------------------------------
# Project to Japanese Population V2 -------------------------------
JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt", colClasses = "character")
JMDC_cePts_18plus <-JMDC_cePts_18plus %>% select(-weight)

Projection_Weights_Japan <- fread("Projection Weights Japan.txt", colClasses = "character")
sum(as.numeric(Projection_Weights_Japan$total_population))
names(Projection_Weights_Japan)[2] <- "GENDER_OF_MEMBER"
Projection_Weights_Japan <- Projection_Weights_Japan %>% select(age, GENDER_OF_MEMBER, total_population)
Projection_Weights_Japan$age <- as.numeric(Projection_Weights_Japan$age)
Projection_Weights_Japan$total_population <- as.numeric(Projection_Weights_Japan$total_population)
sum(Projection_Weights_Japan$total_population)

Projection_Weights_Japan %>% filter(age>65) %>% group_by(GENDER_OF_MEMBER) %>% summarise(n=sum(total_population))


To_apply <- Projection_Weights_Japan %>% filter(age>65) %>% group_by(age, GENDER_OF_MEMBER) %>% summarise(n=sum(total_population)) %>%
  mutate(n=ifelse(GENDER_OF_MEMBER=="M", n/14885729, n/19326156))

To_apply %>%  group_by(GENDER_OF_MEMBER) %>% summarise(n=sum(n))

sum(To_apply$n)


To_apply <- To_apply %>% arrange(GENDER_OF_MEMBER, age) %>% ungroup() %>% group_by(GENDER_OF_MEMBER) %>% mutate(n2=cumsum(n))
data.frame(To_apply %>% arrange(age, GENDER_OF_MEMBER))


TO_change <- JMDC_cePts_18plus %>% filter(age>65) %>% select(MEMBER_ID, GENDER_OF_MEMBER)
TO_change <- TO_change %>% arrange(GENDER_OF_MEMBER)
TO_change %>% group_by(GENDER_OF_MEMBER) %>% count()
TO_change <- TO_change %>% group_by(GENDER_OF_MEMBER) %>% mutate(ID=row_number())


To_apply <- To_apply %>% mutate(samples=ifelse(GENDER_OF_MEMBER=="F", n*47212, n*67188)) %>%
  group_by(GENDER_OF_MEMBER) %>% mutate(samples2=round(cumsum(samples)))

To_apply <- To_apply %>% select(age, GENDER_OF_MEMBER, samples2)

TO_change %>% group_by(GENDER_OF_MEMBER) %>% summarise(n=max(ID))


temp <- TO_change %>% left_join(To_apply, by=c("GENDER_OF_MEMBER"="GENDER_OF_MEMBER")) %>% filter(ID<=samples2) %>%
  group_by(MEMBER_ID) %>% filter(samples2==min(samples2))

temp <- temp %>% select(MEMBER_ID, GENDER_OF_MEMBER, age)

data.frame(temp %>% group_by(age, GENDER_OF_MEMBER) %>% count())
names(temp)[3] <- "age2"


JMDC_cePts_18plus <- JMDC_cePts_18plus %>% left_join(temp) %>%
  mutate(age=ifelse(is.na(age2), age, age2)) 


Samples <- JMDC_cePts_18plus %>% group_by(GENDER_OF_MEMBER, age) %>% count()
Samples$age <- as.numeric(Samples$age)
Samples <- Projection_Weights_Japan %>% left_join(Samples) %>% mutate(weight=total_population/n)
Samples <- Samples %>% select(age, GENDER_OF_MEMBER, weight)
Samples <-Samples %>% drop_na()



Samples %>% rename("Gender"="GENDER_OF_MEMBER") %>%
  ggplot(aes(age, weight, colour=Gender)) +
  geom_line(size=2) +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(18,95,3)) +
    xlab("\n Age (years)") + ylab("Projection Weight \n") +
  scale_colour_manual(values=c("#a52a2a","#0d2b4e")) 
  

JMDC_cePts_18plus <- JMDC_cePts_18plus %>% mutate(age=as.numeric(age)) %>% left_join(Samples)
fwrite(JMDC_cePts_18plus, "JMDC_cePts_18plus.txt")


Dx_pats <- fread("NASH_NAFLD_Dx_pats.txt")

max(JMDC_cePts_18plus$OBSERVATION_START)

JMDC_cePts_18plus %>% rename("Gender"="GENDER_OF_MEMBER") %>% inner_join(Dx_pats) %>% filter(Dx=="NAFLD") %>% summarise(n=sum(weight))

JMDC_cePts_18plus %>% rename("Gender"="GENDER_OF_MEMBER") %>%
 group_by(Gender) %>%  summarise(n=sum(weight))


data.frame(JMDC_cePts_18plus %>% rename("Gender"="GENDER_OF_MEMBER") %>%
             inner_join(Dx_pats) %>% filter(Dx=="NASH") %>%
  group_by(age, Gender) %>% summarise(n=sum(weight))) %>%
  ggplot(aes(age, n, colour=Gender, fill=Gender)) +
  geom_col(alpha=0.7) +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(18,95,3)) +
    xlab("\n Age (years)") + ylab("Projected Population \n") +
  scale_colour_manual(values=c("#a52a2a","#0d2b4e")) +
      scale_fill_manual(values=c("#a52a2a","#0d2b4e")) 




data.frame(JMDC_cePts_18plus %>% rename("Gender"="GENDER_OF_MEMBER") %>%
  group_by(age, Gender) %>% count()) %>%
  ggplot(aes(age, n, colour=Gender, fill=Gender)) +
  geom_col(alpha=0.7) +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(18,95,3)) +
    xlab("\n Age (years)") + ylab("Available Sample Counts \n") +
  scale_colour_manual(values=c("#a52a2a","#0d2b4e")) +
      scale_fill_manual(values=c("#a52a2a","#0d2b4e")) 


# --------------------------


ce18_NASH_NAFLD_pts_Alldiags <- fread("ce18_NASH-NAFLD_pts_Alldiags.txt")
head(ce18_NASH_NAFLD_pts_Alldiags)


# Annual Checkups --------------------------------------


JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt")
Dx_pats <- fread("NASH_NAFLD_Dx_pats.txt")

sum(JMDC_cePts_18plus$weight) # 
JMDC_cePts_18plus[ , .(group_sum = sum(weight)), by = age] 

Dx_pats <- merge(x = Dx_pats, y = JMDC_cePts_18plus[ , c("MEMBER_ID", "weight")], by = "MEMBER_ID", all.x=TRUE)
sum(Dx_pats$weight)   # 9357147
Dx_pats[ , .(group_sum = sum(weight)), by = Dx]   # 1: NAFLD 9056614.5   # 2:  NASH  300532.1


ce18_NASH_NAFLD_pts_All_annual_Checkups <- fread("ce18_NASH-NAFLD_pts_All_annual_Checkups.txt")
names(ce18_NASH_NAFLD_pts_All_annual_Checkups)

ce18_NASH_NAFLD_pts_All_annual_Checkups <- Dx_pats[ce18_NASH_NAFLD_pts_All_annual_Checkups, on = .(MEMBER_ID), nomatch = NULL]

match <- c("MEMBER_ID", "Dx", "weight", "BMI", "ABDOMINAL_CIRCUMFERENCE", "SYSTOLIC_BP", "DIASTOLIC_BP", "TRIGLYCERIDE", 
           "HDL_CHOLESTEROL", "LDL_CHOLESTEROL", "AST", "ALT",  "G_GT", "FASTING_BLOOD_SUGAR", 
           "CASUAL_BLOOD_SUGAR", "HBA1C", "DRINKING_HABIT", "SERUM_URIC_ACID", "SERUM_CREATININE", "DRINKING_HABIT", "AMOUNT_DRINKING")

which_names <- which(names(ce18_NASH_NAFLD_pts_All_annual_Checkups) %in%  match)

ce18_NASH_NAFLD_pts_All_annual_Checkups <- ce18_NASH_NAFLD_pts_All_annual_Checkups[, ..which_names]

LFTs <- ce18_NASH_NAFLD_pts_All_annual_Checkups[, c("MEMBER_ID", "Dx", "AST", "ALT", "G_GT")]

LFTs <- na.omit(LFTs)

length(unique(LFTs$MEMBER_ID)) # 

LFTs_short <- LFTs[ , .(AST_mean = mean(AST), ALT_mean = mean(ALT), G_GT_mean = mean(G_GT), Dx=Dx), by = MEMBER_ID]  
LFTs_short <- distinct(LFTs_short)

mean(LFTs_short$AST_mean) # 30.03016
mean(LFTs_short$ALT_mean) # 41.42022
mean(LFTs_short$G_GT_mean) # 64.54446

LFTs_short[ , .(mean_AST = mean(AST_mean)), by = Dx]    # 1:  NASH 36.45747    2: NAFLD 29.80431
LFTs_short[ , .(mean_ALT = mean(ALT_mean)), by = Dx]    # 1:  NASH 56.34009    2: NAFLD 40.89596
LFTs_short[ , .(mean_G_GT = mean(G_GT_mean)), by = Dx]    # 1:  NASH 74.96688    2: NAFLD 64.17823


LFTs_short = melt(LFTs_short, id.vars = c("MEMBER_ID", "Dx"), 
                  measure.vars = c("AST_mean", "ALT_mean", "G_GT_mean"),  
                  variable.name = "test", value.name = "value")

LFTs_short %>%
  ggplot(aes(value, colour=Dx, fill=Dx)) +
  geom_density(alpha=0.7) +
  facet_wrap(~test, scales = "free_x") +
  xlim(0,200) +
  theme_minimal() +
  xlab("\n Result Distribution (Historic Average per Patient) \n") + 
  ylab("Patient Density \n") +
  scale_fill_manual(values=c("#0081A7", "#B5838D")) +
  scale_colour_manual(values=c("#0081A7", "#B5838D")) 




LFTs_short <- LFTs[ , .(AST_max = max(AST), ALT_max = max(ALT), G_GT_max = max(G_GT), Dx=Dx), by = MEMBER_ID]  
LFTs_short <- distinct(LFTs_short)
mean(LFTs_short$AST_max) # 42.8027
mean(LFTs_short$ALT_max) # 63.37529
mean(LFTs_short$G_GT_max) # 95.72698

LFTs_short[ , .(mean_AST = mean(AST_max)), by = Dx]    # 1:  NASH 53.99792    2: NAFLD 42.41034
LFTs_short[ , .(mean_ALT = mean(ALT_max)), by = Dx]    # 1:  NASH 88.46024    2: NAFLD 62.49614
LFTs_short[ , .(mean_G_GT = mean(G_GT_max)), by = Dx]    # 1:  NASH 112.14574    2: NAFLD 95.15156


LFTs_short = melt(LFTs_short, id.vars = c("MEMBER_ID", "Dx"), 
                  measure.vars = c("AST_max", "ALT_max", "G_GT_max"),  
                  variable.name = "test", value.name = "value")

LFTs_short %>%
  ggplot(aes(value, colour=Dx, fill=Dx)) +
  geom_density(alpha=0.7) +
  facet_wrap(~test, scales = "free_x") +
  xlim(0,250) +
  theme_minimal() +
  xlab("\n Result Distribution (Historic Max per Patient) \n") + 
  ylab("Patient Density \n") +
  scale_fill_manual(values=c("#0081A7", "#B5838D")) +
  scale_colour_manual(values=c("#0081A7", "#B5838D")) 


Other <- ce18_NASH_NAFLD_pts_All_annual_Checkups[ , .(BMI_max = max(BMI ), 
                   ABDOMINAL_CIRCUMFERENCE_max = max(ABDOMINAL_CIRCUMFERENCE), 
                   SYSTOLIC_BP_max = max(SYSTOLIC_BP),
                   DIASTOLIC_BP_max = max(DIASTOLIC_BP),
                   TRIGLYCERIDE_max = max(TRIGLYCERIDE),
                   HDL_CHOLESTEROL_max = max(HDL_CHOLESTEROL),
                   LDL_CHOLESTEROL_max = max(LDL_CHOLESTEROL),
                   FASTING_BLOOD_SUGAR_max = max(FASTING_BLOOD_SUGAR),
                   CASUAL_BLOOD_SUGAR_max = max(CASUAL_BLOOD_SUGAR),
                   HBA1C_max = max(HBA1C),
                   DRINKING_HABIT_max = max(DRINKING_HABIT),
                   AMOUNT_DRINKING_max = max(AMOUNT_DRINKING ),
                   Dx=Dx), by = MEMBER_ID]  

Other <- distinct(Other)

Other %>% mutate(BMI_max=ifelse(BMI_max>=25, 1,0)) %>% group_by(Dx, BMI_max) %>% count()

Other[ , .(mean_BMI = mean(BMI_max, na.rm=T)), by = Dx]    # 1:  NASH 27.92331    2: NAFLD 26.72808
Other[ , .(mean_ABDOMINAL_CIRCUMFERENCE = mean(ABDOMINAL_CIRCUMFERENCE_max, na.rm=T)), by = Dx]    # 1:  NASH 94.76396    2: NAFLD 92.04353
Other[ , .(mean_HBA1C = mean(HBA1C_max, na.rm=T)), by = Dx]    # 1:  NASH 6.243201    2: NAFLD 6.127791
Other[ , .(mean_SBP = mean(SYSTOLIC_BP_max, na.rm=T)), by = Dx]    # 1:  NASH 136.5469    2: NAFLD 136.6835
Other[ , .(mean_DBP = mean(DIASTOLIC_BP_max, na.rm=T)), by = Dx]    # 1:  NASH 136.5469    2: NAFLD 136.6835
Other[ , .(mean_Tri = mean(TRIGLYCERIDE_max, na.rm=T)), by = Dx]    # 1:  NASH 216.5326    2: NAFLD 211.7621
Other[ , .(mean_HDL = mean(HDL_CHOLESTEROL_max, na.rm=T)), by = Dx]    # 1:  NASH 60.57573    2: NAFLD 63.81205
Other[ , .(mean_LDL = mean(LDL_CHOLESTEROL_max, na.rm=T)), by = Dx]    # 1:  NASH 148.4634    2: NAFLD 147.1801
Other[ , .(mean_Fasting = mean(FASTING_BLOOD_SUGAR_max, na.rm=T)), by = Dx]    # 1:  NASH 115.4893    2: NAFLD 113.6343
Other[ , .(mean_Casual = mean(CASUAL_BLOOD_SUGAR_max, na.rm=T)), by = Dx]    # 1:  NASH 122.4167    2: NAFLD 124.2581
Other[ , .(mean_DRINKING_HABIT = mean(DRINKING_HABIT_max, na.rm=T)), by = Dx]    # 1:  NASH 2.589758    2: NAFLD 2.380795
Other[ , .(mean_AMOUNT_DRINKING = mean(AMOUNT_DRINKING_max, na.rm=T)), by = Dx]    # 1:  NASH 2.076975    2: NAFLD 2.303056



Other %>%
  ggplot(aes(AMOUNT_DRINKING_max , colour=Dx, fill=Dx)) +
  geom_density(alpha=0.7, show.legend = FALSE) +
  #xlim(50,250) +
  theme_minimal() +
  xlab("\n Drinking Amount Distribution (Historic Max per Patient) \n") + 
  ylab("Patient Density \n") +
  scale_fill_manual(values=c("#0081A7", "#B5838D")) +
  scale_colour_manual(values=c("#0081A7", "#B5838D")) 


# ---------------------------------
# Dxs ----------------------------

ce18_NASH_NAFLD_pts_Alldiags <- fread("ce18_NASH-NAFLD_pts_Alldiags.txt")
length(unique(ce18_NASH_NAFLD_pts_Alldiags$STANDARD_DISEASE_NAME))
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[,c("MEMBER_ID", "STANDARD_DISEASE_CODE", "STANDARD_DISEASE_NAME")]

Diagnosis_master <- fread("Diagnosis_master.csv")
Diagnosis_master <- Diagnosis_master[,c("standard_disease_code", "icd10_level4_code", "icd10_level4_name")]
names(Diagnosis_master)[1] <- "STANDARD_DISEASE_CODE"

ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[Diagnosis_master, on = .(STANDARD_DISEASE_CODE), nomatch = NULL]
ce18_NASH_NAFLD_pts_Alldiags <- distinct(ce18_NASH_NAFLD_pts_Alldiags[, -c("STANDARD_DISEASE_CODE", "STANDARD_DISEASE_NAME")])

#E11, O24, E08, E09, E13, E10
#E65, E66

T2D_Dx <- distinct(ce18_NASH_NAFLD_pts_Alldiags[grepl("E11", icd10_level4_code), c("MEMBER_ID")]) 
T2D_Dx$group <- "T2D"
OBE_Dx <- distinct(ce18_NASH_NAFLD_pts_Alldiags[grepl("E65", icd10_level4_code)|grepl("E66", icd10_level4_code), c("MEMBER_ID")]) 
OBE_Dx$group <- "OBE"
HTN_Dx <- distinct(ce18_NASH_NAFLD_pts_Alldiags[grepl("I10", icd10_level4_code), c("MEMBER_ID")]) 
HTN_Dx$group <- "HTN"
Lip_Dx <- distinct(ce18_NASH_NAFLD_pts_Alldiags[grepl("E78", icd10_level4_code), c("MEMBER_ID")]) 
Lip_Dx$group <- "Lip"


JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt")
Dx_pats <- fread("NASH_NAFLD_Dx_pats.txt")
Dx_pats <- merge(x = Dx_pats, y = JMDC_cePts_18plus[ , c("MEMBER_ID", "weight")], by = "MEMBER_ID", all.x=TRUE)

T2D_Dx <- merge(x = Dx_pats, y = T2D_Dx, by = "MEMBER_ID", all.x=TRUE)
OBE_Dx <- merge(x = Dx_pats, y = OBE_Dx, by = "MEMBER_ID", all.x=TRUE)
HTN_Dx <- merge(x = Dx_pats, y = HTN_Dx, by = "MEMBER_ID", all.x=TRUE)
Lip_Dx <- merge(x = Dx_pats, y = Lip_Dx, by = "MEMBER_ID", all.x=TRUE)



T2D_Dx[ , .(group_sum = sum(weight)), by = c("Dx", "group")] 


OBE_Dx[ , .(group_sum = sum(weight)), by = c("Dx", "group")] 


 HTN_Dx[ , .(group_sum = sum(weight)), by = c("Dx", "group")] 
 

 
Lip_Dx[ , .(group_sum = sum(weight)), by = c("Dx", "group")] 
 


Any <- T2D_Dx %>% select(MEMBER_ID, Dx, weight) %>% distinct() %>%
  bind_rows(OBE_Dx %>% select(MEMBER_ID, Dx, weight) %>% distinct()) %>%
  bind_rows(HTN_Dx %>% select(MEMBER_ID, Dx, weight) %>% distinct()) %>%
  bind_rows(Lip_Dx %>% select(MEMBER_ID, Dx, weight) %>% distinct()) %>% distinct()

Any[ , .(group_sum = sum(weight)), by = c("Dx")] 




# ------------------------------------------------------
# Procedures -------------------------------------------------
ce18_NASH_NAFLD_pts_Procedures <- fread("ce18_NASH-NAFLD_pts_Procedures.txt")
ce18_NASH_NAFLD_pts_Procedures <- distinct(ce18_NASH_NAFLD_pts_Procedures[, c("MEMBER_ID", "STANDARDIZED_PROCEDURE_NAME")])
unique(ce18_NASH_NAFLD_pts_Procedures$STANDARDIZED_PROCEDURE_NAME)

Transplants <- distinct(ce18_NASH_NAFLD_pts_Procedures[grepl("transpl", STANDARDIZED_PROCEDURE_NAME), c("MEMBER_ID")]) 

Other <- distinct(ce18_NASH_NAFLD_pts_Procedures[grepl("elastography", STANDARDIZED_PROCEDURE_NAME)|
                                                   grepl("hardness", STANDARDIZED_PROCEDURE_NAME)|
                                                    grepl("function", STANDARDIZED_PROCEDURE_NAME)|
                                                    grepl("clearance", STANDARDIZED_PROCEDURE_NAME), c("MEMBER_ID")]) 



JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt")
Dx_pats <- fread("NASH_NAFLD_Dx_pats.txt")
Dx_pats <- merge(x = Dx_pats, y = JMDC_cePts_18plus[ , c("MEMBER_ID", "weight")], by = "MEMBER_ID", all.x=TRUE)
Dx_pats[ , .(group_sum = sum(weight)), by = c("Dx")] 



Other <- Dx_pats[Other, on = .(MEMBER_ID), nomatch = NULL]
Other[ , .(group_sum = sum(weight)), by = c("Dx")] 



Transplants <- Dx_pats[Transplants, on = .(MEMBER_ID), nomatch = NULL]
Transplants[ , .(group_sum = sum(weight)), by = c("Dx")] 

# ---------------------------------------------------------------
# Rxs ------------------------------------------------------------------------------------------
Drug_master <- fread("Drug_master.csv", colClasses = "character")
Drug_master <- Drug_master[,c("jmdc_drug_code", "atc_level4_name", "who_atc_name", "general_name")]
names(Drug_master)[1] <- "JMDC_DRUG_CODE"

ce18_NASH_NAFLD_pts_Rxs <- fread("ce18_NASH-NAFLD_pts_Rxs.txt", colClasses = "character")
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs[Drug_master, on = .(JMDC_DRUG_CODE), nomatch = NULL]

#lookup <- ce18_NASH_NAFLD_pts_Rxs %>% select(atc_level4_name, who_atc_name, general_name) %>% distinct()
#fwrite(lookup, "Drugs_Lookup_IDs.csv")

Drugs_Lookup_IDs <- fread("Drugs_Lookup_IDs.csv")

names(ce18_NASH_NAFLD_pts_Rxs)
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs[ , -c("MEDICAL_FACILITY_ID", "chunck_id")]

ce18_NASH_NAFLD_pts_Rxs <- merge(x = ce18_NASH_NAFLD_pts_Rxs, y = Drugs_Lookup_IDs, by = c("atc_level4_name", "who_atc_name", "general_name"), all=FALSE, allow.cartesian=TRUE)

na.omit(ce18_NASH_NAFLD_pts_Rxs)
unique(ce18_NASH_NAFLD_pts_Rxs$UNIT_OF_ADMINISTERED_AMOUNT)
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs[ , -c("atc_level4_name", "who_atc_name", "general_name", "MONTH_AND_YEAR_OF_MEDICAL_CARE", 
                                                         "JMDC_DRUG_CODE", "DRUG_NAME", "UNIT_OF_ADMINISTERED_AMOUNT")]

ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% mutate(YoRx = str_sub(DATE_OF_PRESCRIPTION, 1L, 4L)) 
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% mutate(MoRx = str_sub(DATE_OF_PRESCRIPTION, 5L, 6L))
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% mutate(DayoRx = str_sub(DATE_OF_PRESCRIPTION, 7L, 8L))
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% mutate(DATE_OF_PRESCRIPTION  = paste(paste(YoRx, MoRx, sep="-"), DayoRx, sep="-"))
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs[ , -c("YoRx", "MoRx", "DayoRx")]

ce18_NASH_NAFLD_pts_Rxs$DATE_OF_PRESCRIPTION <- as.Date(ce18_NASH_NAFLD_pts_Rxs$DATE_OF_PRESCRIPTION)
ce18_NASH_NAFLD_pts_Rxs$PRESCRIBED_AMOUNT_PER_DAY <- as.numeric(ce18_NASH_NAFLD_pts_Rxs$PRESCRIBED_AMOUNT_PER_DAY)
ce18_NASH_NAFLD_pts_Rxs$ADMINISTERED_DAYS <- as.numeric(ce18_NASH_NAFLD_pts_Rxs$ADMINISTERED_DAYS)
ce18_NASH_NAFLD_pts_Rxs$ADMINISTERED_AMOUNT <- as.numeric(ce18_NASH_NAFLD_pts_Rxs$ADMINISTERED_AMOUNT)
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs[ , -c("ADMINISTERED_AMOUNT", "PRESCRIBED_AMOUNT_PER_DAY", "group", "class")]

ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% mutate(MoRx = str_sub(DATE_OF_PRESCRIPTION, 1L, 7L))

length(unique(ce18_NASH_NAFLD_pts_Rxs$MoRx))
unique(ce18_NASH_NAFLD_pts_Rxs$DATE_OF_PRESCRIPTION)
unique(ce18_NASH_NAFLD_pts_Rxs$MoRx)
max(ce18_NASH_NAFLD_pts_Rxs$MoRx, na.rm=T)
min(ce18_NASH_NAFLD_pts_Rxs$MoRx, na.rm=T)
ce18_NASH_NAFLD_pts_Rxs[is.na(MoRx),]
ce18_NASH_NAFLD_pts_Rxs <- na.omit(ce18_NASH_NAFLD_pts_Rxs)
max(ce18_NASH_NAFLD_pts_Rxs$MoRx, na.rm=T) # "2023-01"
min(ce18_NASH_NAFLD_pts_Rxs$MoRx, na.rm=T) # "2016-10"

ce18_NASH_NAFLD_pts_Rxs$FROM <- ce18_NASH_NAFLD_pts_Rxs$DATE_OF_PRESCRIPTION
ce18_NASH_NAFLD_pts_Rxs$TO <- ce18_NASH_NAFLD_pts_Rxs$DATE_OF_PRESCRIPTION + ce18_NASH_NAFLD_pts_Rxs$ADMINISTERED_DAYS
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs[, -c("DATE_OF_PRESCRIPTION")]

ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs[drug!="",]
unique(ce18_NASH_NAFLD_pts_Rxs$drug)

Drugs_Lookup_IDs <- Drugs_Lookup_IDs %>% select(drug, group, class) %>% distinct() %>% arrange(class, group, drug) %>%
   mutate(drug_id=row_number()) %>% select(drug, drug_id)

ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% left_join(Drugs_Lookup_IDs) %>% select(-c(ADMINISTERED_DAYS, drug))

ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs[, -c("MoRx")]
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% arrange(MEMBER_ID, drug_id, FROM, TO)

ce18_NASH_NAFLD_pts_Rxs$FROM_n <- as.numeric(ce18_NASH_NAFLD_pts_Rxs$FROM) 
ce18_NASH_NAFLD_pts_Rxs$TO_n <- as.numeric(ce18_NASH_NAFLD_pts_Rxs$TO) 


ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% group_by(MEMBER_ID, drug_id) %>% mutate(FROM_n =ifelse(lag(TO_n)>FROM_n,lag(TO_n), FROM_n))
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% group_by(MEMBER_ID, drug_id) %>% mutate(Elapsed=FROM_n-lag(TO_n))
ce18_NASH_NAFLD_pts_Rxs$Elapsed[is.na(ce18_NASH_NAFLD_pts_Rxs$Elapsed)] <- 0
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% mutate(FROM_n=ifelse(is.na(FROM_n), as.numeric(FROM), FROM_n))

ce18_NASH_NAFLD_pts_Rxs$FROM <- as.Date(ce18_NASH_NAFLD_pts_Rxs$FROM_n, origin = "1970-01-01")
ce18_NASH_NAFLD_pts_Rxs$TO <- as.Date(ce18_NASH_NAFLD_pts_Rxs$TO_n, origin = "1970-01-01")


ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% mutate(FROM_n=ifelse(is.na(FROM_n), as.numeric(FROM), FROM_n))
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% mutate(TO_n = ifelse(TO_n-FROM_n<93, TO_n+93-(TO_n-FROM_n), TO_n  ))

ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% group_by(MEMBER_ID, drug_id) %>% mutate(FROM_n =ifelse(lag(TO_n)>FROM_n,lag(TO_n), FROM_n))
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% group_by(MEMBER_ID, drug_id) %>% mutate(Elapsed=FROM_n-lag(TO_n))
ce18_NASH_NAFLD_pts_Rxs$Elapsed[is.na(ce18_NASH_NAFLD_pts_Rxs$Elapsed)] <- 0
ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% mutate(FROM_n=ifelse(is.na(FROM_n), as.numeric(FROM), FROM_n))

ce18_NASH_NAFLD_pts_Rxs$FROM <- as.Date(ce18_NASH_NAFLD_pts_Rxs$FROM_n, origin = "1970-01-01")
ce18_NASH_NAFLD_pts_Rxs$TO <- as.Date(ce18_NASH_NAFLD_pts_Rxs$TO_n, origin = "1970-01-01")

ce18_NASH_NAFLD_pts_Rxs <- ce18_NASH_NAFLD_pts_Rxs %>% mutate(TO=ifelse(TO<FROM, FROM, TO))
ce18_NASH_NAFLD_pts_Rxs$TO <- as.Date(ce18_NASH_NAFLD_pts_Rxs$TO, origin = "1970-01-01")

temp <- ce18_NASH_NAFLD_pts_Rxs %>% select(MEMBER_ID, drug_id, FROM, TO) %>% distinct()
temp$newcolumn <- "-"


# temp <- temp %>% ungroup() %>% rowwise() %>% mutate(newcolumn = 
#                                                paste(
#                                                  (seq(as.Date(FROM), as.Date(TO), by="month")),
#                                                  collapse = "|")
#                                              )


# for (i in 1:nrow(temp)) {
#   n <- paste((seq(as.Date(temp$FROM[i]), as.Date(temp$TO[i]), by="month")), collapse = "|")
#   temp$newcolumn[i] <- n
# }


temp <- temp %>% ungroup() 
temp$FROM <- as.character(temp$FROM)
temp$TO <- as.character(temp$TO)
temp$FROM <- str_sub(temp$FROM, 1L, 7L)
temp$TO <- str_sub(temp$TO, 1L, 7L)

Lookup <- data.frame(seq(as.Date("2016-10-01"), by = "month", length.out = 88))
names(Lookup)[1] <- "Exact"
Lookup$Exact <- str_sub(Lookup$Exact, 1L, 7L)
Lookup <- distinct(Lookup)
Lookup$ID <- row_number(Lookup)

temp <- temp %>% left_join(Lookup, by=c("FROM"="Exact")) %>% mutate(FROM=ID) %>% select(-ID) %>%
  left_join(Lookup, by=c("TO"="Exact")) %>% mutate(TO=ID) %>% select(-ID) %>% distinct()


temp <- temp %>% ungroup() %>% rowwise() %>% mutate(newcolumn = 
                                                paste(
                                                  (seq(FROM, TO, by=1)),
                                                  collapse = ",")
                                              )

temp <- temp %>% select(MEMBER_ID, drug_id, newcolumn) %>% distinct()


temp <- separate_rows(temp, newcolumn, sep = ",", convert=T)
temp <- temp %>% distinct()

temp2 <- temp %>% mutate(Exp=1) %>% spread(key=newcolumn, value=Exp)

fwrite(temp2, "temp2_gap93.txt", sep="\t")



temp2_gap31 <- fread("temp2_gap31.txt", sep="\t")
temp2_gap62 <- fread("temp2_gap62.txt", sep="\t")
temp2_gap93 <- fread("temp2_gap93.txt", sep="\t")

length(unique(temp2_gap31$MEMBER_ID)) # 102454
length(unique(temp2_gap62$MEMBER_ID)) # 102454
length(unique(temp2_gap93$MEMBER_ID)) # 102454

dim(unique(na.omit(temp2_gap31[, .(MEMBER_ID, `30`)]), by = "MEMBER_ID")) # 52849
dim(unique(na.omit(temp2_gap62[, .(MEMBER_ID, `30`)]), by = "MEMBER_ID")) # 53710
dim(unique(na.omit(temp2_gap93[, .(MEMBER_ID, `30`)]), by = "MEMBER_ID")) # 54845

dim(unique(na.omit(temp2_gap31[, .(MEMBER_ID, `75`)]), by = "MEMBER_ID")) # 77552
dim(unique(na.omit(temp2_gap62[, .(MEMBER_ID, `75`)]), by = "MEMBER_ID")) # 78485
dim(unique(na.omit(temp2_gap93[, .(MEMBER_ID, `75`)]), by = "MEMBER_ID")) # 79440


JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt")
Dx_pats <- fread("NASH_NAFLD_Dx_pats.txt")
Dx_pats <- merge(x = Dx_pats, y = JMDC_cePts_18plus[ , c("MEMBER_ID", "weight")], by = "MEMBER_ID", all.x=TRUE)

sum(Dx_pats$weight) # 9357147
temp2_gap31 %>% left_join(Dx_pats) %>% select(MEMBER_ID, weight) %>% distinct() %>% summarise(n=sum(weight)) # 5302063 # 0.5666324

temp2_gap31 %>% left_join(Dx_pats) %>% select(MEMBER_ID, weight, `75`) %>% drop_na() %>% select(MEMBER_ID, weight) %>% distinct() %>% summarise(n=sum(weight))  # 4311913
temp2_gap62 %>% left_join(Dx_pats) %>% select(MEMBER_ID, weight, `75`) %>% drop_na() %>% select(MEMBER_ID, weight) %>% distinct() %>% summarise(n=sum(weight))  # 4351547
temp2_gap93 %>% left_join(Dx_pats) %>% select(MEMBER_ID, weight, `75`) %>% drop_na() %>% select(MEMBER_ID, weight) %>% distinct() %>% summarise(n=sum(weight))  # 4390791

Drugs_Lookup_IDs <- fread("Drugs_Lookup_IDs.csv")
Drugs_Lookup_IDs <- unique(Drugs_Lookup_IDs[, .(drug, group, class)])[order(class, group, drug)][, drug_id := .I][, .(group, class, drug, drug_id)]

over_time <- Dx_pats[Drugs_Lookup_IDs[temp2_gap62, on = "drug_id"], on = "MEMBER_ID"]

over_time <- gather(over_time, Month, Drugs, `1`:`88`, factor_key=TRUE)
over_time <- over_time %>% drop_na() %>% select(MEMBER_ID, weight, Month, group) %>% distinct()
over_time <- over_time %>% group_by(Month, group) %>% summarise(n=sum(weight))

over_time %>% mutate(Month=as.numeric(Month)) %>% filter(Month>=15&Month<=74) %>%
  ggplot(aes(Month-16, 100*n/9357147, colour=group, fill=group)) +
  geom_line() +
  theme_minimal()


temp2_gap62 <- temp2_gap62 %>% select(MEMBER_ID, drug_id, `15`:`74`)
temp2_gap62 <- gather(temp2_gap62, Month, Drugs, `15`:`74`, factor_key=TRUE)
temp2_gap62$Month <- as.numeric(as.character(temp2_gap62$Month))
temp2_gap62 <- temp2_gap62 %>% mutate(Month=Month-14) %>% drop_na() %>% select(-Drugs)
length(unique(temp2_gap62$Month))
temp2_gap62 <- temp2_gap62 %>% arrange(MEMBER_ID, Month, drug_id)
length(unique(temp2_gap62$MEMBER_ID))

temp2_gap62 <- temp2_gap62 %>% group_by(MEMBER_ID, Month) %>% mutate(drug_id=paste0(drug_id, collapse=","))  %>% distinct()
temp2_gap62 <- temp2_gap62 %>% ungroup() %>% spread(key=Month, value=drug_id)

temp2_gap62[is.na(temp2_gap62)] <- "-"


names(temp2_gap62)[2:61] <- paste0("month", seq(1,60,by=1) )

temp2_gap62 <- Dx_pats %>% inner_join(temp2_gap62) 

Dx_pats[ , .(group_sum = sum(weight)), by = Dx] 



Drugs_Lookup_IDs <- fread("Drugs_Lookup_IDs.csv")
Drugs_Lookup_IDs <- unique(Drugs_Lookup_IDs[, .(drug, group, class)])[order(class, group, drug)][, drug_id := .I][, .(group, class, drug, drug_id)]


fwrite(temp2_gap62, "NASH_NAFLD_Drug_Histories_G62.txt", sep="\t")

temp2_gap62 <- fread("NASH_NAFLD_Drug_Histories_G62.txt", sep="\t")

temp2_gap62[ , .(group_sum = sum(weight)), by = Dx] 


over_time <- gather(temp2_gap62, Month, Drugs, month1:month60, factor_key=TRUE)
over_time <- over_time %>% filter(Drugs!="-") %>% select(MEMBER_ID, Dx, weight, Month, Drugs) %>% distinct()
over_time <- separate_rows(over_time, Drugs, sep = ",", convert=T)

length(unique(over_time$Drugs))

over_time <- over_time %>% left_join(Drugs_Lookup_IDs %>% select(group, class, drug_id), by=c("Drugs"="drug_id")) %>%
  select(-Drugs) %>% distinct()

over_time <- over_time %>% group_by(Dx, Month, class, group) %>% summarise(n=sum(weight))

unique(over_time$group)

over_time %>% filter(Dx=="NAFLD") %>% mutate(Month=parse_number(as.character(Month))) %>% 
  mutate(group=factor(group, levels=c("Biguanide", "Glinide", "Glitazone", "Glucosidase inhibitor", "Sulfonylurea", "DPP4", "SGLT2", "Insulin", "GLP1", "Hepatoprotective", "Fibrate", "Ion-exchange resin", "Other", "PCSK9", "Statin"))) %>%
  ggplot(aes(Month, n/4973035.6, colour=group, fill=group)) +
  geom_line(linewidth=2, alpha=0.8) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)+
  scale_colour_manual(values=c("#fad6a3","#f5ad47","#ff9933","#cfccc9", "#f2a6b0", "#663e05", "#360566","#660557","#660505",
                                        "#154904","#0c5fe4","#649cf7","#c5dafc", "#05265c","#020e22")) +
  ylab("% of NAFLD patients ON each drug group over time \n") + xlab("\n")


# temp2_gap62 <- fread("temp2_gap62.txt", sep="\t")
# temp2_gap62 <- gather(temp2_gap62, Month, Drugs, `1`:`88`, factor_key=TRUE)
# temp2_gap62$Month <- as.numeric(as.character(temp2_gap62$Month))
# temp2_gap62 <- temp2_gap62 %>% drop_na()
# length(unique(temp2_gap62$MEMBER_ID)) # 102454
# temp2_gap62 <- temp2_gap62 %>% arrange(MEMBER_ID, Month, drug_id)
# temp2_gap62 <- temp2_gap62 %>% group_by(MEMBER_ID, Month) %>% mutate(drug_id=paste0(drug_id, collapse=","))  %>% distinct() %>% filter(grepl(",", drug_id))
# temp2_gap62 <- temp2_gap62 %>% ungroup() %>% spread(key=Month, value=drug_id)
# temp2_gap62[is.na(temp2_gap62)] <- "-"
# names(temp2_gap62)[2:61] <- paste0("month", seq(1,60,by=1) )
# temp2_gap62 <- Dx_pats %>% inner_join(temp2_gap62) 



# ---------------------------------------------------------------

# NASH vs NAFLD Dxs -------------------------------------------------------------

ce18_NASH_NAFLD_pts_Alldiags <- fread("ce18_NASH-NAFLD_pts_Alldiags.txt")
length(unique(ce18_NASH_NAFLD_pts_Alldiags$STANDARD_DISEASE_NAME))
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[,c("MEMBER_ID", "STANDARD_DISEASE_CODE", "STANDARD_DISEASE_NAME")]

Diagnosis_master <- fread("Diagnosis_master.csv")
Diagnosis_master <- Diagnosis_master[,c("standard_disease_code", "icd10_level4_code", "icd10_level4_name")]
names(Diagnosis_master)[1] <- "STANDARD_DISEASE_CODE"

ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[Diagnosis_master, on = .(STANDARD_DISEASE_CODE), nomatch = NULL]
ce18_NASH_NAFLD_pts_Alldiags <- distinct(ce18_NASH_NAFLD_pts_Alldiags[, -c("STANDARD_DISEASE_CODE", "STANDARD_DISEASE_NAME")])

JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt")
Dx_pats <- fread("NASH_NAFLD_Dx_pats.txt")
Dx_pats <- merge(x = Dx_pats, y = JMDC_cePts_18plus[ , c("MEMBER_ID", "weight")], by = "MEMBER_ID", all.x=TRUE)

Dx_pats <- merge(x = Dx_pats, y = ce18_NASH_NAFLD_pts_Alldiags[ , c("MEMBER_ID", "icd10_level4_code")], by = "MEMBER_ID", all.x=TRUE)

Dx_pats <- Dx_pats[, -c("weight")]
Dx_pats$Exp <- 1

Dx_pats[, .(n = .N), by = .(MEMBER_ID, Dx)][, .(n = .N), by = Dx]   # 1: NAFLD 210752   # 2:  NASH   7510

to_track <- unique(Dx_pats[, .(n = .N), by = .(Dx, icd10_level4_code)][, n := ifelse(Dx == "NAFLD", n / 210752, n / 7510)][(n >= 0.05 & Dx=="NAFLD") | (n >= 0.05 & Dx=="NASH") ][,.(icd10_level4_code)])

Dx_pats <- merge(x = to_track, y = Dx_pats, by = "icd10_level4_code", all.x=TRUE)

#temp <- temp[, .SD[sample(.N, 1000)], by = Dx]



Dx_pats$Exp <- 1

Dx_pats <- dcast(Dx_pats, MEMBER_ID + Dx ~ icd10_level4_code, value.var = "Exp")
Dx_pats[is.na(Dx_pats)] <-0
dim(Dx_pats)

temp <- Dx_pats

setDT(temp)
temp <- temp[, Dx := ifelse(Dx == "NAFLD", 0, 1)]
head(temp)

temp <- temp %>% select(-MEMBER_ID)

summary(glm(Dx ~ . , data = temp))

# -------------------------------------------------------------
# LFTs in Fibrosis vs Cirrhosis Dxed -----------------------------
JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt", colClasses = "character")

ce18_NASH_diags <- fread("ce18_NASH_diags.txt", colClasses = "character")

NASH_JMDC_Diagnosis_Codes <- read_xlsx("NASH_JMDC_Lookup_Codes.xlsx", sheet = "NASH_JMDC_Diagnosis_Codes", col_types = "text")


# 0.00260104
NASH_pats <- ce18_NASH_diags %>% filter(STANDARD_DISEASE_CODE=="8843497") %>% select(MEMBER_ID) %>% distinct() 



# 0.07299258
NAFLD_pats <- ce18_NASH_diags %>% filter(STANDARD_DISEASE_CODE=="8850319"|
                                          STANDARD_DISEASE_CODE=="8850318"|
                                          STANDARD_DISEASE_CODE=="5718008") %>% select(MEMBER_ID) %>% distinct() %>% anti_join(NASH_pats)


Fibrosis_pats <- NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Fibrosis") %>% select(standard_disease_code) %>%
  inner_join(ce18_NASH_diags, by=c("standard_disease_code"="STANDARD_DISEASE_CODE")) %>% 
  select(MEMBER_ID) %>% distinct()

Cirrhosis_pats <- NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Cirrhosis") %>% select(standard_disease_code) %>%
  inner_join(ce18_NASH_diags, by=c("standard_disease_code"="STANDARD_DISEASE_CODE")) %>% 
  select(MEMBER_ID) %>% distinct()


Fibrosis_pats$Fibrosis <- "Fibrosis"
Cirrhosis_pats$Cirrhosis <- "Cirrhosis"



JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt")
Dx_pats <- fread("NASH_NAFLD_Dx_pats.txt")

sum(JMDC_cePts_18plus$weight) # 107186025
JMDC_cePts_18plus[ , .(group_sum = sum(weight)), by = age] 

Dx_pats <- merge(x = Dx_pats, y = JMDC_cePts_18plus[ , c("MEMBER_ID", "weight")], by = "MEMBER_ID", all.x=TRUE)
sum(Dx_pats$weight)   # 9357147
Dx_pats[ , .(group_sum = sum(weight)), by = Dx]   # 1: NAFLD 9056614.5   # 2:  NASH  300532.1


ce18_NASH_NAFLD_pts_All_annual_Checkups <- fread("ce18_NASH-NAFLD_pts_All_annual_Checkups.txt")
names(ce18_NASH_NAFLD_pts_All_annual_Checkups)

ce18_NASH_NAFLD_pts_All_annual_Checkups <- Dx_pats[ce18_NASH_NAFLD_pts_All_annual_Checkups, on = .(MEMBER_ID), nomatch = NULL]

match <- c("MEMBER_ID", "Dx", "weight", "BMI", "ABDOMINAL_CIRCUMFERENCE", "SYSTOLIC_BP", "DIASTOLIC_BP", "TRIGLYCERIDE", 
           "HDL_CHOLESTEROL", "LDL_CHOLESTEROL", "AST", "ALT",  "G_GT", "FASTING_BLOOD_SUGAR", 
           "CASUAL_BLOOD_SUGAR", "HBA1C", "DRINKING_HABIT", "SERUM_URIC_ACID", "SERUM_CREATININE", "DRINKING_HABIT", "AMOUNT_DRINKING")

which_names <- which(names(ce18_NASH_NAFLD_pts_All_annual_Checkups) %in%  match)

ce18_NASH_NAFLD_pts_All_annual_Checkups <- ce18_NASH_NAFLD_pts_All_annual_Checkups[, ..which_names]

LFTs <- ce18_NASH_NAFLD_pts_All_annual_Checkups[, c("MEMBER_ID", "Dx", "AST", "ALT", "G_GT")]

LFTs <- na.omit(LFTs)

length(unique(LFTs$MEMBER_ID)) # 195401 (/218262) = 0.8952589 of the targets have FLTs

LFTs_short <- LFTs[ , .(AST_max = max(AST), ALT_max = max(ALT), G_GT_max = max(G_GT), Dx=Dx), by = MEMBER_ID]  
LFTs_short <- distinct(LFTs_short)
mean(LFTs_short$AST_max) # 42.8027
mean(LFTs_short$ALT_max) # 63.37529
mean(LFTs_short$G_GT_max) # 95.72698

LFTs_short %>% left_join(Fibrosis_pats) %>% left_join(Cirrhosis_pats) %>%
  group_by(Dx, Fibrosis, Cirrhosis) %>% summarise(n=mean(G_GT_max))


LFTs_short %>% left_join(Fibrosis_pats) %>% left_join(Cirrhosis_pats) %>%
  group_by(Dx, Fibrosis, Cirrhosis) %>% count()





# --------------------

# ICD10s for Liver Transplant --------------------------

ce18_NASH_NAFLD_pts_Alldiags <- fread("ce18_NASH-NAFLD_pts_Alldiags.txt")
length(unique(ce18_NASH_NAFLD_pts_Alldiags$STANDARD_DISEASE_NAME))
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[,c("MEMBER_ID", "STANDARD_DISEASE_CODE", "STANDARD_DISEASE_NAME")]

Diagnosis_master <- fread("Diagnosis_master.csv")
Diagnosis_master <- Diagnosis_master[,c("standard_disease_code", "icd10_level4_code", "icd10_level4_name")]
names(Diagnosis_master)[1] <- "STANDARD_DISEASE_CODE"

ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[Diagnosis_master, on = .(STANDARD_DISEASE_CODE), nomatch = NULL]
ce18_NASH_NAFLD_pts_Alldiags <- distinct(ce18_NASH_NAFLD_pts_Alldiags[, -c("STANDARD_DISEASE_NAME")])

JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt")
Dx_pats <- fread("NASH_NAFLD_Dx_pats.txt")
Dx_pats <- merge(x = Dx_pats, y = JMDC_cePts_18plus[ , c("MEMBER_ID", "weight")], by = "MEMBER_ID", all.x=TRUE)

ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% select(MEMBER_ID, STANDARD_DISEASE_CODE) %>% distinct()

Dx_pats <- merge(x = Dx_pats, y = ce18_NASH_NAFLD_pts_Alldiags[ , c("MEMBER_ID", "STANDARD_DISEASE_CODE")], by = "MEMBER_ID", all.x=TRUE)

Liver_Transplants <- Dx_pats %>% filter(grepl("8831478", STANDARD_DISEASE_CODE)|
                     grepl("8844845", STANDARD_DISEASE_CODE)|
                     grepl("8831480", STANDARD_DISEASE_CODE)|
                      grepl("8848347", STANDARD_DISEASE_CODE)|
                       grepl("8847670", STANDARD_DISEASE_CODE)|
                       grepl("8847617", STANDARD_DISEASE_CODE)) %>% select(MEMBER_ID, Dx, weight) %>% distinct()


Liver_Transplants %>% group_by(Dx) %>% summarise(n=sum(weight))






# ------------------------------------------------------

# Liver Cancer Hepatitis ---------------------------------

ce18_NASH_NAFLD_pts_Alldiags <- fread("ce18_NASH-NAFLD_pts_Alldiags.txt")
length(unique(ce18_NASH_NAFLD_pts_Alldiags$STANDARD_DISEASE_NAME))
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[,c("MEMBER_ID", "STANDARD_DISEASE_CODE")]

Diagnosis_master <- fread("Diagnosis_master.csv")
Diagnosis_master <- Diagnosis_master[,c("standard_disease_code", "icd10_level4_code", "icd10_level4_name")]
names(Diagnosis_master)[1] <- "STANDARD_DISEASE_CODE"


LiverCancerCodes <- Diagnosis_master %>% filter(grepl("C22", icd10_level4_code)|grepl("Z8505", icd10_level4_code)|grepl("C787", icd10_level4_code)) %>% select(STANDARD_DISEASE_CODE) %>% distinct() 
HepatitisCodes <- Diagnosis_master %>% filter(grepl("B15", icd10_level4_code)|grepl("B16", icd10_level4_code)|grepl("B17", icd10_level4_code)|grepl("B18", icd10_level4_code)|grepl("B19", icd10_level4_code)) %>% select(STANDARD_DISEASE_CODE) %>% distinct() 

LiverCancerPats <- LiverCancerCodes %>% inner_join(ce18_NASH_NAFLD_pts_Alldiags) %>% select(MEMBER_ID) %>% distinct()
HepatitisPats <- HepatitisCodes %>% inner_join(ce18_NASH_NAFLD_pts_Alldiags) %>% select(MEMBER_ID) %>% distinct()


JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt")
Dx_pats <- fread("NASH_NAFLD_Dx_pats.txt")
Dx_pats <- merge(x = Dx_pats, y = JMDC_cePts_18plus[ , c("MEMBER_ID", "weight")], by = "MEMBER_ID", all.x=TRUE)

Dx_pats %>% group_by(Dx) %>% summarise(n=sum(weight))

Dx_pats %>% inner_join(LiverCancerPats) %>% group_by(Dx) %>% summarise(n=sum(weight))


Dx_pats %>% inner_join(HepatitisPats) %>% group_by(Dx) %>% summarise(n=sum(weight))
# ----------------------------
# Annual Checkups Random Sample --------------------------------------


JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt")
sum(JMDC_cePts_18plus$weight) # 107186025
JMDC_cePts_18plus[ , .(group_sum = sum(weight)), by = age] 

ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups <- fread("ce18_Non-NASH-NAFLD_smplpts_All_annual_Checkups.txt")
names(ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups)

ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups <- merge(x = ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups, y = JMDC_cePts_18plus[ , c("MEMBER_ID", "weight")], by = "MEMBER_ID", all.x=TRUE)

match <- c("MEMBER_ID", "weight", "BMI", "ABDOMINAL_CIRCUMFERENCE", "SYSTOLIC_BP", "DIASTOLIC_BP", "TRIGLYCERIDE", 
           "HDL_CHOLESTEROL", "LDL_CHOLESTEROL", "AST", "ALT",  "G_GT", "FASTING_BLOOD_SUGAR", 
           "CASUAL_BLOOD_SUGAR", "HBA1C", "DRINKING_HABIT", "SERUM_URIC_ACID", "SERUM_CREATININE", "DRINKING_HABIT", "AMOUNT_DRINKING")

which_names <- which(names(ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups) %in%  match)

ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups <- ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups[, ..which_names]

LFTs <- ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups[, c("MEMBER_ID", "AST", "ALT", "G_GT")]

LFTs <- na.omit(LFTs)

ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups %>% select(MEMBER_ID, weight) %>% distinct() %>% summarise(n=sum(weight))

length(unique(ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups$MEMBER_ID)) # 192616 
length(unique(LFTs$MEMBER_ID)) # 192054 

LFTs_short <- LFTs[ , .(AST_max = max(AST), ALT_max = max(ALT), G_GT_max = max(G_GT)), by = MEMBER_ID]  
LFTs_short <- distinct(LFTs_short)
mean(LFTs_short$AST_max) # 27.73462
mean(LFTs_short$ALT_max) # 32.42516
mean(LFTs_short$G_GT_max) # 47.56781

LFTs_short = melt(LFTs_short, id.vars = c("MEMBER_ID"), 
                  measure.vars = c("AST_max", "ALT_max", "G_GT_max"),  
                  variable.name = "test", value.name = "value")

LFTs_short %>%
  ggplot(aes(value, colour=test, fill=test)) +
  geom_density(alpha=0.7) +
  facet_wrap(~test, scales = "free_x") +
  xlim(0,250) +
  theme_minimal() +
  xlab("\n Result Distribution (Historic Max per Patient) \n") + 
  ylab("Patient Density \n") +
  scale_fill_manual(values=c("#0b1251", "#b60c2b", "#e6bb0f")) +
  scale_colour_manual(values=c("#0b1251", "#b60c2b", "#e6bb0f")) 


Other <- ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups[ , .(BMI_max = max(BMI ), 
                   ABDOMINAL_CIRCUMFERENCE_max = max(ABDOMINAL_CIRCUMFERENCE), 
                   SYSTOLIC_BP_max = max(SYSTOLIC_BP),
                   DIASTOLIC_BP_max = max(DIASTOLIC_BP),
                   TRIGLYCERIDE_max = max(TRIGLYCERIDE),
                   HDL_CHOLESTEROL_max = max(HDL_CHOLESTEROL),
                   LDL_CHOLESTEROL_max = max(LDL_CHOLESTEROL),
                   FASTING_BLOOD_SUGAR_max = max(FASTING_BLOOD_SUGAR),
                   CASUAL_BLOOD_SUGAR_max = max(CASUAL_BLOOD_SUGAR),
                   HBA1C_max = max(HBA1C),
                   DRINKING_HABIT_max = max(DRINKING_HABIT),
                   AMOUNT_DRINKING_max = max(AMOUNT_DRINKING ),
                   weight=weight), by = MEMBER_ID]  

Other <- distinct(Other)

Other %>% mutate(BMI_max=ifelse(BMI_max>=25, 1,0)) %>% group_by(BMI_max) %>% summarise(n=sum(weight))


Other[ , .(mean_BMI = mean(BMI_max, na.rm=T))]    # 1:  NASH 27.92331    2: NAFLD 26.72808
Other[ , .(mean_ABDOMINAL_CIRCUMFERENCE = mean(ABDOMINAL_CIRCUMFERENCE_max, na.rm=T))]    # 1:  NASH 94.76396    2: NAFLD 92.04353
Other[ , .(mean_HBA1C = mean(HBA1C_max, na.rm=T))]    # 1:  NASH 6.243201    2: NAFLD 6.127791
Other[ , .(mean_SBP = mean(SYSTOLIC_BP_max, na.rm=T))]    # 1:  NASH 136.5469    2: NAFLD 136.6835
Other[ , .(mean_DBP = mean(DIASTOLIC_BP_max, na.rm=T))]    # 1:  NASH 136.5469    2: NAFLD 136.6835
Other[ , .(mean_Tri = mean(TRIGLYCERIDE_max, na.rm=T))]    # 1:  NASH 216.5326    2: NAFLD 211.7621
Other[ , .(mean_HDL = mean(HDL_CHOLESTEROL_max, na.rm=T))]    # 1:  NASH 60.57573    2: NAFLD 63.81205
Other[ , .(mean_LDL = mean(LDL_CHOLESTEROL_max, na.rm=T))]    # 1:  NASH 148.4634    2: NAFLD 147.1801
Other[ , .(mean_Fasting = mean(FASTING_BLOOD_SUGAR_max, na.rm=T))]    # 1:  NASH 115.4893    2: NAFLD 113.6343
Other[ , .(mean_Casual = mean(CASUAL_BLOOD_SUGAR_max, na.rm=T))]    # 1:  NASH 122.4167    2: NAFLD 124.2581
Other[ , .(mean_DRINKING_HABIT = mean(DRINKING_HABIT_max, na.rm=T))]    # 1:  NASH 2.589758    2: NAFLD 2.380795
Other[ , .(mean_AMOUNT_DRINKING = mean(AMOUNT_DRINKING_max, na.rm=T))]    # 1:  NASH 2.076975    2: NAFLD 2.303056



Other %>%
  ggplot(aes(AMOUNT_DRINKING_max , colour=Dx, fill=Dx)) +
  geom_density(alpha=0.7, show.legend = FALSE) +
  #xlim(50,250) +
  theme_minimal() +
  xlab("\n Drinking Amount Distribution (Historic Max per Patient) \n") + 
  ylab("Patient Density \n") +
  scale_fill_manual(values=c("#0081A7", "#B5838D")) +
  scale_colour_manual(values=c("#0081A7", "#B5838D")) 


# 



# Labs in NASH NAFLD and other 

ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups <- fread("ce18_Non-NASH-NAFLD_smplpts_All_annual_Checkups.txt")
ce18_NASH_NAFLD_pts_All_annual_Checkups <- fread("ce18_NASH-NAFLD_pts_All_annual_Checkups.txt")

match <- c("MEMBER_ID", "weight", "BMI", "ABDOMINAL_CIRCUMFERENCE", "SYSTOLIC_BP", "DIASTOLIC_BP", "TRIGLYCERIDE", 
           "HDL_CHOLESTEROL", "LDL_CHOLESTEROL", "AST", "ALT",  "G_GT", "FASTING_BLOOD_SUGAR", 
           "CASUAL_BLOOD_SUGAR", "HBA1C", "SERUM_URIC_ACID", "SERUM_CREATININE")

which_names <- which(names(ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups) %in%  match)
ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups <- ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups[, ..which_names]

which_names <- which(names(ce18_NASH_NAFLD_pts_All_annual_Checkups) %in%  match)
ce18_NASH_NAFLD_pts_All_annual_Checkups <- ce18_NASH_NAFLD_pts_All_annual_Checkups[, ..which_names]

Labs <- ce18_NASH_NAFLD_pts_All_annual_Checkups %>% bind_rows(ce18_Non_NASH_NAFLD_smplpts_All_annual_Checkups)


Labs = melt(Labs, id.vars = c("MEMBER_ID", "Dx"), 
                  measure.vars = c("BMI", "ABDOMINAL_CIRCUMFERENCE", "SYSTOLIC_BP", "DIASTOLIC_BP", "TRIGLYCERIDE", 
           "HDL_CHOLESTEROL", "LDL_CHOLESTEROL", "AST", "ALT",  "G_GT", "FASTING_BLOOD_SUGAR", 
           "CASUAL_BLOOD_SUGAR", "HBA1C", "SERUM_URIC_ACID", "SERUM_CREATININE"),  
                  variable.name = "test", value.name = "value")

Labs <- Labs %>% drop_na() %>% distinct() 
Labs <- Labs %>% group_by(MEMBER_ID, test) %>% summarise(value=max(value))

Dx_pats <- fread("NASH_NAFLD_Dx_pats.txt")
Labs <- Labs %>% left_join(Dx_pats)

Labs %>% group_by(Dx, test) %>% summarise(n=mean(value)) %>%
  spread(key=Dx, value=n)


Labs <- Labs %>% spread(key=test, value=value) %>% mutate(Dx=ifelse(is.na(Dx), "Other", Dx))
dim(Labs) # 6598125
sum(is.na(Labs)) # 0.09531541

missing.values <- Labs %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)


levels <-
    (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
      ggplot() +
        geom_bar(aes(x = reorder(key, desc(pct)), 
                     y = pct, fill=isna), 
                 stat = 'identity', alpha=0.8) +
      scale_x_discrete(limits = levels) +
      scale_fill_manual(name = "", 
                        values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
      coord_flip() +
      labs(title = "Percentage of missing values", x =
             'Variable', y = "% of missing values")


grid.arrange(percentage.plot, row.plot, ncol = 2)


LabsImputed <- imputePCA(Labs[,-c(1,2)],ncp=2, scale = T)

LabsImputed <- Labs %>% select(MEMBER_ID, Dx) %>% bind_cols(LabsImputed$completeObs)



LabsImputed %>% gather(test, value, BMI:SERUM_CREATININE) %>% ungroup() %>%
  mutate(value=ifelse(value<0,0,value)) %>%
  group_by(Dx, test) %>% summarise(n=mean(value)) %>%
  spread(key=Dx, value=n)
 


LabsImputed <- LabsImputed %>% gather(test, value, BMI:SERUM_CREATININE) %>% ungroup() %>%
  mutate(value=ifelse(value<0,0,value))

fwrite(LabsImputed, "LabsImputed.txt", sep="\t")

LabsImputed %>%
  filter(test=="TRIGLYCERIDE") %>%
   ggplot(aes(value, colour=Dx, fill=Dx)) +
  geom_density(alpha=0.7) +
  facet_wrap(~test, scales = "free_x") +
  xlim(25,500) +
  theme_minimal() +
  xlab("\n Result Distribution (Historic Max per Patient) \n") + 
  ylab("Patient Density \n") +
  scale_fill_manual(values=c("#0b1251", "#b60c2b", "#e6bb0f")) +
  scale_colour_manual(values=c("#0b1251", "#b60c2b", "#e6bb0f")) 


# -------------------------

# Segment "Other JP Population" based on lab scores -----------------------------

LabsImputed <- fread("LabsImputed.txt", sep="\t")
LabsImputed <- LabsImputed %>% mutate(Dx=ifelse(Dx=="Other",0,1))
LabsImputed <- LabsImputed %>% spread(key=test, value=value)
sum(is.na(LabsImputed))


# library(psych)
# library(factoextra)
# 
# sample <- LabsImputed %>% group_by(Dx) %>% sample_n(100)
# 
# pairs.panels(sample[,-c(1,2)],
#              gap = 0,
#              bg = c("#e6bb0f", "#b60c2b")[as.factor(sample$Dx) ],
#              pch=21,
#              hist.col="#0b1251",
#              alpha=0.3)
# 
# 
# pc <- prcomp(LabsImputed[,-c(1,2)],
#              center = TRUE,
#             scale. = TRUE)
# 
# attributes(pc)
# pc$rotation
# print(pc)
# summary(pc)



# 
# fviz_eig(pc)
# 
# pairs.panels(pc$x[1:1000,],
#              gap=0,
#             # bg = c("#e6bb0f", "#b60c2b")[as.factor(sample$Dx) ],
#              pch=21,
#              hist.col="#0b1251",
#              alpha=0.3)
# 
# 
# pc$x[,1:2] %>% bind_cols(LabsImputed %>% select(Dx)) %>%
#   sample_n(1000) %>%
#   ggplot(aes(PC1, PC2, colour=Dx, value=Dx)) +
#   geom_point()


test <- LabsImputed
test <- test[ , -c(1)]
test <- test %>% group_by(Dx) %>% sample_n(20000)

modelAll_1_rf <- randomForest(Dx ~ ., data = test)
summary(modelAll_1_rf)
data.frame(modelAll_1_rf$importance) %>% arrange(IncNodePurity)


to_pred <- LabsImputed %>% select(-MEMBER_ID) %>% group_by(Dx) %>% sample_n(20000)

predict <- predict(modelAll_1_rf, to_pred, type = 'response')

ignore <- to_pred %>% bind_cols(data.frame(predict))

unique(ignore$Dx)

ignore %>% mutate(Dx=as.factor(Dx)) %>%
  mutate(Dx=ifelse(Dx==1,"NASH/NAFLD Dx", "Other JP Pop")) %>%
  ggplot((aes(predict, colour=Dx, fill=Dx))) +
  geom_density(alpha=0.7) +
  scale_fill_manual(values = c("brown3", "azure4")) +
    scale_colour_manual(values = c("brown3", "azure4")) +
  theme_minimal() +
  xlab("\n Predicted Probability / Propensity Score") +
  ylab("Patient (kernel) density \n")




ignore %>% filter(Dx==0) %>%
  mutate(Dx=as.factor(Dx)) %>%
  filter(predict>=0.6)



ignore %>% mutate(Dx=as.factor(Dx)) %>%
  mutate(Dx=ifelse(Dx==1,"NASH/NAFLD Dx", "Other JP Pop")) %>%
  mutate(predict=ifelse(predict>=0.6, 1,0)) %>%
  group_by(Dx, predict) %>% count()

ignore %>% mutate(Dx=as.factor(Dx)) %>%
  filter(Dx==0) %>%
  mutate(Dx=ifelse(Dx==1,"NASH/NAFLD Dx", "Other JP Pop")) %>%
  mutate(predict=ifelse(predict>=0.6, 1,0)) %>%
      gather(test, value, ABDOMINAL_CIRCUMFERENCE:TRIGLYCERIDE) %>%
  group_by(predict, test) %>% summarise(n=mean(value)) %>%
  spread(key=predict, value=n)
  


ignore %>% 
    mutate(Dx=ifelse(Dx==1,"NASH/NAFLD Dx", "Other JP Pop")) %>%
  ggplot(aes(predict, SERUM_URIC_ACID, colour=as.factor(Dx), fill=as.factor(Dx))) +
  geom_smooth() +
    scale_fill_manual(values = c("brown3", "azure4")) +
    scale_colour_manual(values = c("brown3", "azure4")) +
  theme_minimal() +
  xlab("\n Predicted Probability / Propensity Score") +
  ylab("Serum Uric Acid \n")

fwrite(ignore, "RandomForestScores_Predictiojs.txt", sep="\t")


# ---------------------------------
# NASH vs NAFLD within random sample --------------------------------

RandomForestScores_Predictiojs <- fread("RandomForestScores_Predictiojs.txt", sep="\t")

RandomForestScores_Predictiojs %>% 
    mutate(Dx=ifelse(Dx==1,"NASH/NAFLD Dx", "Other JP Pop")) %>%
  ggplot(aes(predict, colour=as.factor(Dx), fill=as.factor(Dx))) +
  geom_density(alpha=0.8) +
    scale_fill_manual(values = c("brown3", "azure4")) +
    scale_colour_manual(values = c("brown3", "azure4")) +
  theme_minimal() +
  xlab("\n Predicted Probability / Propensity Score") +
  ylab("Patient density\n")


RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>%
  filter(Dx==0) %>%
  arrange(predict) 


RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% select(predict) %>% 
  filter(predict>0.6)

RandomForestScores_Predictiojs %>%
  mutate(group=ifelse(predict>=0.8390667, "NASH_Pred", "NAFLD_Pred")) %>%
   ggplot(aes(predict, colour=as.factor(group), fill=as.factor(group))) +
  geom_density(aes(y=after_stat(scaled)), alpha=0.8) +
    scale_fill_manual(values = c("#3f7eca", "#01295a")) +
    scale_colour_manual(values = c("#3f7eca", "#01295a")) +
  theme_minimal() +
  xlab("\n Predicted Probability / Propensity Score") +
  ylab("Patient density\n")


# ------------
# Drug Usage before/after 1st NASH Dx --------------------------------------------------

Lookup <- data.frame(seq(as.Date("2016-10-01"), by = "month", length.out = 88))
names(Lookup)[1] <- "Exact"
Lookup$Exact <- str_sub(Lookup$Exact, 1L, 7L)
Lookup <- distinct(Lookup)
Lookup$ID <- row_number(Lookup) # We've used 15-74 which is 2017-12 to 2022-11

Lookup <- Lookup %>% filter(ID>=15 & ID <=74)

Lookup$ID <- Lookup$ID - 14

ce18_NASH_NAFLD_pts_Alldiags <- fread("ce18_NASH-NAFLD_pts_Alldiags.txt")
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% distinct() %>% filter(grepl("8843497", STANDARD_DISEASE_CODE))
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% group_by(MEMBER_ID) %>% filter(MONTH_AND_YEAR_OF_MEDICAL_CARE==min(MONTH_AND_YEAR_OF_MEDICAL_CARE))
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% select(MEMBER_ID, MONTH_AND_YEAR_OF_MEDICAL_CARE) %>% distinct() 
ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE <- as.character(ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE)


ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE <- paste0(
  paste0( str_sub(ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE, 1L, 4L), "-"),
          str_sub(ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE, 5L, 6L)
  )

ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% left_join(Lookup, by=c("MONTH_AND_YEAR_OF_MEDICAL_CARE"="Exact"))
names(ce18_NASH_NAFLD_pts_Alldiags)[3] <- "First_NASH"



Drugs_Lookup_IDs <- fread("Drugs_Lookup_IDs.csv")
Drugs_Lookup_IDs <- unique(Drugs_Lookup_IDs[, .(drug, group, class)])[order(class, group, drug)][, drug_id := .I][, .(group, class, drug, drug_id)]

temp2_gap62 <- fread("NASH_NAFLD_Drug_Histories_G62.txt", sep="\t")
over_time <- gather(temp2_gap62, Month, Drugs, month1:month60, factor_key=TRUE)
over_time <- over_time %>% filter(Drugs!="-") %>% select(MEMBER_ID, Dx, weight, Month, Drugs) %>% distinct()
over_time <- separate_rows(over_time, Drugs, sep = ",", convert=T)
over_time <- over_time %>% left_join(Drugs_Lookup_IDs %>% select(group, class, drug_id), by=c("Drugs"="drug_id")) %>%
  select(-Drugs) %>% distinct()

over_time$Month <- parse_number(as.character(over_time$Month))

over_time <- over_time %>% inner_join(ce18_NASH_NAFLD_pts_Alldiags %>% select(-MONTH_AND_YEAR_OF_MEDICAL_CARE)) %>% drop_na()

over_time <- over_time %>% mutate(Lapsed=Month-First_NASH) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

over_time <- over_time %>% select(MEMBER_ID, Month) %>% distinct() %>% group_by(MEMBER_ID) %>% count() %>% filter(n>=25) %>%
  select(MEMBER_ID) %>% left_join(over_time)

over_time %>% select(MEMBER_ID, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 38140


over_time %>% select(MEMBER_ID, weight, group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


over_time %>% select(MEMBER_ID, weight, group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(group=ifelse(is.na(group),"Lapsed",group)) %>%
  filter(group!="Lapsed") %>%
  mutate(group=factor(group, levels=c("Biguanide", "Glinide", "Glitazone", "Glucosidase inhibitor", 
                                      "Sulfonylurea", "DPP4", "SGLT2", "Insulin", "GLP1", "Hepatoprotective",
                                      "Fibrate", "Ion-exchange resin", "Other", "PCSK9", "Statin"))) %>%
  rename("Drug Group"="group") %>%
  mutate(n=n/38140) %>%
  ggplot(aes(Lapsed,n, colour=`Drug Group`)) +
  geom_line(size=2, alpha=.8) +
  theme_minimal() +
  scale_colour_manual(values=c("#fad6a3","#f5ad47","#ff9933","#cfccc9", "#663e05", "#360566","#660557","#660505",
                                        "#154904","#0c5fe4","#649cf7","#c5dafc", "#05265c","#020e22")) +
  scale_y_continuous(labels = scales::percent)+
 # ylim(0,65) +
  xlab("\n No. Elapsed Months \n(Before/After 1st NASH Dx)") +
  ylab("Population % \n")



over_time %>% select(MEMBER_ID, weight, group) %>% distinct()  %>%
  group_by(group) %>% summarise(n=sum(as.numeric(weight)))



over_time %>% select(MEMBER_ID, weight, group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(group=ifelse(is.na(group),"Lapsed",group)) %>%
    filter(group!="Lapsed") %>%
  mutate(group=factor(group, levels=c("Biguanide", "Glinide", "Glitazone", "Glucosidase inhibitor", 
                                      "Sulfonylurea", "DPP4", "SGLT2", "Insulin", "GLP1", "Hepatoprotective",
                                      "Fibrate", "Ion-exchange resin", "Other", "PCSK9", "Statin"))) %>%
    mutate(n=ifelse(group=="Biguanide", n/15330.,
                    ifelse(group=="Glinide", n/1481.,
                           ifelse(group=="Glitazone",n/3139.,
                                  ifelse(group=="Glucosidase inhibitor",n/2653.,
                                         ifelse(group=="Sulfonylurea", n/3193,
                                                ifelse(group=="DPP4", n/16139.,
                                                       ifelse(group=="SGLT2", n/12725.,
                                                              ifelse(group=="Insulin", n/3825.,
                                                                     ifelse(group=="GLP1", n/3738.,
                                                                            ifelse(group=="Hepatoprotective",n/5681.,
                                                                                   ifelse(group=="Fibrate",n/5568.,
                                                                                          ifelse(group=="Ion-exchange resin", n/128.,
                                                                                                 ifelse(group=="Other", n/9781.,
                                                                                                        ifelse(group=="PCSK9", n/20.9,
                                                                                                               ifelse(group=="Statin", n/26276., NA)))))))))))))))) %>%
      rename("Drug Group"="group") %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Group`)) +
  geom_line(size=2, alpha=0.8) +
  theme_minimal() +
    scale_colour_manual(values=c("#fad6a3","#f5ad47","#ff9933","#cfccc9", "#f2a6b0", "#663e05", "#360566","#660557","#660505",
                                        "#154904","#0c5fe4","#649cf7","#c5dafc", "#05265c","#020e22")) +
  xlab("\n No. Elapsed Months \n(Before/After 1st NASH Dx)") +
  ylab("Population % (of Class-experienced) \n")


# -------------------------------------------
# Drug Usage before/after 1st NAFLD Dx --------------------------------------------------

Lookup <- data.frame(seq(as.Date("2016-10-01"), by = "month", length.out = 88))
names(Lookup)[1] <- "Exact"
Lookup$Exact <- str_sub(Lookup$Exact, 1L, 7L)
Lookup <- distinct(Lookup)
Lookup$ID <- row_number(Lookup) # We've used 15-74 which is 2017-12 to 2022-11

Lookup <- Lookup %>% filter(ID>=15 & ID <=74)

Lookup$ID <- Lookup$ID - 14

ce18_NASH_NAFLD_pts_Alldiags <- fread("ce18_NASH-NAFLD_pts_Alldiags.txt")
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% distinct() %>% filter(grepl("8850319", STANDARD_DISEASE_CODE)|
                                                                                         grepl("8850318", STANDARD_DISEASE_CODE)|
                                                                                         grepl("5718008", STANDARD_DISEASE_CODE))
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% group_by(MEMBER_ID) %>% filter(MONTH_AND_YEAR_OF_MEDICAL_CARE==min(MONTH_AND_YEAR_OF_MEDICAL_CARE))
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% select(MEMBER_ID, MONTH_AND_YEAR_OF_MEDICAL_CARE) %>% distinct() 
ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE <- as.character(ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE)


ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE <- paste0(
  paste0( str_sub(ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE, 1L, 4L), "-"),
          str_sub(ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE, 5L, 6L)
  )

ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% left_join(Lookup, by=c("MONTH_AND_YEAR_OF_MEDICAL_CARE"="Exact"))
names(ce18_NASH_NAFLD_pts_Alldiags)[3] <- "First_NAFLD"



Drugs_Lookup_IDs <- fread("Drugs_Lookup_IDs.csv")
Drugs_Lookup_IDs <- unique(Drugs_Lookup_IDs[, .(drug, group, class)])[order(class, group, drug)][, drug_id := .I][, .(group, class, drug, drug_id)]

temp2_gap62 <- fread("NASH_NAFLD_Drug_Histories_G62.txt", sep="\t")
over_time <- gather(temp2_gap62, Month, Drugs, month1:month60, factor_key=TRUE)
over_time <- over_time %>% filter(Drugs!="-") %>% select(MEMBER_ID, Dx, weight, Month, Drugs) %>% distinct()
over_time <- separate_rows(over_time, Drugs, sep = ",", convert=T)
over_time <- over_time %>% left_join(Drugs_Lookup_IDs %>% select(group, class, drug_id), by=c("Drugs"="drug_id")) %>%
  select(-Drugs) %>% distinct()

over_time$Month <- parse_number(as.character(over_time$Month))

over_time <- over_time %>% inner_join(ce18_NASH_NAFLD_pts_Alldiags %>% select(-MONTH_AND_YEAR_OF_MEDICAL_CARE)) %>% drop_na()

over_time <- over_time %>% mutate(Lapsed=Month-First_NAFLD) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

over_time <- over_time %>% filter(Dx=="NAFLD") %>%
  select(MEMBER_ID, Month) %>% distinct() %>% group_by(MEMBER_ID) %>% count() %>% filter(n>=25) %>%
  select(MEMBER_ID) %>% left_join(over_time)

over_time %>% select(MEMBER_ID, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 703589


over_time %>% select(MEMBER_ID, weight, group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


over_time %>% select(MEMBER_ID, weight, group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(group=ifelse(is.na(group),"Lapsed",group)) %>%
  filter(group!="Lapsed") %>%
  mutate(group=factor(group, levels=c("Biguanide", "Glinide", "Glitazone", "Glucosidase inhibitor", 
                                      "Sulfonylurea", "DPP4", "SGLT2", "Insulin", "GLP1", "Hepatoprotective",
                                      "Fibrate", "Ion-exchange resin", "Other", "PCSK9", "Statin"))) %>%
  rename("Drug Group"="group") %>%
  mutate(n=n/703589) %>%
  ggplot(aes(Lapsed,n, colour=`Drug Group`)) +
  geom_line(size=2, alpha=.8) +
  theme_minimal() +
  scale_colour_manual(values=c("#fad6a3","#f5ad47","#ff9933","#cfccc9", "#f2a6b0", "#663e05", "#360566","#660557","#660505",
                                        "#154904","#0c5fe4","#649cf7","#c5dafc", "#05265c","#020e22")) +
  scale_y_continuous(labels = scales::percent)+
 # ylim(0,65) +
  xlab("\n No. Elapsed Months \n(Before/After 1st NAFLD Dx)") +
  ylab("Population % \n")

# -----------------------------

# GLP1 among NASH/NAFLD with or without T2D -----------------------------

Drugs_Lookup_IDs <- fread("Drugs_Lookup_IDs.csv")
Drugs_Lookup_IDs <- unique(Drugs_Lookup_IDs[, .(drug, group, class)])[order(class, group, drug)][, drug_id := .I][, .(group, class, drug, drug_id)]

temp2_gap62 <- fread("NASH_NAFLD_Drug_Histories_G62.txt", sep="\t")
over_time <- gather(temp2_gap62, Month, Drugs, month1:month60, factor_key=TRUE)
over_time <- over_time %>% filter(Drugs!="-") %>% select(MEMBER_ID, Dx, weight, Month, Drugs) %>% distinct()
over_time <- separate_rows(over_time, Drugs, sep = ",", convert=T)
over_time <- over_time %>% left_join(Drugs_Lookup_IDs %>% select(group, class, drug_id), by=c("Drugs"="drug_id")) %>%
  select(-Drugs) %>% distinct()

over_time <- over_time %>% select(-c(Dx, Month)) %>% distinct()



GLP1 <- over_time %>% filter(group=="GLP1") %>% select(MEMBER_ID, weight) %>% distinct()
GLP1$GLP1 <-  "GLP1"
OtherAntidia <- over_time %>% filter(group!="GLP1"&class=="Antidiabetic") %>% select(MEMBER_ID, weight) %>% distinct()
OtherAntidia$OtherAntidia <-  "OtherAntidia"





ce18_NASH_NAFLD_pts_Alldiags <- fread("ce18_NASH-NAFLD_pts_Alldiags.txt")
length(unique(ce18_NASH_NAFLD_pts_Alldiags$STANDARD_DISEASE_NAME))
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[,c("MEMBER_ID", "STANDARD_DISEASE_CODE", "STANDARD_DISEASE_NAME")]

Diagnosis_master <- fread("Diagnosis_master.csv")
Diagnosis_master <- Diagnosis_master[,c("standard_disease_code", "icd10_level4_code", "icd10_level4_name")]
names(Diagnosis_master)[1] <- "STANDARD_DISEASE_CODE"

ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[Diagnosis_master, on = .(STANDARD_DISEASE_CODE), nomatch = NULL]
ce18_NASH_NAFLD_pts_Alldiags <- distinct(ce18_NASH_NAFLD_pts_Alldiags[, -c("STANDARD_DISEASE_CODE", "STANDARD_DISEASE_NAME")])

#E11, O24, E08, E09, E13, E10
#E65, E66

T2D_Dx <- distinct(ce18_NASH_NAFLD_pts_Alldiags[grepl("E11", icd10_level4_code), c("MEMBER_ID")]) 
T2D_Dx$group <- "T2D"
JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt")
T2D_Dx <- T2D_Dx %>% left_join(JMDC_cePts_18plus %>% select(MEMBER_ID, weight))

Dx_pats <- fread("NASH_NAFLD_Dx_pats.txt")
Dx_pats <- Dx_pats %>% left_join(JMDC_cePts_18plus %>% select(MEMBER_ID, weight))

Dx_pats  %>% left_join(T2D_Dx) %>% left_join(OtherAntidia) %>% left_join(GLP1) %>%
  mutate(group=ifelse(OtherAntidia=="OtherAntidia", "T2D", group)) %>%
  group_by(Dx, group, GLP1) %>% summarise(n=sum(weight))


# --------------------------------

# Estimate FIB4 among NASH/NAFLD patients ---------------

LabsImputed <- fread("LabsImputed.txt", sep="\t")
LabsImputed <- LabsImputed %>% mutate(Dx=ifelse(Dx=="Other",0,1))
LabsImputed <- LabsImputed %>% spread(key=test, value=value)
sum(is.na(LabsImputed))


RandomForestScores_Predictions <- fread("RandomForestScores_Predictiojs.txt", sep="\t")

RandomForestScores_Predictions <- RandomForestScores_Predictions %>% left_join(LabsImputed)

RandomForestScores_Predictions <- RandomForestScores_Predictions %>% arrange(predict)
RandomForestScores_Predictions <- RandomForestScores_Predictions %>% filter(Dx==1)

RandomForestScores_Predictions <- RandomForestScores_Predictions %>% mutate(ID=row_number())

RandomForestScores_Predictions <- RandomForestScores_Predictions %>% mutate(Stage=ifelse(ID>=18400, "Cirrhosis",
                                                       ifelse(ID>=14000, "Fibrosis", "NASH-only")))




Cutoffs <- RandomForestScores_Predictions %>% group_by(Stage) %>% filter(predict==max(predict)) 
Cutoffs$predict

RandomForestScores_Predictions %>%
  ggplot(aes(predict, colour=Stage)) +
  geom_density()


RandomForestScores_Predictions <- fread("RandomForestScores_Predictiojs.txt", sep="\t")

RandomForestScores_Predictions <- RandomForestScores_Predictions %>% left_join(LabsImputed)

RandomForestScores_Predictions <- RandomForestScores_Predictions %>% arrange(predict)

RandomForestScores_Predictions <- RandomForestScores_Predictions %>% mutate(Stage=ifelse(predict<=0.7890333 , "NASH-only", 
                                                       ifelse(predict<=0.8967333 , "NASH-Fibrosis", "NASH-Cirrhosis")))

RandomForestScores_Predictions <- RandomForestScores_Predictions <- RandomForestScores_Predictions %>% mutate(HighRisk=ifelse(predict>0.6, "HighRisk", "LowRisk"))

RandomForestScores_Predictions <- RandomForestScores_Predictions %>%mutate(NASNAFLDgroup=ifelse(predict>=0.8390667, "NASH_Pred", "NAFLD_Pred")) 

RandomForestScores_Predictions %>% filter(Dx==1) %>% group_by(HighRisk, NASNAFLDgroup, Stage) %>% count()

RandomForestScores_Predictions %>% filter(Dx==1) %>% group_by(Dx, HighRisk, NASNAFLDgroup, Stage) %>% summarise(n=mean(ALT))

RandomForestScores_Predictions %>% filter(Dx==1) %>% group_by(Dx, HighRisk, NASNAFLDgroup, Stage) %>% summarise(n=mean(ALT))


RandomForestScores_Predictions %>% filter(HighRisk=="HighRisk") %>% group_by(NASNAFLDgroup, Stage) %>% count()




RandomForestScores_Predictions %>% filter(Dx==1) %>% 
  gather(test, value, ABDOMINAL_CIRCUMFERENCE:TRIGLYCERIDE) %>%
  group_by(Stage, test) %>% summarise(n=mean(value)) %>%
  spread(key=Stage, value=n)
  


RandomForestScores_Predictions %>%
  filter(NASNAFLDgroup=="NASH_Pred"|Dx==1) %>%
   ggplot(aes(predict, colour=as.factor(Stage), fill=as.factor(Stage))) +
  geom_density(aes(y=after_stat(scaled)), alpha=0.8) +
    scale_fill_manual(values = c("#59295a", "#01295a", "#3f7eca")) +
    scale_colour_manual(values = c("#59295a", "#01295a","#3f7eca")) +
  theme_minimal() +
  xlab("\n Predicted Probability / Propensity Score") +
  ylab("Patient density\n")

RandomForestScores_Predictions <- RandomForestScores_Predictions %>% select(MEMBER_ID, predict, Stage, HighRisk, NASNAFLDgroup)





# ----------------
# Drug usage based on risk groups ------------------------

RandomForestScores_Predictions
JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt")
RandomForestScores_Predictions <- merge(x = RandomForestScores_Predictions, y = JMDC_cePts_18plus[ , c("MEMBER_ID", "weight")], by = "MEMBER_ID", all.x=TRUE)

Drugs_Lookup_IDs <- fread("Drugs_Lookup_IDs.csv")
Drugs_Lookup_IDs <- unique(Drugs_Lookup_IDs[, .(drug, group, class)])[order(class, group, drug)][, drug_id := .I][, .(group, class, drug, drug_id)]

temp2_gap62 <- fread("NASH_NAFLD_Drug_Histories_G62.txt", sep="\t")

over_time <- gather(temp2_gap62, Month, Drugs, month1:month60, factor_key=TRUE)
over_time <- over_time %>% filter(Drugs!="-") %>% select(MEMBER_ID, Dx, weight, Drugs) %>% distinct()
over_time <- separate_rows(over_time, Drugs, sep = ",", convert=T)

over_time <- over_time %>% left_join(Drugs_Lookup_IDs %>% select(group, class, drug_id), by=c("Drugs"="drug_id")) %>%
  select(-Drugs) %>% distinct()

RandomForestScores_Predictions <- over_time %>% select(MEMBER_ID) %>% distinct() %>% inner_join(RandomForestScores_Predictions)

RandomForestScores_Predictions %>% group_by(HighRisk) %>% summarise(n=sum(weight))

RandomForestScores_Predictions %>% left_join(over_time) %>%
  group_by(HighRisk, class, group) %>% summarise(n=sum(weight)) %>%
  mutate(n=ifelse(HighRisk=="HighRisk", n/282930., n/158611.)) %>%
  spread(key=HighRisk, value=n)



RandomForestScores_Predictions %>% group_by(NASNAFLDgroup) %>% summarise(n=sum(weight))

RandomForestScores_Predictions %>% left_join(over_time) %>%
  group_by(NASNAFLDgroup, class, group) %>% summarise(n=sum(weight)) %>%
  mutate(n=ifelse(NASNAFLDgroup=="NASH_Pred", n/84252., n/357289.)) %>%
  spread(key=NASNAFLDgroup, value=n)




RandomForestScores_Predictions %>% group_by(Stage) %>% summarise(n=sum(weight))

RandomForestScores_Predictions %>% left_join(over_time) %>%
  group_by(Stage, class, group) %>% summarise(n=sum(weight)) %>%
  mutate(n=ifelse(Stage=="NASH-Cirrhosis", n/33176.,
                  ifelse(Stage=="NASH-Fibrosis", n/98044., n/310321.))) %>%
  spread(key=Stage, value=n)

# -----------------------
# Drug Usage before/after 1st T2Dm / Dyslipidemia Dx -------------------

Lookup <- data.frame(seq(as.Date("2016-10-01"), by = "month", length.out = 88))
names(Lookup)[1] <- "Exact"
Lookup$Exact <- str_sub(Lookup$Exact, 1L, 7L)
Lookup <- distinct(Lookup)
Lookup$ID <- row_number(Lookup) # We've used 15-74 which is 2017-12 to 2022-11

Lookup <- Lookup %>% filter(ID>=15 & ID <=74)

Lookup$ID <- Lookup$ID - 14

ce18_NASH_NAFLD_pts_Alldiags <- fread("ce18_NASH-NAFLD_pts_Alldiags.txt")

Diagnosis_master <- fread("Diagnosis_master.csv")
Diagnosis_master <- Diagnosis_master[,c("standard_disease_code", "icd10_level4_code", "icd10_level4_name")]
names(Diagnosis_master)[1] <- "STANDARD_DISEASE_CODE"

ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[Diagnosis_master, on = .(STANDARD_DISEASE_CODE), nomatch = NULL]

ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% distinct() %>% filter(grepl("E78", icd10_level4_code))
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% group_by(MEMBER_ID) %>% filter(MONTH_AND_YEAR_OF_MEDICAL_CARE==min(MONTH_AND_YEAR_OF_MEDICAL_CARE))
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% select(MEMBER_ID, MONTH_AND_YEAR_OF_MEDICAL_CARE) %>% distinct() 
ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE <- as.character(ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE)


ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE <- paste0(
  paste0( str_sub(ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE, 1L, 4L), "-"),
          str_sub(ce18_NASH_NAFLD_pts_Alldiags$MONTH_AND_YEAR_OF_MEDICAL_CARE, 5L, 6L)
  )

ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags %>% left_join(Lookup, by=c("MONTH_AND_YEAR_OF_MEDICAL_CARE"="Exact"))
names(ce18_NASH_NAFLD_pts_Alldiags)[3] <- "First_T2DM"
ce18_NASH_NAFLD_pts_Alldiags <- na.omit(ce18_NASH_NAFLD_pts_Alldiags)


Drugs_Lookup_IDs <- fread("Drugs_Lookup_IDs.csv")
Drugs_Lookup_IDs <- unique(Drugs_Lookup_IDs[, .(drug, group, class)])[order(class, group, drug)][, drug_id := .I][, .(group, class, drug, drug_id)]

temp2_gap62 <- fread("NASH_NAFLD_Drug_Histories_G62.txt", sep="\t")
over_time <- gather(temp2_gap62, Month, Drugs, month1:month60, factor_key=TRUE)
over_time <- over_time %>% filter(Drugs!="-") %>% select(MEMBER_ID, Dx, weight, Month, Drugs) %>% distinct()
over_time <- separate_rows(over_time, Drugs, sep = ",", convert=T)
over_time <- over_time %>% left_join(Drugs_Lookup_IDs %>% select(group, class, drug_id), by=c("Drugs"="drug_id")) %>%
  select(-Drugs) %>% distinct()

over_time$Month <- parse_number(as.character(over_time$Month))

over_time <- over_time %>% inner_join(ce18_NASH_NAFLD_pts_Alldiags %>% select(-MONTH_AND_YEAR_OF_MEDICAL_CARE)) %>% drop_na()

over_time <- over_time %>% mutate(Lapsed=Month-First_T2DM) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

over_time <- over_time %>% select(MEMBER_ID, Month) %>% distinct() %>% group_by(MEMBER_ID) %>% count() %>% filter(n>=25) %>%
  select(MEMBER_ID) %>% left_join(over_time)

over_time %>% select(MEMBER_ID, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 65540


over_time %>% select(MEMBER_ID, weight, group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


over_time %>% select(MEMBER_ID, weight, group, Lapsed) %>% distinct() %>%
  group_by(Lapsed, group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(group=ifelse(is.na(group),"Lapsed",group)) %>%
  filter(group!="Lapsed") %>%
  mutate(group=factor(group, levels=c("Biguanide", "Glinide", "Glitazone", "Glucosidase inhibitor", 
                                      "DPP4", "SGLT2", "Insulin", "GLP1", "Hepatoprotective",
                                      "Fibrate", "Ion-exchange resin", "Other", "PCSK9", "Statin"))) %>%
  rename("Drug Group"="group") %>%
  mutate(n=n/65540) %>%
  ggplot(aes(Lapsed,n, colour=`Drug Group`)) +
  geom_line(size=2, alpha=.8) +
  theme_minimal() +
  scale_colour_manual(values=c("#fad6a3","#f5ad47","#ff9933","#cfccc9", "#663e05", "#360566","#660557","#660505",
                                        "#154904","#0c5fe4","#649cf7","#c5dafc", "#05265c","#020e22")) +
  scale_y_continuous(labels = scales::percent)+
 # ylim(0,65) +
  xlab("\n No. Elapsed Months \n(Before/After 1st Dyslipidemia Dx)") +
  ylab("Population % \n")

# --------------


# Dxs prior to NASH onset ------------------------------

Diagnosis_master <- fread("Diagnosis_master.csv")
Diagnosis_master <- Diagnosis_master[,c("standard_disease_code", "icd10_level4_code")]
names(Diagnosis_master)[1] <- "STANDARD_DISEASE_CODE"

ce18_NASH_NAFLD_pts_Alldiags <- fread("ce18_NASH-NAFLD_pts_Alldiags.txt")
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[,c("MEMBER_ID", "MONTH_AND_YEAR_OF_MEDICAL_CARE", "STANDARD_DISEASE_CODE", "STANDARD_DISEASE_NAME")]

First_NASH <- ce18_NASH_NAFLD_pts_Alldiags %>% filter(STANDARD_DISEASE_CODE=="8843497") %>% group_by(MEMBER_ID) %>% filter(MONTH_AND_YEAR_OF_MEDICAL_CARE==min(MONTH_AND_YEAR_OF_MEDICAL_CARE))
First_NASH <- First_NASH %>% select(MEMBER_ID, MONTH_AND_YEAR_OF_MEDICAL_CARE)
names(First_NASH)[2] <- "First"

First_NASH <- First_NASH %>% left_join(ce18_NASH_NAFLD_pts_Alldiags) %>% filter(MONTH_AND_YEAR_OF_MEDICAL_CARE<=First)
First_NASH <- First_NASH %>% ungroup()  

setDT(First_NASH)
First_NASH <- First_NASH[Diagnosis_master, on = .(STANDARD_DISEASE_CODE), nomatch = NULL]
First_NASH <- distinct(First_NASH[, -c("STANDARD_DISEASE_CODE")])
First_NASH <- First_NASH %>% distinct()


Lookup <- data.frame(seq(as.Date("2016-10-01"), by = "month", length.out = 88))
names(Lookup)[1] <- "Exact"
Lookup$Exact <- str_sub(Lookup$Exact, 1L, 7L)
Lookup <- distinct(Lookup)
Lookup$ID <- row_number(Lookup) # We've used 15-74 which is 2017-12 to 2022-11
Lookup <- Lookup %>% filter(ID>=15 & ID <=74)
Lookup$ID <- Lookup$ID - 14

First_NASH <- First_NASH %>% filter(First>=201911)

JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt", colClasses = "character")

First_NASH <- First_NASH %>% left_join(JMDC_cePts_18plus %>% select(MEMBER_ID, weight)) 

First_NASH %>% select(MEMBER_ID, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))

data.frame(First_NASH %>% select(MEMBER_ID, icd10_level4_code, STANDARD_DISEASE_NAME, weight) %>% distinct() %>%
  group_by(icd10_level4_code, STANDARD_DISEASE_NAME) %>% summarise(n=sum(as.numeric(weight))/100121.8) %>%
  arrange(-n))


First_NASH %>% filter(grepl("C22", icd10_level4_code)|grepl("Z8505", icd10_level4_code)) %>% select(MEMBER_ID, weight) %>% 
  distinct() %>% summarise(n=sum(as.numeric(weight))/100121.8)



First_NASH %>% filter(grepl("B15", icd10_level4_code)|grepl("B16", icd10_level4_code)|grepl("B17", icd10_level4_code)|grepl("B18", icd10_level4_code)|grepl("B19", icd10_level4_code)) %>% select(MEMBER_ID, weight) %>% 
  distinct() %>% summarise(n=sum(as.numeric(weight))/100121.8)

# -----------------

# Distance betweem Dxs and NASH ---------------------
Diagnosis_master <- fread("Diagnosis_master.csv")
Diagnosis_master <- Diagnosis_master[,c("standard_disease_code", "icd10_level4_code")]
names(Diagnosis_master)[1] <- "STANDARD_DISEASE_CODE"

ce18_NASH_NAFLD_pts_Alldiags <- fread("ce18_NASH-NAFLD_pts_Alldiags.txt")
names(ce18_NASH_NAFLD_pts_Alldiags)
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[,c("MEMBER_ID", "DATE_OF_MEDICAL_CARE_START", "STANDARD_DISEASE_CODE", "STANDARD_DISEASE_NAME")]

First_NASH <- ce18_NASH_NAFLD_pts_Alldiags %>% filter(STANDARD_DISEASE_CODE=="8843497") %>% group_by(MEMBER_ID) %>% filter(DATE_OF_MEDICAL_CARE_START==min(DATE_OF_MEDICAL_CARE_START))
First_NASH <- First_NASH %>% select(MEMBER_ID, DATE_OF_MEDICAL_CARE_START)
names(First_NASH)[2] <- "First"


First_NASH$First <- paste0(
  paste0( str_sub(First_NASH$First, 1L, 4L), "-"),
          paste0(
            paste0(  str_sub(First_NASH$First, 5L, 6L), "-") , str_sub(First_NASH$First, 7L, 8L)
          )
  )

First_NASH$First <- as.Date(First_NASH$First)

First_NASH <- First_NASH %>% left_join(ce18_NASH_NAFLD_pts_Alldiags) 

First_NASH$DATE_OF_MEDICAL_CARE_START <- paste0(
  paste0( str_sub(First_NASH$DATE_OF_MEDICAL_CARE_START, 1L, 4L), "-"),
          paste0(
            paste0(  str_sub(First_NASH$DATE_OF_MEDICAL_CARE_START, 5L, 6L), "-") , str_sub(First_NASH$DATE_OF_MEDICAL_CARE_START, 7L, 8L)
          )
  )

First_NASH$DATE_OF_MEDICAL_CARE_START <- as.Date(First_NASH$DATE_OF_MEDICAL_CARE_START)

First_NASH <-  First_NASH %>% filter(DATE_OF_MEDICAL_CARE_START<=First)

First_NASH <- First_NASH %>% ungroup()  

setDT(First_NASH)
First_NASH <- First_NASH[Diagnosis_master, on = .(STANDARD_DISEASE_CODE), nomatch = NULL]
First_NASH <- distinct(First_NASH[, -c("STANDARD_DISEASE_CODE")])
First_NASH <- First_NASH %>% distinct()


Lookup <- data.frame(seq(as.Date("2016-10-01"), by = "month", length.out = 88))
names(Lookup)[1] <- "Exact"
Lookup$Exact <- str_sub(Lookup$Exact, 1L, 7L)
Lookup <- distinct(Lookup)
Lookup$ID <- row_number(Lookup) # We've used 15-74 which is 2017-12 to 2022-11
Lookup <- Lookup %>% filter(ID>=15 & ID <=74)
Lookup$ID <- Lookup$ID - 14

First_NASH <- First_NASH %>% filter(First>="2019-11-01")

First_NASH <- First_NASH %>% mutate(Lapsed=First-DATE_OF_MEDICAL_CARE_START)
First_NASH$Lapsed <- as.numeric(First_NASH$Lapsed/30.5)
unique(First_NASH$STANDARD_DISEASE_NAME)


length(unique(First_NASH$MEMBER_ID)) #3692

data.frame(First_NASH %>% group_by(STANDARD_DISEASE_NAME) %>% count() %>% filter(n>300) %>% arrange(-n))



Cirrhosis <- First_NASH %>% filter(grepl("cirrhosis", STANDARD_DISEASE_NAME)) 

Cirrhosis %>% select(STANDARD_DISEASE_NAME) %>% distinct() %>% inner_join(First_NASH) %>%
  group_by(MEMBER_ID) %>% filter(Lapsed==min(Lapsed)) %>% slice(1) %>% ungroup() %>% #  summarise(n=median(Lapsed)) 
  filter(Lapsed<=12) %>%
  ggplot(aes(Lapsed)) +
  geom_density(linewidth=2) +
  theme_minimal() +
  xlab("\n Number of Months from Cirrhosis Dx to 1st NASH Dx") +
  ylab("Patient (kernel) density \n")


Cirrhosis <- First_NASH %>% filter(grepl("cirrhosis", STANDARD_DISEASE_NAME)) 
length(unique(Cirrhosis$MEMBER_ID)) / length(unique(First_NASH$MEMBER_ID)) # 0.3095883




cancer <- First_NASH %>% filter(grepl("C22", icd10_level4_code)|grepl("Z8505", icd10_level4_code)) 

cancer %>% select(STANDARD_DISEASE_NAME) %>% distinct() %>% inner_join(First_NASH) %>%
  group_by(MEMBER_ID) %>% filter(Lapsed==min(Lapsed)) %>% slice(1) %>% ungroup() %>%  # summarise(n=me(Lapsed)) 
  filter(Lapsed<=12) %>%
  ggplot(aes(Lapsed)) +
  geom_density(linewidth=2) +
  theme_minimal() +
  xlab("\n  Number of Months from Liver Cancer Dx to 1st NASH Dx") +
  ylab("Patient (kernel) density \n")


cancer <- First_NASH %>% filter(grepl("C22", icd10_level4_code)|grepl("Z8505", icd10_level4_code)) 
length(unique(cancer$MEMBER_ID)) / length(unique(First_NASH$MEMBER_ID)) # 0.3003792



# --------------------




# Drug usage cirrhotic vs non-cirrhotic -------------

JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt", colClasses = "character")
ce18_NASH_diags <- fread("ce18_NASH_diags.txt", colClasses = "character")

NASH_JMDC_Diagnosis_Codes <- fread("NASH_JMDC_Diagnosis_Codes.csv", colClasses = "character")

NASH_pats <- ce18_NASH_diags %>% filter(STANDARD_DISEASE_CODE=="8843497") %>% select(MEMBER_ID) %>% distinct() 

NASH_pats$Dx <- "NASH"

NAFLD_pats <- ce18_NASH_diags %>% filter(STANDARD_DISEASE_CODE=="8850319"|
                                          STANDARD_DISEASE_CODE=="8850318"|
                                          STANDARD_DISEASE_CODE=="5718008") %>% select(MEMBER_ID) %>% distinct() %>% anti_join(NASH_pats)

NAFLD_pats$Dx <- "NAFLD"



Fibrosis_pats <- NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Fibrosis") %>% select(standard_disease_code) %>%
  inner_join(ce18_NASH_diags, by=c("standard_disease_code"="STANDARD_DISEASE_CODE")) %>% 
  select(MEMBER_ID) %>% distinct()

Fibrosis_pats$Fibrosis <- "Fibrosis"

Cirrhosis_pats <- NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Cirrhosis") %>% select(standard_disease_code) %>%
  inner_join(ce18_NASH_diags, by=c("standard_disease_code"="STANDARD_DISEASE_CODE")) %>% 
  select(MEMBER_ID) %>% distinct()

Cirrhosis_pats$Cirrhosis <- "Cirrhosis"

NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Fibrosis")  %>% select(standard_disease_code )
NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Cirrhosis")  %>% select(standard_disease_code )


Diagnosis_master <- fread("Diagnosis_master.csv", colClasses = "character")

NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Fibrosis")  %>% select(standard_disease_code ) %>%
  inner_join(Diagnosis_master %>% select(standard_disease_code, standard_disease_name_j, standard_disease_name))


NASH_JMDC_Diagnosis_Codes %>% filter(Type=="Cirrhosis")  %>% select(standard_disease_code ) %>%
  inner_join(Diagnosis_master %>% select(standard_disease_code, standard_disease_name_j, standard_disease_name))


temp <- NASH_pats %>% bind_rows(NAFLD_pats) %>% left_join(Fibrosis_pats) %>% left_join(Cirrhosis_pats) %>% 
  left_join(JMDC_cePts_18plus %>% select(MEMBER_ID, weight))

temp %>%  group_by(Dx, Fibrosis, Cirrhosis) %>% summarise(n=sum(as.numeric(weight)))


Drugs_Lookup_IDs <- fread("Drugs_Lookup_IDs.csv")
Drugs_Lookup_IDs <- unique(Drugs_Lookup_IDs[, .(drug, group, class)])[order(class, group, drug)][, drug_id := .I][, .(group, class, drug, drug_id)]


temp2_gap62 <- fread("NASH_NAFLD_Drug_Histories_G62.txt", sep="\t")
temp2_gap62[ , .(group_sum = sum(weight)), by = Dx] 

over_time <- gather(temp2_gap62, Month, Drugs, month1:month60, factor_key=TRUE)
over_time <- over_time %>% filter(Drugs!="-") %>% select(MEMBER_ID,  Drugs) %>% distinct()
over_time <- separate_rows(over_time, Drugs, sep = ",", convert=T)

over_time <- over_time %>% left_join(Drugs_Lookup_IDs %>% select(group, class, drug_id), by=c("Drugs"="drug_id")) %>%
  select(-Drugs) %>% distinct()


temp %>% inner_join(over_time)  %>% select(MEMBER_ID, Dx, Cirrhosis, weight) %>% distinct() %>% group_by(Dx, Cirrhosis) %>% summarise(n=sum(as.numeric(weight)))


temp %>% inner_join(over_time)  %>%  group_by(Dx, Cirrhosis, group) %>%
  summarise(n=sum(as.numeric(weight))) %>%
  spread(key=group, value=n)




# -------------------------
# Drug usage 12 months before and after 1st GLP1 -------------------------------

Drugs_Lookup_IDs <- fread("Drugs_Lookup_IDs.csv")
Drugs_Lookup_IDs <- unique(Drugs_Lookup_IDs[, .(drug, group, class)])[order(class, group, drug)][, drug_id := .I][, .(group, class, drug, drug_id)]

temp2_gap62 <- fread("NASH_NAFLD_Drug_Histories_G62.txt", sep="\t")

string_GLP1  <- paste0("\\b(",paste0(Drugs_Lookup_IDs$drug_id[Drugs_Lookup_IDs$group == "GLP1"], collapse = "|"),")\\b")



temp2_gap62 <- gather(temp2_gap62, Month, Drugs, month1:month60, factor_key=TRUE)
temp2_gap62 <- temp2_gap62 %>% filter(Drugs!="-")

temp2_gap62$Month <- as.character(temp2_gap62$Month)
temp2_gap62$Month <- parse_number(temp2_gap62$Month)

GLP1_Pats <- temp2_gap62 %>% filter(grepl(string_GLP1, Drugs)) %>% select(MEMBER_ID, Month) %>% distinct() %>% group_by(MEMBER_ID) %>%
  filter(Month==min(Month)) %>% rename("FirstGLP1"="Month")
temp2_gap62 <- GLP1_Pats %>% left_join(temp2_gap62) 

temp2_gap62 <- separate_rows(temp2_gap62, Drugs, sep = ",", convert=T)

temp2_gap62 <- temp2_gap62 %>% left_join(Drugs_Lookup_IDs %>% select(group, drug_id), by=c("Drugs"="drug_id")) 

temp2_gap62 %>% select(MEMBER_ID, weight, Dx) %>% distinct() %>% ungroup() %>% group_by(Dx ) %>% summarise(n=sum(as.numeric(weight))) 


temp2_gap62 <- temp2_gap62 %>%  mutate(Lapsed=Month-FirstGLP1) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

temp2_gap62 <- temp2_gap62 %>% select(MEMBER_ID, Month) %>% distinct() %>% group_by(MEMBER_ID) %>% count() %>% filter(n>=25) %>%
  select(MEMBER_ID) %>% left_join(temp2_gap62)

temp2_gap62 %>% select(MEMBER_ID, weight, Dx) %>% distinct() %>% ungroup() %>%  group_by(Dx ) %>% summarise(n=sum(as.numeric(weight))) 


temp2_gap62 %>% select(MEMBER_ID, weight, Dx, group, Lapsed) %>% distinct() %>%
  group_by(Dx, Lapsed,group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


temp2_gap62 %>% select(MEMBER_ID, weight, Dx , group, Lapsed) %>% distinct() %>%
  filter(Dx=="NASH") %>%
  mutate(group=factor(group, levels=c("Biguanide", "Glinide", "Glitazone", "Glucosidase inhibitor", "Sulfonylurea", "DPP4", "SGLT2", "Insulin", "GLP1", "Hepatoprotective", "Fibrate", "Ion-exchange resin", "Other", "PCSK9", "Statin"))) %>%
  group_by(Lapsed, group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  #mutate(group=ifelse(is.na(group),"Lapsed",group)) %>%
  rename("Drug Group"="group") %>%
  mutate(n=n/5116.) %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Group`)) +
  geom_line(linewidth=2, alpha=.6) +
  theme_minimal() +
 scale_colour_manual(values=c("#fad6a3","#f5ad47","#ff9933","#cfccc9", "#f2a6b0", "#663e05", "#360566","#660557","#660505",
                                        "#154904","#0c5fe4","#649cf7","#c5dafc", "#05265c","#020e22")) +
  #ylim(0,65) +
  xlab("\n No. Elapsed Months \n(Before/After 1st GLP1)") +
  ylab("Population % \n")



temp2_gap62 %>% select(MEMBER_ID, weight, Dx , group, Lapsed) %>% distinct() %>%
  filter(Dx=="NAFLD") %>%
  mutate(group=factor(group, levels=c("Biguanide", "Glinide", "Glitazone", "Glucosidase inhibitor", "Sulfonylurea", "DPP4", "SGLT2", "Insulin", "GLP1", "Hepatoprotective", "Fibrate", "Ion-exchange resin", "Other", "PCSK9", "Statin"))) %>%
  group_by(Lapsed, group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  #mutate(group=ifelse(is.na(group),"Lapsed",group)) %>%
  rename("Drug Group"="group") %>%
  mutate(n=n/63099.) %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Group`)) +
  geom_line(linewidth=2, alpha=.6) +
  theme_minimal() +
 scale_colour_manual(values=c("#fad6a3","#f5ad47","#ff9933","#cfccc9", "#f2a6b0", "#663e05", "#360566","#660557","#660505",
                                        "#154904","#0c5fe4","#649cf7","#c5dafc", "#05265c","#020e22")) +
  #ylim(0,65) +
  xlab("\n No. Elapsed Months \n(Before/After 1st GLP1)") +
  ylab("Population % \n")


temp2_gap62 %>% select(MEMBER_ID, weight, Dx, group) %>% distinct()  %>%
  group_by(Dx, group) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=Dx, value=n)





temp2_gap62 %>% select(MEMBER_ID, weight, Dx, group, Lapsed) %>% distinct() %>%
  filter(Dx=="NASH") %>%
  mutate(group=factor(group, levels=c("Biguanide", "Glinide", "Glitazone", "Glucosidase inhibitor", "Sulfonylurea", "DPP4", "SGLT2", "Insulin", "GLP1", "Hepatoprotective", "Fibrate", "Ion-exchange resin", "Other", "PCSK9", "Statin"))) %>%
  group_by(Lapsed, group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
    mutate(n=ifelse(group=="Biguanide", n/4465,
                    ifelse(group=="Glinide", n/1457,
                           ifelse(group=="Glitazone",n/815,
                                  ifelse(group=="Glucosidase inhibitor",n/2249,
                                         ifelse(group=="Sulfonylurea", n/19.1,
                                                ifelse(group=="DPP4", n/4550 ,
                                                       ifelse(group=="SGLT2", n/4197,
                                                              ifelse(group=="Insulin", n/1824,
                                                                     ifelse(group=="GLP1", n/5116,
                                                                            ifelse(group=="Hepatoprotective", n/321,
                                                                                   ifelse(group=="Fibrate", n/740,
                                                                                          ifelse(group=="Ion-exchange resin", n/56.3   ,
                                                                                                 ifelse(group=="Other", n/1755,
                                                                                                        ifelse(group=="PCSK9", n/1000   ,
                                                                                                               ifelse(group=="Statin", n/3327,NA)))))))))))))))) %>%
  rename("Drug Group"="group") %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Group`)) +
  geom_line(size=2, alpha=0.6) +
  theme_minimal() +
 scale_colour_manual(values=c("#fad6a3","#f5ad47","#ff9933","#cfccc9", "#f2a6b0", "#663e05", "#360566","#660557","#660505",
                                        "#154904","#0c5fe4","#649cf7","#c5dafc", "#05265c","#020e22")) +
  xlab("\n No. Elapsed Months \n(Before/After 1st GLP1)") +
  ylab("Population % (of Class-experienced) \n")



temp2_gap62 %>% select(MEMBER_ID, weight, Dx, group, Lapsed) %>% distinct() %>%
  filter(Dx=="NAFLD") %>%
  mutate(group=factor(group, levels=c("Biguanide", "Glinide", "Glitazone", "Glucosidase inhibitor", "Sulfonylurea", "DPP4", "SGLT2", "Insulin", "GLP1", "Hepatoprotective", "Fibrate", "Ion-exchange resin", "Other", "PCSK9", "Statin"))) %>%
  group_by(Lapsed, group) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
    mutate(n=ifelse(group=="Biguanide", n/50340,
                    ifelse(group=="Glinide", n/13266,
                           ifelse(group=="Glitazone",n/11639,
                                  ifelse(group=="Glucosidase inhibitor",n/18690,
                                         ifelse(group=="Sulfonylurea", n/260,
                                                ifelse(group=="DPP4", n/52341 ,
                                                       ifelse(group=="SGLT2", n/51020,
                                                              ifelse(group=="Insulin", n/32079,
                                                                     ifelse(group=="GLP1", n/63099,
                                                                            ifelse(group=="Hepatoprotective", n/6101,
                                                                                   ifelse(group=="Fibrate", n/8742,
                                                                                          ifelse(group=="Ion-exchange resin", n/87.4   ,
                                                                                                 ifelse(group=="Other", n/9087,
                                                                                                        ifelse(group=="PCSK9", n/29.1   ,
                                                                                                               ifelse(group=="Statin", n/40428,NA)))))))))))))))) %>%
  rename("Drug Group"="group") %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Group`)) +
  geom_line(size=2, alpha=0.6) +
  theme_minimal() +
 scale_colour_manual(values=c("#fad6a3","#f5ad47","#ff9933","#cfccc9", "#f2a6b0", "#663e05", "#360566","#660557","#660505",
                                        "#154904","#0c5fe4","#649cf7","#c5dafc", "#05265c","#020e22")) +
  xlab("\n No. Elapsed Months \n(Before/After 1st GLP1)") +
  ylab("Population % (of Class-experienced) \n")

# ------------------------------
# Pathways --------------------------------


Drugs_Lookup_IDs <- fread("Drugs_Lookup_IDs.csv")
Drugs_Lookup_IDs <- unique(Drugs_Lookup_IDs[, .(drug, group, class)])[order(class, group, drug)][, drug_id := .I][, .(group, class, drug, drug_id)]

temp2_gap62 <- fread("NASH_NAFLD_Drug_Histories_G62.txt", sep="\t")

string_GLP1  <- paste0("\\b(",paste0(Drugs_Lookup_IDs$drug_id[Drugs_Lookup_IDs$group == "GLP1"], collapse = "|"),")\\b")

temp2_gap62 <- gather(temp2_gap62, Month, Drugs, month1:month60, factor_key=TRUE)
temp2_gap62$Month <- as.character(temp2_gap62$Month)
temp2_gap62$Month <- parse_number(temp2_gap62$Month)
temp2_gap62 <- temp2_gap62 %>% group_by(MEMBER_ID) %>% filter(Month<=12)
temp2_gap62 <- temp2_gap62 %>% filter(Drugs == "-")
Naive_until_12 <- temp2_gap62 %>% group_by(MEMBER_ID) %>% count() %>% filter(n==12) %>% select(MEMBER_ID)


temp2_gap62 <- fread("NASH_NAFLD_Drug_Histories_G62.txt", sep="\t")
temp2_gap62 <- Naive_until_12 %>% left_join(temp2_gap62)
temp2_gap62 <- gather(temp2_gap62, Month, Drugs, month1:month60, factor_key=TRUE)
temp2_gap62$Month <- as.character(temp2_gap62$Month)
temp2_gap62$Month <- parse_number(temp2_gap62$Month)


temp2_gap62_GP1 <- temp2_gap62 %>% group_by(MEMBER_ID, weight) %>% 
  slice(if(any(grepl(string_GLP1,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_GLP1,Drugs)) else NA) 

temp2_gap62_GP1 <- separate_rows(temp2_gap62_GP1, Drugs, sep = ",", convert=T)

unique(Drugs_Lookup_IDs$class)

temp2_gap62_GP1 <- temp2_gap62_GP1 %>% filter(Drugs!="-") %>%  mutate(Drugs=as.numeric(Drugs)) %>%
  left_join(Drugs_Lookup_IDs %>% select(group, class, drug_id), by=c("Drugs"="drug_id")) %>%
  mutate(class=ifelse(group=="GLP1", group, class)) %>% select(-group) %>% distinct() %>%
  mutate(class=ifelse(class=="GLP1", "GLP1",
                      ifelse(class=="Hepatoprotective", "Hepato",
                             ifelse(class=="Antidiabetic", "Antidia",
                                    ifelse(class=="Lipid-lowering", "Lipids", NA))))) %>% select(-Drugs) %>% distinct()

unique(temp2_gap62_GP1$class)

temp2_gap62_GP1 <- temp2_gap62_GP1 %>% mutate(Index=ifelse(class=="Lipids", 1,
                                                           ifelse(class=="Antidia",2,
                                                                  ifelse(class=="Hepato",3,4 ))))

temp2_gap62_GP1 <- temp2_gap62_GP1 %>% arrange(MEMBER_ID, weight, Month, Index) %>% select(-c(Month, Index)) %>% distinct() 

temp2_gap62_GP1 <- temp2_gap62_GP1 %>% group_by(MEMBER_ID, weight) %>% mutate(class=paste(class, collapse=" -> ")) %>% distinct()

temp2_gap62_GP1 %>% select(MEMBER_ID, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight))

data.frame(temp2_gap62_GP1 %>% group_by(class) %>% summarise(n=sum(weight)/32287) %>% arrange(-n))

# -------------------
# Labs in GLP1-experienced vs naive patients ------------------

Drugs_Lookup_IDs <- fread("Drugs_Lookup_IDs.csv")
Drugs_Lookup_IDs <- unique(Drugs_Lookup_IDs[, .(drug, group, class)])[order(class, group, drug)][, drug_id := .I][, .(group, class, drug, drug_id)]

string_GLP1  <- paste0("\\b(",paste0(Drugs_Lookup_IDs$drug_id[Drugs_Lookup_IDs$group == "GLP1"], collapse = "|"),")\\b")

temp2_gap62 <- fread("NASH_NAFLD_Drug_Histories_G62.txt", sep="\t")

temp2_gap62 <- gather(temp2_gap62, Month, Drugs, month1:month60, factor_key=TRUE)
temp2_gap62 <- temp2_gap62 %>% filter(Drugs!="-")

temp2_gap62$Month <- as.character(temp2_gap62$Month)
temp2_gap62$Month <- parse_number(temp2_gap62$Month)

Any_GLP1 <- temp2_gap62 %>% filter(grepl(string_GLP1, Drugs)) %>% select(MEMBER_ID) %>%  distinct()
GLP1_Pats <- temp2_gap62 %>% filter(grepl(string_GLP1, Drugs)) %>% select(MEMBER_ID) %>%  group_by(MEMBER_ID) %>% count() %>% filter(n>3)
GLP1_Pats <- GLP1_Pats %>% ungroup() %>% select(-n)
Any_GLP1 <- Any_GLP1 %>% anti_join(GLP1_Pats)


LabsImputed <- fread("LabsImputed.txt", sep="\t")
LabsImputed %>% anti_join(Any_GLP1) %>% left_join(GLP1_Pats %>% mutate(GLP1="GLP1")) %>%
  group_by(GLP1, test) %>% summarise(n=mean(value)) %>% spread(key=GLP1, value=n)

# ---------------
# Visualize individual aptients breakdown for classification --------------------------------
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)
library(heatmaply)
library(htmlwidgets)

RandomForestScores_Predictiojs <- fread("RandomForestScores_Predictiojs.txt", sep="\t")
RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% mutate(ID=row_number())

RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% gather(Test, Result, ABDOMINAL_CIRCUMFERENCE:TRIGLYCERIDE)

RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>%
  left_join(RandomForestScores_Predictiojs %>% filter(Dx==1) %>% group_by(Test) %>% summarise(Dx_mean=mean(Result)))

RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% mutate(Fold=Result/Dx_mean)
RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% select(ID, Dx, predict, Test, Fold)

RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% spread(key=Test, value=Fold)

RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% arrange(-predict) %>% filter(Dx==1 & predict>0.8) %>%  sample_n(50)

RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% select(-c(Dx, predict))
RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% select(-c(ID))

RandomForestScores_Predictiojs <- as.matrix(RandomForestScores_Predictiojs)

#heatmap(RandomForestScores_Predictiojs, scale="column", col = cm.colors(256))

mat <-RandomForestScores_Predictiojs

p <- heatmaply(mat, 
        #dendrogram = "column",
        xlab = "", ylab = "", 
        main = "",
        scale = "row",
        margins = c(60,100,40,20),
        grid_color = "white",
        grid_width = 0.00001,
        titleX = FALSE,
        hide_colorbar = TRUE,
        branches_lwd = 0.1,
        fontsize_row = 5, fontsize_col = 5,
        labCol = colnames(mat),
        heatmap_layers = theme(axis.line=element_blank())
        )

p

saveWidget(p, file= "heatmapInter.html")

# ---------------------------------------------------------------
# % Above > SD --------------------------
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)
library(heatmaply)
library(htmlwidgets)

RandomForestScores_Predictiojs <- fread("RandomForestScores_Predictiojs.txt", sep="\t")
RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% mutate(ID=row_number())

RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% gather(Test, Result, ABDOMINAL_CIRCUMFERENCE:TRIGLYCERIDE)

RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>%
  left_join(RandomForestScores_Predictiojs %>% filter(Dx==1) %>% group_by(Test) %>% summarise(Dx_mean=mean(Result))) %>%
    left_join(RandomForestScores_Predictiojs %>% filter(Dx==1) %>% group_by(Test) %>% summarise(Dx_sd=sd(Result))) 


RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% mutate(Above=ifelse(Result>Dx_mean,1,0))
RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% mutate(Above_1sd=ifelse(Result>(Dx_mean+Dx_sd),1,0))
RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% mutate(Above_2sd=ifelse(Result>(Dx_mean+(2*Dx_sd)),1,0))
RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% mutate(Above_3sd=ifelse(Result>(Dx_mean+(3*Dx_sd)),1,0))

RandomForestScores_Predictiojs <- RandomForestScores_Predictiojs %>% filter(predict>0.6)

length(unique(RandomForestScores_Predictiojs$ID))  #16118

RandomForestScores_Predictiojs %>% filter(Above==1) %>% group_by(ID) %>% count() %>% ungroup() %>% group_by(n) %>% count() %>% mutate(nn=nn/16118)
RandomForestScores_Predictiojs %>% filter(Above_1sd==1) %>% group_by(ID) %>% count() %>% ungroup() %>% group_by(n) %>% count() %>% mutate(nn=nn/16118)
RandomForestScores_Predictiojs %>% filter(Above_2sd==1) %>% group_by(ID) %>% count() %>% ungroup() %>% group_by(n) %>% count() %>% mutate(nn=nn/16118)
RandomForestScores_Predictiojs %>% filter(Above_3sd==1) %>% group_by(ID) %>% count() %>% ungroup() %>% group_by(n) %>% count() %>% mutate(nn=nn/16118)

# -------------------------------------
# DIA & OBE among NASH --------------------

ce18_NASH_NAFLD_pts_Alldiags <- fread("ce18_NASH-NAFLD_pts_Alldiags.txt")
length(unique(ce18_NASH_NAFLD_pts_Alldiags$STANDARD_DISEASE_NAME))
ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[,c("MEMBER_ID", "STANDARD_DISEASE_CODE", "STANDARD_DISEASE_NAME")]

Diagnosis_master <- fread("Diagnosis_master.csv")
Diagnosis_master <- Diagnosis_master[,c("standard_disease_code", "icd10_level4_code", "icd10_level4_name")]
names(Diagnosis_master)[1] <- "STANDARD_DISEASE_CODE"

ce18_NASH_NAFLD_pts_Alldiags <- ce18_NASH_NAFLD_pts_Alldiags[Diagnosis_master, on = .(STANDARD_DISEASE_CODE), nomatch = NULL]
ce18_NASH_NAFLD_pts_Alldiags <- distinct(ce18_NASH_NAFLD_pts_Alldiags[, -c("STANDARD_DISEASE_CODE", "STANDARD_DISEASE_NAME")])

#E11, O24, E08, E09, E13, E10
#E65, E66

T2D_Dx <- distinct(ce18_NASH_NAFLD_pts_Alldiags[grepl("E11", icd10_level4_code), c("MEMBER_ID")]) 
T2D_Dx$group <- "T2D"


JMDC_cePts_18plus <- fread("JMDC_cePts_18plus.txt")
Dx_pats <- fread("NASH_NAFLD_Dx_pats.txt")
Dx_pats <- merge(x = Dx_pats, y = JMDC_cePts_18plus[ , c("MEMBER_ID", "weight")], by = "MEMBER_ID", all.x=TRUE)

T2D_Dx <- merge(x = Dx_pats, y = T2D_Dx, by = "MEMBER_ID", all.x=TRUE)
names(T2D_Dx)[4] <- "T2D"

ce18_NASH_NAFLD_pts_All_annual_Checkups <- fread("ce18_NASH-NAFLD_pts_All_annual_Checkups.txt")
names(ce18_NASH_NAFLD_pts_All_annual_Checkups)

ce18_NASH_NAFLD_pts_All_annual_Checkups <- ce18_NASH_NAFLD_pts_All_annual_Checkups %>% select(MEMBER_ID, BMI) 
ce18_NASH_NAFLD_pts_All_annual_Checkups <- ce18_NASH_NAFLD_pts_All_annual_Checkups %>% distinct() %>% group_by(MEMBER_ID) %>% summarise(BMI=max(BMI))


ce18_NASH_NAFLD_pts_All_annual_Checkups %>% ungroup() %>% select(MEMBER_ID) %>% distinct() %>%
  left_join(T2D_Dx) %>%
  left_join(ce18_NASH_NAFLD_pts_All_annual_Checkups) %>% filter(Dx=="NASH") %>%
  mutate(OBE=ifelse(BMI>=30, "OBE", NA)) %>%
  group_by(T2D, OBE) %>% summarise(n=100*sum(weight)/(85500+36331+77251+45915))
  
# ----------------------
