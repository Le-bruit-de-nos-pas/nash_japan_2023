

pack <- c("RPostgres","DBI","bit64","data.table","tidyverse","lubridate","openxlsx")
newPack <- pack[!(pack %in% installed.packages()[,"Package"])]

if(length(newPack)>0){
  install.packages(newPack)




for (i in pack){
  library(i, character.only = T)



# DB Creation and Data Processing

db <- "Rimegepant-JP-JMDC-OCT2022"
host_db <- "localhost"  
db_port <- 5432  
db_user <- "postgres"  
db_password <- "xxxxxx"

con <- dbConnect(RPostgres::Postgres(),
                 dbname = db, 
                 host = host_db, 
                 port = db_port, 
                 user = db_user,
                 password = db_password)


query <- "SELECT
          pid,
          now() - pg_stat_activity.query_start AS duration,
          query,
          state FROM pg_stat_activity;"
test <- dbGetQuery(con, query)

query <- "SELECT pg_terminate_backend(4588)" # place the active pid in between the ()
query <- dbGetQuery(con, query)




## Create DB on Postgres

# JMDC environment--------------------------------------------------------------
jmdc <- new.env()

# JMDC tables
jmdc$tables <- data.table(read.xlsx("JMDC_Documentation/JMDC_Table_Field_List.xlsx", sheet = "Table list"), stringsAsFactors = F)
names(jmdc$tables) <- c("table","file","records")
jmdc$tables <- jmdc$tables[, upload := (!str_detect(table, "Material"))*1] # leaving Material tables out


# JMDC table fields (columns)
jmdc$tabfields <- data.table(read.xlsx("JMDC_Documentation/JMDC_Table_Field_List.xlsx", sheet = "Field list"), stringsAsFactors = F)
names(jmdc$tabfields) <- c("table","field","type")
jmdc$tabfields <- jmdc$tabfields[, DropField := str_detect(field,"_j$")*1] # exclude fields written in Japanese 


### Sample Test DB


# Testing upload of the extracts into the the DB
# Create sample files with 10000 rows, from the Extract files for further upload into the postgreSQL DB Test schema

files <- jmdc$tables[upload == 1, .(table)]
files <- DBtables$table[DBtables$upload == 1]

extrD <- list.files("JMDC_Extracts/") # files in the JMDC_Extracts directory 
smplD <- list.files("JMDC_Samples/")  # files in the JMDC_Samples directory

for(f in files$table){
  smplExist <- sum(paste0(f,".csv") %in% smplD)
  if(length(files) > 0 & smplExist == 0){
    print(f)
    t <- read.csv(paste0("JMDC_Extracts/",f,".csv"), sep = ",", nrows = 10000, header = T)
    fwrite(t, paste0("JMDC_Samples/",f,".csv"))
  }
}



# Create schema test
query <- paste0("DROP SCHEMA IF EXISTS test CASCADE;")
dbSendQuery(con, query)
query <- paste0("CREATE SCHEMA test;")
dbSendQuery(con, query)


# Create tables in schema test
tables <- jmdc$tables[upload == 1, .(table)]

for(t in tables$table){
  print(t)
  query <- paste0("DROP TABLE IF EXISTS test.",t,";")
  dbSendQuery(con, query)
  
  query <- paste0("CREATE TABLE test.",t," (",paste0(jmdc$tabfields$field[jmdc$tabfields$table == t]," ",
                                                     jmdc$tabfields$type[jmdc$tabfields$table == t],collapse = ","),
                  ") TABLESPACE pg_default;")
  dbSendQuery(con, query)
}




# Upload of sample tables
tables <- jmdc$tables[upload == 1, .(table)]
wd <- getwd()

for(t in tables$table){
  print(t)
  query <- paste0("COPY test.",tolower(t)," FROM '", wd,"/JMDC_Samples/",t,".csv' DELIMITER ',' CSV HEADER;")
  dbSendQuery(con, query)
}



# Drop Japanese language fields 
for(t in tables$table){
  print(t)
  drpfields <- jmdc$tabfields$field[jmdc$tabfields$table == t & jmdc$tabfields$DropField == 1]
  for(f in drpfields){
    cat(f)
    query <- paste0("ALTER TABLE test.",tolower(t)," DROP COLUMN ",f,";")
    dbSendQuery(con, query)
  }
}









# Existing tables in the schema test
query <- "SELECT * FROM information_schema.tables WHERE table_schema = 'test'"
t     <- dbGetQuery(con, query)


# Existing indexes in the schemas's tables
query <- "SELECT * FROM pg_indexes WHERE schemaname = 'test'"
ind   <- dbGetQuery(con, query)


# Create indexes
toindex <- t$table_name[t$table_name == "diagnosis" | t$table_name == "procedure" | t$table_name == "drug" | 
                          t$table_name == "claims" | t$table_name == "annual_health_checkup"]
indexed <- ind$tablename


for(i in toindex){
  if(!(i %in% indexed)){
    print(i)
    start <- Sys.time()
    query <- paste0("CREATE INDEX ",i,"_member_id ON test.",i," (member_id);")
    dbGetQuery(con, query)  
    end   <- Sys.time()
    print(end - start)
  }
}





### Full DB


# Create schema JMDC
#query <- paste0("DROP SCHEMA IF EXISTS jmdc CASCADE;")
#dbSendQuery(con, query)

#query <- paste0("CREATE SCHEMA jmdc;")
#dbSendQuery(con, query)



# Create tables in schema JMDC
tables <- jmdc$tables[upload == 1, .(table)]

for(t in tables$table){
  query <- paste0("SELECT table_name FROM information_schema.tables WHERE table_schema = 'jmdc';")
  jmdcT <- dbGetQuery(con, query)
  if(length(tables$table) > 0 & !(tolower(t) %in% jmdcT$table_name)){
    print(t)
    query <- paste0("CREATE TABLE jmdc.",t," (",paste0(jmdc$tabfields$field[jmdc$tabfields$table == t]," ",
                                                     jmdc$tabfields$type[jmdc$tabfields$table == t],collapse = ","),
                  ") TABLESPACE pg_default;")
    dbSendQuery(con, query)
  }
}




# Upload of the jmdc Extracts
tables <- jmdc$tables[upload == 1, .(table)]
wd <- getwd()


for(t in tables$table){
  query <- paste0("SELECT table_name FROM information_schema.tables WHERE table_schema = 'jmdc';")
  jmdcT <- dbGetQuery(con, query)
  if(tolower(t) %in% jmdcT$table_name){
    query   <- paste0("SELECT EXISTS(SELECT 1 FROM jmdc.",tolower(t),");")
    hasrows <- dbGetQuery(con, query)
    if(hasrows == FALSE){
      print(t)
      start <- Sys.time()
      query <- paste0("COPY jmdc.",tolower(t)," FROM '", wd,"/JMDC_Extracts/",t,".csv' DELIMITER ',' CSV HEADER;")
      dbSendQuery(con, query)
      end   <- Sys.time()
      print(end - start)
    }
  } 
}



# Existing tables in the schema
query <- "SELECT * FROM information_schema.tables WHERE table_schema = 'jmdc'"
t     <- dbGetQuery(con, query)


# Existing indexes in the schemas's tables
query <- "SELECT * FROM pg_indexes WHERE schemaname = 'jmdc'"
ind   <- dbGetQuery(con, query)

# Create indexes
toindex <- t$table_name[t$table_name == "diagnosis" | t$table_name == "procedure" | t$table_name == "drug" | t$table_name == "claims" | 
                          t$table_name == "enrollment" | t$table_name == "annual_health_checkup"]
indexed <- ind$tablename



for(i in toindex){
  if(!(i %in% indexed)){
    print(i)
    start <- Sys.time()
    query <- paste0("CREATE INDEX ",i,"_member_id ON jmdc.",i," (member_id);")
    dbGetQuery(con, query)  
    end   <- Sys.time()
    print(end - start)
  }
}




start <- Sys.time()
query <- paste0("CREATE INDEX drug_DrugCode ON jmdc.drug (jmdc_drug_code);")
dbGetQuery(con, query)  
end   <- Sys.time()
print(end - start)

start <- Sys.time()
query <- paste0("CREATE INDEX diagnosis_DiagCode ON jmdc.diagnosis (standard_disease_code);")
dbGetQuery(con, query)  
end   <- Sys.time()
print(end - start)

start <- Sys.time()
query <- paste0("CREATE INDEX procedure_ProcCode ON jmdc.procedure (standardized_procedure_code);")
dbGetQuery(con, query)  
end   <- Sys.time()
print(end - start)



# Existing indexes in the schemas's tables
query <- "SELECT * FROM pg_indexes WHERE schemaname = 'jmdc'"
ind   <- dbGetQuery(con, query)





## Data Processing pipeline
### Definitions


# Postgres Database reference variables - environment---------------------------
db <- new.env()

db$schema    <- "jmdc"                # Postgres db schema
db$pat       <- "patient"             # Postgres db schema's patient table
db$rx        <- "drug"                # Postgres db schema's drugs table
db$dx        <- "diagnosis"           # Postgres db schema's diagnosis table
db$dx_lkp    <- "diagnosis_master"    # Postgres db schema's look up medical table
db$prod_lkp  <- "drug_master"         # Postgres db schema's look up product table
db$enroll    <- "enrollment"          # Postgres db schema's enrollment table
db$procd     <- "procedure"           # Postgres db schema's enrollment table
db$procd_lkp <- "enrollment"          # Postgres db schema's enrollment table


# Definitions environment-------------------------------------------------------
defs <- new.env()

defs$disease <- "JPMigraine" # Disease

# Defining Enrollment window (74 months)
# The enrollment window is the 74 months we've required JMDC to consider at the time of the data request (May-2015 to June-2021). This time-window is  

defs$maxEnrdd <- ymd("2021-06-30") # Database most recent record date is 2021/06/30 <= 'SELECT MAX(date_of_prescription) FROM jmdc.drug;'
defs$minEnrdd <- ymd("2015-05-01") # Database older record date is 2015/05
time_length(interval(defs$minEnrdd, defs$maxEnrdd), "month") # 74
defs$maxEnrym <- ym("202106")      # Database most recent record month and year -> for queried dates evaluation (most db dates have just year & month)

# Defining Maximum years of age any respondent should have => 100 years old (moreover since JMDC DB does not cover insured people over <=> 80 years old)
defs$minYobdd <- ymd("1921-06-30")
defs$minYOB   <- 1921
defs$maxAge   <- 100
time_length(interval(defs$minYobdd, defs$maxEnrdd), "year") # 100

defs$pop <- data.table(read.xlsx("Population data/JP_Age_Distribution_2020.xlsx", sheet = "Data transformed", 
                                 cols = c(1,3:4)), stringsAsFactors = F)                                                  # Japan pop data
defs$dxs <- data.table(read.xlsx("Migraine_Diagnosis_10-2022.xlsx"), stringsAsFactors = F)                                # dx's of interest
defs$med <- data.table(read.xlsx("Migraine_Drugs_10-2022.xlsx", sheet = "Drugs_Pat_Selection"), stringsAsFactors = F)     # drugs for pts selection
#defs$rxs <- data.table(read.xlsx("Migraine_Drugs_10-2022.xlsx", sheet = "Drugs_to_track"), stringsAsFactors = F)         # drugs to track          
#defs$cmb <- data.table(read.xlsx("Migraine UK comorbidities.xlsx", sheet = "Sheet1", cols = c(1,3:4)))                   # Comorbidities of interest                                        


# Pats environment-------------------------------------------------------------- 
# to hold patient vectors throughout the data processing pipeline
pats <- new.env()


# Convert enrollment time window into month periods
defs$minEnrdd # "2015-05-01"
defs$maxEnrdd # "2021-06-30"

convertMonths <- data.frame(min = seq(ymd("2015-04-16"), by = "month", length = 75),
                            max = seq(ymd("2015-05-15"), by = "month", length = 75),
                             id = 1:75)


# Add 60 month window of analysis id
convertMonths$window_w60m[convertMonths$id >= 1 & convertMonths$id <= 13] <- 0
convertMonths$window_w60m[convertMonths$id > 13 & convertMonths$id <= 73] <- c(seq(1,60,1))
convertMonths$window_w60m[convertMonths$id > 73] <- 0




# Date reference table (enrollment time window reference)
dateRef       <- data.frame(date = seq(defs$minEnrdd, defs$maxEnrdd, by = 1))
dateRef$month <- 0



for(i in 1:75){
  cat(i)
  sel <- dateRef$date <= convertMonths$max[i] & dateRef$date >= convertMonths$min[i]
  dateRef$month[sel] <- paste0(i)
}

defs$convertMonths <- convertMonths
defs$dateRef       <- dateRef
rm(convertMonths, dateRef)



# Function to load data from the DB in batches
# Arguments: 'data' -> a vector of values respecting ideally to an indexed data field in the DB table; 'by' -> batch length

pagify <- function(data = NULL, by = 1000){
  pagemin <- seq(1,length(data), by = by)
  pagemax <- pagemin - 1 + by
  pagemax[length(pagemax)] <- length(data)
  pages   <- list(min = pagemin, max = pagemax)
}




### Patient funnel

# Identify respondents:
# - continuously enrolled throughout the enrollment window
# - with consistent Age: Age is not missing, unique age data and with realistic age 
# - with consistent Gender: gender is not missing, unique gender
# - aged 18+


# Continuous enrolled respondents-----------------------------------------------
query   <- paste0("SELECT member_id, month_and_year_of_birth_of_member, gender_of_member, observation_start, observation_end FROM ",
                  db$schema,".",db$enroll,";")

enroll  <- setDT(dbGetQuery(con, query))

length(unique(enroll$member_id)) - nrow(enroll) # 0 -> unique member_id (s) only => unique enrollment, age and gender info

contEnr <- enroll
contEnr[, cont_enroll := (ym(observation_start) <= defs$minEnrdd & ym(observation_end) >= defs$maxEnrym)*1]

sum(contEnr$cont_enroll) # 2,739,909
contEnr <- contEnr[cont_enroll == 1]
pats$cntEnr <- contEnr$member_id



# Consistent age----------------------------------------------------------------
contEnr <- contEnr[, age := as.integer(time_length(interval(ym(month_and_year_of_birth_of_member), defs$maxEnrym), "year"))]
contEnr[, age_consist := (!is.na(age) & age <= defs$maxAge & age != 0)*1]

sum(contEnr$cont_enroll == 1 & contEnr$age_consist == 1) # 2,739,909 => Everyone continuously enrolled has consistent age


# Consistent Gender-------------------------------------------------------------
unique(contEnr$gender_of_member) # "Male", "Female"
contEnr[, gender_consist := ((gender_of_member == "Male" | gender_of_member == "Female") & !is.na(gender_of_member))*1]

sum(contEnr$cont_enroll == 1 & contEnr$gender_consist == 1) # 2,739,909 => Everyone continuously enrolled has consistent gender



# Continuous Enrolled 18+-------------------------------------------------------
# Respondents aged 18+ with consistent Age and Gender and with at least a diagnosis and a prescription
cntEnr18 <- contEnr[age >= 18 & age_consist == 1 & gender_consist == 1, .(member_id, gender = gender_of_member, age)]
length(unique(cntEnr18$member_id)) - nrow(cntEnr18) # 0
nrow(cntEnr18)/nrow(enroll) # 18% of the respondents in the enrollment table are Continuous enrolled 18+ members

fwrite(cntEnr18,"Processed Data/ContEnr18.txt")
rm(enroll, contEnr)







# Calculation of weights--------------------------------------------------------

# Continuous enrolled age and gender sample counts
cntEnr18      <- fread("Processed Data/ContEnr18.txt", integer64 = "character", stringsAsFactors = F)
pats$cntEnr18 <- cntEnr18$member_id
ce18 <- cntEnr18[, .(samples_count = .N), keyby = .(age, gender)]


# JP population 18+ 
pop <- melt(defs$pop, id = "Age") 
sum(pop$value) # 126,146,099 JP total population

pop <- pop[Age >= 18,.(Age, gender = variable, population = value)][order(Age,-gender)]
sum(pop$population) # 108,917,975 JP population 18+


# Calculating projection weights with distribution of the Japanese 75+ age group over the 65 - 74 ce18 age groups, proportionally**********************
max(pop$Age)  # 111
max(ce18$age) # 75 -> jmdc has only respondents up to the age of 75 (after 75 years of age, people change health insurance and are seen in different clinical/hospital setting, which jmdc doesn't capture)


# JP population age based regrouping 
pop[, Age2 := ifelse(Age <= 74, Age, 75)]
pop <- pop[, .(pop2 = sum(population)), by = .(Age2, gender)]
names(pop)[c(1,3)] <- c("Age","pop")


# Calculate ce18+ 65 - 74 age groups share <= age group 75 is to small (only 7 samples) so it'll be placed together with the 74 age group 
ce18$samples_count[ce18$age == 74 & ce18$gender == "Male"] <- ce18$samples_count[ce18$age == 74 & ce18$gender == "Male"] +
                                                              ce18$samples_count[ce18$age == 75 & ce18$gender == "Male"]

ce18$samples_count[ce18$age == 74 & ce18$gender == "Female"] <- ce18$samples_count[ce18$age == 74 & ce18$gender == "Female"] + 
                                                                ce18$samples_count[ce18$age == 75 & ce18$gender == "Female"]

ce18 <- ce18[ce18$age < 75,]

ce18[age >= 65, samples65plus := sum(samples_count), by = .(gender)]
ce18[age >= 65, share65plus := samples_count/samples65plus]
ce18[is.na(ce18)] <- 0




# Determine weights

weights <- pop[ce18, on = .(Age = age, gender)]
weights$pop75plus[weights$Age >= 65 & weights$gender == "Male"]  <- pop$pop[pop$Age == 75 & pop$gender == "Male"] 
weights$pop75plus[weights$Age >= 65 & weights$gende == "Female"] <- pop$pop[pop$Age == 75 & pop$gender == "Female"]
weights$pop75plus[is.na(weights$pop75plus)] <- 0

weights$pop_Transformed <- weights$pop + weights$share65plus * weights$pop75plus
sum(weights$pop_Transformed) - sum(pop$pop) # 0 -> check okay

weights$weight <- round(weights$pop_Transformed / weights$samples_count, 2)
weights <- weights[,.(age = Age, gender, samples_count, population = pop_Transformed, weight)] 

fwrite(weights,"Processed Data/weights.csv")






# Continuous enrolled 18+ patients weights**************************************

ce18w <- weights[,.(age, gender, weight)][cntEnr18, on = .(age, gender)]

# Assigning 75 year old continuous enrolled 18+ members the 74 age group weight (given that the 7 ce18 75 years samples were put together with 74 age group)
ce18w[age == 75 & gender == "Female", weight := weights$weight[weights$age == 74 & weights$gender == "Female"]]
ce18w[age == 75 & gender == "Male", weight := weights$weight[weights$age == 74 & weights$gender == "Male"]]

ce18w <- ce18w[,.(member_id,age,gender,weight)]

sum(ce18w$weight) - sum(pop$pop) # -300 -> Check okay => difference is due to rounding

fwrite(ce18w ,"Processed Data/ContEnr18W.txt")
pats$ce18w <- ce18w[,.(member_id, weight)] 






# patient projection visualization----------------------------------------------
weights <- fread("Processed Data/weights.csv", stringsAsFactors = F)
data <- melt(weights, id = c("age","gender"))
names(data)[c(3,4)] <- c("measure","n")
data <- data[measure != "weight", perc_share := round(n/sum(n)*100,1), by = .(measure)] 

p <- ggplot(data[measure != "weight"], aes(x = age, y = perc_share, fill = gender)) + geom_col() + facet_wrap(~measure)
p <- p + scale_fill_manual(values = c("lightcoral","dodgerblue4"))
p <- p + scale_x_continuous(breaks = seq(18,100,3))
#p <- p + scale_y_continuous(expand = c(0,0), limits = c(0,2.1))
p <- p + theme_classic() + theme(legend.title = element_blank(), legend.position = "top", legend.justification = "right")
p <- p + ggtitle("Sample vs Pop. Age & Gender penetration (%) distribution")
p

ggsave("Processed Data/Age&Gender_penetration.png",device = "png", plot = p, height = 3.5, width = 5.5)

p <- ggplot(data[measure == "samples_count"], aes(x = age, y = n, fill = gender)) + geom_col()
p <- p + scale_fill_manual(values = c("lightcoral","dodgerblue4"))
p <- p + scale_x_continuous(breaks = seq(18,100,3))
p <- p + scale_y_continuous(expand = c(0,0))
p <- p + theme_classic() + theme(legend.title = element_blank(), legend.position = "top", legend.justification = "right") 
p <- p + ggtitle("Sample Age & Gender distribution")
p

ggsave("Processed Data/Age&Gender_dist_samples.png",device = "png", plot = p, height = 3.5, width = 5.5)

p <- ggplot(data[measure == "weight"], aes(x = age, y = n, color = gender)) + geom_line(size = 1)
p <- p + scale_color_manual(values = c("lightcoral","dodgerblue4"))
p <- p + scale_x_continuous(breaks = seq(18,100,3))
p <- p + scale_y_continuous(limits = c(0,as.integer(max(data$n[data$measure == "weight"])+1)))
p <- p + theme_classic() + theme(legend.title = element_blank(), legend.position = "top", legend.justification = "right")
p <- p + ggtitle("Weights by Age & Gender") + ylab("weight")
p

ggsave("Processed Data/Age&Gender_weights.png",device = "png", plot = p, height = 3.5, width = 5.5)

rm(ce18, pop, ce18w, p)


# Identify Migraine diagnosed continuous enrolled 18+ patients, in the 74m enrollment window, based on Vanguard's selection of diagnosis 
# Create table with all Migraine diagnosis per patient


# Identify Migraine diagnosed continuous enrolled 18+ patients******************
dxs <- paste0(defs$dxs$standard_disease_code, collapse = "','")

cntEnr18      <- fread("Processed Data/ContEnr18.txt", integer64 = "character", stringsAsFactors = F)
pats$cntEnr18 <- cntEnr18$member_id

pages <- pagify(pats$cntEnr18, 500)

ce18Dxs <- data.table()



for(i in 1:length(pages$max)){
  pts <- paste0(pats$cntEnr18[pages$min[i]:pages$max[i]], collapse = "','")
  
  start <- Sys.time()
  query <- paste0("SELECT member_id, 
                          month_and_year_of_medical_care, 
                          standard_disease_code, 
                          suspicion_flag 
                  FROM ",db$schema,".",db$dx," 
                  WHERE member_id IN ('",pts,"') AND standard_disease_code IN ('",dxs,"');")
  data  <- setDT(dbGetQuery(con, query))
  
  if(nrow(data) > 0){
    data$month_and_year_of_medical_care <- ym(data$month_and_year_of_medical_care)
    data <- data[month_and_year_of_medical_care >= defs$minEnrdd & month_and_year_of_medical_care <= defs$maxEnrdd] # only dxs within 74m window
    data <- unique(data) # exclude duplicates (same dx/susp flag in same dd for same member_id )
    data[, record_count := max(seq_len(.N)), by = .(member_id, month_and_year_of_medical_care, standard_disease_code)]
    data[, no_susp_flag := (suspicion_flag == 0)*1]
    data <- data[record_count == 1 | (record_count == 2 & no_susp_flag == 1)] # consider only dx obs with susp.flag == 0 for duplicate records (where everything is the same except one of the records has a suspicion flag == 0 and another a suspicion flag == 1)
    
    ce18Dxs <- rbind(ce18Dxs, data[,.(member_id, date_medical_care = month_and_year_of_medical_care, standard_disease_code, suspicion_flag)])
  }
  rm(data)
  
  end   <- Sys.time()
  print(end - start)
  
  if(i %% 1000 == 0){
    fwrite(ce18Dxs, paste0("Processed Data/temp_bckup_files/ce18Dxs_",i,".txt"))
  }
  
  print(paste0(i, "of", length(pages$max), " rows: ", nrow(ce18Dxs)))
  
}

fwrite(ce18Dxs,paste0("Processed Data/temp_bckup_files/ce18Dxs_",i,".txt"))




# Checks
length(unique(ce18Dxs$member_id)) # 108,425 continuous enrolled 18+ Migraine diagnosed pts
length(unique(ce18Dxs$member_id)) / length(pats$cntEnr18) # 5% of the continuous enrolled 18+ pats are Migraine dxed in the 74m window <=> Prevalence
sum(defs$dxs$standard_disease_code %in% ce18Dxs$standard_disease_code) / nrow(defs$dxs) # 100% of the Migraine dx codes are found in the ce18's records
sum(ce18Dxs$member_id %in% pats$cntEnr18) / nrow(ce18Dxs) # 100%
time_length(interval(defs$minEnrdd, min(ce18Dxs$date_medical_care)), "month") # 0 months
time_length(interval(max(ce18Dxs$date_medical_care), defs$maxEnrym), "month") # 0 months

fwrite(ce18Dxs,"Processed Data/ce18MigDxs.txt")

# Identify Migraine treated continuous enrolled 18+ pts, in the 74m enrollment window, based on Vanguard's selection of drugs to select pts of interest 
# Create table with all Migraine Rxs per patient

# Identify continuous enrolled 18+ patients using Migraine drugs (in the 74m enrollment window)******************

# Drugs of interest to select Migraine patients
drgs <- paste0(defs$med$jmdc_drug_code, collapse = "','")

cntEnr18      <- fread("Processed Data/ContEnr18.txt", integer64 = "character", stringsAsFactors = F)
pats$cntEnr18 <- cntEnr18$member_id

pages <- pagify(pats$cntEnr18, 500)

ce18Rxs <- data.table()




for(i in 1:length(pages$max)){
  pts <- paste0(pats$cntEnr18[pages$min[i]:pages$max[i]], collapse = "','")
  
  start <- Sys.time()
  query <- paste0("SELECT member_id, 
                          month_and_year_of_medical_care, 
                          jmdc_drug_code, 
                          drug_name,
                          date_of_prescription 
                  FROM ",db$schema,".",db$rx," 
                  WHERE member_id IN ('",pts,"') AND jmdc_drug_code IN ('",drgs,"');")
  data  <- setDT(dbGetQuery(con, query))
  
  if(nrow(data) > 0){
    data$month_and_year_of_medical_care <- ym(data$month_and_year_of_medical_care)
    data <- data[month_and_year_of_medical_care >= defs$minEnrdd & month_and_year_of_medical_care <= defs$maxEnrdd] # only rxs within 74m window
    data <- data[,.(nr_scripts = .N), by = member_id:date_of_prescription]
    names(data)[2] <- "date_medical_care"
    
    ce18Rxs <- rbind(ce18Rxs, data)
  }
  rm(data)

  end   <- Sys.time()
  print(end - start)
  
  if(i %% 1000 == 0){
    fwrite(ce18Rxs, paste0("Processed Data/temp_bckup_files/ce18Rxs_",i,".txt"))
  }
  
  print(paste0(i, "of", length(pages$max), " rows: ", nrow(ce18Rxs)))
  
}

fwrite(ce18Rxs,paste0("Processed Data/temp_bckup_files/ce18Rxs_",i,".txt"))

# Checks
length(unique(ce18Rxs$member_id)) # 57,134
sum(defs$med$jmdc_drug_code %in% ce18Rxs$jmdc_drug_code) / nrow(defs$med) # 91% of the Migraine product codes are found in the ce18's records

unique(defs$med$general_name[!defs$med$jmdc_drug_code %in% ce18Rxs$jmdc_drug_code]) # "Zolmitriptan" "Sumatriptan"
temp <- defs$med[general_name == "Zolmitriptan" | general_name == "Sumatriptan"]
temp <- temp[, missing := (!jmdc_drug_code %in% ce18Rxs$jmdc_drug_code)*1]
sum(temp$missing)/nrow(temp) # only 15% of the Zolmitriptan/Sumatriptan drug codes were not found among the ce18+ patients

sum(ce18Rxs$member_id %in% pats$cntEnr18)/nrow(ce18Rxs) # 100%
time_length(interval(defs$minEnrdd, min(ce18Rxs$date_medical_care)), "month") # 0 months
time_length(interval(max(ce18Rxs$date_medical_care), defs$maxEnrym), "month") # 0 months

fwrite(ce18Rxs,"Processed Data/ce18Migrxs.txt")
rm(data, temp)


cntEnr18 <- fread("Processed Data/ContEnr18.txt", integer64 = "character", stringsAsFactors = F)  # continuous enrolled 18+
ce18Dxs  <- fread("Processed Data/ce18MigDxs.txt", integer64 = "character", stringsAsFactors = F) # continuous enrolled 18+ Migraine dxs
ce18Rxs  <- fread("Processed Data/ce18Migrxs.txt", integer64 = "character", stringsAsFactors = F) # continuous enrolled 18+ Migraine rxs




# Identify Migraine patients - Continuous enrolled 18+ pts who are either Migraine diagnosed OR Migraine treated
migpts   <- cntEnr18[, ':='(mig_dx = (member_id %in% ce18Dxs$member_id)*1, mig_rx = (member_id %in% ce18Rxs$member_id)*1)]

sum(migpts$mig_dx) - length(unique(ce18Dxs$member_id)) # 0 -> check ok
sum(migpts$mig_rx) - length(unique(ce18Rxs$member_id)) # 0 -> check ok

migpts[, mig_pts := (mig_dx == 1 | mig_rx == 1)*1]
pats$migpts <- migpts$patid[migpts$mig_pts == 1]

sum(migpts$mig_pts)/nrow(cntEnr18)# 5% of the continuous enrolled 18+ are Migraine patients of interest <=> Prevalence 
sum(migpts$mig_rx[migpts$mig_dx == 0]) / sum(migpts$mig_pts) # 1% of the identified Migraine Pts have just Migraine Rxs (No Migraine Dxs)


# Bring weights to quantify Migraine patients age and gender groups***************************************
ce18w  <- fread("Processed Data/ContEnr18W.txt", integer64 = "character", stringsAsFactors = F)
migpts <- ce18w[,.(member_id, weight)][migpts, on = .(member_id)]
migpts <- migpts[,.(member_id, gender, age, weight, mig_dx, mig_rx, mig_pts)]

sum(migpts$weight[migpts$mig_pts == 1]) # 4,964,824 Migraine projected patients
fwrite(migpts[mig_pts == 1, .(member_id, weight, mig_pts)], "Processed Data/ce18MigPts.txt")

# Aggregated Migraine patients samples by age and gender groups*******************************************
data <- migpts[mig_pts == 1]
data <- data[,.(migPts = .N), by = .(age,gender)][order(age,gender)]

fwrite(data,"Processed Data/MigrainePts_by_Age&Gender.csv")
rm(data)

