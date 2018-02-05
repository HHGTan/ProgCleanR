library(plyr, warn.conflicts = F);library(dplyr, warn.conflicts = F)
library(readxl);library(writexl)

# CLEAN MRI PARTICIPICIPITATION 

# Description
# This is a test and I have no idea
print("Test")

# An extra push
print("push")
print("pull")

# Progeny Query:  (Found at path: Netherlands/MRI_Deelname)
# 'Deelname MRI 3T' equals "Ja"                 OR
# '015 MRI 3T' equals "Y"                       OR
# 'IC MRI 3T getekend op:' is not blank         OR
# 'FollowUp[1-6]_datum_scan' is not blank       
#

# Read data
#df1 <- read_excel("/Users/htan4/Downloads/ssexport-6.xlsx") #voor meral's updates versie
#df1 <- read_excel("/Users/htan4/Downloads/ssexport-12.xlsx") # 29/11/2017
df1 <- read_excel("/Users/htan4/Downloads/ssexport-14.xlsx") # 13/12/2017


# Pre-testing: Are all ALS numbers present?
df1$testID <- ifelse((df1$`Deelname MRI 3T` =="Ja" | df1$`015 MRI 3T`=="Y" | is.na(df1$`IC MRI 3T getekend op:`)) & 
                       is.na(df1$`ALS number`), 1,0)


##############
#PARTICIPATION

#Participation test1: Participation fields should correspond
#do following variables correspond: `Deelname MRI 3T`=="Ja", `015 MRI 3T`=="Y" AND `IC MRI 3T getekend op` != NA
df1$test_part1 <- ifelse( df1$`Deelname MRI 3T` =="Ja" & is.na(df1$`Deelname MRI 3T`)==F & df1$`015 MRI 3T`=="Y" & is.na(df1$`IC MRI 3T getekend op:`) == FALSE, 0, 1)

#Participation test2: Does participate in MRI study, but has a df1$`MRI 3T: Reden weigering` field
df1$test_part2 <- ifelse( df1$test_part1==0 & (is.na(df1$`MRI 3T: Reden weigering`)==F |  is.na(df1$`MRI 3T: Reden weigering anders, namelijk`)==F),
                    1,0)



###########
# MRI SCANS

# MRI test1: There are two progeny fields for the dates of the first MRI scan. If both empty and subject participates, return 1.
df1$test_mri1 <- ifelse(df1$test_part1==0 & is.na(df1$`Datum 1e MRI 3T`) & is.na(df1$FollowUp1_datum_scan),1,0)

# MRI test2: If two fields for date do not match, then return 1. ==> Strive to complete and always use FollowUp1
df1$test_mri2 <- ifelse(df1$`Datum 1e MRI 3T` != df1$FollowUp1_datum_scan & 
                       !is.na(df1$`Datum 1e MRI 3T`) & !is.na(df1$FollowUp1_datum_scan), 1,0)


# MRI test3: check if date of followup MRIs is in order.
df1$test_mri3 <- sapply(1:nrow(df1), function(i){
  dfx <- df1[i,c("ALS number", grep("datum_scan", colnames(df1), value=T))]
  dfx <- as.data.frame(dfx)
  x<- is.unsorted(dfx[1,grep("datum_scan", colnames(dfx))], na.rm=T)
  return(as.numeric(x))
})


# MRI test4: Are there signs of missing data on scans. e.g. if FU2 is not present but FU3 is, then data are missing.
# Compare total number of scans with last scan (date field). If last scan > total number of scans, then data is missing.
df1$test_mri4 <- sapply(1:nrow(df1), function(i){
  dfx <- df1[i, grep("datum_scan", colnames(df1), value=T)]
  total_scans <- length(which(!is.na(dfx)))
  
  #If no scans were made then last scan is set to 0
  if (total_scans == 0){
    last_scan <- 0
  } else {
    last_scan <- max(which(!is.na(dfx)))
  }
  
  out <- as.numeric(last_scan!=total_scans)
  return(out)
})


# MRI test5: For each follow_up, see if all fields are completed.
#Get number of followup fields.
FU_cols <- grep("FollowUp",colnames(df1),value = T)
FU_number <- unique(gsub("(FollowUp[1-9])(.*)", "\\1", FU_cols))

df1$test_mri5 <- sapply(1:nrow(df1), function(i){
  # For each followup, check if all fields are completed.
  out1 <- sapply(FU_number,function(j){
    sumNA<- sum(is.na(df1[i,grep(j,colnames(df1))]))
    ncols <- length(grep(j,colnames(df1)))
    testperFU <- ifelse(sumNA==ncols | sumNA ==0, 0, 1)
  return(testperFU)
  })
  # If number of errors is not 0, then 1. 
  out2 <- ifelse(sum(out1)==0,0,1)
  return(out2)
})


##################
# EXCLUSION FIELDS

#Exclusion test1: check if date of exclusion is never sooner than the date of the last MRI scan.
# If there are absolutely no scan dates (all NAs), last_scan_date will give a warning, but does (correctly) result in NA.
df1$test_excl1 <- sapply(1:nrow(df1), function(i){
  dfx <- df1[i,c("ALS number","MRI 3T: Exclusie datum",grep("datum_scan", colnames(df1), value=T))]
  last_scan_date <- max(t(dfx[1,grep("datum_scan", colnames(dfx))]), na.rm=T)
  exclusion_date <- dfx[1,"MRI 3T: Exclusie datum"]
  compdate <- (last_scan_date > exclusion_date)
  res <- ifelse(is.na(compdate), 0, ifelse(compdate==T, 1, 0))
  return(res)
})

#Exclusion test2: If there is a "MRI 3T: Exclusie anders, namelijk", then there should always be a "MRI 3T: Exclusie reden" 
df1$test_excl2 <- ifelse( is.na(df1$`MRI 3T: Exclusie reden`) & !is.na(df1$`MRI 3T: Exclusie anders, namelijk`),1,0)

#Exclusion test3 : If either one of "MRI 3T: Exclusie datum" and "MRI 3T: Exclusie reden" are filled in, then the other one should be as well.
#Note: There are a lot of exclusions without a date
df1$test_excl3 <- ifelse( is.na(df1$`MRI 3T: Exclusie datum`) != is.na(df1$`MRI 3T: Exclusie reden`),1,0 )




#################
#INFORMED CONSENT

#IC test1: All 4 IC fields should either be filled in or all absent.
df1$test_ic1 <-apply(is.na(df1[,grep("IC MRI 3T",colnames(df1), value=T)]),1, function(k){
  error <- ifelse(sum(k)==0 | sum(k)==4,0,1)
  return(error)
})



########
#SUMMARY

#Per subject
df1$totaltest <- apply(df1[,grep("^test",colnames(df1),value=T)],1,sum)
library(Hmisc, warn.conflicts = F)
describe(df1$totaltest)
detach("package:Hmisc",unload=T)

#Per test
colSums(df1[,grep("^test",colnames(df1),value=T)])


#Write to excel file.
#write_xlsx(df1,path = paste0("~/Documents/MRI_clean",format(Sys.Date(), "%Y%m%d"),".xlsx"), col_names = T)
