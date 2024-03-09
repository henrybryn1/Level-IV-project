library(haven)
#library(tidyverse)
library(sjmisc)
#library(utilities)

#Womens ind Questionnaire
IRdat = read_sav("C:/Users/Henry Jones/OneDrive/Documents/Durham Maths work/Fourth year/Project IV/HIV Data/ZMIR51FL.SAV")

IRdf <- as.data.frame(IRdat)

print(toupper("v001 v002 v003 v005 v012 v013 v015 v028 v190 v133 v026 v501 v502 v511 v525 v531 v761 v766a v766b v101 v130 v131 v751 v775 v778 v781 v763a v763b v763c slangi s1012a v463a v463b v463c  v844 v845 v846 v840 v843 v785"))

IRdf$IID <- IRdf$V001*100000 + IRdf$V002*100 + IRdf$V003

IRdf <- subset(IRdf, select = c("IID", "V001", "V002", "V003", "V005", "V012", "V013", "V015", "V028", "V190", "V133", "V026", "V501", "V502", "V511", "V525", "V531", "V761", "V766A", "V766B", "V101", "V130", "V131", "V751", "V775", "V778", "V781", "V763A", "V763B", "V763C", "SLANGI", "S1012A", "V463A", "V463B", "V463C", "V844", "V845", "V846", "V840", "V843", "V785"))


#Mens ind Questionnaire
MRdat = read_sav("C:/Users/Henry Jones/OneDrive/Documents/Durham Maths work/Fourth year/Project IV/HIV Data/ZMMR51FL.SAV")

MRdf <- as.data.frame(MRdat)

print(toupper("IID mv001 mv002 mv003 mv005 mv012 mv013 mv015 mv028 mv190 mv133 mv026 mv501 mv502 mv511 mv525 mv531 mv761 mv766a mv766b mv101 mv130 mv131 mv483 mv751 mv775 mv778 mv781 mv763a mv763b mv763c mv767a mv767b mv767c mv793 smlangi sm812 mv463a mv463b mv463c mv785 mv844 mv845 mv846"))

MRdf$IID <- MRdf$MV001*100000 + MRdf$MV002*100 + MRdf$MV003

MRdf <- subset(MRdf, select = c("IID", "MV001", "MV002", "MV003", "MV005", "MV012", "MV013", "MV015", "MV028", "MV190", "MV133", "MV026", "MV501", "MV502", "MV511", "MV525", "MV531", "MV761", "MV766A", "MV766B", "MV101", "MV130", "MV131", "MV483", "MV751", "MV775", "MV778", "MV781", "MV763A", "MV763B", "MV763C", "MV767A", "MV767B", "MV767C", "MV793", "SMLANGI", "SM812", "MV463A", "MV463B", "MV463C", "MV785", "MV844", "MV845", "MV846"))

#Household Questionnaire

PRdat = read_sav("C:/Users/Henry Jones/OneDrive/Documents/Durham Maths work/Fourth year/Project IV/HIV Data/ZMPR51FL.SAV")

PRdf <- as.data.frame(PRdat)

print(toupper("IID hv001 hv002 hvidx hv003 hv005 hv006 hv016 hv017 hv018 hv021 hv022 hv024 hv025 hv026 hv030 hv103 hv104 hv105 hv106 hv107 hv108 hv117 hv118 hv270 hv271 hml16 ha61 ha63 ha64 hb61 hb63 hb64"))

PRdf$IID <- PRdf$HV001*100000 + PRdf$HV002*100 + PRdf$HVIDX

PRdf <- subset(PRdf, select = c("IID", "HV001", "HV002", "HVIDX", "HV003", "HV005", "HV006", "HV016", "HV017", "HV018", "HV021", "HV022", "HV024", "HV025", "HV026", "HV030", "HV103", "HV104", "HV105", "HV106", "HV107", "HV108", "HV117", "HV118", "HV270", "HV271", "HML16", "HA61", "HA63", "HA64", "HB61", "HB63", "HB64"))



#HIV test results

ARdat = read_sav("C:/Users/Henry Jones/OneDrive/Documents/Durham Maths work/Fourth year/Project IV/HIV Data/ZMAR51FL.SAV")

ARdf <- as.data.frame(ARdat)

ARdf$IID <- ARdf$HIVCLUST*100000 + ARdf$HIVNUMB*100 + ARdf$HIVLINE



which(!(MRdf$IID %in% IRdf$IID))

#Merge men with women
merge_mf <- merge(MRdf, IRdf, by = "IID", all = TRUE)
dim(merge_mf)
merged_mf <- merge_mf[order(merge_mf$IID), ]

# Merge with household
merged_household <- merge(merge_mf, PRdf, by = "IID", all = TRUE)
merged_household <- merged_household[order(merged_household$IID), ]

# Merge with zmar51fl_m.dta
ZM_master <- merge(merged_household, ARdf, by = "IID", all = TRUE)

dim(ZM_master)

#dim(IRdf) + dim(MRdf) + dim(PRdf) + dim(ARdf)
which(duplicated(ZM_master$IID))

colnames(ZM_master)[which(names(ZM_master) == "HV104")] <- "sex"

ZM_master$hhweight <- ZM_master$HV005/1000000
ZM_master$hivweight <- ZM_master$HIV05/1000000
ZM_master$wiweight <- ZM_master$V005/1000000
ZM_master$miweight <- ZM_master$MV005/1000000

ZM_master$universe <- ifelse(ZM_master$HV117 == 1 | ZM_master$HV118 == 1, 1, 0)


table(ZM_master$universe, ZM_master$sex)


ZM_master$hivconsent <- ifelse((ZM_master$HIV03 == 1 | ZM_master$HIV03 == 0) & ZM_master$universe == 1, 1, 0)
ZM_master$hivconsent <- ifelse((ZM_master$HIV03 != 1 & ZM_master$HIV03!= 0)|is.na(ZM_master$HIV03)  & ZM_master$universe == 1, 0, ZM_master$hivconsent)

ZM_master$interview <- ifelse((ZM_master$V015 == 1 | ZM_master$MV015 == 1) & ZM_master$universe == 1, 1, 0)

ZM_master$hiv <- ifelse(ZM_master$HIV03 == 1 & ZM_master$universe == 1, 1, 0)

ZM_master$urban <- ifelse(ZM_master$HV025 == 1, 1, 0)

ZM_master$education <- ZM_master$HV108

ZM_master$education[which(ZM_master$education %in% c(98,99))] <- NA
                          
table(ZM_master$HV106[ZM_master$universe == 1], ZM_master$education[ZM_master$universe == 1])

ZM_master$age <- ZM_master$HV105

#age > 60 listed as NA
ZM_master$agecat5 <- cut(ZM_master$age, breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60),
                         labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-60"))

ZM_master$wealthcat <- ZM_master$HV270

ZM_master$location <- ZM_master$HV026

colnames(ZM_master)[which(names(ZM_master) == "HV024")] <- "region"

ZM_master$marital <- ifelse(ZM_master$sex == 2, ZM_master$V502, ZM_master$MV502)
attributes(ZM_master$marital)$labels <- attributes(ZM_master$V502)$labels

table(ZM_master$marital[ZM_master$interview == 1], ZM_master$sex[ZM_master$interview == 1], useNA = "always")

ZM_master$wstd <- ifelse(ZM_master$V763A == 1 | ZM_master$V763B == 1 | ZM_master$V763C == 1, 1, NA)
ZM_master$mstd <- ifelse(ZM_master$V763A == 0 & ZM_master$V763B == 0 & ZM_master$V763C == 0, 0, ZM_master$wstd)
ZM_master$wstd[ZM_master$V763A==0 & (ZM_master$V763B !=1 & ZM_master$V763C!=1)] <- 0


ZM_master$mstd <- ifelse(ZM_master$MV763A == 1 | ZM_master$MV763B == 1 | ZM_master$MV763C == 1, 1, NA)
ZM_master$mstd <- ifelse(ZM_master$MV763A == 0 & ZM_master$MV763B == 0 & ZM_master$MV763C == 0, 0, ZM_master$mstd)
ZM_master$mstd[ZM_master$MV763A==0 & (ZM_master$V763A !=1 & ZM_master$V763C!=1)] <- 0

ZM_master$std <- ifelse(ZM_master$sex == 2, ZM_master$wstd, ZM_master$mstd)

stdlab <- 0:1
names(stdlab) <- c("nostd", "std")
attributes(ZM_master$std)$labels <- stdlab


table(ZM_master$std[ZM_master$interview == 1], ZM_master$sex[ZM_master$interview == 1], useNA = "always")



# Generate weversex variable
ZM_master$weversex <- ifelse(ZM_master$V525 > 0 & ZM_master$V525 != 98 & ZM_master$V525 != 99 & !is.na(ZM_master$V525), 1, 0)


# Generate meversex variable
ZM_master$meversex <- ifelse(ZM_master$MV525 > 0 & ZM_master$MV525 != 98 & ZM_master$MV525 != 99 & !is.na(ZM_master$MV525), 1, 0)


# Generate eversex variable
ZM_master$eversex <- ifelse(ZM_master$sex == 2, ZM_master$weversex, ZM_master$meversex)
ZM_master$eversex[which((ZM_master$MV766B > 0 & ZM_master$MV766B < 98) | (ZM_master$V766B > 0 & ZM_master$V766B < 98))] <- 1

# Generate wage1sex variable
ZM_master$wage1sex <- ZM_master$V525
ZM_master$wage1sex[which(ZM_master$V525 == 96)] <- ZM_master$V511[which(ZM_master$V525 == 96)]
ZM_master$wage1sex[which(ZM_master$v525 == 99 | ZM_master$v525 == 98)] <- NA

# Generate wage1sex_cat variable
ZM_master$wage1sex_cat <- NA
ZM_master$wage1sex_cat[which(ZM_master$wage1sex == 0)] <- 1
ZM_master$wage1sex_cat[which(ZM_master$wage1sex <= 15 & ZM_master$wage1sex > 0)] <- 2
ZM_master$wage1sex_cat[which(ZM_master$wage1sex > 15 & !is.na(ZM_master$wage1sex))] <- 3

# Generate mage1sex variable
ZM_master$mage1sex <- ZM_master$MV525
ZM_master$mage1sex[which(ZM_master$MV525 == 96)] <- ZM_master$MV511[which(ZM_master$MV525 == 96)]
ZM_master$mage1sex[which(ZM_master$mage1sex == 99 | ZM_master$mage1sex == 98)] <- NA

# Generate mage1sex_cat variable
ZM_master$mage1sex_cat <- NA
ZM_master$mage1sex_cat[ZM_master$mage1sex == 0] <- 1
ZM_master$mage1sex_cat[which(ZM_master$mage1sex <= 15 & ZM_master$mage1sex > 0)] <- 2
ZM_master$mage1sex_cat[which(ZM_master$mage1sex > 15 & !is.na(ZM_master$mage1sex))] <- 3
ZM_master$mage1sex[which(ZM_master$mage1sex == 99)] <- NA

# Generate age1sex_cat variable
ZM_master$age1sex_cat <- ifelse(ZM_master$sex == 1, ZM_master$mage1sex_cat, ZM_master$wage1sex_cat)
ZM_master$age1sex_cat[which(ZM_master$sex == 2)] <- ZM_master$wage1sex_cat[which(ZM_master$sex == 2)]


newlab <- 1:3
names(newlab) <- c("nosex", "under16", "over16")
attributes(ZM_master$age1sex_cat)$labels <- newlab

# Generate wsex12month variable
ZM_master$wsex12month <- NA
ZM_master$wsex12month[which(ZM_master$V766B > 0 & ZM_master$V766B != 98 & ZM_master$V766B != 99 & !is.na(ZM_master$V766B))] <- 1
ZM_master$wsex12month[ZM_master$V766B == 0] <- 0

# Generate msex12month variable
ZM_master$msex12month <- NA
ZM_master$msex12month[which(ZM_master$MV766B > 0 & ZM_master$MV766b != 98 & ZM_master$MV766b != 99 & !is.na(ZM_master$MV766b))] <- 1
ZM_master$msex12month[ZM_master$MV766b == 0] <- 0

# Generate sex12month variable
ZM_master$sex12month <- ifelse(ZM_master$sex == 2, ZM_master$wsex12month, ZM_master$msex12month)

# Generate wcondom variable
ZM_master$wcondom <- ifelse(ZM_master$wsex12month == 0, 0, NA)
ZM_master$wcondom[ZM_master$V761 == 0] <- 0
ZM_master$wcondom[ZM_master$V761 == 1] <- 1

# Generate mcondom variable
ZM_master$mcondom <- ifelse(ZM_master$msex12month == 0, 0, NA)
ZM_master$mcondom[ZM_master$MV761 == 0] <- 0
ZM_master$mcondom[ZM_master$MV761 == 1] <- 1

# Generate condom variable
ZM_master$condom <- ifelse(ZM_master$sex == 2, ZM_master$wcondom, ZM_master$mcondom)


newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(ZM_master$condom)$labels <- newlab

# Generate whighhiv variable
ZM_master$whighhiv <- ifelse(ZM_master$V766A > 0 & ZM_master$V766A < 98, 1, NA)
ZM_master$whighhiv[ZM_master$V766A == 0] <- 0

# Generate mhighhiv variable
ZM_master$mhighhiv <- NA
ZM_master$mhighhiv[which(ZM_master$MV766A > 0 & ZM_master$MV766A < 98)] <-  1
ZM_master$mhighhiv[ZM_master$MV766A == 0] <- 0

# Generate highhiv variable
ZM_master$highhiv <- ifelse(ZM_master$sex == 2, ZM_master$whighhiv, ZM_master$mhighhiv)

newlab <- 0:1
names(newlab) <- c("lhivrisk", "hhivrisk")
attributes(ZM_master$highhiv)$labels <- newlab

# Generate mpartner variable
ZM_master$mpartner <- NA
ZM_master$mpartner[which(ZM_master$MV766B != 98)] <- ZM_master$MV766B[which(ZM_master$MV766B != 98)]
ZM_master$mpartner[which(ZM_master$MV766B >= 2 & ZM_master$MV766B < 98)] <- 2

# Generate wpartner variable
ZM_master$wpartner <- NA
ZM_master$wpartner[which(ZM_master$V766B != 98)] <- ZM_master$V766B[which(ZM_master$V766B != 98)]
ZM_master$wpartner[which(ZM_master$V766B >= 2 & ZM_master$V766B < 98)] <- 2

# Generate partner variable
ZM_master$partner <- ifelse(ZM_master$sex == 2, ZM_master$wpartner, ZM_master$mpartner)
ZM_master$partner[ZM_master$partner == 99] <- NA


newlab <- 0:2
names(newlab) <- c("nopartner", "1partner", "2+partner")
attributes(ZM_master$partner)$labels <- newlab

# Display table for partner
table(ZM_master$partner[ZM_master$interview == 1], ZM_master$sex[ZM_master$interview == 1], useNA = "always")



ZM_master$knowhiv <- NA
ZM_master$knowhiv[which(ZM_master$V751 == 1 & ZM_master$sex == 2)] <- 1
ZM_master$knowhiv[which(ZM_master$V751 == 0 & ZM_master$sex == 2)] <- 0
ZM_master$knowhiv[which(ZM_master$MV751 == 1 & ZM_master$sex == 1)] <- 1
ZM_master$knowhiv[which(ZM_master$MV751 == 0 & ZM_master$sex == 1)] <- 0

# Display table for knowhiv
table(ZM_master$knowhiv[ZM_master$interview == 1], ZM_master$sex[ZM_master$interview == 1], useNA = "always")

# Knows someone who has or died of AIDS
ZM_master$wknowsdiedofaids <- ZM_master$V775
ZM_master$wknowsdiedofaids[which(ZM_master$wknowsdiedofaids == 9)] <- NA
ZM_master$wknowsdiedofaids[which(ZM_master$knowhiv == 0 & is.na(ZM_master$wknowsdiedofaids))] <- 0
ZM_master$wknowsdiedofaids[which(ZM_master$V844 == 2 & is.na(ZM_master$wknowsdiedofaids))] <- 0
ZM_master$wknowsdiedofaids[which((ZM_master$V844 == 1 | ZM_master$V845 == 1 | ZM_master$V846 == 1) &  is.na(ZM_master$wknowsdiedofaids))] <- 1

ZM_master$mknowsdiedofaids <- ZM_master$MV775
ZM_master$mknowsdiedofaids[which(ZM_master$mknowsdiedofaids == 9)] <- NA
ZM_master$mknowsdiedofaids[which(ZM_master$knowhiv == 0 & is.na(ZM_master$mknowsdiedofaids))] <- 0
ZM_master$mknowsdiedofaids[which(ZM_master$MV844 == 2 & is.na(ZM_master$mknowsdiedofaids))] <- 0
ZM_master$mknowsdiedofaids[which((ZM_master$MV844 == 1 | ZM_master$MV845 == 1 | ZM_master$MV846 == 1) &  is.na(ZM_master$mknowsdiedofaids))] <- 1


ZM_master$knowsdiedofaids <- ifelse(ZM_master$sex == 2, ZM_master$wknowsdiedofaids, ZM_master$mknowsdiedofaids)


newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(ZM_master$knowsdiedofaids)$labels <- newlab

# Would care for relative with AIDS

ZM_master$waidscare <- ZM_master$V778
ZM_master$waidscare[which(ZM_master$waidscare == 9)] <- NA
ZM_master$waidscare[which(ZM_master$waidscare == 8)] <- 0

ZM_master$maidscare <- ZM_master$MV778
ZM_master$maidscare[which(ZM_master$maidscare == 9)] <- NA
ZM_master$maidscare[which(ZM_master$maidscare == 8)] <- 0
ZM_master$aidscare <- ifelse(ZM_master$sex == 2, ZM_master$waidscare, ZM_master$maidscare)

newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(ZM_master$aidscare)$labels <- newlab

# Ever tested HIV
ZM_master$wevertestedHIV <- ifelse(ZM_master$V781 == 1, 1,
                                   ifelse(ZM_master$V781 == 0, 0, NA))
ZM_master$wevertestedHIV[ZM_master$V80 == 1 & is.na(ZM_master$wevertestedHIV)] <- 1


ZM_master$mevertestedHIV <- ifelse(ZM_master$MV781 == 1, 1,
                                   ifelse(ZM_master$MV781 == 0, 0, NA))



ZM_master$evertestedHIV <- ifelse(ZM_master$sex == 2, ZM_master$wevertestedHIV, ZM_master$mevertestedHIV)


ZM_master$evertestedHIV[which(is.na(ZM_master$evertestedHIV) & ZM_master$knowhiv == 0)] <- 0

newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(ZM_master$evertestedHIV)$labels <- newlab


# Alcohol
ZM_master$walcohol <- ifelse(ZM_master$S1012A == 1 & ZM_master$sex == 2 & ZM_master$universe == 1, 1,
                             ifelse(ZM_master$S1012A == 0 & ZM_master$sex == 2 & ZM_master$universe == 1, 0, NA))

ZM_master$malcohol <- ifelse(ZM_master$SM812 == 1 & ZM_master$sex == 1 & ZM_master$universe == 1, 1,
                             ifelse(ZM_master$SM812 == 0 & ZM_master$sex == 1 & ZM_master$universe == 1, 0, NA))

ZM_master$alcohol <- ifelse(ZM_master$sex == 2, ZM_master$walcohol, ZM_master$malcohol)

newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(ZM_master$alcohol)$labels <- newlab


# Smoking
ZM_master$wsmoke <- ifelse(ZM_master$V463A == 1 | ZM_master$V463B == 1 | ZM_master$V463C == 1, 1, NA)
ZM_master$wsmoke[which(ZM_master$V463A == 0 & ZM_master$V463B == 0 & ZM_master$V463C == 0)] <- 0

ZM_master$msmoke <- ifelse(ZM_master$MV463A == 1 | ZM_master$MV463B == 1 | ZM_master$MV463C == 1, 1, NA)
ZM_master$msmoke[which(ZM_master$MV463A == 0 & ZM_master$MV463B == 0 & ZM_master$MV463C == 0)] <- 0

ZM_master$smoke <- ifelse(ZM_master$sex == 2, ZM_master$wsmoke, ZM_master$msmoke)

newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(ZM_master$smoke)$labels <- newlab


# Ethnicity
ZM_master$ethnicity <- ZM_master$V131
ZM_master$ethnicity[which(is.na(ZM_master$ethnicity))] <- ZM_master$MV131[which(is.na(ZM_master$ethnicity))]

for (i in 1:67) {
  count <- length(which(ZM_master$ethnicity == i & ZM_master$universe == 1))
  if(count < 200){
    ZM_master$ethnicity[ZM_master$ethnicity == i] <- 99
  }
}

# Display table for ethnicity
#table(ZM_master$ethnicity[ZM_master$interview == 1], ZM_master$sex[ZM_master$interview == 1], useNA = "always")

# Religion
ZM_master$religion <- ifelse(ZM_master$sex == 2, ZM_master$V130, ZM_master$MV130)
ZM_master$religion[which(ZM_master$religion == 96)] <- 3
ZM_master$religion[which(ZM_master$religion == 99)] <- NA

newlab <- 1:3
names(newlab) <- c("Catholic", "Protestant", "Other")
attributes(ZM_master$religion)$labels <- newlab


# Language
ZM_master$mlanguage <- ZM_master$SMLANGI
ZM_master$mlanguage[which(ZM_master$mlanguage == 6 | ZM_master$mlanguage == 3 | ZM_master$mlanguage == 5)] <- 9

ZM_master$wlanguage <- ZM_master$SLANGI
ZM_master$wlanguage[which(ZM_master$wlanguage == 6 | ZM_master$wlanguage == 3 | ZM_master$wlanguage == 5)] <- 9

ZM_master$language <- ifelse(ZM_master$sex == 1, ZM_master$mlanguage, ZM_master$wlanguage)

unique(ZM_master$language)

newlab <- c(1,2,4,7,8,9)
names(newlab) <- c("English", "Bemba", "Lozi", "Nyanja", "Tonga", "Other")
attributes(ZM_master$language)$labels <- newlab


# Survey strata
ZM_master$stratum <- interaction(ZM_master$region, ZM_master$urban)


# Creating Interviewer IDs
ZM_master$interviewerID <- ifelse(ZM_master$sex == 1 & ZM_master$universe == 1, ZM_master$MV028, NA)
ZM_master$interviewerID <- ifelse(ZM_master$sex == 2 & ZM_master$universe == 1, ZM_master$V028, ZM_master$interviewerID)

ZM_master$interviewerID_50w <- ifelse(ZM_master$sex == 2, ZM_master$interviewerID, NA)
ZM_master$interviewerID_50m <- ifelse(ZM_master$sex == 1, ZM_master$interviewerID, NA)

# Creating Household Interviewer IDs
ZM_master$hhinterviewerID <- ifelse(ZM_master$universe == 1, ZM_master$HV018, NA)

# Filtering Household and Individual Questionnaires
ZM_master$hhinterviewerID_50w <- ifelse(!is.na(ZM_master$agecat5) & !is.na(ZM_master$education) & !is.na(ZM_master$wealthcat) & !is.na(ZM_master$region) & !is.na(ZM_master$location) & ZM_master$universe == 1 & ZM_master$sex == 2 & !is.na(ZM_master$hivconsent), ZM_master$hhinterviewerID, NA)
ZM_master$hhinterviewerID_50m <- ifelse(!is.na(ZM_master$agecat5) & !is.na(ZM_master$education) & !is.na(ZM_master$wealthcat) & !is.na(ZM_master$region) & !is.na(ZM_master$location) & ZM_master$universe == 1 & ZM_master$sex == 1 & !is.na(ZM_master$hivconsent), ZM_master$hhinterviewerID, NA)

# Fieldworker Handling
for (i in 1:999) {
  count <- length(which(ZM_master$interviewerID_50m == i))
  ZM_master$interviewerID_50m <- ifelse(ZM_master$interviewerID_50m == i & count < 50, 1000, ZM_master$interviewerID_50m)
  count <- length(which(ZM_master$interviewerID_50w == i))
  ZM_master$interviewerID_50w <- ifelse(ZM_master$interviewerID_50w == i & count < 50, 1000, ZM_master$interviewerID_50w)

  count <- length(which(ZM_master$hhinterviewerID_50m == i))
  ZM_master$hhinterviewerID_50m <- ifelse(ZM_master$hhinterviewerID_50m == i & count < 50, 1000, ZM_master$hhinterviewerID_50m)
  count <- length(which(ZM_master$hhinterviewerID_50w == i))
  ZM_master$hhinterviewerID_50w <- ifelse(ZM_master$hhinterviewerID_50w == i & count < 50, 1000, ZM_master$hhinterviewerID_50w)
  
}
ZM_master$interviewerID <- ifelse(ZM_master$sex == 2, ZM_master$interviewerID_50w, ZM_master$interviewerID_50m)

#########################################################################
ZM_master <- ZM_master[order(ZM_master$HV001, ZM_master$HV006),]


# Generating the First Month Variable

ZM_master$firstmonth <- NA
for(i in 1:length(unique(ZM_master$HV001))){
  ZM_master$firstmonth <- ifelse(ZM_master$HV001 == i, min(ZM_master$HV006[ZM_master$HV001 == i]), ZM_master$firstmonth)
}


# Calculating the Day of Successful Household Visit
ZM_master$day <- ifelse(ZM_master$firstmonth == 4, ZM_master$HV006 * 30 + ZM_master$HV016,
                        ifelse(ZM_master$firstmonth == 5, ZM_master$HV006 * 31 + ZM_master$HV016,
                               ifelse(ZM_master$firstmonth == 6, ZM_master$HV006 * 30 + ZM_master$HV016,
                                      ifelse(ZM_master$firstmonth == 7, ZM_master$HV006 * 31 + ZM_master$HV016,
                                             ifelse(ZM_master$firstmonth == 8, ZM_master$HV006 * 31 + ZM_master$HV016,
                                                    ifelse(ZM_master$firstmonth == 9, ZM_master$HV006 * 30 + ZM_master$HV016,
                                                           ifelse(ZM_master$firstmonth == 10, ZM_master$HV006 * 31 + ZM_master$HV016, NA)))))))
#dayrank ignored
# Generating Day Rank Position
#ZM_master <- ZM_master[order(ZM_master$HV001, ZM_master$day),]
#ZM_master$dayrankoverall <- interaction(ZM_master$HV001, ZM_master$day)
# Creating an Overall Day Rank Variable
#for (i in 2:320) {
#  ZM_master$dayrankoverall <- ifelse(is.na(ZM_master$dayrankoverall), ZM_master[[paste0("dayrank_", i)]], ZM_master$dayrankoverall)
#}
#ZM_master <- ZM_master[, -grep("^dayrank_", names(ZM_master))]
# Calculating Relative Day Rank and Determining First Day
#ZM_master$dayrankmax <- ave(ZM_master$dayrankoverall, ZM_master$hv001, FUN = function(x) tail(x, 1))
#ZM_master$dayrankrelative <- ZM_master$dayrankmax - ZM_master$dayrankoverall + ZM_master$hv017
#ZM_master$firstday <- ifelse(ZM_master$dayrankmax <= ZM_master$dayrankrelative, 1, 0)



ind_vars_m <- ZM_master[,c("agecat5", "education", "wealthcat", "location", "region", "marital", "std", "age1sex_cat", "highhiv", "partner", "condom", "aidscare", "knowsdiedofaids", "evertestedHIV", "smoke", "alcohol", "religion", "ethnicity", "language")]

reg_ind_vars_m <- ind_vars_m[,c("agecat5", "wealthcat", "location", "region", "marital", "age1sex_cat", "partner", "religion", "ethnicity", "language")]
reg_ind_vars_m <- to_dummy(reg_ind_vars_m, var.name = "name", suffix = "label")

reg_ind_vars_m[,c("education", "std", "highhiv", "condom", "aidscare", "knowsdiedofaids", "evertestedHIV", "smoke", "alcohol")] <- ind_vars_m[,c("education", "std", "highhiv", "condom", "aidscare", "knowsdiedofaids", "evertestedHIV", "smoke", "alcohol")]


ind_vars_w <- ZM_master[,c("agecat5", "education", "wealthcat", "location", "region", "marital", "std", "age1sex_cat", "highhiv", "partner", "condom", "aidscare", "knowsdiedofaids", "evertestedHIV", "smoke", "alcohol", "religion", "ethnicity", "language")]

reg_ind_vars_w <- ind_vars_w[,c("agecat5", "wealthcat", "location", "region", "marital", "age1sex_cat", "partner", "religion", "ethnicity", "language")]
reg_ind_vars_w <- to_dummy(reg_ind_vars_m, var.name = "name", suffix = "label")

reg_ind_vars_w[,c("education", "std", "highhiv", "condom", "aidscare", "knowsdiedofaids", "evertestedHIV", "smoke", "alcohol")] <- ind_vars_m[,c("education", "std", "highhiv", "condom", "aidscare", "knowsdiedofaids", "evertestedHIV", "smoke", "alcohol")]


hh_vars <- ZM_master[,c("agecat5", "education", "wealthcat", "region", "location")]
reg_hh_vars <- hh_vars[,c("agecat5", "wealthcat", "region", "location")]
reg_hh_vars <- to_dummy(reg_hh_vars, var.name = "name", suffix = "label")

reg_hh_vars$education <- hh_vars$education

##very slow, improve with ifelse
m_ind_nmiss <- rep(NA, dim(ind_vars_m)[1])
for (i in 1:length(m_ind_nmiss)){
  if(ZM_master$sex[i] == 1){m_ind_nmiss[i] <- length(which(is.na(ind_vars_m[i,])))}
}


ZM_master$tot_ind_m <- ifelse(ZM_master$universe == 1 & ZM_master$interview == 1 & ZM_master$sex == 1 & !is.na(ZM_master$hivconsent) & m_ind_nmiss == 0 & !is.na(ZM_master$interviewerID), 1, NA)

ZM_master$tot_ind_m_hiv <- ifelse(ZM_master$tot_ind_m == 1 & !is.na(ZM_master$hiv), 1, NA)

length(which(!is.na(ZM_master$tot_ind_m_hiv)))

ZM_master$tot_ind_m_nonhiv <- ifelse(ZM_master$tot_ind_m == 1 & is.na(ZM_master$hiv), 1, NA)

length(which(!is.na(ZM_master$tot_ind_m_nonhiv)))


w_ind_nmiss <- rep(NA, dim(ind_vars_w)[1])
for (i in 1:length(w_ind_nmiss)){
  if(ZM_master$sex[i] == 2){w_ind_nmiss[i] <- length(which(is.na(ind_vars_w[i,])))}
}


ZM_master$tot_ind_w <- ifelse(ZM_master$universe == 1 & ZM_master$interview == 1 & ZM_master$sex == 2 & !is.na(ZM_master$hivconsent) & w_ind_nmiss == 0 & !is.na(ZM_master$interviewerID), 1, NA)

ZM_master$tot_ind_w_hiv <- ifelse(ZM_master$tot_ind_w == 1 & !is.na(ZM_master$hiv), 1, NA)

length(which(!is.na(ZM_master$tot_ind_w_hiv)))

ZM_master$tot_ind_w_nonhiv <- ifelse(ZM_master$tot_ind_w == 1 & is.na(ZM_master$hiv), 1, NA)

length(which(!is.na(ZM_master$tot_ind_w_nonhiv)))

length(which(ZM_master$universe == 1 & ZM_master$sex == 1))


ZM_master$hh_nmiss <- rowSums(is.na(hh_vars))

# #Household interview sample: men, total
# #ZM_master$tot_hh_m <- ifelse(ZM_master$universe == 1 & ZM_master$sex == 1 & ZM_master$hh_nmiss == 0 & !is.na(ZM_master$hivconsent) &
#                           ZM_master$hv103 != 9 & !is.na(ZM_master$firstday) & !is.na(ZM_master$hhinterviewerID), 1, 0)
# 
# # Household interview sample: men, HIV status available
# ZM_master$tot_hh_m_hiv <- ifelse(ZM_master$tot_hh_m == 1 & !is.na(ZM_master$hiv), 1, 0)
# 
# # Household interview sample: men, NO HIV status, NO interview
# ZM_master$tot_hh_m_nonhiv_nonind <- ifelse(ZM_master$tot_hh_m == 1 & is.na(ZM_master$hiv) & ZM_master$tot_ind_m != 1, 1, 0)
# 
# # Labeling variables
# label(ZM_master$tot_hh_m) <- "Household interview sample: men, total"
# label(ZM_master$tot_hh_m_hiv) <- "Household interview sample: men, HIV status available"
# label(ZM_master$tot_hh_m_nonhiv_nonind) <- "Household interview sample: men, NO HIV status, NO interview"
# 
# # Tabulating variables
# table(ZM_master$tot_hh_m)
# table(ZM_master$tot_hh_m_hiv)
# table(ZM_master$tot_hh_m_nonhiv_nonind)
# 
# # Household interview sample: women, total
# ZM_master$tot_hh_w <- ifelse(ZM_master$universe == 1 & ZM_master$sex == 2 & ZM_master$hh_nmiss == 0 & !is.na(ZM_master$hivconsent) &
#                           ZM_master$hv103 != 9 & !is.na(ZM_master$firstday) & !is.na(ZM_master$hhinterviewerID), 1, 0)
# 
# # Household interview sample: women, HIV status available
# ZM_master$tot_hh_w_hiv <- ifelse(ZM_master$tot_hh_w == 1 & !is.na(ZM_master$hiv), 1, 0)
# 
# # Household interview sample: women, NO HIV status, NO interview
# ZM_master$tot_hh_w_nonhiv_nonind <- ifelse(ZM_master$tot_hh_w == 1 & is.na(ZM_master$hiv) & ZM_master$tot_ind_w != 1, 1, 0)
# 
# # Labeling variables
# label(ZM_master$tot_hh_w) <- "Household interview sample: women, total"
# label(ZM_master$tot_hh_w_hiv) <- "Household interview sample: women, HIV status available"
# label(ZM_master$tot_hh_w_nonhiv_nonind) <- "Household interview sample: women, NO HIV status, NO interview"
# 
# # Tabulating variables
# table(ZM_master$tot_hh_w)
# table(ZM_master$tot_hh_w_hiv)
# table(ZM_master$tot_hh_w_nonhiv_nonind)
# 
# 
# 
# 
# names(ZM_master)

attributes(ZM_master$marital)$labels <- attributes(ZM_master$V502)$labels
attributes(ZM_master$std)$labels <- stdlab
newlab <- 1:3
names(newlab) <- c("nosex", "under16", "over16")
attributes(ZM_master$age1sex_cat)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(ZM_master$condom)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("lhivrisk", "hhivrisk")
attributes(ZM_master$highhiv)$labels <- newlab
newlab <- 0:2
names(newlab) <- c("nopartner", "1partner", "2+partner")
attributes(ZM_master$partner)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(ZM_master$knowsdiedofaids)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(ZM_master$aidscare)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(ZM_master$evertestedHIV)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(ZM_master$alcohol)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(ZM_master$smoke)$labels <- newlab
newlab <- 1:3
names(newlab) <- c("Catholic", "Protestant", "Other")
attributes(ZM_master$religion)$labels <- newlab
newlab <- c(1,2,4,7,8,9)
names(newlab) <- c("English", "Bemba", "Lozi", "Nyanja", "Tonga", "Other")
attributes(ZM_master$language)$labels <- newlab




Final_data <- ZM_master[which(ZM_master$tot_ind_m == 1 | ZM_master$tot_ind_w == 1), c("interviewerID", "sex", "hivconsent", "hiv", "agecat5", "education", "wealthcat", "location", "region", "marital", "std", "age1sex_cat", "highhiv", "partner", "condom", "aidscare", "knowsdiedofaids", "evertestedHIV", "smoke", "alcohol", "religion", "ethnicity", "language")]

attributes(Final_data$marital)$labels <- attributes(ZM_master$V502)$labels
attributes(Final_data$std)$labels <- stdlab
newlab <- 1:3
names(newlab) <- c("nosex", "under16", "over16")
attributes(Final_data$age1sex_cat)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(Final_data$condom)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("lhivrisk", "hhivrisk")
attributes(Final_data$highhiv)$labels <- newlab
newlab <- 0:2
names(newlab) <- c("nopartner", "1partner", "2+partner")
attributes(Final_data$partner)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(Final_data$knowsdiedofaids)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(Final_data$aidscare)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(Final_data$evertestedHIV)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(Final_data$alcohol)$labels <- newlab
newlab <- 0:1
names(newlab) <- c("no", "yes")
attributes(Final_data$smoke)$labels <- newlab
newlab <- 1:3
names(newlab) <- c("Catholic", "Protestant", "Other")
attributes(Final_data$religion)$labels <- newlab
newlab <- c(1,2,4,7,8,9)
names(newlab) <- c("English", "Bemba", "Lozi", "Nyanja", "Tonga", "Other")
attributes(Final_data$language)$labels <- newlab

newlab <- unique(Final_data$interviewerID)
names(newlab) <- unique(Final_data$interviewerID)
attributes(Final_data$interviewerID)$labels <- newlab

datavars <- as.data.frame(matrix(NA, ncol = 3, nrow = ncol(Final_data)-1))
colnames(datavars) <- c("variables", "levels", "reference")
datavars$variables <- colnames(Final_data)[-1]

for(i in 2:dim(Final_data)[2]){
  datavars$levels[i-1] <- toString(names(attributes(Final_data[,i])$labels))
}

datavars$levels[2:3] <- c("yes, no", "yes, no")

datavars$levels[4] <- toString(attributes(Final_data[,5])$levels)

datavars$reference <- c(NA, NA, NA, "15-19", "Richer", "Countryside", "Eastern", "Currently married", NA, "over16", NA, "1partner", NA, NA, NA, NA, NA, NA, "Other", "Other", "Other")

datavars <- datavars[-5,]

write.csv(datavars, "C:\\Users\\Henry Jones\\OneDrive\\Documents\\Durham Maths work\\Fourth year\\Project IV\\HIV Data\\datavars.csv")


Final_dummy <- Final_data[,c("interviewerID", "agecat5", "wealthcat", "location", "region", "marital", "age1sex_cat", "partner", "religion", "ethnicity", "language")]
Final_dummy <- to_dummy(Final_dummy, var.name = "name", suffix = "label")

Final_dummy[,c( "sex", "hivconsent", "hiv","education", "std", "highhiv", "condom", "aidscare", "knowsdiedofaids", "evertestedHIV", "smoke", "alcohol")] <- Final_data[, c("sex", "hivconsent", "hiv","education", "std", "highhiv", "condom", "aidscare", "knowsdiedofaids", "evertestedHIV", "smoke", "alcohol")]

Final_data <- Final_dummy


dim(Final_data)

length(which(Final_data$hiv == 1))/length(which(Final_data$hivconsent == 1))

write.csv(Final_data, "C:\\Users\\Henry Jones\\OneDrive\\Documents\\Durham Maths work\\Fourth year\\Project IV\\HIV Data\\Final_ZM_data_full.csv")

names(Final_data)

Final_data$sex[which(Final_data$sex == 2)] <- 0

Final_data_reduced <- subset(Final_data, select = -c(interviewerID_1000, `agecat5_15-19`, wealthcat_Richer, location_Countryside, region_Eastern, `marital_Currently married`, age1sex_cat_over16, partner_1partner, religion_Other, ethnicity_99, language_Other))

write.csv(Final_data_reduced, "C:\\Users\\Henry Jones\\OneDrive\\Documents\\Durham Maths work\\Fourth year\\Project IV\\HIV Data\\Final_ZM_data_reduced.csv")






