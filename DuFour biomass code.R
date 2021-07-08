# Fri Oct 18 11:00:51 2019 ------------------------------

# This code was created by Mark DuFour - Fisheries Biologist II
# Sandusky Fisheries Research Station - Lake Erie
# Ohio Department of Natural Resources - Division of Wildlife

## Estimaing Total Biomass (kg) in Ohio waters (West Basin) from Lake Erie western basin trawl survey
## Year - Month - Species - Species aggregates
## Year: 1992-2018
## Month: May - Fall (5-9)
## Species.... age-groups...

## load packages
library(RODBC)   

library(magrittr)
library(utf8)
#library(plyr)
require(here)
library(tidyverse)

## set working directory
#library(rstudioapi)
#setwd(dirname(rstudioapi::getActiveDocumentContext()))
#getwd()



##########################################
## Bring in Effort, Catch, and Length data
##########################################
## bring in data
#chan1 <-  odbcConnectAccess2007("C:/Users/dufour/Documents/1_Projects/1_Trawling/3_Database/Trawling Database")   #specifies the file path
chan1 <-  odbcConnectAccess2007("data sources/Trawling Database")   #specifies the file path

Effort <- sqlFetch(chan1, "Trawl Effort") 
Catch  <- sqlFetch(chan1, "Trawl Catch") 
odbcCloseAll()

table(Effort$Year,Effort$T_MON)

#chan2 <-  odbcConnectAccess2007("C:/Users/dufour/Documents/1_Projects/1_Trawling/3_Database/Trawl lengths xp")   #specifies the file path
chan2 <-  odbcConnectAccess2007("data sources/Trawl lengths xp")   #specifies the file path

Lengths1 <- sqlFetch(chan2, "TRAWL LENGTHS 1985-89") 
Lengths2 <- sqlFetch(chan2, "TRAWL LENGTHS 1990-94") 
Lengths3 <- sqlFetch(chan2, "TRAWL LENGTHS 1995-99") 
Lengths4 <- sqlFetch(chan2, "TRAWL LENGTHS 2000-04") 
Lengths5 <- sqlFetch(chan2, "TRAWL LENGTHS 2005-09") 
Lengths6 <- sqlFetch(chan2, "TRAWL LENGTHS 2010-14") 
Lengths7 <- sqlFetch(chan2, "TRAWL LENGTHS 2015-19") 
odbcCloseAll()


## Effort data - standardize column names, toss low DO and bad EFFSTAT, subset, and sort
head(Effort)
colnames(Effort) <- c("PROJCODE","CRN","YEAR","MONTH","DAY","AGENCY","GRID","LATITUDE","LONGITUDE","GEAR","DEPTH","EFFDUR",   
                      "EFFDIST","EFFSTAT","STATIME","ENDTIME","BTEMP","STEMP","SECCHI","COMMENTS","BOTOXY","TOTDIST","SPEED","T_MON","SITENO") 
Effort$EFFSTAT <- ifelse(Effort$YEAR >= 2009 & Effort$BOTOXY < 2, 0, Effort$EFFSTAT) ## change Effstat on low DO samples
Effort$YEAR <- ifelse(Effort$YEAR==12,2012,Effort$YEAR)
Effort$YEAR <- ifelse(Effort$YEAR==13,2013,Effort$YEAR)
Effort <-  filter(Effort, EFFSTAT == 1) ## keep only good Effstats (1) 
Effort$PROJCODE <- ifelse(Effort$PROJCODE=="LEA-IF00_001","LEA_IF00_001",Effort$PROJCODE) ## correction?
Effort <-  filter(Effort, YEAR >= 1992) ## remove 1987
Effort <-  filter(Effort, YEAR <  2019)
Effort <-  filter(Effort, SITENO > 2)
Effort <-  filter(Effort, SITENO < 42)
Effort <-  filter(Effort, LONGITUDE < -82.55)
Effort <-  arrange(Effort,YEAR, CRN, T_MON) ## sort 

head(Effort)
dim(Effort)


## Catch data - Add year, change agecodes, subset, and sort 
head(Catch)
#Catch$PROJCODE <- ifelse(Catch$PROJCODE=='LEA-IF00_001','LEA_IF00_001',Catch$PROJCODE) ## correction
Catch$YEAR <- ifelse(Catch$PROJCODE=='LEA_IF87_001',1987,
              ifelse(Catch$PROJCODE=='LEA_IF88_001',1988,
              ifelse(Catch$PROJCODE=='LEA_IF89_001',1989,
              ifelse(Catch$PROJCODE=='LEA_IF90_001',1990,
              ifelse(Catch$PROJCODE=='LEA_IF91_001',1991,
              ifelse(Catch$PROJCODE=='LEA_IF92_001',1992,
              ifelse(Catch$PROJCODE=='LEA_IF93_001',1993,
              ifelse(Catch$PROJCODE=='LEA_IF94_001',1994,
              ifelse(Catch$PROJCODE=='LEA_IF95_001',1995,
              ifelse(Catch$PROJCODE=='LEA_IF96_001',1996,
              ifelse(Catch$PROJCODE=='LEA_IF97_001',1997,
              ifelse(Catch$PROJCODE=='LEA_IF98_001',1998,
              ifelse(Catch$PROJCODE=='LEA_IF99_001',1999,
              ifelse(Catch$PROJCODE=='LEA_IF00_001',2000,
              ifelse(Catch$PROJCODE=='LEA_IF01_001',2001,
              ifelse(Catch$PROJCODE=='LEA_IF02_001',2002,
              ifelse(Catch$PROJCODE=='LEA_IF03_001',2003,
              ifelse(Catch$PROJCODE=='LEA_IF04_001',2004,
              ifelse(Catch$PROJCODE=='LEA_IF05_001',2005,
              ifelse(Catch$PROJCODE=='LEA_IF06_001',2006,
              ifelse(Catch$PROJCODE=='LEA_IF07_001',2007,
              ifelse(Catch$PROJCODE=='LEA_IF08_001',2008,
              ifelse(Catch$PROJCODE=='LEA_IF09_001',2009,
              ifelse(Catch$PROJCODE=='LEA_IF10_001',2010,
              ifelse(Catch$PROJCODE=='LEA_IF11_001',2011,
              ifelse(Catch$PROJCODE=='LEA_IF12_001',2012,
              ifelse(Catch$PROJCODE=='LEA_IF13_001',2013,
              ifelse(Catch$PROJCODE=='LEA_IF14_001',2014,
              ifelse(Catch$PROJCODE=='LEA_IF15_001',2015,
              ifelse(Catch$PROJCODE=='LEA_IF16_001',2016,
              ifelse(Catch$PROJCODE=='LEA_IF17_001',2017,
              ifelse(Catch$PROJCODE=='LEA_IF18_001',2018,Catch$PROJCODE)))))))))))))))))))))))))))))))) ## add year column

Catch <-  filter(Catch, YEAR >= 1992) ## remove 1987
Catch$AGE <- ifelse(Catch$AGE==5,4,Catch$AGE) ## change all age codes 5 to 4
splitted <- t(sapply(Catch$CRN, function(x) substring(x, first=c(4), last=c(4))))  ## add T_MON column
Catch <- cbind(Catch, splitted[1,])                                                ## add T_MON column  
Catch <- Catch[,c(6:7,2:5)] ## remove and rearrange columns
colnames(Catch)[2] <- "T_MON" ## change column name
Catch <- arrange(Catch, YEAR, T_MON, CRN, SPECIES, AGE)  ## sort by...

head(Catch)
dim(Catch)



## Length data - Combine, subset, and sort
header <- colnames(Lengths1)
colnames(Lengths2) <- header
colnames(Lengths3) <- header
colnames(Lengths4) <- header
colnames(Lengths5) <- header
colnames(Lengths6) <- header
colnames(Lengths7) <- header

Length <- rbind(Lengths1,Lengths2,Lengths3,Lengths4,Lengths5,Lengths6,Lengths7) ## bind separate length files
Length <- Length[,c(1,2,4,6,7,8)] ## keep some columns
Length <-  filter(Length, YEAR >= 1992) ## remove 1987
Length$AGE <- ifelse(Length$AGE==5,4,Length$AGE) ## combine agecode 4 and 5
Length$LENGTH <- ifelse(Length$YEAR == 1997 & Length$SPECIES == 201 &  Length$LENGTH==196,126,Length$LENGTH) ## correction?
splitted <- t(sapply(Length$CRN, function(x) substring(x, first=c(4), last=c(4))))   ## add T_MON column
Length <- cbind(Length, splitted[1,])                                                ## add T_MON column  
Length <- Length[,c(1,8,3:6)]                  ## Remove and rearrange columns
colnames(Length)[c(2,5)] <- c("T_MON","AGE")     ## change column name
Length <- arrange(Length,YEAR,T_MON,SPECIES,AGE) ## sort by...

head(Length)
dim(Length)

## Length_sum - summarize length data (mean, sd, n) by year, crn, spec, age
mLength   <- aggregate(LENGTH~YEAR+T_MON+CRN+SPECIES+AGE,data=Length, mean)  ## calculate mean
sdLength  <- aggregate(LENGTH~YEAR+T_MON+CRN+SPECIES+AGE,data=Length, sd)     ## calculate sd
nLength  <- plyr::count(Length, c("YEAR","T_MON","CRN","SPECIES","AGE"))        ## generate counts
msdLength <- merge(mLength,sdLength,by=c("YEAR","T_MON","CRN","SPECIES","AGE"),all=T) ## bring mean and sd together
Length_sum   <- merge(msdLength,nLength,by=c("YEAR","T_MON","CRN","SPECIES","AGE"),all=T) ## add counts 
colnames(Length_sum) <- c("YEAR","T_MON","CRN","SPECIES","AGE","M","S","N") ## renames columns
Length_sum$S <- ifelse(is.na(Length_sum$S),0,Length_sum$S) ## change NA values to 0

head(Length_sum)
dim(Length_sum)


####################################################################
## Combine lengths and catch and assign lengths based on normal pdf 
####################################################################
head(Catch)
head(Length_sum)

Catch_len <- merge(Catch,Length_sum,by=c("YEAR","T_MON","CRN","SPECIES","AGE"),all=T) ## merge two datasets
Catch_len$CATCH <- ifelse(Catch_len$CATCH < Catch_len$N,Catch_len$N,Catch_len$CATCH) ## adjust catch data to match length counts
# N is probably number of lengths


Catch_len <- Catch_len[complete.cases(Catch_len$N),]  ## pitch rows with NA length counts
Catch_len$NUM <- Catch_len$CATCH-Catch_len$N ## calculate difference in CAtch and length counts (for simulating lengths below)

#################################################################################
## there is an issue where we have lengths but not catch... must correct dbase ##
#################################################################################
Catch_len <- Catch_len[complete.cases(Catch_len$NUM),] ## temp fix to keep code running
#Catch_len <- subset(Catch_len, Catch_len$NUM > 0)
#Catch_len$NUM <- ifelse(is.na(Catch_len$NUM),0,Catch_len$NUM)
#Catch_len$CATCH <- ifelse(is.na(Catch_len$NUM),Catch_len$N,Catch_len$CATCH)
#Catch_len$CATCH <- ifelse(is.na(Catch_len$CATCH) & !is.na(Catch_len$N),Catch_len$N,Catch_len$CATCH)
#################################################################################


## Replicate rows, and draw random lengths 
Catch_len2 <- Catch_len[rep(seq(nrow(Catch_len)),Catch_len$NUM),1:10] ## replicate rows for 'unmeasured' fish
draws <- rnorm(dim(Catch_len2)[1],0,1) ## random draw for each 'unmeasured' fish
Catch_len2$X <- Catch_len2$M+Catch_len2$S*draws ## adjust random draws with species-age specific mean and sd 
Catch_len2 <- arrange(Catch_len2,YEAR,T_MON,SPECIES,AGE) ## sort by...
colnames(Catch_len2)[11] <- "LENGTH" ## rename X
Catch_len2 <- Catch_len2[,c(1:5,11)] ## remove and rearrange columns
Catch_len2$LENGTH <- round(Catch_len2$LENGTH,0) ## round simulated lengtsh to 0 sig-figs
Catch_len2$SIM <- 1 ## add sim toggle

head(Catch_len2)
dim(Catch_len2)

## Combine measured and simulated length data
Length$SIM <- 0 ## add sim toggle
Length2 <- rbind(Length,Catch_len2) ## bind two measured and simulated fish

head(Length2)
dim(Length2)


###########################################################
## Bring in L-W regression estimates and merge with Length2 (mm-g) #should this be Ohio Specific or updated?
###########################################################
#LWreg <- read.csv("C:/Users/dufour/Documents/1_Projects/1_Trawling/4_Abundance Indices/1_SAS_programs/2_Inter-agency/FTG/forlenwt.csv",sep=",",header=T)  ## bring in data
LWreg <- readxl::read_excel("data sources/forlenwt.xls") 

colnames(LWreg) <- c("SPECIES","INT","SLOPE","AGE") ## rename columns

## merge data
head(LWreg)
head(Length2)

Weight <- merge(Length2,LWreg,by=c("SPECIES","AGE"), all=T) ## merge two data sets
Weight$WGT <- Weight$INT*(Weight$LENGTH^Weight$SLOPE) ## estimate weight (g) with model coefs
Weight <- Weight[complete.cases(Weight$CRN),]  ## pitch missing weights (some species with reg coefs not present?)
Weight <- arrange(Weight,YEAR,CRN,SPECIES,AGE) ## sort by...

## sum weights (g)
Weight_sum <- aggregate(WGT~YEAR+T_MON+CRN+SPECIES+AGE,data=Weight,FUN="sum")  ## sum weights
Weight_sum <- arrange(Weight_sum,YEAR,T_MON,CRN,SPECIES,AGE) ## sort by...

head(Weight_sum)
dim(Weight_sum)

## merge catch and weight data
head(Catch)
head(Weight_sum)

Combo <- merge(Catch,Weight_sum,by=c("YEAR","T_MON","CRN","SPECIES","AGE"),all=T) ## merge two data sets
Combo <- arrange(Combo,YEAR,CRN,SPECIES,AGE,CATCH) ## sort by...

head(Combo)
dim(Combo)

###########################################################
## A series of reclassifications...
###########################################################
NewCatch <- Combo
NewCatch$SPECIES <- ifelse(NewCatch$SPECIES == 206,204,NewCatch$SPECIES) ## mimic to sand shinder
NewCatch$SPECIES <- ifelse(NewCatch$SPECIES == 210 | NewCatch$SPECIES == 184,180,NewCatch$SPECIES) ## dace to general minnow

## creating new columns
NewCatch$GRMALE <- ifelse(NewCatch$SPECIES==61 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$GRMGS  <- ifelse(NewCatch$SPECIES==63 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$GRMWF  <- ifelse(NewCatch$SPECIES==91 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$GRMSMEL <- ifelse(NewCatch$SPECIES==121 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$GRMSMEL2 <- ifelse(NewCatch$SPECIES==121 & NewCatch$AGE>=2,NewCatch$WGT,0)
NewCatch$GRMWS0 <- ifelse(NewCatch$SPECIES==163 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$NOTROGRM <- ifelse(NewCatch$SPECIES==180 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$NOTR2GRM <- ifelse(NewCatch$SPECIES==180 & NewCatch$AGE>=2,NewCatch$WGT,0)
NewCatch$CHUBGRM <- ifelse(NewCatch$SPECIES==191 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$CHUB2GRM <- ifelse(NewCatch$SPECIES==191 & NewCatch$AGE>=2,NewCatch$WGT,0)
NewCatch$ESHINGR <- ifelse(NewCatch$SPECIES==196 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$ESHIN2GR <- ifelse(NewCatch$SPECIES==196 & NewCatch$AGE>=2,NewCatch$WGT,0)
NewCatch$SSHINGR <- ifelse(NewCatch$SPECIES==201 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$SSHIN2GR <- ifelse(NewCatch$SPECIES==201 & NewCatch$AGE>=2,NewCatch$WGT,0)
NewCatch$SANDSGRM <- ifelse(NewCatch$SPECIES==204 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$BLUNTGRM <- ifelse(NewCatch$SPECIES==208 & NewCatch$AGE>=1,NewCatch$WGT,0)
NewCatch$FATHEGRM <- ifelse(NewCatch$SPECIES==209 & NewCatch$AGE>=1,NewCatch$WGT,0)
NewCatch$ROLLGRM  <- ifelse(NewCatch$SPECIES==216 & NewCatch$AGE>=1,NewCatch$WGT,0)
NewCatch$TPERGRM <- ifelse(NewCatch$SPECIES==291 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$TPER2GRM <- ifelse(NewCatch$SPECIES==291 & NewCatch$AGE>=2,NewCatch$WGT,0)
NewCatch$WPERGRM <- ifelse(NewCatch$SPECIES==301 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$WPER2GRM <- ifelse(NewCatch$SPECIES==301 & NewCatch$AGE==2,NewCatch$WGT,0)
NewCatch$WBASSGRM <- ifelse(NewCatch$SPECIES==302 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$WBAS2GRM <- ifelse(NewCatch$SPECIES==302 & NewCatch$AGE==2,NewCatch$WGT,0)
NewCatch$YPERGRM <- ifelse(NewCatch$SPECIES==331 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$YPER2GRM <- ifelse(NewCatch$SPECIES==331 & NewCatch$AGE==2,NewCatch$WGT,0)
NewCatch$WALLGRM <- ifelse(NewCatch$SPECIES==334 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$WALL2GRM <- ifelse(NewCatch$SPECIES==334 & NewCatch$AGE==2,NewCatch$WGT,0)
NewCatch$LPERGRM <- ifelse(NewCatch$SPECIES==342 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$LPER2GRM <- ifelse(NewCatch$SPECIES==342 & NewCatch$AGE>=2,NewCatch$WGT,0)
NewCatch$DARTGRM <- ifelse(NewCatch$SPECIES==344 & NewCatch$AGE>=1,NewCatch$WGT,0)
NewCatch$SILVEGRM <- ifelse(NewCatch$SPECIES==361 & NewCatch$AGE>=1,NewCatch$WGT,0)
NewCatch$GOBYGRM <- ifelse(NewCatch$SPECIES==366 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$GOBY2GRM <- ifelse(NewCatch$SPECIES==366 & NewCatch$AGE>=2,NewCatch$WGT,0)
NewCatch$DRUMGRM <- ifelse(NewCatch$SPECIES==371 & NewCatch$AGE==1,NewCatch$WGT,0)
NewCatch$DRUM2GRM <- ifelse(NewCatch$SPECIES==371 & NewCatch$AGE==2,NewCatch$WGT,0)

NewCatch <- arrange(NewCatch,YEAR,T_MON, CRN) ## sort by...


## sum species weights (g) by Year, month, and CRN

NewCatch_m <- aggregate(cbind(GRMALE, GRMGS, GRMWF, GRMSMEL, GRMSMEL2, GRMWS0, NOTROGRM, NOTR2GRM, CHUBGRM, CHUB2GRM, 
                              ESHINGR, ESHIN2GR, SSHINGR, SSHIN2GR, SANDSGRM, BLUNTGRM, FATHEGRM, TPERGRM, TPER2GRM, 
                              WPERGRM, WPER2GRM, WBASSGRM, WBAS2GRM, YPERGRM, YPER2GRM, WALLGRM, WALL2GRM, LPERGRM, 
                              LPER2GRM, DARTGRM, SILVEGRM, GOBYGRM, GOBY2GRM, DRUMGRM, DRUM2GRM)~YEAR+T_MON+CRN,data=NewCatch,FUN="sum")
## rename columns
colnames(NewCatch_m) <- c("YEAR","T_MON","CRN","ALEWIFG", "GSHADG", "WHITFISG", "SMELT0G", "SMELT2G",
                          "WSUCKR0G", "NOTROPIG", "NOTROP2G", "SCHUB0G", "SCHUB2G", "ESHIN0G",
                          "ESHIN2G", "SSHIN0G", "SSHIN2G", "SANDSHIG", "BLUNTNOG", "FATHEADG",
                          "TPERCH0G", "TPERCH2G", "WPERCH0G", "WPERCH2G","WBASSG", "WBASS2G", 
                          "YPERCH0G", "YPERCH2G", "WALLEY0G", "WALLEY2G", "LPERCH0G", "LPERCH2G", 
                          "DARTERG", "BSILVERG", "GOBY0G", "GOBY2G", "DRUM0G", "DRUM2G")


###################################################################
## VOLUME SAMPLED AND BIOMASS PER AREA (g/m2) FOR EACH SPECIES-AGE FIELD
###################################################################
head(NewCatch_m)

Combo2 <- merge(NewCatch_m,Effort,by=c("YEAR","T_MON","CRN"),all=T)  ## merge two datasets
Combo2$DEPTH <- ifelse(Combo2$DEPTH <= 6,1,2)  ## classify depths as 1 (<=6m) and 2 (>6m)
Combo2 <- Combo2[complete.cases(Combo2$EFFSTAT),] ## Remove NA EFFSTATS - previously removed from Effort if > 1


## FPC corrections (BEFORE AND AFTER 2002)
## Before 2002 <- old Explorer to New Explorer to Kenosay
## 2002 and after <- New Explorer to Kenosay
Combo2$GSHADG  <- ifelse(Combo2$YEAR<2002, Combo2$GSHADG*1.216, Combo2$GSHADG*2.362)
Combo2$ESHIN0G <- ifelse(Combo2$YEAR<2002, Combo2$ESHIN0G*2.170, Combo2$ESHIN0G*1.494)
Combo2$ESHIN2G <- ifelse(Combo2$YEAR<2002, Combo2$ESHIN2G*2.170, Combo2$ESHIN2G*1.494)
Combo2$WBASSG  <- ifelse(Combo2$YEAR<2002, Combo2$WBASSG*1.644, Combo2$WBASSG*3.203)
Combo2$YPERCH0G  <- ifelse(Combo2$YEAR<2002, Combo2$YPERCH0G*1.321, Combo2$YPERCH0G)
Combo2$YPERCH2G  <- ifelse(Combo2$YEAR<2002, Combo2$YPERCH2G*1.185, Combo2$YPERCH2G)
#Combo2$YPERCH3G  <- ifelse(Combo2$YEAR<2002, Combo2$YPERCH3G*1.321, Combo2$YPERCH3G)
Combo2$WALLEY0G  <- ifelse(Combo2$YEAR<2002, Combo2$WALLEY0G*1.520, Combo2$WALLEY0G*1.561)
Combo2$DRUM2G  <- ifelse(Combo2$YEAR<2002, Combo2$DRUM2G*1.505, Combo2$DRUM2G*0.598)
Combo2$TPERCH0G <- ifelse(Combo2$YEAR < 2002,Combo2$TPERCH0G,Combo2$TPERCH0G*0.704)
Combo2$TPERCH2G <- ifelse(Combo2$YEAR < 2002,Combo2$TPERCH2G,Combo2$TPERCH2G*0.704)
Combo2$WPERCH0G <- ifelse(Combo2$YEAR < 2002,Combo2$WPERCH0G,Combo2$WPERCH0G*1.121)
Combo2$GOBY0G <- ifelse(Combo2$YEAR < 2002,Combo2$GOBY0G,Combo2$GOBY0G*0.423)
Combo2$GOBY2G <- ifelse(Combo2$YEAR < 2002,Combo2$GOBY2G,Combo2$GOBY2G*0.423)


## Trawl dimenssion (new and old Explorer)
Combo2$GAPE  <- ifelse(Combo2$YEAR < 2002, 1.5, 2.4)
Combo2$WINGS <- ifelse(Combo2$YEAR < 2002, 4.83, 5.00)

## Area swept and volume sampled - these aren't even used?
Combo2$EFFDIST <- Combo2$EFFDIST*1852 #convert nmi to m (?)
Combo2$TOTDIST <- Combo2$TOTDIST*1852
Combo2$DEPLDIST <- Combo2$TOTDIST-Combo2$EFFDIST
Combo2$HYPOT <- (Combo2$DEPTH^2)+((Combo2$DEPLDIST/2)^2)
Combo2$DEPDIST <- (Combo2$HYPOT^0.5)
Combo2$RETDIST <- (Combo2$HYPOT^0.5)
Combo2$TOTADIST <- Combo2$DEPDIST+Combo2$EFFDIST+Combo2$RETDIST
Combo2$VOLUME <- Combo2$EFFDIST*Combo2$GAPE*Combo2$WINGS
Combo2$AREASWEP <- Combo2$EFFDIST*Combo2$WINGS

## Calculate biomass as grams per square-m 
Combo2[,4:38]  <- Combo2[,4:38]/Combo2$AREASWEP



## Create species groups (clueids, shiners, soft-rayed, spiny-rayed, and total)
Combo2$CLUPGRM <- Combo2$ALEWIFG+Combo2$GSHADG
Combo2$SHINGRM <- Combo2$ESHIN0G+Combo2$ESHIN2G+Combo2$SSHIN0G+Combo2$SSHIN2G+Combo2$NOTROPIG+Combo2$NOTROP2G+Combo2$SANDSHI
Combo2$SOFTGRM <- Combo2$ESHIN0G+Combo2$ESHIN2G+Combo2$WHITFISG+Combo2$SSHIN0G+Combo2$SSHIN2G+Combo2$NOTROPIG+Combo2$NOTROP2G+Combo2$SANDSHIG+Combo2$SCHUB0G+
                  Combo2$SCHUB2G+Combo2$SMELT0G+Combo2$SMELT2G+Combo2$GOBY0G+Combo2$GOBY2G+Combo2$TPERCH0G+Combo2$TPERCH2G+Combo2$FATHEADG+Combo2$WSUCKR0G+
                  Combo2$LPERCH0G+Combo2$LPERCH2G+Combo2$DARTERG+Combo2$BSILVERG
Combo2$SPINGRM <- Combo2$WPERCH0G+Combo2$YPERCH0G+Combo2$WBASSG+Combo2$DRUM0G+Combo2$WALLEY0G
Combo2$TOTGRM  <- Combo2$ALEWIFG+Combo2$GSHADG+Combo2$WHITFISG+Combo2$SMELT0G+Combo2$SMELT2G+Combo2$WSUCKR0G+Combo2$ESHIN0G+Combo2$ESHIN2G+
                  Combo2$SSHIN0G+Combo2$SSHIN2G+Combo2$SANDSHIG+Combo2$FATHEADG+Combo2$TPERCH0G+Combo2$TPERCH2G+
                  Combo2$WPERCH0G+Combo2$WBASSG+Combo2$YPERCH0G+Combo2$WALLEY0G+Combo2$LPERCH0G+Combo2$LPERCH2G+Combo2$DARTERG+Combo2$BSILVERG+
                  Combo2$GOBY0G+Combo2$GOBY2G+Combo2$DRUM0G+Combo2$NOTROPIG+Combo2$NOTROP2G+Combo2$SCHUB0G+Combo2$SCHUB2G

Combo2 <- arrange(Combo2,YEAR,T_MON,DEPTH) ## sort by...

head(Combo2)
dim(Combo2)

Combo2_exp <- cbind(Combo2[,1:3],Combo2[,4:38]*10000) # convert to g/ha (?) 
write.csv(Combo2_exp, "data derived/DuFour Ohio_WBtrawl_CRNdensity_1992_2018.csv")


## Calculate average biomass as grams per square-m 
Totals <- aggregate(cbind(ALEWIFG, GSHADG, WHITFISG, SMELT0G, SMELT2G,
                          WSUCKR0G, NOTROPIG, NOTROP2G, SCHUB0G, SCHUB2G, ESHIN0G,
                          ESHIN2G, SSHIN0G, SSHIN2G, SANDSHIG, BLUNTNOG, FATHEADG,
                          TPERCH0G, TPERCH2G, WPERCH0G, WPERCH2G, WBASSG, WBASS2G,
                          YPERCH0G, YPERCH2G, WALLEY0G, WALLEY2G, LPERCH0G, LPERCH2G, DARTERG, BSILVERG, GOBY0G, GOBY2G,
                          DRUM0G, DRUM2G, CLUPGRM, SHINGRM, SOFTGRM, SPINGRM, TOTGRM)~YEAR+T_MON+DEPTH,data=Combo2,FUN="mean")
## rename columns
colnames(Totals) <- c("YEAR","T_MON","DEPTH","MNALE", "MNGS", "MNWHITF", "MNSMEL", "MNSMEL2", "MNWSUCK",
                      "MNNOTROP", "MNNOTROP2", "MNCHUB", "MNCHUB2", "MNESHIN", "MNESHIN2", "MNSSHIN", "MNSSHIN2",
                      "MNSANDS", "MNBLUNT", "MNFATHED", "MNTPER", "MNTPER2", "MNWPER", "MNWPER2",
                      "MNWBASS", "MNWBASS2", "MNYPER", "MNYPER2", "MNWALL0", "MNWALL2", "MNLPER", "MNLPER2",
                      "MNDARTER", "MNBSILV", "MNGOBY0", "MNGOBY2", "MNDRUM", "MNDRUM2",
                      "MNCLUP", "MNSHIN", "MNSOFT", "MNSPIN", "MNTOTAL")

Totals <- arrange(Totals,YEAR,T_MON,DEPTH) ## sort by...

head(Totals)
dim(Totals)


## Cacluate Total Biomass in grams for each depth strata
## volume per depth strata was determined using GIS analysis - described in FTG report (not sure of year)
## This is only Ohio waters
Totals$ALETOT   <- ifelse(Totals$DEPTH==1,Totals$MNALE*11.773E+9,Totals$MNALE*4.670E+9)
Totals$SHADTOT  <- ifelse(Totals$DEPTH==1,Totals$MNGS*11.773E+9,Totals$MNGS*4.670E+9)
Totals$WFISHTO  <- ifelse(Totals$DEPTH==1,Totals$MNWHITF*11.773E+9,Totals$MNWHITF*4.670E+9)
Totals$SMELTOT  <- ifelse(Totals$DEPTH==1,Totals$MNSMEL*11.773E+9,Totals$MNSMEL*4.670E+9)
Totals$SMELADTO <- ifelse(Totals$DEPTH==1,Totals$MNSMEL2*11.773E+9,Totals$MNSMEL2*4.670E+9)
Totals$WSUCKTOT <- ifelse(Totals$DEPTH==1,Totals$MNWSUCK*11.773E+9,Totals$MNWSUCK*4.670E+9)
Totals$NOTROTOT <- ifelse(Totals$DEPTH==1,Totals$MNNOTROP*11.773E+9,Totals$MNNOTROP*4.670E+9)
Totals$NOTRO2TOT<- ifelse(Totals$DEPTH==1,Totals$MNNOTROP2*11.773E+9,Totals$MNNOTROP2*4.670E+9)
Totals$CHUBTOT  <- ifelse(Totals$DEPTH==1,Totals$MNCHUB*11.773E+9,Totals$MNCHUB*4.670E+9)
Totals$CHUBADTO <- ifelse(Totals$DEPTH==1,Totals$MNCHUB2*11.773E+9,Totals$MNCHUB2*4.670E+9)
Totals$ESHINTOT <- ifelse(Totals$DEPTH==1,Totals$MNESHIN*11.773E+9,Totals$MNESHIN*4.670E+9)
Totals$ESHIN2TO <- ifelse(Totals$DEPTH==1,Totals$MNESHIN2*11.773E+9,Totals$MNESHIN2*4.670E+9)
Totals$SSHINTOT <- ifelse(Totals$DEPTH==1,Totals$MNSSHIN*11.773E+9,Totals$MNSSHIN*4.670E+9)
Totals$SSHIN2TO <- ifelse(Totals$DEPTH==1,Totals$MNSSHIN2*11.773E+9,Totals$MNSSHIN2*4.670E+9)
Totals$SANDSTOT <- ifelse(Totals$DEPTH==1,Totals$MNSANDS*11.773E+9,Totals$MNSANDS*4.670E+9)
Totals$BLUNTOT  <- ifelse(Totals$DEPTH==1,Totals$MNBLUNT*11.773E+9,Totals$MNBLUNT*4.670E+9)
Totals$FATHETOT <- ifelse(Totals$DEPTH==1,Totals$MNFATHED*11.773E+9,Totals$MNFATHED*4.670E+9)
Totals$TPERTOT  <- ifelse(Totals$DEPTH==1,Totals$MNTPER*11.773E+9,Totals$MNTPER*4.670E+9)
Totals$TPER2TOT <- ifelse(Totals$DEPTH==1,Totals$MNTPER2*11.773E+9,Totals$MNTPER2*4.670E+9)
Totals$WPERTOT  <- ifelse(Totals$DEPTH==1,Totals$MNWPER*11.773E+9,Totals$MNWPER*4.670E+9)
Totals$WPER2TOT <- ifelse(Totals$DEPTH==1,Totals$MNWPER2*11.773E+9,Totals$MNWPER2*4.670E+9)
Totals$WBASSTOT <- ifelse(Totals$DEPTH==1,Totals$MNWBASS*11.773E+9,Totals$MNWBASS*4.670E+9)
Totals$YPERTOT  <- ifelse(Totals$DEPTH==1,Totals$MNYPER*11.773E+9,Totals$MNYPER*4.670E+9)
Totals$YPER2TOT <- ifelse(Totals$DEPTH==1,Totals$MNYPER2*11.773E+9,Totals$MNYPER2*4.670E+9)
Totals$WALLTOT  <- ifelse(Totals$DEPTH==1,Totals$MNWALL0*11.773E+9,Totals$MNWALL0*4.670E+9)
Totals$WALL2TOT <- ifelse(Totals$DEPTH==1,Totals$MNWALL2*11.773E+9,Totals$MNWALL2*4.670E+9)
Totals$LPERTOT  <- ifelse(Totals$DEPTH==1,Totals$MNLPER*11.773E+9,Totals$MNLPER*4.670E+9)
Totals$LPERADTO <- ifelse(Totals$DEPTH==1,Totals$MNLPER2*11.773E+9,Totals$MNLPER2*4.670E+9)
Totals$DARTTOT  <- ifelse(Totals$DEPTH==1,Totals$MNDARTER*11.773E+9,Totals$MNDARTER*4.670E+9)
Totals$BSILVTOT <- ifelse(Totals$DEPTH==1,Totals$MNBSILV*11.773E+9,Totals$MNBSILV*4.670E+9)
Totals$GOBYTOT  <- ifelse(Totals$DEPTH==1,Totals$MNGOBY0*11.773E+9,Totals$MNGOBY0*4.670E+9)
Totals$GOBYADTO <- ifelse(Totals$DEPTH==1,Totals$MNGOBY2*11.773E+9,Totals$MNGOBY2*4.670E+9)
Totals$DRUMTOT  <- ifelse(Totals$DEPTH==1,Totals$MNDRUM*11.773E+9,Totals$MNDRUM*4.670E+9)
Totals$DRUMADTO <- ifelse(Totals$DEPTH==1,Totals$MNDRUM2*11.773E+9,Totals$MNDRUM2*4.670E+9)
Totals$CLUPTOT  <- ifelse(Totals$DEPTH==1,Totals$MNCLUP*11.773E+9,Totals$MNCLUP*4.670E+9)
Totals$SHINTOT  <- ifelse(Totals$DEPTH==1,Totals$MNSHIN*11.773E+9,Totals$MNSHIN*4.670E+9)
Totals$SOFTTOT  <- ifelse(Totals$DEPTH==1,Totals$MNSOFT*11.773E+9,Totals$MNSOFT*4.670E+9)
Totals$SPINTOT  <- ifelse(Totals$DEPTH==1,Totals$MNSPIN*11.773E+9,Totals$MNSPIN*4.670E+9)
Totals$TOTCATCH <- ifelse(Totals$DEPTH==1,Totals$MNTOTAL*11.773E+9,Totals$MNTOTAL*4.670E+9)

head(Totals)
dim(Totals)


## Cacluate Total Biomass in grams by summing depth strata
Totals2 <- aggregate(cbind(ALETOT, SHADTOT, WFISHTO, SMELTOT, SMELADTO, WSUCKTOT, NOTROTOT, NOTRO2TOT, CHUBTOT, CHUBADTO,
                           ESHINTOT, ESHIN2TO, SSHINTOT, SSHIN2TO, SANDSTOT, BLUNTOT, FATHETOT, TPERTOT,
                           TPER2TOT, WPERTOT, WBASSTOT, YPERTOT, YPER2TOT, WALLTOT,
                           LPERTOT, LPERADTO, DRUMTOT, DARTTOT, BSILVTOT, GOBYTOT, GOBYADTO,
                           CLUPTOT, SHINTOT, SOFTTOT, SPINTOT, TOTCATCH)~YEAR+T_MON,data=Totals,FUN="sum")
## rename columns
colnames(Totals2) <- c("YEAR","T_MON","ALETOT", "SHADTOT", "WFISHTO", "SMELTOT", "SMELADTO", "WSUCKTOT", "NOTROTOT", "NOTRO2TOT", 
                       "CHUBTOT", "CHUBADTO", "ESHINTOT", "ESHIN2TO", "SSHINTOT", "SSHIN2TO", "SANDSTOT", "BLUNTOT", "FATHETOT", 
                       "TPERTOT", "TPER2TOT", "WPERTOT", "WBASSTOT", "YPERTOT", "YPER2TOT", "WALLTOT",
                       "LPERTOT", "LPERADTO", "DRUMTOT", "DARTTOT", "BSILVTOT", "GOBYTOT", "GOBYADTO",
                       "CLUPTOT", "SHINTOT", "SOFTTOT", "SPINTOT", "TOTCATCH")

## Convert to millions of g
Totals2[,c(3:38)] <- Totals2[,c(3:38)]/1000000

head(Totals2)
dim(Totals2)


## save as .csv
write.csv(Totals2, "data derived/DuFour Ohio_WBtrawl_biomass_1992_2018.csv")


## plot
max(Totals2$TOTCATCH)
symbols(Totals2$YEAR,-(as.numeric(Totals2$T_MON)),circles=Totals2$TOTCATCH,inches=0.2, pch=16,fg=gray(0,0.2),bg=gray(0,0.2), yaxt="n",
        col.axis="black", col.lab="black",cex.main=1.5,cex.axis=1,xlab="Year",ylab="Month",main="Lake Erie West Basin Total Biomass - Ohio")
axis(2,at=-c(1:5), labels=c("May","June","July","Aug","Fall"),las=2)
mtext("Max value ~140,000 million grams",3,0.5, cex=0.75)





## Mean Kg per hectare
Totals_den0 <- aggregate(cbind(ALEWIFG, GSHADG, WHITFISG, SMELT0G, SMELT2G,
                          WSUCKR0G, NOTROPIG, NOTROP2G, SCHUB0G, SCHUB2G, ESHIN0G,
                          ESHIN2G, SSHIN0G, SSHIN2G, SANDSHIG, BLUNTNOG, FATHEADG,
                          TPERCH0G, TPERCH2G, WPERCH0G, WPERCH2G, WBASSG, WBASS2G,
                          YPERCH0G, YPERCH2G, WALLEY0G, WALLEY2G, LPERCH0G, LPERCH2G, DARTERG, BSILVERG, GOBY0G, GOBY2G,
                          DRUM0G, DRUM2G, CLUPGRM, SHINGRM, SOFTGRM, SPINGRM, TOTGRM)~YEAR+T_MON+DEPTH,data=Combo2,FUN="mean")

Totals_den1 <- aggregate(cbind(ALEWIFG, GSHADG, WHITFISG, SMELT0G, SMELT2G,
                               WSUCKR0G, NOTROPIG, NOTROP2G, SCHUB0G, SCHUB2G, ESHIN0G,
                               ESHIN2G, SSHIN0G, SSHIN2G, SANDSHIG, BLUNTNOG, FATHEADG,
                               TPERCH0G, TPERCH2G, WPERCH0G, WPERCH2G, WBASSG, WBASS2G,
                               YPERCH0G, YPERCH2G, WALLEY0G, WALLEY2G, LPERCH0G, LPERCH2G, DARTERG, BSILVERG, GOBY0G, GOBY2G,
                               DRUM0G, DRUM2G, CLUPGRM, SHINGRM, SOFTGRM, SPINGRM, TOTGRM)~YEAR+T_MON,data=Combo2,FUN="mean")

colnames(Totals_den1) <- c("YEAR","T_MON","MNALE", "MNGS", "MNWHITF", "MNSMEL", "MNSMEL2", "MNWSUCK",
                      "MNNOTROP", "MNNOTROP2", "MNCHUB", "MNCHUB2", "MNESHIN", "MNESHIN2", "MNSSHIN", "MNSSHIN2",
                      "MNSANDS", "MNBLUNT", "MNFATHED", "MNTPER", "MNTPER2", "MNWPER", "MNWPER2",
                      "MNWBASS", "MNWBASS2", "MNYPER", "MNYPER2", "MNWALL0", "MNWALL2", "MNLPER", "MNLPER2",
                      "MNDARTER", "MNBSILV", "MNGOBY0", "MNGOBY2", "MNDRUM", "MNDRUM2",
                      "MNCLUP", "MNSHIN", "MNSOFT", "MNSPIN", "MNTOTAL")

head(Totals_den1)
dim(Totals_den1)
Totals_den1[,c(3:42)]/1000*10000


write.csv(Totals_den0, "data derived/DuFour Ohio_WBtrawl_density_1992_2018.csv")


## plot
symbols(Totals_den0$YEAR,-(as.numeric(Totals_den0$T_MON)),
        circles=Totals_den0$MNTOTAL,
        inches=0.2, 
        pch=16,fg=gray(0,0.2),bg=gray(0,0.2), yaxt="n",
        col.axis="black", col.lab="black",cex.main=1.5,cex.axis=1,xlab="Year",ylab="Month",main="Total kg/hectare")
axis(2,at=-c(1:5), labels=c("May","June","July","Aug","Fall"),las=2)

head(Totals_den)


