########################################################################
########################################################################
###                                                                  ###
###                        SECTION 2 - EXAMPLES                      ###
###                  SUBSET VARIABLES AND DATA CLEANING              ###
###                                                                  ###
########################################################################
########################################################################

##==============================================================##
##                  Set working directory                       ##
##    Edit file pathway to reflect where your files are saved   ##
##==============================================================##
setwd("~/Desktop/Fahim")

##==============================================================##
##                  Packages and Libraries                      ##
##==============================================================##
library(readxl)
library(dplyr)
library(plyr)

##==============================================================##
##                     Load Data Files                          ##
##==============================================================##
mort <- read_excel("deaths_2016.xlsx")
env <- read.csv("Weather_data.csv")
corr <- read.csv("Corr_2016.csv")
pop <- read.csv("Population_Estimates.csv")

##==============================================================##
##                     Subsetting Variables                     ##
##==============================================================##

##------------------------
## The "Drop" Approach        
##------------------------
#mort$Location_of_death <- NULL
#mort$Marital.status <- NULL
#mort$Postalcode <- NULL

#rates <- mort [ - c(5,6,7)]

#rates <- mort [ - c(5:7)]

rates <- subset(mort, select=-c(Location_of_death, Marital_status, Postalcode))

##------------------------
## The "Keep" Approach        
##------------------------
#rates <- mort[ , c("ID", "Sex", "Cause_of_death","dbuid2016", "B_year", "B_month", "B_day", "D_year", "D_month", "D_day")]

#rates <- mort[ c(1:4,8:13)]

#rates <- subset(mort, select = c(ID, Sex, Cause_of_death, dbuid2016, B_year, B_month, B_day, D_year, D_month, D_day))

#rates <- dplyr::select (mort,ID, Sex, Cause_of_death, dbuid2016, B_year, B_month, B_day, D_year, D_month, D_day)

##==============================================================##
##                        ACTIVITY ONE                          ##
##==============================================================##

env<- subset(env, select= -c(WTHNRC12_01, WTHNRC12_02, WTHNRC12_03, WTHNRC12_06, WTHNRC12_07))

corr <- corr[ c(1,3)]

oddsratio <- dplyr::select (mort, ID, Cause_of_death, Postalcode)

pop <- pop[-3]

##==============================================================##
##             Data Cleaning - Renaming Variables               ##
##==============================================================##

env <- plyr::rename(env, c("POSTALCODE12"="Postalcode", 
                            "WTHNRC12_04"="max_temp", 
                                "WTHNRC12_05"="min_temp"))

#names(env)[1] <- "Postalcode"
#names(env)[2] <- "max_temp"
#names(env)[3] <- "min_temp"

#names(env)[names(env)=="POSTALCODE12"] <- "Postalcode"
#names(env)[names(env)=="WTHNRC12_04"] <- "max_temp"
#names(env)[names(env)=="WTHNRC12_05"] <- "min_temp"

##==============================================================##
##                        ACTIVITY TWO                          ##
##==============================================================##
pop <- plyr::rename(pop, c("X"="hruid2017", "Health.Service.Delivery.Area"="HSDA_name", "X.1"="age0001", "X04.Jan"="age0104", 
                            "X09.May"="age0509", "X14.Oct" = "age1014"))

##------------------------
## Change "X" to "age"       
##------------------------
names(pop) <- sub("X","age", names(pop))

##---------------------
## Remove periods      
##---------------------
names(pop) <- sub("\\.", "", names(pop))

##---------------------------------------------------------------------------------------
## To remove a string, specify "" as the replacement (meaning replace with nothing)    
## A "." is interpreted as a special character that means "any character"
## To specify a literal . , the \\ is placed in front. Alternatively, [.] can be used.
##---------------------------------------------------------------------------------------

##==============================================================##
##             Data Cleaning - Removing Duplicates              ##
##==============================================================##

##--------------------------------
## Remove exact duplicates only     
##--------------------------------
#rates <- dplyr::distinct(rates)

##-----------------------------
## Create table of duplicates   
##-----------------------------
duplicates <- rates[duplicated(rates["ID"]),]

##-------------------------------------
## Remove duplicates of one variable    
##-------------------------------------
rates <- rates[!duplicated(rates["ID"]),]

##==============================================================##
##                        ACTIVITY THREE                        ##
##==============================================================##
oddsratio <- oddsratio[!duplicated(oddsratio["ID"]),]

##==============================================================##
##            Data Cleaning - Managing Missing Data             ##
##==============================================================##

##---------------------------
## Check for missing data         
##---------------------------
summary(rates)

plyr::count(is.na(rates$Sex))
plyr::count(is.na(rates$Cause_of_death))
plyr::count(is.na(rates$dbuid2016))
plyr::count(is.na(rates$B_year))
plyr::count(is.na(rates$B_month))
plyr::count(is.na(rates$B_day))
plyr::count(is.na(rates$D_year))
plyr::count(is.na(rates$D_month))
plyr::count(is.na(rates$D_day))

#levels(pop$HSDA_name)
#range(pop$age0001)

##---------------------------------------------
## Delete observations with missing values       
##---------------------------------------------
rates <- rates[!is.na(rates$B_year),]
rates <- rates[!is.na(rates$dbuid2016),]

#rates <- rates[complete.cases(rates),]

##----------------
## Imputation        
##----------------
rates$B_month[is.na(rates$B_month)]<- 6
plyr::count(is.na(rates$B_month))

rates$B_day[is.na(rates$B_day)]<- 15
plyr::count(is.na(rates$B_day))

sex <- c("M","F")
rates$Sex[is.na(rates$Sex)]<-sample(sex, size = 13, replace = TRUE)
plyr::count(is.na(rates$Sex))

##==============================================================##
##                        ACTIVITY FOUR                         ##
##==============================================================##
plyr::count(is.na(oddsratio$Postalcode))
oddsratio <- oddsratio[!is.na(oddsratio$Postalcode),]

##==============================================================##
##              Data Cleaning - Expected Values                 ##
##==============================================================##

##-------------------------------
## Check for unexpected values        
##-------------------------------
freq(rates$B_day)
freq(rates$D_day)

freq(rates$B_month)
freq(rates$D_month)

freq(rates$B_year)
freq(rates$D_year)

##---------------
## Imputation        
##---------------
rates$B_month[rates$B_month > 12]<-6
freq(rates$B_month)

rates$D_month[rates$D_month > 12]<-6
freq(rates$D_month)

##--------------------
## Manual Correction        
##--------------------
rates$B_year[rates$B_year == 1827] <- 1927
rates$B_year[rates$B_year == 1831] <- 1931
rates$B_year[rates$B_year == 1832] <- 1932
rates$B_year[rates$B_year == 1837] <- 1937
freq(rates$B_year)


##==============================================================##
##                 Save a Copy of the Data                      ##
##==============================================================##
setwd("~/Desktop/Megan/Section 2")
write.csv(rates, file = "Rates_Section2.csv")
write.csv(oddsratio, file = "OddsRatio_Section2.csv")
write.csv(corr, file = "Corr_Section2.csv")
write.csv(env, file = "Env_Section2.csv")
write.csv(pop, file = "Pop_Section2.csv")
