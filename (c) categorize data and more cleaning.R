########################################################################
########################################################################
###                                                                  ###
###                       SECTION 3 - EXAMPLE                        ###
###     VARIABLE CREATION, SUBSET OBSERVATIONS AND MORE CLEANING     ###
###                                                                  ###
########################################################################
########################################################################


##==============================================================##
##                  Packages and Libraries                      ##
##==============================================================##
library(dplyr)
library(plyr)

##==============================================================##
##                     Load Data Files                          ##
##==============================================================##
rates<- read.csv("Rates_Section2.csv")
oddsratio <- read.csv("OddsRatio_Section2.csv")
pop <- read.csv("Pop_Section2.csv")
env <- read.csv("Env_Section2.csv")
corr <- read.csv("Corr_Section2.csv")

##==============================================================##
##                  Create New Variables                        ##
##==============================================================##

##-----------------
## Combine dates       
##-----------------
rates$DOD <- as.Date(paste(rates$D_year, rates$D_month, rates$D_day, sep="-"))
rates$DOB <- as.Date(paste(rates$B_year, rates$B_month, rates$B_day, sep="-"))

##----------------
## Calculate age     
##----------------
rates$Age <- rates$DOD - rates$DOB
rates$Age <- (rates$DOD - rates$DOB)/365.25

##-----------------------
## Round age variable       
##-----------------------
rates$Age <- round(rates$Age,1)

##==============================================================##
##                         ACTIVITY ONE                         ##
##==============================================================##

rates$letter <- substring(rates$Cause_of_death, 1, 1)
rates$number <- substring(rates$Cause_of_death, 2, 3)

oddsratio$letter <- substring(oddsratio$Cause_of_death, 1, 1)
oddsratio$number <- substring(oddsratio$Cause_of_death, 2, 3)

##==============================================================##
##                      Check for Outliers                      ##
##==============================================================##
##---------------------
## Check for outliers 
##---------------------
range(rates$Age)

##---------------------
## Correct the issue
##---------------------
rates$Age[rates$Age < 0] <- 0.5

##==============================================================##
##                  Subsetting Observations                     ##
##==============================================================##
##-------------------
## Subset the data  
##-------------------
#rates <- rates[rates$letter== "C", ]

rates <- subset(rates, letter == "C" )

##==============================================================##
##                         ACTIVITY TWO                         ##
##==============================================================##
oddsratio$BC <- substring(oddsratio$Postalcode,1,1)

oddsratio <- oddsratio[oddsratio$BC == "V", ]

##==============================================================##
##                 Categorization and Binning                   ##
##==============================================================##

##---------------------------
## Dichotomize outcome data      
##---------------------------
oddsratio$outcome <-ifelse(oddsratio$letter == "C" & oddsratio$number == 73, 1, 0) 

##-----------------------------
## Categorize causes of death      
##-----------------------------
rates$CauseCat <- ifelse(rates$number == 50, 1, 
                         ifelse(rates$number == 61, 2, 
                                ifelse(rates$number == 33 | rates$number == 34, 3, 
                                       ifelse(rates$number == 18 | rates$number == 19 | rates$number == 20 | rates$number == 21, 4, 5)))) 

##-----------
## Bin ages      
##-----------
rates$AgeCat <- findInterval(rates$Age, c(10,20,30,40,50,60,70,80,90))

#rates$Age <- as.numeric (rates$Age)
#rates$AgeCat <- cut(rates$Age, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 130))
#rates$AgeCat <- cut(rates$Age, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 130), labels = FALSE)  

##==============================================================##
##                       ACTIVITY THREE                         ##
##==============================================================##
env$exposure <- ifelse(env$max_temp < 10 & env$min_temp < -3, 1, 0) 

##==============================================================##
##                    Clean Postal Codes                        ##
##==============================================================##
##-----------------------------------------------
## Split postal code into 6 individual columns      
##-----------------------------------------------
oddsratio$first <-substring(oddsratio$Postalcode,1,1)
oddsratio$second <-substring(oddsratio$Postalcode,2,2)
oddsratio$third <-substring(oddsratio$Postalcode,3,3)
oddsratio$fourth <-substring(oddsratio$Postalcode,4,4)
oddsratio$fifth <-substring(oddsratio$Postalcode,5,5)
oddsratio$sixth <-substring(oddsratio$Postalcode,6,6)

##---------------------------------
## Create alpha and numeric lists    
##---------------------------------
alpha = c(letters, LETTERS)
numb = c(1,2,3,4,5,6,7,8,9,0)

##------------------------------
## Check columns against lists    
##------------------------------
pcclean1 <- subset (oddsratio, first %in% numb )
pcclean2 <- subset (oddsratio, second %in% alpha)
pcclean3 <- subset (oddsratio, third %in% numb)
pcclean4 <- subset (oddsratio, fourth %in% alpha)
pcclean5 <- subset (oddsratio, fifth %in% numb)
pcclean6 <- subset (oddsratio, sixth %in% alpha)

##--------------------
## Fix common errors   
##--------------------
oddsratio$second[oddsratio$second =="O"]<-0
oddsratio$second[oddsratio$second =="L"]<-1
oddsratio$second[oddsratio$second =="S"]<-5
oddsratio$fourth[oddsratio$fourth =="O"]<-0
oddsratio$fourth[oddsratio$fourth =="L"]<-1
oddsratio$fourth[oddsratio$fourth =="S"]<-5
oddsratio$fifth[oddsratio$fifth ==1]<-"L"
oddsratio$sixth[oddsratio$sixth == "O"]<-0
oddsratio$sixth[oddsratio$sixth =="L"]<-1
oddsratio$sixth[oddsratio$sixth =="S"]<-5

##-------------------------------
## Put postal code back together   
##-------------------------------
oddsratio$Postalcode <- paste(oddsratio$first,oddsratio$second,oddsratio$third,oddsratio$fourth,oddsratio$fifth,oddsratio$sixth, sep ="")

##------------------------------------------
## Postal code with less than 6 characters  
##------------------------------------------
oddsratio$Postalcode <- as.character (oddsratio$Postalcode)
oddsratio <- oddsratio[(nchar(oddsratio$Postalcode) != 5),]

##-------------------------------------------------
## Remove last character in 7 digit postal codes
##-------------------------------------------------
oddsratio$Postalcode <- substring(oddsratio$Postalcode,1,6)

##--------------------------------
## Manual cleaning - Inversions
##--------------------------------
oddsratio$first <-substring(oddsratio$Postalcode,1,1)
oddsratio$second <-substring(oddsratio$Postalcode,2,2)
oddsratio$third <-substring(oddsratio$Postalcode,3,3)
oddsratio$fourth <-substring(oddsratio$Postalcode,4,4)
oddsratio$fifth <-substring(oddsratio$Postalcode,5,5)
oddsratio$sixth <-substring(oddsratio$Postalcode,6,6)
  
pcclean1 <- subset (oddsratio, first %in% numb )
pcclean2 <- subset (oddsratio, second %in% alpha)
pcclean3 <- subset (oddsratio, third %in% numb)
pcclean4 <- subset (oddsratio, fourth %in% alpha)
pcclean5 <- subset (oddsratio, fifth %in% numb)
pcclean6 <- subset (oddsratio, sixth %in% alpha)


oddsratio$Postalcode[oddsratio$Postalcode == "V2A42V"] <- "V2A4V2"

#Any observations with postal code errors will not match the environmental dataset. This will eliminate them.

##==============================================================##
##                       ACTIVITY FOUR                          ##
##==============================================================##
corr$X <- NULL

env <- env[ c(2,5)]

pop$X <- NULL

rates <- rates [ , c("Sex", "dbuid2016", "AgeCat", "CauseCat")]

oddsratio <- oddsratio [ , c("outcome","Postalcode")]

##==============================================================##
##                 Save a Copy of the Data                      ##
##==============================================================##
write.csv(rates, file = "Rates_Section3.csv")
write.csv(oddsratio, file = "OddsRatio_Section3.csv")
write.csv(corr, file = "Corr_Section3.csv")
write.csv(env, file = "Env_Section3.csv")
write.csv(pop, file = "Pop_Section3.csv")

