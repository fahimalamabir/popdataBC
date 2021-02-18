########################################################################
########################################################################
###                                                                  ###
###                       SECTION 4 - EXAMPLE                        ###
###                 Joining, Merging and Reshaping                   ###
###                                                                  ###
########################################################################
########################################################################

##==============================================================##
##                  Set working directory                       ##
##    Edit file pathway to reflect where your files are saved   ##
##==============================================================##
setwd("~/Desktop/Megan/Section 3")

##==============================================================##
##                  Packages and Libraries                      ##
##==============================================================##
library(dplyr)
library(plyr)
library(tidyr)
install.packages("questionr")
library(questionr)

##==============================================================##
##                     Load Data Files                          ##
##==============================================================##
rates<- read.csv("Rates_Section3.csv")
oddsratio <- read.csv("OddsRatio_Section3.csv")
pop <- read.csv("Pop_Section3.csv")
env <- read.csv("Env_Section3.csv")
corr <- read.csv("Corr_Section3.csv")

rates$X <- NULL
oddsratio$X <- NULL
pop$X <- NULL
env$X <- NULL
corr$X <- NULL

##==============================================================##
##                     Joining and Merging                      ##
##==============================================================##
#analysis1 <- left_join(rates, corr, by = "dbuid2016")
#analysis1 <- right_join(corr, rates, by = "dbuid2016")
#analysis1 <- analysis1[!is.na(analysis1$hruid2017),]

analysis1 <- inner_join(rates, corr, by = "dbuid2016")

##==============================================================##
##                        ACTIVITY ONE                          ##
##==============================================================##
analysis2 <- left_join(oddsratio, env, by = "Postalcode")
analysis2 <- analysis2[!is.na(analysis2$exposure),]

##==============================================================##
##                       Reshaping Data                         ##
##==============================================================##
pop <- gather(pop, "Age", "population", 4:24)

#pop <- spread(pop, Age, population)

##==============================================================##
##                       ACTIVITY TWO                           ##
##==============================================================##
##---------
## Rename      
##---------
pop <- plyr::rename(pop, c("Gender" = "Sex"))

##-------------
## Categorize     
##-------------
pop$AgeCat <- ifelse(pop$Age == "age0001"| pop$Age == "age0104"| pop$Age == "age0509", 0,
                     ifelse(pop$Age == "age1014"| pop$Age == "age1519", 1,
                            ifelse(pop$Age == "age2024"| pop$Age == "age2529", 2,
                                   ifelse(pop$Age == "age3034"| pop$Age == "age3539", 3,
                                          ifelse(pop$Age == "age4044"| pop$Age == "age4549", 4,
                                                 ifelse(pop$Age == "age5054"| pop$Age == "age5559", 5,
                                                        ifelse(pop$Age == "age6064"| pop$Age == "age6569", 6,
                                                               ifelse(pop$Age == "age7074"| pop$Age == "age7579", 7,
                                                                      ifelse(pop$Age == "age8084"| pop$Age == "age8589", 8,
                                                                             ifelse(pop$Age == "age90", 9, 10))))))))))
##---------------------------------------------------
## A second, shorter option to categorize the data     
##---------------------------------------------------
category.maps <- c(age0001 = 0, age0104 = 0, age0509 = 0, age1014 = 1, age1519 = 1, age2024 = 2, age2529 = 2, age3034 = 3,
                   age3539 = 3, age4044 = 4, age4549 = 4, age5054 = 5, age5559 = 5, age6064 = 6, age6569 = 6, age7074 = 7,
                   age7579 = 7, age8084 = 8, age8589 = 8, age90 = 9)
    
pop$AgeCat = category.maps[pop$Age]

##---------
## Create  
##---------
analysis1$PROV <- substr(analysis1$hruid2017,1,2)
analysis1$hruid2017 <- substr(analysis1$hruid2017,3,4)

##---------
## Subset    
##---------
analysis1 <- subset(analysis1, PROV == 59 )

##-------
## Drop   
##-------
pop$Age <- NULL
analysis1$dbuid2016 <-NULL
analysis1$PROV <- NULL

##==============================================================##
##                       Aggregate Data                         ##
##==============================================================##
analysis1$Sex <- as.character(analysis1$Sex)
analysis1$AgeCat <- as.character(analysis1$AgeCat)
analysis1$CauseCat <- as.character(analysis1$CauseCat)
analysis1$hruid2017 <- as.character(analysis1$hruid2017)

analysis1$Count <- 1

agg <- aggregate(analysis1$Count, by = list(analysis1$Sex, analysis1$AgeCat, analysis1$CauseCat, analysis1$hruid2017), FUN = sum)

sum(agg$x)

##==============================================================##
##                     ACTIVITY THREE                           ##
##==============================================================##
agg <- plyr::rename(agg, c("Group.1"="Sex",
                        "Group.2"="AgeCat", 
                          "Group.3"="CauseCat",
                            "Group.4"="hruid2017",
                              "x"="Count"))

##==============================================================##
##                       Analysis One                           ##
##==============================================================##
##---------
## Join
##---------
pop$AgeCat <- as.character (pop$AgeCat)
pop$hruid2017 <- as.character(pop$hruid2017)
pop$Sex <- as.character(pop$Sex)

agg <- left_join (agg, pop, by = c("Sex", "AgeCat", "hruid2017"))

##------------
## Calculate
##------------
agg$rate <- (agg$Count/agg$population *100000)

##==============================================================##
##                      ACTIVITY FOUR                           ##
##==============================================================##
aggHR <- agg[ c(4,5,7)]
aggHR <- aggregate(. ~hruid2017, aggHR, sum)
aggHR$rate <- ((aggHR$Count/aggHR$population)*100000)

aggAge <- agg[ c(2,5,7)]
aggAge <- aggregate(. ~AgeCat, aggAge, sum)
aggAge$rate <- ((aggAge$Count/aggAge$population)*100000)

##==============================================================##
##                 Analysis 2 - Odds Ratio                      ##
##==============================================================##
##-----------------------------------------------------
## Calculate the odds ratio manually 
## odds ratio = (exposed cases * unexposed controls)/
##              (exposed controls * unexposed cases)
##-----------------------------------------------------
oddsratiotable <- table (analysis2$outcome, analysis2$exposure)
oddsratiotable
OR <- (5*7127)/(138*29)
OR

##--------------------------------
## Using the odds.ratio function       
##--------------------------------
oddsratiotable2 <- matrix(c(5, 138, 29, 7127), ncol=2)
colnames(oddsratiotable2) <- c('+ outcome','- outcome')
rownames(oddsratiotable2) <- c('+ exposure','- exposure')
oddsratiotable2 <- as.table (oddsratiotable2)
oddsratiotable2
ORcheck <-odds.ratio(oddsratiotable2)
ORcheck

