########################################################################
########################################################################
###                                                                  ###
###                        SECTION 1 - EXAMPLES                      ###
###            LOADING DATA, LIBRARIES AND DATA EXPLORATION          ###
###                                                                  ###
########################################################################
########################################################################

##==============================================================##
##                  Set working directory                       ##
##    Edit file pathway to reflect where your files are saved   ##
##==============================================================##
##--------------------------------------------------------
## Note that file paths usually contain a back slash (\)
## In R, a forward slash is required instead (/)
##--------------------------------------------------------
setwd("~/Desktop/Fahim/Section 1")

##==============================================================##
##                  Packages and Libraries                      ##
##==============================================================##
##-----------------------------------------------------------------
## Note that plyr should be loaded before dplyr to avoid an error
##-----------------------------------------------------------------
library(readxl)
library(plyr)
library(dplyr)


##==============================================================##
##                     Load Data Files                          ##
##==============================================================##
mort<- read_excel("deaths_2016.xlsx")
pop <- read.csv ("Population_Estimates.csv")
corr <- read.csv ("Corr_2016.csv")
env <- read.csv ("Weather_data.csv")

##==============================================================##
##                     Explore the Data                         ##
##      Use these functions to fill in the data dictionary      ##
##==============================================================##
str(mort)
summary(mort)

str(pop)
summary(pop)

str(corr)
summary(corr)

str(env)
summary(env)

##==============================================================##
##               Further Exploration - Examples                 ##
##==============================================================##

##----------------------------------------------------
## What are the names of the health regions in BC?
##----------------------------------------------------
levels(corr$hrname_english)

##----------------------------------------------------
## How many men died of cancer? How many women?
##----------------------------------------------------
count(mort$Sex)

##----------------------------------------------------
## How many babies are in BC (under one year)?
##----------------------------------------------------
sum(pop$X.1)

##----------------------------------------------------
## What is the average daily temperature in BC?
##----------------------------------------------------
mean(env$WTHNRC12_03)
