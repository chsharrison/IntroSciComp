#From Timeline
#11/14/2019: 1) Make Q-Q plots ****already did this last class***
#            2) Determine which tests have the appropriate assumptions 
#            3) Find packages and read documentation about the testing.
#            4) Run proper test


#Load Libraries
library(dplyr)
library(zoo)
library(ggplot2)
library(lubridate)
library(plotly)
library(psych)
library(ggalt)
library(RColorBrewer)
library(gridExtra)

#set working directory 
setwd("~/UTRGV/Python Class")

#Load data into R
bio<- read.csv("lettucebioassay2.csv", header=TRUE)
length<- read.csv("lettucelength.csv", header=TRUE)

#make sure the column names and types are correct
bio$SGI <- (as.numeric(bio$SGI))
names(length)[names(length) == "ï..Treatment"] <- "Treatment"

# Calculate percent germination and add it to bio dataframe
bio$pct <- ((bio$X1 + bio$X2 + bio$X3 + bio$X4 + bio$X5 +bio$X6)/20 *100)

# 1) Q-Q plots were done in code Class 12-1

# 2) none of the tests that I suggested in my proposal have the proper assumtions for my data.
#Becasue the assumption of normality was not met, even when the data was transformed I looked into other
#statistical methods to test for variation among the mean. It looks like the Kruskal-Wallis test is the prefered 
#alternative to the ANOVA when using non-parometric data

# 3) Look into packages and documentation for Kruskal_Wallis test
# The package needed is the stats package which is included in base R so it doesnot need to be installed
# the function that will ultimately be needed is kruskal.test()
# Documentation for this is found by following the code below
?kruskal.test()

#4) Perform Kruskal-Wallis Test
# This is the tutorial I am following http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

#check and set levels of "treatment" variable 
levels(bio$Treatment)
bio$Treatment <- ordered(bio$Treatment,
                         levels = c("DI_99", "ANAC_33", "ANAC_66", "ANAC_99", "ATRI_33", "ATRI_66", "ATRI_99", "BLAC_33", "BLAC_66", "BLAC_99", "COLI_33", "COLI_66",
                                    "COLI_99", "EBON_33", "EBON_66", "EBON_99", "ELBO_33", "ELBO_66", "ELBO_99", "HCIL_33", "HCIL_66", "HCIL_99",
                                    "HUIS_33", "HUIS_66", "HUIS_99", "TURK_33", "TURK_66", "TURK_99", "VLS_33",  "VLS_66",  "VLS_99"))
levels(length$Treatment)
length$Treatment <- ordered(length$Treatment,
                         levels = c("DI_99", "ANAC_33", "ANAC_66", "ANAC_99", "ATRI_33", "ATRI_66", "ATRI_99", "BLAC_33", "BLAC_66", "BLAC_99", "COLI_33", "COLI_66",
                                    "COLI_99", "EBON_33", "EBON_66", "EBON_99", "ELBO_33", "ELBO_66", "ELBO_99", "HCIL_33", "HCIL_66", "HCIL_99",
                                    "HUIS_33", "HUIS_66", "HUIS_99", "TURK_33", "TURK_66", "TURK_99", "VLS_33",  "VLS_66",  "VLS_99"))


#get basic stats
sum_SGI <- group_by(bio, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(SGI, na.rm = TRUE),
    sd = sd(SGI, na.rm = TRUE),
    median = median(SGI, na.rm = TRUE),
    IQR = IQR(SGI, na.rm = TRUE)
  )

sum_MGT <- group_by(bio, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(MGT, na.rm = TRUE),
    sd = sd(MGT, na.rm = TRUE),
    median = median(MGT, na.rm = TRUE),
    IQR = IQR(MGT, na.rm = TRUE)
  )

sum_pct <- group_by(bio, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(pct, na.rm = TRUE),
    sd = sd(pct, na.rm = TRUE),
    median = median(pct, na.rm = TRUE),
    IQR = IQR(pct, na.rm = TRUE)
  )

sum_length <- group_by(length, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(length_mm, na.rm = TRUE),
    sd = sd(length_mm, na.rm = TRUE),
    median = median(length_mm, na.rm = TRUE),
    IQR = IQR(length_mm, na.rm = TRUE)
  )

#perform Kruskal_Wallis test

kruskal.test(SGI ~ Treatment, data = bio)
#for SGI the chi-sq = 116.83, df= 30, p-value = 3.44e-12
#there is significant variation between the means

kruskal.test(MGT ~ Treatment, data = bio)
#for MGT the chi-sq = 116.45, df = 30 and p-value = 3.985e-12
#there is significant variation between the means

kruskal.test(pct ~ Treatment, data = bio)
#for pct the chi-sq = 43.113, df = 30, p-value = 0.05726
# there is not significant difference between the means percent germination, this does not supprise me

kruskal.test(length_mm ~ Treatment, data = length)
#for length the chi-sq = 306.73, df = 30, p-value = <2.2e-16
#there is a significant difference between the means 

#post-hoc testing will need to be performed on SGI, MGT, and legnth to determine which groups vary significantly 
#from the control