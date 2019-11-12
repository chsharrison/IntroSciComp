#From Timeline
#11/12/2019: 1) Load data into R 
#            2) Make histograms of Data 
#            3) Make Linear Regressions 
#            4) Make Histograms of Residuals


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

# 1) Load data into R
bio<- read.csv("lettucebioassay2.csv", header=TRUE)
length<- read.csv("lettucelength.csv", header=TRUE)

#make sure the column names and types are correct
bio$SGI <- (as.numeric(bio$SGI))
bio$Treatment <- (as.character(bio$Treatment))
names(length)[names(length) == "ï..Treatment"] <- "Treatment"

# Calculate percent germination and add it to bio dataframe
bio$pct <- ((bio$X1 + bio$X2 + bio$X3 + bio$X4 + bio$X5 +bio$X6)/20 *100)

# 2) Make histograms of SGI (seed germination index), seedling length, percent germination, and MGT (mean germination time)

#################SGI#########################
sgi_hist <- hist(bio$SGI)
sgi_hist
#density plot
plot(density(bio$SGI))
# Q-Q plot
qqnorm(bio$SGI); qqline(bio$SGI, col = 2)
#double check normality
shapiro.test(bio$SGI)
#definitely doesn't look to be normally distributed

#what happens if the data is log transformed
bio$SGI_log <- log(bio$SGI)
sgi_log_hist <- hist(bio$SGI_log)
sgi_log_hist
#density plot
plot(density(bio$SGI_log))
# Q-Q plot
qqnorm(bio$SGI_log); qqline(bio$SGI_log, col = 2)
#double check normality
shapiro.test(bio$SGI_log)
#still not normal

#####################length#####################
length_hist <- hist(length$length_mm)
length_hist
#density plot
plot(density(length$length_mm))
#Q-Q plot
qqnorm(length$length_mm); qqline(length$length_mm, col = 2)
#double check normality 
shapiro.test(length$length_mm)
#definitely doesn't look to be normally distributed

#what happens if the data is log transformed
length$length_log <- log(length$length_mm)
length_log_hist <- hist(length$length_log)
length_log_hist
#density plot
plot(density(length$length_log))
#Q-Q plot
qqnorm(length$length_log); qqline(length$length_log, col = 2)
#double check normality 
shapiro.test(length$length_log)
# log transforming the data is even worse!!!



################### percent germination################
pct_hist <- hist(bio$pct)
pct_hist
#density plot
plot(density(bio$pct))
# Q-Q plot
qqnorm(bio$pct); qqline(bio$pct, col = 2)
#double check normality
shapiro.test(bio$pct)
#definitely doesn't look to be normally distributed

#what happens if the data is log transformed
bio$pct_log <- log(bio$pct)
pct_log_hist <- hist(bio$pct_log)
pct_log_hist
#density plot
plot(density(bio$pct_log))
# Q-Q plot
qqnorm(bio$pct_log); qqline(bio$pct_log, col = 2)
#double check normality
shapiro.test(bio$pct_log)
#log transformation makes it worse again!


#################### MGT###############
MGT_hist <- hist(bio$MGT)
MGT_hist
#density plot
plot(density(bio$MGT))
# Q-Q plot
qqnorm(bio$MGT); qqline(bio$MGT, col = 2)
#double check normality
shapiro.test(bio$MGT)
#definitely doesn't look to be normally distributed
#what happens if the data is log transformed
bio$MGT_log <- log(bio$MGT)
MGT_log_hist <- hist(bio$MGT_log)
MGT_log_hist
#density plot
plot(density(bio$MGT_log))
# Q-Q plot
qqnorm(bio$MGT_log); qqline(bio$MGT_log, col = 2)
#double check normality
shapiro.test(bio$MGT_log)
#and again, it is worse!

# 3) & 4) Make linear models and histogram of residuals

############# SGI###############
SGI_treat <- lm(SGI ~ Treatment, bio)
plot(residuals(SGI_treat))
hist(residuals(SGI_treat))

shapiro.test(residuals(SGI_treat))
summary(SGI_treat)


SGI_treat <- lm(SGI ~ Treatment, bio)
plot(residuals(SGI_treat))
hist(residuals(SGI_treat))

shapiro.test(residuals(SGI_treat))
summary(SGI_treat)

###########length#################
length_treat <- lm(length_mm ~ Treatment, length)
plot(residuals(length_treat))
hist(residuals(length_treat))

shapiro.test(residuals(length_treat))
summary(length_treat)

##############Percent germination#############
pct_treat <- lm(pct ~ Treatment, bio)
plot(residuals(pct_treat))
hist(residuals(pct_treat))

shapiro.test(residuals(pct_treat))
summary(pct_treat)


####################MGT####################
MGT_treat <- lm(MGT ~ Treatment, bio)
plot(residuals(MGT_treat))
hist(residuals(MGT_treat))

shapiro.test(residuals(MGT_treat))
summary(MGT_treat)

# It looks like none of these are normally distributed and neither are the residuals
# This means that both the ANOVA and ANCOVA are innappropriate tests for these data sets

### after doing some further research it looks like the PERMANOVA is probably not the correct test either
### there are other non-parometric tests such as the Kruskal Wallis test and the Wilcoxen rank sum test
### I will look into these options 

# https://www.healthknowledge.org.uk/public-health-textbook/research-methods/1b-statistical-methods/parametric-nonparametric-tests 

