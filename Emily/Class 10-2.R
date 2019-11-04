## Instructional code for MARS 5470 Special Topics: Intro to Scientific Computing @ UTRGV
## Written by C.A. Gabler
## Uses public examples and various data of Chris' and from the Gabler Lab
## Created 20191030, largely from existing code, last edit 20191031

######## PART 1 - GENERAL NOTES ABOUT R LANGUAGE/ENVIRONMENT ####

#### 1.1 - Commenting ####

# Putting a pound sign ('#', aka hashtag) at the beginning of a line makes it a 'comment', like this.
# Comment lines are not part of the code itself, it is for the sake of explanation or notes.
# Note that comment lines are in green in RStudio. This helps differentiate code vs. non-code text.

# Putting four #s at the end of a line lets you "fold" everything between that line and the next line with 4 #s ####
# Folding is a way to show and hide sections or long chunks of code for ease of navigation.
# This line and the line before it will be hidden if you fold line 16, because the next 4 #s are on line 19.
# You can fold/umfold a section that by clicking the little arrow next to the row number. ####

#### 1.2 - Case sensetivity ####

# R is case sensitive, so 'A' and 'a' are different symbols.

#### 1.3 - Symbology ####

# Normally all alphanumeric symbols are allowed: A-Z, a-z, and 0-9.
# . and _ are allowed.
# Names must start with . or a letter. If a name starts with . the second character cannot be a digit.
# No (reasonable) limit to the length of names.
# Other characters are allowed depending on the program locale, so different languages/alphabets are okay for
# the console and scripting, but for universal compatability (writing packages), stick to alphanumeric.

#### 1.4 - Elementary commands ####

# Basic commands consist of either expressions or assignments. 
# Expressions are evaluated, printed in the console (unless specifically made invisible), and the value is lost.
# Example:

2 + 2

# Assignments are evaluated (if they include expressions), and the value is passed to a variable, 
# but the result is not automatically printed. 
# Simple assignments use '<-' or '=' to assign values or expressions as a named object.
# Those objects can then be called for use elsewhere, 
# Example:

a = 250
assignment <- 2 + 2

# Just as assignments can include expressions, expressions can include assigned variables.

assignment + assignment

# Commands are separated by new lines (more common) or by a semicolon (';', less common).
# Elementary commands can be grouped into one compound expression by braces ('{' and '}')


#### 1.5 - Objects and data permanency ####

# The entities R creates and manipulates are known as 'objects'.
# Objects include variables, arrays of numbers, character strings, functions, or more general structures 
# built from such components.
# Objects appear in the top-right RStudio window under the relevant heading "Data", "Values", etc.
# Remove objects with the rm() command.

rm(a)
rm(assignment)

# Workspaces can be saved for ease of continuation.
# Different working directories are recommended for separate projects or groups of analyses.

#### 1.6 - More info ####

# For a more complete and thorough walkthrough of core R commands and capabilities, 
# see the R-intro PDF in our shared folder. 


######## PART 2 - GET LOADED ####

# Here we load data and load/install 'packages' for special stuff we like to do with that data.
# Some people prefer to load data/packages deeper within the code only where they are needed. 
# Either way works. I personally like to keep data/packages together at the beginning so it's easier to find later.

# If preferred, you can specify a particular working directory for R using the command:
# setwd("<filepath>")
# where <filepath> is the location of the desired folder, e.g., C:/Not Video Games/R-stats
# You can then use the 'Files' tab in the bottom-right window in RStudio to access the files in this directory.


#### 2.1 - Load data as a data frame ####

# A data frame is basically a table/spreadsheet/matrix of data.
# R can only import certain formats. The standard universal spreadsheet format is Comma Separated Values (CSV).
# Data frames are considered 'objects' in this object-based language.
# Note: When you load a data frame in RStudio, it appears in the top-right window under the "Data" heading in 
# the "Environment" tab.

## The standard way to load a data frame you created outside of R is to point at a CSV file.
setwd("~/UTRGV/Python Class/ Sci Comp guest lecture")
fert <- read.csv("~/UTRGV/Python Class/Sci Comp guest lecture/basic_stats.csv", header=T)
#fert <- read.csv("C:/Users/yjt362/Google Drive/000 R-stats/shared/Sci Comp guest lecture/basic_stats.csv", header=T)
#fert <- read.csv("E:/Google Drive/000 R-stats/shared/Sci Comp guest lecture/basic_stats.csv", header=T)

# The file path is unique to your computer. Pay attention to where you save things.

# To be prompted to select a file, use the command:
fert <- read.csv(file.choose())

#### 2.2 - Load any required packages for analyses or modeling of focal data ####

install.packages("car")
require(car)
library(car)

if(!require(car)){install.packages("car")}
# needed for type III sums of squares (very important)

# tweak program options to allow for type III sums of squares (trust me, this is important) 
options(contrasts=c("contr.sum", "contr.poly"))

install.packages("ggplot2")
require(ggplot2)
library(ggplot2)
View(fert)

# needed for premo graphing capabilities
install.packages("Hmisc")
require(Hmisc)
# needed for some stat functions in ggplot2

install.packages("pscl")
require(pscl)
# needed for some model fitting and plotting functions


######## PART 3 - LINEAR MODEL FAMILY: REGRESSION, ANOVA, LOGISTIC REGRESSION ####

#### 3.1 - Regression ####

### 3.1.1 - Was there a relationship between starting height and change in height?

## First, basic visualization to explore data. 

# vanilla integrated plot function
plot(fert$start.height, fert$change.height)

## more plotting options using ggplot2
# ggplot2 is a powerful and elegant graphing tool for both simple and complex professional-quality figures.
ggplot(fert, aes(start.height, change.height)) + geom_point() 
# jitter to avoid overplotting, not really necessary here
ggplot(fert, aes(start.height, change.height)) + geom_jitter(width=0.1, height=0.1)
# add a best fit line using the geom_smooth function, linear model is specified as the method here
ggplot(fert, aes(start.height, change.height)) + geom_point() + geom_smooth(method = "lm", se = FALSE) 
# add a confidence band for the best fit line by showing the standard error
ggplot(fert, aes(start.height, change.height)) + geom_point() + geom_smooth(method = "lm", se = TRUE)

## EXERCISE: Why is the confidence band shaped like a bowtie?

##################################################################
#So that it will include zero is there is a little bit of a tilt#
#################################################################

## EXERCISE: How is bowtie tilt related to statistical significance of a regression model?

###############################################################################
#if bowtie does not cover zero then you cannot say the null hypothesis is true#
##############################################################################

## Correlation tests
cor.test(fert$change.height, fert$start.height)
cor(fert$change.height, fert$start.height) # quick version, no stats

# Linear model fit
start.change <- lm(change.height ~ start.height, fert)

# Extract model results
summary(start.change)

## EXERCISE: Was there a relationship between starting height and change in height?

###################
# no there was not#
##################

## EXERCISE: Interpreting linear model output for regressions


### 3.1.2 - Was there a relationship between soil moisture and change in height?

# visualize
plot(fert$soil.moisture, fert$change.height)
ggplot(fert, aes(soil.moisture, change.height)) + geom_point() 
ggplot(fert, aes(soil.moisture, change.height)) + geom_jitter(width=0.1, height=0.1)
ggplot(fert, aes(soil.moisture, change.height)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(fert, aes(soil.moisture, change.height)) + geom_point() + geom_smooth(method = "lm", se = TRUE)

## EXERCISE: What does bowtie tilt tell you is the likely result?

#relationship is likely significant 

# correlation test
cor.test(fert$change.height, fert$soil.moisture)
cor(fert$change.height, fert$soil.moisture)

# linear model
moist.change <- lm(change.height ~ soil.moisture, fert)
summary(moist.change)

## EXERCISE: Was there a relationship between soil moisture and change in height?

#########################################################################
#Yes there was a positive relationship between soil moisture and height#
########################################################################

### 3.1.3 - Multiple regression

# visualize
ggplot(fert, aes(start.height, change.height)) + geom_point() + geom_smooth(method = "lm", se = TRUE)
ggplot(fert, aes(soil.moisture, change.height)) + geom_point() + geom_smooth(method = "lm", se = TRUE)

## EXERCISE: How does one visualize a multiple regression?

#######################################################################################
#usually in a series of plots, otherwise can be done by building a model with multiple#
#variables and compare                                                                #  
#######################################################################################

# linear model
mr1.change <- lm(change.height ~ start.height + soil.moisture, fert)
summary(mr1.change)
summary(moist.change)
summary(start.change)

## EXERCISE: What is actually happening here?

####################################################################
#starting height is not a predictor of the change in height because#
#no treatments had been applied yet#
############################################################################

######## 3.2 - ANOVA ####

### 3.2.1 - Did fertilizer influence the height of plants at the start of the experiment?

## First, basic visualization to explore data. 

# Vanilla integrated plot function
plot(fert$fertilizer, fert$start.height)
# Box and whisker plots show the median (not mean) and quartile cutoffs.

# More ggplot2 options for categorical data; more on ggplot2 to come
ggplot(fert, aes(fertilizer, start.height)) + 
  geom_point() 
ggplot(fert, aes(fertilizer, start.height)) + 
  geom_jitter(width = 0.1, height = 0) 
ggplot(fert, aes(fertilizer, start.height)) + 
  geom_bar(stat="summary") 
ggplot(fert, aes(fertilizer, start.height)) + geom_bar(stat="summary") + 
  stat_summary(fun.data = "mean_cl_boot", colour="red", size=0.5)
ggplot(fert, aes(fertilizer, start.height)) + 
  stat_summary(fun.data = "mean_cl_boot", colour="red", size=2)
ggplot(fert, aes(fertilizer, start.height)) + 
  geom_jitter(width = 0.05, height = 0) + 
  stat_summary(fun.data = "mean_cl_boot", colour="red", size=1)
ggplot(fert, aes(fertilizer, start.height)) + 
  geom_jitter(width = 0.05, height = 0, aes(colour = fertilizer)) + 
  stat_summary(fun.data = "mean_cl_boot", colour="black", size=1)

# So, a difference in average starting height, but is this difference significant?

##############################################################
#No, confidence interval overlap and spread of data is similar#
###############################################################

## Second, do we meet assumption of normality?

hist(fert$start.height)
hist(fert$start.height, breaks = 20)
# not looking much like a bell

plot(density(fert$start.height))
# definitely not a very normal curve

qqnorm(fert$start.height)
qqline(fert$start.height, col = 2)
qqnorm(fert$start.height); qqline(fert$start.height, col = 2)
# So, uhm, yep, there are points and a line there and...?

## EXERCISE: So is this normal or not? Will you answer stand up to peer review?

##################################################################################
#No it is not, not bell shaped and the edges of the qq plot diverge from the line#
##################################################################################

# There's a test for this: Shapiro-Wilk test for normality performs a clear-cut hypothesis test about 
# whether data conform to normality. It is easily defensible, but sensitive... 
# Chris will probably rant here a bit about the robustness of linear models to non-normality.
shapiro.test(fert$start.height)

## EXERCISE: What does this P-value mean? 

###############################################################################
#this p-value says that there is a very very low chance that the data is normal# 
###############################################################################

## Fit the model. 

fert.start <- lm(start.height ~ fertilizer, fert)
fert.start.log <- lm(log(start.height) ~ fertilizer, fert)
fert.start.sqrt <- lm(sqrt(start.height) ~ fertilizer, fert)

## EXERCISE: My previous question about normality was a trick question. Why?

######################################################
#the residuals need to be normal, not the data itself#
######################################################

#### Answer hidden ####
# Contrary to popular belief, data do NOT need to be normal to meet the assumptions of ANOVA.
# Rather, the RESIDUALS must follow a normal distribution. We'll come back to this. 
####

#### ####
plot(residuals(fert.start))
plot(residuals(fert.start.log))
plot(residuals(fert.start.sqrt))

hist(residuals(fert.start))
hist(residuals(fert.start.log))
hist(residuals(fert.start.sqrt))

shapiro.test(residuals(fert.start))
shapiro.test(residuals(fert.start.log))
shapiro.test(residuals(fert.start.sqrt))

# Note the syntax here.
# Note nothing appeared, but now we have the model as an object.

## Extract model results.

summary(fert.start)
# Note these are not ANOVA output, this is model info

# aov() is the omnibus call for ANOVA, but it is not always appropriate.
aov(fert.start)
summary(aov(fert.start))
# Note similarities with summary() for some values 

# Not all ANOVAs are the same. The default ANOVA in R uses Type I Sums of Squares (SS). 
# The way SS are calculated has to do with how variance is partitioned. 
# The key points for now is that the order of variables matters with Type I SS, 
# and that is usually NOT what we want.
# In most cases it is better to use Type III (aka marginal) SS, where the order of variables does not matter.

# The easiest way to use Type III SS is to use the Anova() function in the 'car' package and specify 'type="III"'
Anova(fert.start, type="III")
# This is usually what you are after with ANOVAs

## Quick way to get ANOVA output 

summary(aov(fert$start.height ~ fert$fertilizer))


### 3.2.2 - Did fertilizer influence the change in height during the experiment?

## visualize
plot(fert$fertilizer, fert$change.height)
ggplot(fert, aes(fertilizer, change.height)) + 
  geom_point() 
ggplot(fert, aes(fertilizer, change.height)) + 
  geom_jitter(width = 0.05, height = 0) 
ggplot(fert, aes(fertilizer, change.height)) + 
  geom_bar(stat="summary") 
ggplot(fert, aes(fertilizer, change.height)) + 
  geom_bar(stat="summary") + 
  stat_summary(fun.data = "mean_cl_boot", colour="red", size=0.5)
ggplot(fert, aes(fertilizer, change.height)) + 
  stat_summary(fun.data = "mean_cl_boot", colour="red", size=2)
ggplot(fert, aes(fertilizer, change.height)) + 
  geom_jitter(width = 0.05, height = 0) + 
  stat_summary(fun.data = "mean_cl_boot", colour="red", size=1)
ggplot(fert, aes(fertilizer, change.height)) + 
  geom_jitter(width = 0.05, height = 0, aes(colour=fertilizer)) + 
  stat_summary(fun.data = "mean_cl_boot", colour="black", size=1)

## fit and evaluate
fert.change <- lm(change.height ~ fertilizer, fert)
summary(fert.change)
Anova(fert.change, type="III")
# one step method (simpler but less control)
summary(aov(fert$change.height ~ fert$fertilizer))

hist(fert$change.height)
hist(residuals(fert.change))

shapiro.test(fert$change.height)
shapiro.test(residuals(fert.change))

#### 3.3 - Logistic regression ####

### Did initial plant height influence whether a plant produced fruit?

# visualize
plot(fert$start.height, fert$fruit.YN)
plot(fert$start.height, fert$fruit.bin)

as.factor(fert$fruit.bin)

# fit model
moist.fruit <- glm(fruit.bin ~ start.height, family=binomial, fert)
moist.fruit <- glm(fruit.YN ~ start.height, family=binomial, fert)
# Note we're using glm() here instead of lm(). One thing glm() does that lm() does not is allow us to specify 
# different distribution families and link functions. More on these concepts later.
# For logistic regressions, we want the 'binomial' distribution family.

summary(moist.fruit)

aov(moist.fruit)
summary(aov(moist.fruit))

## EXERCISE: Why doesn't aov() work with 'fruit.YN'?

Anova(moist.fruit, type="III")

# more plotting options
plot(fert$start.height, fert$fruit.YN)
ggplot(fert, aes(start.height, fruit.bin)) + geom_point() 
ggplot(fert, aes(start.height, fruit.bin)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(fert, aes(start.height, fruit.bin)) + geom_point() + geom_smooth(method = "lm", se = TRUE)


#### Real logistic regression example: elevation bounds of tidal wetlands (from USGS work) ####
macf <- read.csv("C:/Users/Chris/Google Drive/000 R-stats/shared/Sci Comp guest lecture/macroclimate n960 15-08-23 for elev bounds.csv",header=T)
macf <- read.csv("C:/Users/yjt362/Google Drive/000 R-stats/shared/Sci Comp guest lecture/macroclimate n960 15-08-23 for elev bounds.csv",header=T)
macf <- read.csv("E:/Google Drive/000 R-stats/shared/Sci Comp guest lecture//macroclimate n960 15-08-23 for elev bounds.csv",header=T)

require(pscl)

# pick estuary if doing individually
i = "Lower_Laguna_Madre"
i = "Upper_Laguna_Madre"
i = "Mission-Aransas_Bay"
i = "San_Antonio_Bay"
i = "Galveston_Bay"
i = "Lake_Pontchartrain"
i = "Grand_Bay"
i = "Weeks_Bay"
i = "Tampa_Bay"
i = "Ten_Thousand_Islands"


mac <- subset(macf, estuary==i)

## EXERCISE: Look at the dataset, discuss objectives

plot(mac$elrelHW, mac$filter2)

split <- mean(mac$elrelHW) # adjust for SA
#split <- -0.25
#plot(elrelHW, critF, cex=1.3)
#abline(v=split)

macflo <- subset(macf, (estuary==i & !(filter2==0 & elrelHW>split)))
macfup <- subset(macf, (estuary==i & !(filter2==0 & elrelHW<split)))

# test trim worked
plot(macflo$elrelHW, macflo$filter2)
plot(macfup$elrelHW, macfup$filter2)

# fit LogReg models
macfLB <- glm(filter2 ~ elrelHW, family=binomial, macflo)
macfUB <- glm(filter2 ~ elrelHW, family=binomial, macfup)

## Summary stats
summary(macfLB)
log(0.5/(1-0.5))-coef(macfLB)[1]/coef(macfLB)[2]
with(macfLB, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=FALSE))
Anova(macfLB, type="III")
pR2(macfLB)

summary(macfUB)
log(0.5/(1-0.5))-coef(macfUB)[1]/coef(macfUB)[2]
with(macfUB, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=FALSE))
pR2(macfUB)

# expression for wolframAlpha
# 1/(1 + exp(-1.59701-11.066289*x))

#### create logistic regression figs for each boundary per site ####
lowD1 <- log(0.5/(1-0.5))-coef(macfLB)[1]/coef(macfLB)[2]
upD1 <- log(0.5/(1-0.5))-coef(macfUB)[1]/coef(macfUB)[2]

#bar1 = -0.7269
#bar2 = -0.4906
#bar3 = 0.0062
#bar4 = 0.3854

# lower  
#png(paste("C:/Users/Chris/Google Drive/0 USGS/1 macroclimate project figs/bound elevs/",
#          mac$estuary,"- low bound elrelHW.png"), 
#    height=9, width=12, units="in",res=200)
par(oma=c(0,1,0,0))
x <- seq(min(mac$elrelHW), max(mac$elrelHW), 0.01)
y <- predict(macfLB, list(elrelHW = x), type="response")
plot(filter2 ~ elrelHW, macflo, main = i)
lines(x,y, lwd=2, col="blue")
abline(v=lowD1, col="red")
#abline(v=bar1, col="green")
#abline(v=bar2, col="green")
#dev.off()

# upper
#png(paste("C:/Users/Chris/Google Drive/0 USGS/1 macroclimate project figs/bound elevs/",
#          mac$estuary,"- up bound elrelHW.png"), 
#    height=9, width=12, units="in",res=200)
par(oma=c(0,1,0,0))
x <- seq(min(mac$elrelHW), max(mac$elrelHW), 0.01)
y <- predict(macfUB, list(elrelHW = x), type="response")
plot(filter2 ~ elrelHW, macfup, main = i)
lines(x,y, lwd=2, col="blue")
abline(v=upD1, col="red")
#abline(v=bar3, col="green")
#abline(v=bar4, col="green")
#dev.off()

######## PART 4 - GENERALIZED LINEAR MODELS ####

#### 4.1 - Real example: Gus Plamann brine shrimp biofloc experiment ####

#### 4.1.1 - load packages

install.packages("ggplot2")
install.packages("plyr")
install.packages("boot")
install.packages("sem")
install.packages("aod")
install.packages("vegan")
install.packages("lme4")
install.packages("car")
install.packages("agricolae")
install.packages("Hmisc")
install.packages("pscl")
install.packages("AER")
install.packages("vcdExtra")

require(ggplot2)
require(plyr)
require(boot)
require(sem)
require(aod)
require(vegan)
require(lme4)
require(car)
require(agricolae)
require(Hmisc)
require(pscl)
require(AER)
require(vcdExtra)

#### 4.1.2 - load data

bsbf <- read.csv("C:/Users/yjt362/Google Drive/000 R-stats/shared/Sci Comp guest lecture/gus brine biofloc 190715.csv", header=T)
bsbf <- read.csv("D:/Google Drive/000 R-stats/shared/Sci Comp guest lecture/gus brine biofloc 190715.csv", header=T)
bsbf <- read.csv("E:/Google Drive/000 R-stats/shared/Sci Comp guest lecture/gus brine biofloc 190715.csv", header=T)

bsbf.size <- read.csv("C:/Users/yjt362/Google Drive/000 R-stats/shared/Sci Comp guest lecture/gus brine biofloc size 190718.csv", header=T)
bsbf.size <- read.csv("E:/Google Drive/000 R-stats/shared/Sci Comp guest lecture/gus brine biofloc size 190718.csv", header=T)

str(bsbf)
bsbf$day <- as.factor(bsbf$day)

str(bsbf.size)
bsbf.size$day <- as.factor(bsbf.size$day)

## subset data

#bucks.levels <- subset(bucks, water.level.expt=="y")
bsbf.nod12 <- subset(bsbf, timing!="Day 1")
bsbf.nod12 <- subset(bsbf.nod12, timing!="Day 2")

bsbf.size.nod12 <- subset(bsbf.size, timing!="Day 1")
bsbf.size.nod12 <- subset(bsbf.size.nod12, timing!="Day 2")

bsbf.timing <- subset(bsbf, biofloc!="High")
bsbf.size.timing <- subset(bsbf.size, biofloc!="High")

## set working directory

setwd("C:/Users/yjt362/Google Drive/000 R-stats/shared/Sci Comp guest lecture")
setwd("D:/Google Drive/000 R-stats/shared/Sci Comp guest lecture")
setwd("E:/Google Drive/000 R-stats/shared/Sci Comp guest lecture")



#### 4.1.3 - Fit models 

## standard Poisson GLM

total.pois.nod12 <- glm(total ~ biofloc * temp * light + day, family = poisson, bsbf.nod12)
summary(total.pois.nod12)
anova(total.pois.nod12, test = "Chisq")
Anova(total.pois.nod12, type="III")

# But is this zero-inflated?
hist(bsbf.nod12$total)
zero.test(bsbf.nod12$total)

## Zero-inflated Poisson

total.zip.2ord.nod12 <- zeroinfl(total ~ biofloc + temp + light + day +
                                    biofloc:temp + biofloc:light + biofloc:day +
                                    temp:light + temp:day + light:day | temp + light + day, 
                                  dist = "poisson", bsbf.nod12)
summary(total.zip.2ord.nod12)
Anova(total.zip.2ord.nod12, type="III")

# Variance inflation test to confirm we don't have autocorrelated variables
vif(total.pois.nod12)
# 10 or higher suggests correlated predictor variables

# But is this overdispersed? 

mean(bsbf.nod12$total)
var(bsbf.nod12$total)

dispersiontest(total.zip.2ord.nod12, trafo = 1)

total.pois.2ord.nod12 <- glm(total ~ biofloc + temp + light + day +
                                   biofloc:temp + biofloc:light + biofloc:day +
                                   temp:light + temp:day + light:day,
                             family = "poisson", bsbf.nod12)

dispersiontest(total.pois.2ord.nod12, trafo = 1)

## Zero-inflated negative binomial

total.zinb.2ord.nod12 <- zeroinfl(total ~ biofloc + temp + light + day +
                                    biofloc:temp + biofloc:light + biofloc:day +
                                    temp:light + temp:day + light:day | temp + light + day, 
                                  dist = "negbin", bsbf.nod12)
# Do not need to specify "trials" in this context

summary(total.zinb.2ord.nod12)
Anova(total.zinb.2ord.nod12, type="III")

vif(total.zinb.2ord.nod12)

## EXERCISE: Interpreting model results

#### 4.2 - Graphs from real example ####

temp.labels <- c("24" = "24 C", "27" = "27 C")
light.labels <- c("High" = "High light", "Low" = "Low light")
timing.labels <- c("Control" = "Control", "Day 0" = "Added day 0", "Day 1" = "Added day 1", "Day 2" = "Added day 2")
biofloc.labels <- c("Control" = "Control", "High" = "High biofloc", "Low" = "Low biofloc")

## hatched by biofloc 
ggplot(bsbf.nod12, aes(biofloc, total)) + 
  geom_jitter(width = 0.25, height = 0, aes(colour = biofloc), size = 2.5, shape = "circle open", stroke = 1) +
  scale_color_manual(values = c("dodgerblue", "red", "orange")) +
  stat_summary(fun.data = "mean_cl_boot", colour="black", size = 1) + 
  #facet_wrap(~temp) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Bioflocculant addition", y = "Total brine shrimp hatched", scale_size) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("hatched x biofloc nod12.png", width = 4.5, height = 4.5)

# post-hoc
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$biofloc, p.adj="none")
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$biofloc, p.adj="holm")

## hatched by temp 
ggplot(bsbf.nod12, aes(temp, total)) + 
  geom_jitter(width = 0.25, height = 0, aes(colour = temp), size = 2.5, shape = "circle open", stroke = 1) +
  scale_color_manual(values = c("darkblue","indianred")) +
  stat_summary(fun.data = "mean_cl_boot", colour="black", size = 1) + 
  #facet_wrap(~temp) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Water temperature", y = "Total brine shrimp hatched", scale_size) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("hatched x temp nod12.png", width = 3.5, height = 4.5)

# post-hoc
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$temp, p.adj="none")
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$temp, p.adj="holm")

## hatched by day
ggplot(bsbf.nod12, aes(day, total)) + 
  geom_jitter(width = 0.25, height = 0, aes(colour = day), size = 2.5, shape = "circle open", stroke = 1) +
  scale_color_manual(values = c("gray60", "gray45", "gray30", "gray15", "gray0")) +
  #scale_color_manual(values = c("slategray1", "slategray2", "slategray3", "slategray4", "gray20")) +
  stat_summary(fun.data = "mean_cl_boot", colour="black", size = 1) + 
  #facet_wrap(~day) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Days after hydrating cysts", y = "Total brine shrimp hatched", scale_size) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("hatched x day nod12.png", width = 4.5, height = 4.5)

# post-hoc
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$day, p.adj="none")
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$day, p.adj="holm")
# AB A BC C C

## hatched x biofloc + temp  
ggplot(bsbf.nod12, aes(temp, total)) + 
  geom_jitter(width = 0.25, height = 0, aes(colour = biofloc), size = 2.5, shape = "circle open", stroke = 1) +
  scale_color_manual(values = c("dodgerblue", "red", "orange")) +
  stat_summary(fun.data = "mean_cl_boot", colour="black", size = 1) + 
  facet_wrap(~biofloc, labeller = labeller(biofloc = biofloc.labels)) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Water temperature", y = "Total brine shrimp hatched", scale_size) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("hatched x biofloc, temp nod12.png", width = 6.5, height = 4.5)

# post-hoc
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$biofloc:bsbf.nod12$temp, p.adj="none")
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$biofloc:bsbf.nod12$temp, p.adj="holm")
# ab b a b a b

## hatched x biofloc + light 
ggplot(bsbf.nod12, aes(light, total)) + 
  geom_jitter(width = 0.25, height = 0, aes(colour = biofloc), size = 2.5, shape = "circle open", stroke = 1) +
  scale_color_manual(values = c("dodgerblue", "red", "orange")) +
  stat_summary(fun.data = "mean_cl_boot", colour="black", size = 1) + 
  facet_wrap(~biofloc, labeller = labeller(biofloc = biofloc.labels)) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Light treatment", y = "Total brine shrimp hatched", scale_size) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("hatched x biofloc, light nod12.png", width = 6.5, height = 4.5)

# post-hoc
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$biofloc:bsbf.nod12$light, p.adj="none")
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$biofloc:bsbf.nod12$light, p.adj="holm")
# b b a b ab b

## hatched x biofloc + day 
ggplot(bsbf.nod12, aes(day, total)) + 
  geom_jitter(width = 0.25, height = 0, aes(colour = biofloc), size = 2.5, shape = "circle open", stroke = 1) +
  scale_color_manual(values = c("dodgerblue", "red", "orange")) +
  stat_summary(fun.data = "mean_cl_boot", colour="black", size = 1) + 
  facet_wrap(~biofloc, labeller = labeller(biofloc = biofloc.labels)) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Days after hydrating cysts", y = "Total brine shrimp hatched", scale_size) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("hatched x biofloc, day nod12.png", width = 6.5, height = 4.5)

# post-hoc
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$biofloc:bsbf.nod12$day, p.adj="none")
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$biofloc:bsbf.nod12$day, p.adj="none")
# bc a bc c c  ab ab abc bc bc  a ab bc c c


## hatched x light + day  
ggplot(bsbf.nod12, aes(day, total)) + 
  geom_jitter(width = 0.25, height = 0, aes(colour = light), size = 2.5, shape = "circle open", stroke = 1) +
  scale_color_manual(values = c("orange","grey30")) +
  stat_summary(fun.data = "mean_cl_boot", colour="black", size = 1) + 
  facet_wrap(~light, labeller = labeller(light = light.labels)) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Days after hydrating cysts", y = "Total brine shrimp hatched", scale_size) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("hatched x light, day nod12.png", width = 6.5, height = 4.5)

# post-hoc
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$light:bsbf.nod12$day, p.adj="none")
pairwise.t.test(bsbf.nod12$total, bsbf.nod12$day:bsbf.nod12$light, p.adj="holm")

######## PART 5 - MIXED EFFECT MODELS ####

#### 5.1 - Real example: Drew Corder nitrogen assimilation experiment ###

## load packages
install.packages("ggplot2")
install.packages("plyr")
install.packages("boot")
install.packages("sem")
install.packages("aod")
install.packages("vegan")
install.packages("lme4")
install.packages("car")
install.packages("agricolae")
install.packages("Hmisc")
install.packages("devtools")
install_github("vqv/ggbiplot")
install.packages("lmtest")
install.packages("pscl")
install.packages("AER")

require(ggplot2)
require(plyr)
require(boot)
require(sem)
require(aod)
require(vegan)
require(lme4)
require(car)
require(agricolae)
require(Hmisc)
require(ggbiplot)
require(lmtest)
require(pscl)
require(AER)

require(multcompView)
library(lsmeans)
require(lsmeans)

if(!require(psych)){install.packages("psych")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(rcompanion)){install.packages("rcompanion")}

require(devtools)

options(contrasts=c("contr.sum", "contr.poly"))

## load data

bucks <- read.csv("D:/Google Drive/000 data unpublished/drew buckets/drew buckets 190920.csv", header=T)
bucks <- read.csv("C:/Users/yjt362/Google Drive/000 data unpublished/drew buckets/drew buckets 190920.csv", header=T)
bucks <- read.csv("E:/Google Drive/000 data unpublished/drew buckets/drew buckets 190920.csv", header=T)

setwd("E:/Google Drive/000 data unpublished/drew buckets/figs")
setwd("C:/Users/yjt362/Google Drive/000 data unpublished/drew buckets/figs")

## Subset Data 

bucks.levels <- subset(bucks, water.level.expt=="y")
bucks.levels.now5 <- subset(bucks.levels, week!="5")
bucks.levels.biom <- subset(bucks.levels, week=="10")

## Fit models
library(nlme)

## optional: calculate autocorrelation structure using ACF function
log.amm.perc.a <- gls(log(norm.tot.amm.perc) ~ species * water.level * week,
                      data=bucks.levels.now5)

ACF(log.amm.perc.a,
    form = ~ week | bucket)

log.amm.perc.b <- lme(log(norm.tot.amm.perc) ~ species * water.level * week,
                      random = ~1|bucket,
                      data = bucks.levels.now5)

ACF(log.amm.perc.b)

## fit repeated measures ANOVA with bucket as random subject
?corClasses

log.amm.perc <- lme(log(norm.tot.amm.perc) ~ species * water.level * week,
                    random = ~1 | bucket,
                    correlation = corAR1(form = ~ week | bucket,
                                         value = 0.1568),
                    data=bucks.levels.now5,
                    method="REML")

Anova(log.amm.perc)
#Anova(amm.perc, type = 3)

# test random effects
log.amm.perc.fixed <- gls(log(norm.tot.amm.perc) ~ species * water.level * week,
                          data=bucks.levels.now5,
                          method="REML")

# Test for effect of bucket (random subject factor)
anova(log.amm.perc,
      log.amm.perc.fixed)

# model p-value and pseudo R-squared
log.amm.perc.null = lme(log(norm.tot.amm.perc) ~ 1,
                        random = ~1 | bucket,
                        data = bucks.levels.now5)

nagelkerke(log.amm.perc,
           log.amm.perc.null)

log.amm.perc.null.2 = gls(log(norm.tot.amm.perc) ~ 1,
                          data = bucks.levels.now5)

# model P and pseudo R-squared values
nagelkerke(log.amm.perc,
           log.amm.perc.null.2)

# post-hoc for lme using lsmeans
require(multcompView)
library(lsmeans)
require(lsmeans)

marginal.s = lsmeans(log.amm.perc,
                     ~ species)
cld(marginal.s,
    alpha   = 0.05,
    Letters = letters,     ### Use lower-case letters for .group
    adjust  = "tukey")     ###  Tukey-adjusted comparisons

marginal.l = lsmeans(log.amm.perc,
                     ~ water.level)
cld(marginal.l,
    alpha   = 0.05,
    Letters = letters,     ### Use lower-case letters for .group
    adjust  = "tukey")     ###  Tukey-adjusted comparisons

marginal.sl = lsmeans(log.amm.perc,
                      ~ species:water.level)
cld(marginal.sl,
    alpha   = 0.05,
    Letters = letters,     ### Use lower-case letters for .group
    adjust  = "tukey")     ###  Tukey-adjusted comparisons


#### plots of log(norm.tot.amm.perc) ####
levels.labels <- c("Below.surface" = "Below surface", "Saturated" = "Saturated", "Flood" = "Flood")
bucks.levels.now5$water.level <- factor(bucks.levels.now5$water.level, c("Below.surface", "Saturated", "Flood"))

ggplot(bucks.levels.now5, aes(species, log(norm.tot.amm.perc))) + 
  geom_jitter(width = 0.25, height = 0, aes(colour = species), size = 2.5, shape = "circle open", stroke = 1) +
  scale_color_manual(values = c("red","gray30","orange","green","blue","purple","magenta")) +
  stat_summary(fun.data = "mean_cl_boot", colour="black", size = 1) + 
  #facet_wrap(~water.level, labeller = labeller(water.level = levels.labels)) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Species", y = "log (Normalized ammoniacal N)", scale_size) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("log.amm.perc.now5 x S.png", width = 6.5, height = 4.5)

ggplot(bucks.levels.now5, aes(water.level, log(norm.tot.amm.perc))) + 
  geom_jitter(width = 0.25, height = 0, aes(colour = water.level), size = 2.5, shape = "circle open", stroke = 1) +
  scale_color_manual(values = c("red","green","blue")) +
  stat_summary(fun.data = "mean_cl_boot", colour="black", size = 1) + 
  #facet_wrap(~water.level, labeller = labeller(water.level = levels.labels)) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Water level", y = "log (Normalized ammoniacal N)", scale_size) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("log.amm.perc.now5 x L.png", width = 4.5, height = 4.5)

ggplot(bucks.levels.now5, aes(species, log(norm.tot.amm.perc))) + 
  geom_jitter(width = 0.25, height = 0, aes(colour = species), size = 2.5, shape = "circle open", stroke = 1) +
  scale_color_manual(values = c("red","gray30","orange","green","blue","purple","magenta")) +
  stat_summary(fun.data = "mean_cl_boot", colour="black", size = 1) + 
  facet_wrap(~water.level, labeller = labeller(water.level = levels.labels)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Species", y = "log (Normalized ammoniacal N)", scale_size) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("log.amm.perc.now5 x SL.png", width = 6.5, height = 5.5)

#### interaction plots with time ####
base <- bucks.levels.now5$norm.tot.amm.perc
bucks.levels.now5$norm.tot.amm.perc  <- base

bucks.levels.now5$norm.tot.amm.perc <- log(bucks.levels.now5$norm.tot.amm.perc)
log <- bucks.levels.now5$norm.tot.amm.perc 

sum.log.amm.perc.sw <- groupwiseMean(norm.tot.amm.perc ~ species + week,
                                     data   = bucks.levels.now5,
                                     conf   = 0.95,
                                     digits = 3,
                                     traditional = FALSE,
                                     percentile  = TRUE)

sum.log.amm.perc.sw

pd = position_dodge(0.5)

ggplot(sum.log.amm.perc.sw, aes(x = week,
                                y = Mean,
                                color = species)) +
  scale_color_manual(values = c("red","gray30","orange","green","blue","purple","magenta")) +
  geom_pointrange(aes(ymin=Percentile.lower,
                      ymax=Percentile.upper),
                  size = 0.7, position=pd) +
  guides(colour = guide_legend(title.theme = element_text(size = 14, face = "bold"),
                               title = "Species",
                               label.theme = element_text(size = 12))) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  ylab("log (normalized Ammoniacal N)") + 
  xlab("Week") +
  theme(strip.text.x = element_text(size = 14, face = "bold"))


ggsave("log.amm.perc.now5 x WS.png", width = 6.5, height = 4.5)

sum.log.amm.perc.lw <- groupwiseMean(norm.tot.amm.perc ~ water.level + week,
                                     data   = bucks.levels.now5,
                                     conf   = 0.95,
                                     digits = 3,
                                     traditional = FALSE,
                                     percentile  = TRUE)

sum.log.amm.perc.lw

pd = position_dodge(0.4)

ggplot(sum.log.amm.perc.lw, aes(x = week,
                                y = Mean,
                                color = water.level)) +
  scale_color_manual(values = c("red","green","blue")) +
  geom_pointrange(aes(ymin=Percentile.lower,
                      ymax=Percentile.upper),
                  size = 0.7, position=pd) +
  guides(colour = guide_legend(title.theme = element_text(size = 14, face = "bold"),
                               title = "Water level",
                               label.theme = element_text(size = 12))) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  ylab("log (Normalized ammoniacal N)") + 
  xlab("Week")

ggsave("log.amm.perc.now5 x WL.png", width = 6.5, height = 4.5)

bucks.levels.now5$norm.tot.amm.perc  <- base

#### assumption check ####
library(rcompanion)

x = residuals(log.amm.perc)

plotNormalHistogram(x)

shapiro.test(residuals(log.amm.perc))

plot(fitted(log.amm.perc),
     residuals(log.amm.perc))

bptest(log.amm.perc)
