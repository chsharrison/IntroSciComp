# All code used for analysis and figures for my final project is in this notebook

#Load Libraries
library(dplyr) #for adata manipulation
library(ggplot2) #for plotting
library(gridExtra) #allows for multiplot figures
library("ggpubr") #this is a package for making figures that I have never used before


#set working directory 
setwd("~/UTRGV/Python Class")

# Load data into R
bio<- read.csv("lettucebioassay2.csv", header=TRUE)
length<- read.csv("lettucelength.csv", header=TRUE)

#make sure the column names and types are correct
bio$SGI <- (as.numeric(bio$SGI))
bio$Treatment <- (as.character(bio$Treatment))
names(length)[names(length) == "ï..Treatment"] <- "Treatment"

# Calculate percent germination and add it to bio dataframe
bio$pct <- ((bio$X1 + bio$X2 + bio$X3 + bio$X4 + bio$X5 +bio$X6)/20 *100)

# Make histograms of SGI (seed germination index), seedling length, percent germination, and MGT (mean germination time)

#################SGI#########################
par(mfrow=c(2,2))
sgi_hist <- hist(bio$SGI)
sgi_hist
# Q-Q plot
qqnorm(bio$SGI); qqline(bio$SGI, col = 2)
#double check normality
shapiro.test(bio$SGI)
#definitely doesn't look to be normally distributed

#what happens if the data is log transformed
bio$SGI_log <- log(bio$SGI)
sgi_log_hist <- hist(bio$SGI_log)
sgi_log_hist
# Q-Q plot
qqnorm(bio$SGI_log); qqline(bio$SGI_log, col = 2)
#double check normality
shapiro.test(bio$SGI_log)
#still not normal

#####################length#####################
par(mfrow=c(2,2))
length_hist <- hist(length$length_mm)
length_hist
#Q-Q plot
qqnorm(length$length_mm); qqline(length$length_mm, col = 2)
#double check normality 
shapiro.test(length$length_mm)
#definitely doesn't look to be normally distributed

#what happens if the data is log transformed
length$length_log <- log(length$length_mm)
length_log_hist <- hist(length$length_log)
length_log_hist
#Q-Q plot
qqnorm(length$length_log); qqline(length$length_log, col = 2)
#double check normality 
shapiro.test(length$length_log)
# log transforming the data is even worse!!!



################### percent germination################
par(mfrow=c(2,2))
pct_hist <- hist(bio$pct)
pct_hist
# Q-Q plot
qqnorm(bio$pct); qqline(bio$pct, col = 2)
#double check normality
shapiro.test(bio$pct)
#definitely doesn't look to be normally distributed

#what happens if the data is log transformed
bio$pct_log <- log(bio$pct)
pct_log_hist <- hist(bio$pct_log)
pct_log_hist
# Q-Q plot
qqnorm(bio$pct_log); qqline(bio$pct_log, col = 2)
#double check normality
shapiro.test(bio$pct_log)
#log transformation makes it worse again!


#################### MGT###############
par(mfrow=c(2,2))
MGT_hist <- hist(bio$MGT_idv)
MGT_hist
# Q-Q plot
qqnorm(bio$MGT_idv); qqline(bio$MGT_idv, col = 2)
#double check normality
shapiro.test(bio$MGT_idv)
#definitely doesn't look to be normally distributed
#what happens if the data is log transformed
bio$MGT_log <- log(bio$MGT_idv)
MGT_log_hist <- hist(bio$MGT_log)
MGT_log_hist
# Q-Q plot
qqnorm(bio$MGT_log); qqline(bio$MGT_log, col = 2)
#double check normality
shapiro.test(bio$MGT_log)


#figure with all four histograms
par(mfrow=c(2,2))
hist(bio$SGI)
hist(bio$MGT_idv)
hist(bio$pct)
hist(length$length_mm)

#Figure with all four Q-Q plots
par(mfrow=c(2,2))
qqnorm(bio$SGI); qqline(bio$SGI, col = 2)
qqnorm(bio$MGT_idv); qqline(bio$MGT_idv, col = 2)
qqnorm(bio$pct); qqline(bio$pct, col = 2)
qqnorm(length$length_mm); qqline(length$length_mm, col = 2)

dev.off()
# Make linear models and histogram of residuals

############# SGI###############
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
MGT_treat <- lm(MGT_idv ~ Treatment, bio)
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


#  none of the tests that I suggested in my proposal have the proper assumtions for my data.
#Becasue the assumption of normality was not met, even when the data was transformed I looked into other
#statistical methods to test for variation among the mean. It looks like the Kruskal-Wallis test is the prefered 
#alternative to the ANOVA when using non-parometric data

# Look into packages and documentation for Kruskal_Wallis test
# The package needed is the stats package which is included in base R so it doesnot need to be installed
# the function that will ultimately be needed is kruskal.test()
# Documentation for this is found by following the code below
?kruskal.test()

#Perform Kruskal-Wallis Test
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
    mean = mean(MGT_idv, na.rm = TRUE),
    sd = sd(MGT_idv, na.rm = TRUE),
    median = median(MGT_idv, na.rm = TRUE),
    IQR = IQR(MGT_idv, na.rm = TRUE)
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

kruskal.test(MGT_idv ~ Treatment, data = bio)
#for MGT the chi-sq = 117.92, df = 30 and p-value = 2.277e-12
#there is significant variation between the means

kruskal.test(pct ~ Treatment, data = bio)
#for pct the chi-sq = 43.113, df = 30, p-value = 0.05726
# there is not significant difference between the means percent germination, this does not supprise me

kruskal.test(length_mm ~ Treatment, data = length)
#for length the chi-sq = 306.73, df = 30, p-value = <2.2e-16
#there is a significant difference between the means 

#post-hoc testing will need to be performed on SGI, MGT, and legnth to determine which groups vary significantly 
#from the control

#################################################################################################################

#Determine proper post-hoc test to use
#     post hoc test to use is Wilcoxon Rank Sum Test, also known as Mann-Whitney test
# These are the assumptions of the Wilcoxon Rank Sum Test:
#                             1. All the observations from both groups are independent of each other,
#                             2. The responses are ordinal (i.e., one can at least say, of any two observations, which is the greater),
#                             3. Under the null hypothesis H0, the distributions of both populations are equal.[3]
#                             4. The alternative hypothesis H1 is that the distributions are not equal.

# look at the documentation for the test
?wilcox.test()

# I wnat to do a pairwise comparison between variables so I will use the pairwise version of this test

#look at the documentation for the pairwise version
?pairwise.wilcox.test()
# here is the syntax
#pairwise.wilcox.test(x, g, p.adjust.method = p.adjust.methods,
#                     paired = FALSE, ...)
# x is the response variable, for my data this is SGI, MGT, pct, and length_mm
# g is the vector used to group the data, for my data this is treatment

#########I may also try grouping by species only just out of curiosity################



# 2) run the post-hoc testing

########### SGI ##############
pairwise.wilcox.test(bio$SGI, bio$Treatment,
                     p.adjust.method = "none")
# when conpared to DI everything is significant, p-values hover around 0.028

########### MGT ###################
pairwise.wilcox.test(bio$MGT_idv, bio$Treatment,
                     p.adjust.method = "none")
#when compared to the control only huis_66 is not significant, this seems odd to me because
#the values for these two groups are completely different

########## pct #############
pairwise.wilcox.test(bio$pct, bio$Treatment,
                     p.adjust.method = "none")
#no groups vary significantly from one another at all in this parameter, this is not supprising at all

########### length_mm ############

pairwise.wilcox.test(length$length_mm, length$Treatment,
                     p.adjust.method = "none")
#This parameter has some that are significant and others that are not, I will have to make a table for this 


#make some plots, split by concentration to make it easier to visualize
DI <- bio %>%
  filter(Treatment == "DI_99")

bio33 <- bio %>%
  filter(Dillution == 33)
bio33 <- bind_rows(bio33, DI) #add control to this dataframe

bio66 <- bio %>%
  filter(Dillution == 66)
bio66 <- bind_rows(bio66, DI) #add control to this dataframe

bio99 <- bio %>%
  filter(Dillution == 100) #this data frame already includes the control

#Get means of SGI by Dillution
sumSGI <- group_by(bio, Dillution) %>%
  summarise(
    count = n(),
    mean = mean(SGI, na.rm = TRUE),
    sd = sd(SGI, na.rm = TRUE)
  )
sumSGI


sumpct <- group_by(bio, Dillution) %>%
  summarise(
    count = n(),
    mean = mean(pct, na.rm = TRUE),
    sd = sd(pct, na.rm = TRUE)
  )
sumpct

sumpct <- group_by(bio, Dillution) %>%
  summarise(
    count = n(),
    mean = mean(pct, na.rm = TRUE),
    sd = sd(pct, na.rm = TRUE)
  )
sumpct
#Make boxplots 
#SGI

p <- ggboxplot(bio33, x = "Treatment", y = "SGI", title = "Seed Germination Index \n 33 Percent Dilution",
               font.x = 18, font.y = 18, font.main = 18, ylab="Seed Germination Index")
p <- p + geom_hline(yintercept = 59)

ggpar(p, orientation = "horiz")


b <- ggboxplot(bio66, x = "Treatment", y = "SGI", title = "Seed Germination Index \n 66 Percent Dilution",
               font.x = 18, font.y = 18, font.main = 18, ylab="Seed Germination Index")
b <- b + geom_hline(yintercept = 51)

ggpar(b, orientation = "horiz")

c <- ggboxplot(bio99, x = "Treatment", y = "SGI", title = "Seed Germination Index \n 100 Percent Dilution",
               font.x = 18, font.y = 18, font.main = 18, ylab="Seed Germination Index")
c <- c + geom_hline(yintercept = 48)

ggpar(c, orientation = "horiz")

legend.text = element_text(colour="blue", size=10, 
                           face="bold")

#MGT
d <- ggboxplot(bio33, x = "Treatment", y = "MGT_idv", title = "Mean Germination Time \n 33 Percent Dilution",
               font.x = 18, font.y = 18, font.main = 18, ylab="Mean Germination Time")
d <- d + geom_hline(yintercept = 1.83)

ggpar(d, orientation = "horiz")


e <- ggboxplot(bio66, x = "Treatment", y = "MGT_idv", title = "Mean Germination Time \n 66 Percent Dilution",
               font.x = 18, font.y = 18, font.main = 18, ylab="Mean Germination Time")
e <- e + geom_hline(yintercept = 2.06)

ggpar(e, orientation = "horiz")

f <- ggboxplot(bio99, x = "Treatment", y = "MGT_idv", title = "Mean Germination Time \n 100 Percent Dilution",
               font.x = 18, font.y = 18, font.main = 18, ylab="Mean Germination Time")
f <- f + geom_hline(yintercept = 2.32)

ggpar(f, orientation = "horiz")


#different bar graphs
#add todgether all of the

pct <-bio99 %>%
  ggplot(aes(Treatment, pct, fill = Species)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Treatment",
       y = "Percent Germination",
       title = "Percent Germination by Treatment") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

pct

#Get means of MGT by Dillution
sumMGT <- group_by(bio, Dillution) %>%
  summarise(
    count = n(),
    mean = mean(MGT, na.rm = TRUE),
    sd = sd(MGT, na.rm = TRUE)
  )
sumMGT

##################################################
#Cummulative germination plots
day100<- read.csv("day100.csv", header=TRUE)

day_line <- day100 %>%
  ggplot(aes(Day, Germ, color=Species)) +
  geom_line()+
  geom_point()+
  labs(x = "Day",
       y = "Cumulative Germination",
       title =  "Cumulative Germination by Day
       100 percent concentration") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    title =element_text(size=14),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    legend.text = element_text(size=14))
day_line

####Subset length df for plotting######
di <- length %>%
  filter(Treatment == "DI_99")
length33 <- length %>%
  filter(Concentration == 33)
length33 <- bind_rows(length33, di) #add control to this dataframe

length66 <- length %>%
  filter(Concentration == 66)
length66 <- bind_rows(length66, di) #add control to this dataframe

length99 <- length %>%
  filter(Concentration == 100)

#Get means of length by Concentration
sumlen <- group_by(length, Concentration) %>%
  summarise(
    count = n(),
    mean = mean(length_mm, na.rm = TRUE),
    sd = sd(length_mm, na.rm = TRUE)
  )
sumlen

###########length plots##############
g  <- ggboxplot(length33, x = "Treatment", y = "length_mm", title = "Total Seedling Length \n 33 Percent Dilution",
                font.x = 18, font.y = 18, font.main = 18, ylim = c(0,80) ylab="Length (mm)", x.text.angle = 45)
g <- g + geom_hline(yintercept = 52)
g

h  <- ggboxplot(length66, x = "Treatment", y = "length_mm", title = "Total Seedling Length \n 66 Percent Dilution",
                font.x = 18, font.y = 18, font.main = 18, ylim = c(0,80), ylab="Length (mm)", x.text.angle = 45)
h <- h + geom_hline(yintercept = 48)
h

i  <- ggboxplot(length99, x = "Treatment", y = "length_mm", title = "Total Seedling Length \n 100 Percent Dilution",
                font.x = 18, font.y = 18, font.main = 18, ylim = c(0,80), ylab="Length (mm)", x.text.angle = 45)
i <- i + geom_hline(yintercept = 39)
i




