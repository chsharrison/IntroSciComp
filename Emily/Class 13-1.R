#From Timeline
#11/19/2019: 1) Determine proper post-hoc testing
#            2) Perform pos-hoc testing 
#            3) Generate Figures
#            4) Generate Tables


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
library("ggpubr")

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

#################################################################################################################

# 1) Determine proper post-hoc test to use
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

pairwise.wilcox.test(bio$MGT, bio$Treatment,
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


gglin_SGI33 <- ggline(bio33, x = "Treatment", y = "SGI", 
       add = c("mean_se", "jitter"), 
       ylab = "Seed Germination Index", xlab = "Treatment",
       x.text.angle = 45)


gglin_SGI33


gglin_MGT33 <- ggline(bio33, x = "Treatment", y = "MGT", 
                    add = c("mean_se", "jitter"), 
                    ylab = "Mean Germination Time", xlab = "TreatmenT",
                    x.text.angle = 45)

gglin_MGT33

gglin_pct33 <- ggline(bio33, x = "Treatment", y = "pct", 
                      add = c("mean_se", "jitter"), 
                      ylab = "Percent Germination", xlab = "Treatment",
                      x.text.angle = 45)

gglin_pct33



gglin_SGI66 <- ggline(bio66, x = "Treatment", y = "SGI", 
                      add = c("mean_se", "jitter"), 
                      ylab = "Seed Germination Index", xlab = "Treatment",
                      x.text.angle = 45)


gglin_SGI66


gglin_MGT66 <- ggline(bio66, x = "Treatment", y = "MGT", 
                      add = c("mean_se", "jitter"), 
                      ylab = "Mean Germination Time", xlab = "Treatmen",
                      x.text.angle = 45)

gglin_MGT66

gglin_pct66 <- ggline(bio66, x = "Treatment", y = "pct", 
                      add = c("mean_se", "jitter"), 
                      ylab = "Percent germination", xlab = "Treatment",
                      x.text.angle = 45)

gglin_pct66



gglin_SGI99 <- ggline(bio99, x = "Treatment", y = "SGI", 
                      add = c("mean_se", "jitter"), 
                      ylab = "Seed Germination Index", xlab = "Treatment",
                      x.text.angle = 45)


gglin_SGI99


gglin_MGT99 <- ggline(bio99, x = "Treatment", y = "MGT", 
                      add = c("mean_se", "jitter"), 
                      ylab = "Mean Germination Time", xlab = "Treatmen",
                      x.text.angle = 45)

gglin_MGT99

gglin_pct99 <- ggline(bio99, x = "Treatment", y = "pct", 
                      add = c("mean_se", "jitter"), 
                      ylab = "Percent germination", xlab = "Treatment",
                      x.text.angle = 45)

gglin_pct99

p <- ggboxplot(bio33, x = "Treatment", y = "SGI")
ggpar(p, orientation = "horiz")
p
