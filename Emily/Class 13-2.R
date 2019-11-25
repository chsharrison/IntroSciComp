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
library("ggpubr") #this is a package for making figures that I have never used before

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
#Finish Plotting and make presentation ready figures
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

#Make boxplots 
#SGI

p <- ggboxplot(bio33, x = "Treatment", y = "SGI", title = "Seed Germination Index \n 33 Percent Dilution",
               font.x = 18, font.y = 18, font.main = 18)
p <- p + geom_hline(yintercept = 59)

ggpar(p, orientation = "horiz")


b <- ggboxplot(bio66, x = "Treatment", y = "SGI", title = "Seed Germination Index \n 66 Percent Dilution",
               font.x = 18, font.y = 18, font.main = 18)
b <- b + geom_hline(yintercept = 51)

ggpar(b, orientation = "horiz")

c <- ggboxplot(bio99, x = "Treatment", y = "SGI", title = "Seed Germination Index \n 100 Percent Dilution",
               font.x = 18, font.y = 18, font.main = 18)
c <- c + geom_hline(yintercept = 48)

ggpar(c, orientation = "horiz")

legend.text = element_text(colour="blue", size=10, 
                           face="bold")

#MGT
d <- ggboxplot(bio33, x = "Treatment", y = "MGT_idv", title = "Mean Germination Time \n 33 Percent Dilution",
               font.x = 18, font.y = 18, font.main = 18)
d <- d + geom_hline(yintercept = 1.83)

ggpar(d, orientation = "horiz")


e <- ggboxplot(bio66, x = "Treatment", y = "MGT_idv", title = "Mean Germination Time \n 66 Percent Dilution",
               font.x = 18, font.y = 18, font.main = 18)
e <- e + geom_hline(yintercept = 2.06)

ggpar(e, orientation = "horiz")

f <- ggboxplot(bio99, x = "Treatment", y = "MGT_idv", title = "Mean Germination Time \n 100 Percent Dilution",
               font.x = 18, font.y = 18, font.main = 18)
f <- f + geom_hline(yintercept = 2.32)

ggpar(f, orientation = "horiz")


#different bar graphs
pct <-bio %>%
  ggplot(aes(Treatment, pct_tot, fill = Species)) +
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
