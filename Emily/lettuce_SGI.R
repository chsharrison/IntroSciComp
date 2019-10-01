library(dplyr)
library(zoo)
library(ggplot2)
library(lubridate)
library(plotly)
library(psych)
library(dataRetrieval)
library(ggalt)
library(RColorBrewer)

bio<- read.csv("C:/Users/emull/OneDrive/Documents/UTRGV/Grad Research/lettucebioassay2.csv", header=TRUE)

bio$SGI <- (as.numeric(bio$SGI))


sumSGI <- group_by(bio, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(SGI, na.rm = TRUE),
    sd = sd(SGI, na.rm = TRUE)
  )
sumSGI


SGI_b <-bio %>%
  ggplot(aes(Treatment, SGI, color = Species)) +
  geom_boxplot(size = .25, 
               outlier.shape = 20,
               outlier.size = 1) +
  labs(x = "Treatment",
       y = "Seed Germination Index") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

SGI_b

bar <-bio %>%
  ggplot(aes(Treatment, SGI, fill = Species)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Treatment",
       y = "Speed of Germination",
       title = "Speed of Germination by Treatment") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

bar

bar_MGT <-bio %>%
  ggplot(aes(Treatment, MGT_idv, fill = Species)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Treatment",
       y = "Mean Germination Time",
       title = "Mean Germination Time by Treatment") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

bar_MGT

conc_33 <- bio %>%
  filter(Dillution == 33) 

bar_33 <- conc_33 %>%
  ggplot(aes(Species, SGI)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Species",
       y = "Speed of Germination",
       title = "Speed of Germination by Species
       33 percent concentration") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

bar_33

conc_66 <- bio %>%
  filter(Dillution == 66) 

bar_66 <- conc_66 %>%
  ggplot(aes(Species, SGI)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Species",
       y = "Speed of Germination",
       title = "Speed of Germination by Species
       66 percent concentration") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

bar_66

conc_100 <- bio %>%
  filter(Dillution == 100) 

bar_100 <- conc_100 %>%
  ggplot(aes(Species, SGI)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Species",
       y = "Speed of Germination",
       title = "Speed of Germination by Species
       100 percent concentration") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

bar_100

bio$pct_tot <- ((bio$X1 + bio$X2 + bio$X3 + bio$X4 + bio$X5 +bio$X6)/20 *100)
bio$pct_d3 <- ((bio$X1 + bio$X2 + bio$X3)/20 *100)

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

pct_d3 <-bio %>%
  ggplot(aes(Treatment, pct_d3, fill = Species)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Treatment",
       y = "Percent Germination",
       title = "Percent Germination on Day 3 by Treatment") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

pct_d3

conc_33 <- bio %>%
  filter(Dillution == 33) 

bar_33 <- conc_33 %>%
  ggplot(aes(Species, SGI)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Species",
       y = "Speed of Germination",
       title = "Speed of Germination by Species
       33 percent concentration") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

bar_33

conc_66 <- bio %>%
  filter(Dillution == 66) 

bar_66 <- conc_66 %>%
  ggplot(aes(Species, SGI)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Species",
       y = "Speed of Germination",
       title = "Speed of Germination by Species
       66 percent concentration") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

bar_66

conc_100 <- bio %>%
  filter(Dillution == 100) 

bar_100 <- conc_100 %>%
  ggplot(aes(Species, SGI)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Species",
       y = "Speed of Germination",
       title = "Speed of Germination by Species
       100 percent concentration") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

bar_100


pH<- read.csv("C:/Users/emull/OneDrive/Documents/UTRGV/Grad Research/pH_cond.csv", header=TRUE)

pH$pH <- as.character(pH$pH)
pH$pH <- as.numeric(pH$pH)
pH$cond <-as.character(pH$cond)
pH$cond <-as.numeric(pH$cond)

bio2 <- merge(bio, pH, by = "Treatment", all.x = TRUE)

length<- read.csv("C:/Users/emull/OneDrive/Documents/UTRGV/Grad Research/lettucelength.csv", header=TRUE)

names(length)[names(length) == "ï..Treatment"] <- "Treatment"

bio3 <- merge(bio2, length, by = "Treatment", all.x = TRUE)


pH_SGI <- bio3 %>%
  ggplot(aes(pH, SGI_mean)) +
  geom_point(na.rm=TRUE, size=.8, shape = 20)+
  labs(x = "Aqueous extract pH",
       y = "Seed Germination Index") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.text=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans")) 

pH_SGI

pH_length <- bio3 %>%
  ggplot(aes(pH, Mean)) +
  geom_point(na.rm=TRUE, size=.8, shape = 20)+
  labs(x = "Aqueous extract pH",
       y = "Seedling length (mm)") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.text=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans")) 

pH_length

cond_length <- bio3 %>%
  ggplot(aes(cond, Mean)) +
  geom_point(na.rm=TRUE, size=.8, shape = 20)+
  labs(x = "Aqueous extract conductance (us/cm)",
       y = "Seedling length (mm)") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.text=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans", size=12)) 

cond_length

cond_SGI <- bio3 %>%
  ggplot(aes(cond, SGI_mean)) +
  geom_point(na.rm=TRUE, size=.8, shape = 20)+
  labs(x = "Aqueous extract conductance (us/cm)",
       y = "Seedling Germination Index (mm)") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.text=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans", size=12)) 

cond_SGI

corr <- bio3 %>%
  select(Treatment, SGI_mean, Mean, MGT, cond, pH) %>%
  rename(SGI=SGI_mean, Length=Mean, Conductance=cond)


pairs.panels(corr, method = "pearson", 
             main = "Pearson correlation")
