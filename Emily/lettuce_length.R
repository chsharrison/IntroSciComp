library(dplyr)
library(zoo)
library(ggplot2)
library(lubridate)
library(plotly)
library(psych)
library(dataRetrieval)
library(ggalt)
library(RColorBrewer)

length<- read.csv("C:/Users/emull/OneDrive/Documents/UTRGV/Grad Research/lettucelength.csv", header=TRUE)

names(length)[names(length) == "ï..Treatment"] <- "Treatment"

#length$median_mm <- as.numeric(length$median_mm)

mean_l <- length %>%
  ggplot(aes(Treatment, Mean, color = Species)) +
  geom_point(size =6, shape = 20, show.legend = TRUE) +
  geom_hline(yintercept=55.6,linetype = "dashed", color = "grey")+
  labs(x = "Treatment",
       y = "Total Length (mm)") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

mean_l


mm_plot <- length %>%
  ggplot(aes(Treatment, length_mm, color = Species)) +
  geom_boxplot(size = .3, 
               outlier.shape = 20,
               outlier.size = 1) +
  geom_hline(yintercept=55.6,linetype = "dashed", color = "grey")+
  labs(x = "Treatment",
       y = "Total Length (mm)") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

mm_plot

bar <-length %>%
  ggplot(aes(Treatment, length_mm, fill = Species)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Treatment",
       y = "Length of Seedling (mm)",
       title = "Length of Seedling by Treatment") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

bar

conc_33 <- length %>%
  filter(Concentration == 33) 

bar_33 <- conc_33 %>%
  ggplot(aes(Species, length_mm)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Species",
       y = "Mean Length (mm)",
       title = "Mean length by Species
       33 percent concentration") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

bar_33


conc_66 <- length %>%
  filter(Concentration == 66) 

bar_66 <- conc_66 %>%
  ggplot(aes(Species, length_mm)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Species",
       y = "Mean Length (mm)",
       title = "Mean length by Species
       66 percent concentration") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

bar_66


conc_100 <- length %>%
  filter(Concentration == 100) 

bar_100 <- conc_100 %>%
  ggplot(aes(Species, length_mm)) +
  stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  labs(x = "Species",
       y = "Mean Length (mm)",
       title = "Mean length by Species
       100 percent concentration") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title=element_text(size=16, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

bar_100


