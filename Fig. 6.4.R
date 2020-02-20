#Figure 6.4
#Created by: Breyona White and Kaitlyn King
#Additional Help: Jihyun Shin 
#Created: October 24, 2018
#Updated: December 6, 2018
################################################################

#Before getting started, we will clear the environment in case there is any  
#data remaining in my global environment 
rm(list=ls())

#set working directory
setwd("/Users/breyonawhite/Dropbox/NTR Figures to BGraham/Datasets")

#load packages to be used to read the data + graphs
library(foreign)
library(readstata13)
library(ggplot2)
library(dplyr)

#load data
dat <- read.dta13("LA_GRACE cdp 10102018.dta")

#These are requirements that need to be filtered into subsets 
#assigning lacount, electtype and v24(election winner).
# "electtype = 6" is general election, *NOTE: couldn't find 6, instead used G and it seemed to work
# "lacount = 1" is district in la county (all analyses should have lacount=1 in them)
# "v24 = 1" is election winner 



#########################################################################################################
#Filtering the data into racial subsets to include both the criteria and  the racial population

#African American population
dat_subsetafm <- dat %>%
  filter(lacount == 1 & v24 == 1 & electtype == "G" & canafam == 1 & latpoppct) 

#Asian American population
dat_subsetasam <- dat %>%
  filter(lacount == 1 & v24 == 1 & electtype == "G" & canasamall == 1 & latpoppct)

#Latina/o population
dat_subsetlat <- dat %>%
  filter(lacount == 1 & v24 == 1 & electtype == "G" & canlatall == 1 & latpoppct)

#White population
dat_subsetwh <- dat %>%
  filter(lacount == 1 & v24 == 1 & electtype == "G" & canwh == 1 & latpoppct)


#########################################################################################################

#11/14 - QUESTION1: What is y-axis (How is the "Percent of Leg Wins" calculated so I can get axis scaled?)

#QUESTION: Stats Consulting Session Questions: The Y-axis isn't scaled correctly, so question is what calculations were performed 
#to produce the "percentage of legislative wins" and how this was scaled on the y-axis?
#ANSWER: By Email communication w/ Jihyun Shi -- needed the percentage in y-axis instead of the count 

#ASIAN AMERICAN POPULATION
plot1c <-  ggplot(dat_subsetasam, aes(x = latpoppct)) +
  geom_histogram(binwidth = 0.08, color="white", aes(y = ((..count..)/sum(..count..))*100)) + #needed percentage in y-axis, not count
  labs(title = "Asian American",
       x = "", #kept the quotes because when removed, R labeled the axis as the input data name 
       y = "") + #^^^^^^
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)) +
  coord_cartesian(ylim = c(0, 30)) + #used this to zoom the plot without changing the underlying data
  theme_minimal() + #to have a minimal theme 
  theme(plot.title = element_text(hjust = 0.5, size = 18)) #to center the title and increase font size to 18
plot1c

#WHITE POPULATION
plot1d <-  ggplot(dat_subsetwh, aes(x = latpoppct)) +
  geom_histogram(binwidth = 0.08, color="white", aes(y = ((..count..)/sum(..count..))*100)) + #also, "color="white
  labs(title = "White",
       x = "",
       y = "") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)) +
  coord_cartesian(ylim = c(0, 35)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18))
plot1d

#AFRICAN AMERICAN POPULATION
plot1a <- ggplot(dat_subsetafm, aes(x = latpoppct)) +
  geom_histogram(binwidth = 0.089, color="white", aes(y = ((..count..)/sum(..count..))*100)) +
  labs(title = "African American",
       x = "",
       y = "") +
  scale_x_continuous(limits = c(0, 1),
                    breaks = seq(0, 1, 0.25)) +
  coord_cartesian(ylim = c(0, 30)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18))
plot1a

#LATINO POPULATION
plot1b <- ggplot(dat_subsetlat, aes(x = latpoppct)) +
  geom_histogram(binwidth = 0.079, color="white", aes(y = ((..count..)/sum(..count..))*100)) +
  labs(title = "Latina/o",
       x = "",
       y = "") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)) +
  coord_cartesian(ylim = c(0, 38)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18))
plot1b


#########################################################################################################

#installign ggpubr package to arrange these graphs on one page
install.packages("ggpubr")
library(ggpubr)

#This allows me to arrange these multiple graphs into a cohesive, single graphic 
figure6.4 <- ggarrange(plot1a, plot1c, plot1b, plot1d, 
          ncol = 2, nrow = 2)
figure6.4

finalfigure6.4 <- annotate_figure(figure6.4, #this annotates the figure to place axis titles for the figure as a whole
                left = text_grob("Percent of Legislative Wins 1996-2016", color = "black", size = 14, rot = 90),
                bottom = text_grob("Latina/o Proportion of District Population", color = "black", size = 14))
finalfigure6.4
ggsave("finalfigure6.4.png", width = 7, height = 5, dpi = 400)





