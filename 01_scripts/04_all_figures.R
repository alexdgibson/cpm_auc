# creating all of the plot that are required for the article
# created 2025-03-17

# load in libraries
library(tidyverse)
library(ggplot2)
library(truncnorm)
library(stats)
library(splines)


# create the histogram of AUC values, the expected distributions, compare the best fit and histogram of residuals
# create the histogram
  ggplot()+
  geom_histogram(binwidth = 0.01, colour = "black", fill = "grey60")+
  scale_x_continuous(limits = c(0.5,1.0), breaks = c(0.5,0.6,0.7,0.8,0.9,1.0))+
  theme_classic()+
  theme(text = element_text(size = 16))+
  labs(x = "AUC Value",
       y = "Count")



# create the histogram of Sensitivity values, the expected distributions, compare the best fit and histogram of residuals
# create the histogram
ggplot()+
  geom_histogram(binwidth = 1)+ # may use bin widths of 2 if patterns in the data are not visible
  scale_x_continuous(limits = c(50, 100), breaks = c(50, 60, 70, 80, 90, 100) ,labels = c("50%", "60%", "70%", "80%", "90%", "100%"))+
  theme_classic()+ 
  theme(text = element_text(size = 16))+
  labs(x = "Sensitivity Value",
       y = "Count")


# create the histogram of specificity values, the expected distributions, compare the best fit and histogram of residuals
# create the histogram
ggplot()+
  geom_histogram(binwidth = 1)+ # may use bin widths of 2 if patterns in the data are not visible 
  scale_x_continuous(limits = c(50, 100), breaks = c(50, 60, 70, 80, 90, 100) ,labels = c("50%", "60%", "70%", "80%", "90%", "100%"))+
  theme_classic()+
  theme(text = element_text(size = 16))+
  labs(x = "Specificity Value",
       y = "Count")


# create the histogram of the difference between the extracted AUC value and reported AUC value
ggplot()+
  geom_histogram(binwidth = 0.001, boundary = 1, colour = "black", fill = "grey60")+
  scale_x_continuous(limits = c(-0.05, 0.05), breaks = c(-0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05))+ # ensure the limits argument is removed to show all data that is present
  theme_classic()+
  theme(text = element_text(size = 16))+
  labs(x = "Difference in Value",
       y = "Count")+
  geom_vline(xintercept = 0, linetype = 'dashed')





