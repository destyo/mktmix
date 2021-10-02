#
# R programming asessment. Marketing Mix Model. 
#
library(dplyr)
library(ggplot2)
library(janitor)

#1----
df_mmm <- read.csv("mktmix.csv")
df_mmm <- clean_names(df_mmm)
glimpse(df_mmm)
