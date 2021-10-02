#
# R programming asessment. Marketing Mix Model. 
#

library(dplyr)
library(ggplot2)
library(janitor)
library(zoo)
library(tidyr)

#1----

df_mmm <- read.csv("mktmix.csv")
df_mmm <- clean_names(df_mmm)
glimpse(df_mmm)
attach(df_mmm)

#2----

ncol(df_mmm)
nrow(df_mmm)
class(base_price)
class(discount)
# Try and guess what they mean. ??

#3----

df_mmm$newspaper_inserts <- if_else(df_mmm$newspaper_inserts == "", 0, 1) 
df_mmm$newspaper_inserts

#4---- 

unique(df_mmm$website_campaign) 
#Three unique values:"Facebook","Twitter","Website Campaign "
df_mmm$facebook <- as.numeric(df_mmm$website_campaign == "Facebook")
df_mmm$twitter <- as.numeric(df_mmm$website_campaign == "Twitter")
df_mmm$websitec <- as.numeric(df_mmm$website_campaign == "Website Campaign ")

#5----

p <- ggplot( data = df_mmm) 
p + geom_line(aes(x=index(df_mmm),y=new_vol_sales), colour= "red")
#I have used the function index of the package 'zoo' for the x axis 

#6---- 

p + geom_histogram(aes(x=new_vol_sales),fill= "red",binwidth=250)
p + geom_boxplot(aes(y=new_vol_sales))
#it can be said by looking at the boxplot that the median is slightly below 20,000
median(new_vol_sales) #it is in fact slightly below 20,000

#7----

df_media <- df_mmm %>% 
  select(tv, radio, stout)
#With R base 
par(mfrow=c(3,1))
plot(y=radio, x=seq_len(nrow(df_media)))
lines(y=radio, x=seq_len(nrow(df_media)))
plot(y=stout, x=seq_len(nrow(df_media)))
lines(y=stout, x=seq_len(nrow(df_media)))
plot(y=tv, x=seq_len(nrow(df_media)))
lines(y=tv, x=seq_len(nrow(df_media)))


df_media <- df_media %>%
  pivot_longer(everything())
#With ggplot2
q <- ggplot(data = df_media)
q + geom_line(aes(y= value, x=seq_len(nrow(df_media)))) +
  facet_grid(name~., scales="free")
#Is there anything worth mentioning from the plot?
#First, each variable has a different scale
#Second, the variable "radio" goes to zero suddenly multiple times. 
#This might indicate that those zero values are actually NAs


