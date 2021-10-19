#----------------------------------------------
# R programming assessment. Marketing Mix Model. 
#----------------------------------------------

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
#The variable 'base_price' contains information about the original 
#price of the product without additional charges or discounts 
#whereas 'discount' contains information about the relative size of the discount
#applied to 'base price' in some products. 

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
#it can be said by taking a look at the boxplot that the median is slightly below 20,000
median(new_vol_sales) 
#it is in fact slightly below 20,000

#7----

df_media <- df_mmm %>% 
  select(tv, radio, stout)
#R base 
par(mfrow=c(3,1))
plot(y=radio, x=seq_len(nrow(df_media)))
lines(y=radio, x=seq_len(nrow(df_media)))
plot(y=stout, x=seq_len(nrow(df_media)))
lines(y=stout, x=seq_len(nrow(df_media)))
plot(y=tv, x=seq_len(nrow(df_media)))
lines(y=tv, x=seq_len(nrow(df_media)))


df_media <- df_media %>%
  pivot_longer(everything())

#ggplot2
q <- ggplot(data = df_media)
q + geom_line(aes(y= value, x=seq_len(nrow(df_media)))) +
  facet_grid(name~., scales="free")

#Is there anything worth mentioning from the plot?
#First, each variable has a different scale
#Second, the variable "radio" has multiple zero values and some NAs at the end
#whereas the others do not have any zeros or NAs. 


#8----

p + geom_point(aes(in_store, new_vol_sales))
#Explain your decision and also comment anything interesting from the plot.

#I have set the target variable "new_vol_sales" as the y
#The target variable is analogous to the dependent variable of a regression model
#and is always represented on the y axis.  

#There seems to be some positive correlation between the stock of the product
#and its sales. This might be because more stock leads to higher sells or more likely
#because higher sells leads to stores expanding their stock. Anyway there
#is not much to say about causation just by taking a look at a scatterplot.

#9----

# Color each dot differently based on the newspaper_inserts column
p + geom_point(aes(in_store, new_vol_sales, color=as.factor(newspaper_inserts))) + theme(legend.title = element_blank())
# Color each dot differently based on the tv column.
p + geom_point(aes(in_store, new_vol_sales, color=tv))

#10----
df_mmm %>% 
  group_by(discount_yesno = (discount_yesno <- discount!=0)) %>%
  summarise(medias = mean(base_price)) %>% 
  ggplot() + geom_col(aes(x=discount_yesno, y= medias, fill=discount_yesno ), show.legend = F,)+
  labs(y="Average Base Price", x="Discount") +
  coord_cartesian(ylim=c(14,15.5))

#The average base price of discounted products is slightly higher than  the average 
#base price of undiscounted products. 

#11----

model.fit <- function(variables) {
  df_aux <- df_mmm %>% 
    select(new_vol_sales,variables)
  my_model <- lm(new_vol_sales ~ ., data = df_aux)
  summary(my_model)$adj.r.squared
    }

model.fit("twitter") #Trial with one variable
model.fit(c("base_price","radio","website_campaign")) #Trial with multiple variables


#12----

list1 <-list(c("base_price", "radio", "tv", "stout"),c("base_price", "in_store", "discount", "radio", "tv", "stout"),c("in_store", "discount"))
sapply(list1,model.fit)
    
#The second subset of variables provides the best model with an adjusted Rsq of 0.784



