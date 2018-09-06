######################## Spending analysis ######################## 
rm(list=ls())
setwd('C:/Users/xintong.yan/Desktop')

## Read data
df <- read.csv('expense_record.csv', header = TRUE)
summary(df)


## Plot bar chart to display everyday spending
# prepare data
library(dplyr)
group_by_ind <- group_by(df, Date)
daily_spending <- summarise(group_by_ind
                            ,count = n()
                            ,sum_spend=sum(Spend)
                            ,max_spend=max(Spend)
                            ,min_spend=min(Spend))
daily_spending <- data.frame(daily_spending)

# draw bar chart
library(ggplot2)
library(ggthemes)

ggplot(daily_spending, aes(x = Date, y = sum_spend, fill = sum_spend, group = factor(1))) + 
  geom_bar(stat = "identity") +
  theme_economist()

# draw line chart
ggplot(daily_spending, aes(x=Date, y=sum_spend,group = 1)) +
  geom_line(color='pink', size=1) +
  geom_point(colour="pink", size=4, shape=21, fill="white")


##########################################################################
# From the bar chart and line chart, we knew that on 2018-08-11, 
# the expenses was so high up.(this is from a occational envent.
# However, this part of the spending should not be given so much attention
# Therefore, I revmoved this so high up spending and just look at others.
##########################################################################
df_rm0811 <- df[-which(df$Date=='2018-08-11'),]

group_by_ind <- group_by(df_rm0811, Date)
daily_spending <- summarise(group_by_ind
                            ,count = n()
                            ,sum_spend=sum(Spend)
                            ,max_spend=max(Spend)
                            ,min_spend=min(Spend))
daily_spending <- data.frame(daily_spending)

ggplot(daily_spending, aes(x = Date, y = sum_spend, fill = sum_spend, group = factor(1))) + 
  geom_bar(stat = "identity") +
  theme_economist()

# draw line chart
ggplot(daily_spending, aes(x=Date, y=sum_spend,group = 1)) +
  geom_line(color='pink', size=1) +
  geom_point(colour="pink", size=4, shape=21, fill="white")+
  theme_economist()


##########################################################################
# There seems to be a seasonal pattern in the data, 
# therefore, we group by the data by weekdays.
##########################################################################
group_by_ind <- group_by(df, Week)
daily_spending <- summarise(group_by_ind
                            ,count = n()
                            ,sum_spend=sum(Spend)
                            ,max_spend=max(Spend)
                            ,min_spend=min(Spend))
daily_spending <- data.frame(daily_spending)

ggplot(daily_spending, aes(x = Week, y = sum_spend, fill = sum_spend, group = factor(1))) + 
  geom_bar(stat = "identity") +
  theme_economist()

# draw line chart
ggplot(daily_spending, aes(x=Week, y=sum_spend,group = 1)) +
  geom_line(color='pink', size=1) +
  geom_point(colour="pink", size=4, shape=21, fill="white")+
  theme_economist()

## then, after we remove 8-11 spending 
group_by_ind <- group_by(df_rm0811, Week)
daily_spending <- summarise(group_by_ind
                            ,count = n()
                            ,sum_spend=sum(Spend)
                            ,max_spend=max(Spend)
                            ,min_spend=min(Spend))
daily_spending <- data.frame(daily_spending)

ggplot(daily_spending, aes(x = Week, y = sum_spend, fill = sum_spend, group = factor(1))) + 
  geom_bar(stat = "identity") +
  theme_economist()

# draw line chart
ggplot(daily_spending, aes(x=Week, y=sum_spend,group = 1)) +
  geom_line(color='pink', size=1) +
  geom_point(colour="pink", size=4, shape=21, fill="white")+
  theme_economist()


##########################################################################
# It seems saturday/Tuesday/thursday, 
# therefore, we group by the data by weekdays.
##########################################################################


# group by Type and see
group_by_ind <- group_by(df, Type)
type_spending <- summarise(group_by_ind
                           ,count = n()
                           ,sum_spend=sum(Spend)
                           ,max_spend=max(Spend)
                           ,min_spend=min(Spend)                           
                           )
type_spending <- data.frame(type_spending)

ggplot(type_spending, aes(x=Type, y=sum_spend, fill=Type, group=factor(1)))+
  geom_bar(stat='identity')+
  theme_economist()+
  geom_text(aes(label = sum_spend, vjust = -0.8, hjust = 0.5, color = Type))+
  ylim(min(type_spending$sum_spend, 0)*1.1, max(type_spending$sum_spend)*1.1)  
  

##########################################################################
# See special items.
##########################################################################
library(sqldf)
sqldf('select Description, sum(Spend) from df where Type = "Meal" group by Description')

library(reshape2)
melt(df, varnames = c('Type','Spend'))



