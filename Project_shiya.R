setwd("~/DS502/Project")
library(dplyr)
library(ggplot2)
library(lubridate)
library(googledrive)
library(caTools)


#Read in the entire dataset
drive_find(n_max=50)
drive_get("~/")
drive_reveal("DS502Project")

#get the most current cleaned data set that does not include "live" projects or records with mising data
drive_download("~/DS502 Project/preprocessed_data.csv", overwrite = TRUE)
df = read.csv("preprocessed_data.csv")

#Explore the data
summary(df)
write.csv(summary(df),"dfoutput.csv")
dim(df)

#Create variables for month, day, minute for launched date and deadline
df$deadline.min=as.integer(minute(df$deadline))
df$deadline.day=as.factor(day(df$deadline))
df$deadline.dow=as.factor(wday(df$deadline))
df$deadline.mth=as.factor(month(df$deadline))
df$deadline.yr=as.factor(year(df$deadline))
df$launched.min=as.integer(minute(df$launched))
df$launched.day=as.factor(day(df$launched))
df$launched.dow=as.factor(wday(df$launched))
df$launched.mth=as.factor(month(df$launched))
df$launched.yr=as.factor(year(df$launched))

#Create a variable for the duration a project is held open for funding
df$ks.duration=as.integer(difftime(df$deadline,df$launched, units = c("days")))

#Create a variable for the number of characters in a name
df$name=as.character(df$name)
df$namesize=nchar(df$name, type = "chars")

#Categorize the "state" variable
df$cat = 0
attach(df)
df$cat[df$state=="successful"]=1

#Review the data
attach(df)
glimpse(df)

#Isolate to used fields
df2 = df[,c("main_category", 
         "currency",
         "state",
         "backers",
         "country",
         "usd_pledged_real",
         "usd_goal_real",
         "deadline.min",
         "deadline.day",
         "deadline.dow",
         "deadline.mth",
         "deadline.yr",
         "launched.min",
         "launched.day",
         "launched.dow",
         "launched.mth",
         "launched.yr",
         "ks.duration",
         "namesize",
         "cat")]

dim(df2)
write.csv(summary(df2), "df2summary.csv")
write.csv(df2, "ProcessedData.csv")

plot(usd_goal_real)
title("Preprocessed no outliers removed")
plot(country, state)
title("Preprocessed no outliers removed")
plot(launched.dow, state)
title("Preprocessed no  outliers removed: Launched Day of Week")
plot(launched.day, state)
title("Preprocessed no  outliers removed: Launched Day of month")
plot(launched.min, state)
title("Preprocessed no outliers removed: Launched Minute")
plot(launched.mth, state)
title("Preprocessed no outliers removed: Launched Month")
plot(deadline.dow, state)
title("Preprocessed no  outliers removed: Deadline Day of Week")
plot(deadline.day, state)
title("Preprocessed no outliers removed: Deadline Day of Month")
plot(deadline.mth, state)
title("Preprocessed no outliers removed: Deadline Month")
plot(namesize, state)
title("Preprocessed no outliers removed: Namesize")
plot(usd_goal_real, state)
title("Preprocessed no outliers removed: Shows some extreme goals that could be removed")

ggplot(df2, aes(x = launched.dow, y = deadline.dow, col = state))+
  geom_point(position = "jitter")

ggplot(df2, aes(x = country, y = backers, col = state))+
  geom_point(position = "jitter")+
  labs(title = "Backers clearly influence outcome",
       subtitle = "Project initators have little influence on this as it part of the outcome they seek")

ggplot(df2, aes(x = ks.duration, y = namesize, col = state))+
  geom_point(position = "jitter")+
  labs(title = "Shows extreme durations that should be removed")

ggplot(df2, aes(x = usd_goal_real, ks.duration, col = state))+
  geom_point(position = "jitter")+
  labs(title = " Duration, Goal and State", 
       subtitle = "Indicates outliers that can be eliminated",
       caption = "elminate ks.duration > 5000 and usd_goal_real > ??")


#look for goal thresholds to consider for elimination
nrow(df2[usd_goal_real>500000,])
nrow(df2[usd_goal_real>750000,])
nrow(df2[usd_goal_real>1000000,])
nrow(df2[usd_goal_real>1500000,])

#look for duration thresholds to consider for elimination
nrow(df2[ks.duration>5000,])
nrow(df2[ks.duration>1000,])
nrow(df2[ks.duration>365,])
nrow(df2[ks.duration>180,])
nrow(df2[ks.duration>90,])


#Limit data set to projects with a goal of < $500k based on outliers found in data exploration
nrow(df2[ks.duration<180,])

df2 = df2[ks.duration<180,]

dim(df2)
write.csv(summary(df2), "df2Updatedsummary.csv")

#Repeat plots
plot(usd_goal_real)
title("Goal amount wtih >$500k removed and ks.duration > 180")
plot(country, state)
title("Goal amount wtih >$500k removed and ks.duration > 180")
plot(launched.dow, state)
title("Goal amount wtih >$500k removed and ks.duration > 180")
plot(launched.day, state)
title("Goal amount wtih >$500k removed and ks.duration > 180")
plot(launched.min, state)
title("Goal amount wtih >$500k removed and ks.duration > 180")
plot(launched.mth, state)
title("Goal amount wtih >$500k removed and ks.duration > 180")
plot(deadline.dow, state)
title("Goal amount wtih >$500k removed and ks.duration > 180")
plot(deadline.day, state)
title("Goal amount wtih >$500k removed and ks.duration > 180")
plot(deadline.mth, state)
title("Goal amount wtih >$500k removed and ks.duration > 180")
plot(namesize, state)
title("Goal amount wtih >$500k removed and ks.duration > 180")
plot(usd_goal_real, state)
title("Goal amount wtih >$500k removed and ks.duration > 180")

ggplot(df2, aes(x = as.integer(usd_goal_real), ks.duration, col = state))+
  geom_point(position = "jitter")+
  labs(title = " Duration, Goal and State", 
       subtitle = "Projects w duration <180 days")

par(mfrow=c(2,2))

ggplot(df2, aes(x = launched.yr, ks.duration, col = state))+
  geom_point(position = "jitter")+
  labs(title = "Launched Year, Duration and State", 
       subtitle = "Projects w duration <180 days")

ggplot(df2, aes(x = launched.yr, usd_goal_real, col = state))+
  geom_point(position = "jitter")+
  labs(title = "Launched Year, Goal and State", 
       subtitle = "Projects w duration <180 days")



ggplot(df2, aes(x = main_category, y = namesize, col = state))+
  geom_point(position = "jitter")+
  labs(title = "Main Category, Name Size and State", 
       subtitle = "Projects w duration <180 days")

max.sucpledge = as.integer(max(df2[df2$state=="successful","usd_goal_real"]))
nrow(df2[df2$state=="successful" & usd_goal_real == max.sucpledge,])
summary(df2)

#Divide into training and "locked" test set
train.size=floor(nrow(df2)*.7)
train.size
set.seed(1)

train_ind = sample(seq_len(nrow(df2)),size = train.size)
ks =df2[train_ind,]
Vaulted=df2[-train_ind,] 



dim(ks)
dim(Vaulted)

#Export training and test set for Kickstarter
write.csv(ks, file="TrainKickstarter.csv",col.names=TRUE)
write.csv(Vaulted, file="VaultedKickstarter.csv",col.names=TRUE)


#test reading in created files
df3=read.csv("TrainKickstarter.csv")
dim(df3)

df4=read.csv("VaultedKickstarter.csv")
dim(df4)


#Tree
#1 Main_category in (Film & Video, Music) or not in (Film & Video, Music)
#2 Currency in (USD) vs not in (USD)
#3 Deadline.month in (Nov, Dec, Jan) not in (Nov, Dec, Jan)
#4 Goal > 49K
#5 Duration of funding (Deadline - Launched) < 90 days
#6 Backers >25
#Do we accept state

# Divide train dataset into new training dataset and validation dataset
> setwd("~/2018 Fall WPI/DS502/Final Project/0ksprojects/dataset")
> df3=read.csv("TrainKickstarter.csv")
> train.size=floor(nrow(df3)*.7)
> train.size
[1] 184172
> set.seed(1)
> train_independent=sample(seq_len(nrow(df3)),size=train.size)
> ks1=df3[train_independent,]
> validation=df3[-train_independent,]
> dim(ks1)
[1] 184172     21
> dim(validation)
[1] 78931    21

# Export new training dataset and validation dataset for Kickstarter
> write.csv(ks1,file="TrainingKickstarter.csv",col.names=TRUE)
> write.csv(validation,file="validationKickstarter.csv",col.names=TRUE)

# reading in created files
> df5=read.csv("TrainingKickstarter.csv")
> dim(df5)
[1] 184172     22
> df6=read.csv("validationKickstarter.csv")
> dim(df6)
[1] 78931    22
