install.packages("tidyverse")
library(tidyverse)
library(lubridate)

dailyActivity <- read.csv("~/Desktop/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
hourlySteps <- read.csv("~/Desktop/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
sleepDay <- read.csv("~/Desktop/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
hourlyintensities <- read.csv("~/Desktop/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
minuteSleep <- read.csv("~/Desktop/Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv") 
heartratesecond <- read_csv("~/Desktop/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")

head(dailyActivity)
head(hourlyintensities)
head(sleepDay)
head(hourlySteps)
head(minuteSleep)
head(heartratesecond)

summary(dailyActivity)
#changing the data type of daily Activity-ActivityDate to Date 
dailyActivity$ActivityDate <- mdy(dailyActivity$ActivityDate)
head(dailyActivity)
summary(dailyActivity)
#change all the data types of the imported files
summary(hourlyintensities)
hourlyintensities$ActivityHour <- mdy_hms(hourlyintensities$ActivityHour)
summary(hourlyintensities)
summary(heartratesecond)
heartratesecond$Time <- mdy_hms(heartratesecond$Time)
summary(heartratesecond)
minuteSleep$date <- mdy_hms(minuteSleep$date)
sleepDay$SleepDay <- mdy_hms(sleepDay$SleepDay)
hourlySteps$ActivityHour <- mdy_hms(hourlySteps$ActivityHour)

#ANALYZE
# Analyze the dailyActivity to understand the number calories taken and the total number of steps that is being covered during which the members are active
dailyActivity %>%
  select(TotalSteps ,Calories ,VeryActiveMinutes, LightlyActiveMinutes,SedentaryMinutes) %>%
  summary()

# 1.The average steps that is covered would be 7638 , with maximum of 36019
# 2.Sedantary Minutes are the most
# 3.Participants least took part in very active minutes
# 4. The calories spent in avg is 2793

sleepDay %>%
  select(TotalTimeInBed, TotalMinutesAsleep ,TotalSleepRecords)%>%
  summary()
# 1. In an average a person spends around 6 hours 59 minutes asleep
# 2. The time they spent in bed would be in average 7 hours 38 minutes.
# 3. In an avg a person spends around 39 minutes lying in bed either restless or awake.

minuteSleep%>%
  count(value)

# According to the fitBase Dictionary , the value Value indicating the sleep state.
#1 = asleep, 2 = restless, 3 = awake

#Participants spent an average of 14023/(172480 + 14023 + 2018) = 7.44% of their "sleep" time in a restless sleep state.
#Participants spent an average of 2018/(172480 + 14023 + 2018) = 1.07% of their "sleep" time in an awake sleep state.
# questions:-
# How is the activity being distributed in a week
#2. How does the daily activity affect the quality of sleep
# 3. How does the heart rate be affected by the activity.
# 5. What is the peak time during which the activity intensifies.
# 6. How is the activity level correlate with the calories being burned

## How is the activity being distributed throughout the day
userActLevel <- dailyActivity %>%
  group_by(Id)%>%
  summarize(MeanStepCount =mean(TotalSteps))%>%
  mutate(MeanUserActLevel = case_when(
    MeanStepCount < 5000 ~ "sedentary",
    MeanStepCount >= 5000 & MeanStepCount < 7500 ~ "lightly active", 
    MeanStepCount >= 7500 & MeanStepCount < 10000 ~ "fairly active", 
    MeanStepCount >= 10000 ~ "very active"
  )) %>%
  ungroup()

head(userActLevel)
# Identifying the distribution of steps over each hour of the day by user acativity level

hourlyStepu <- merge(hourlySteps ,userActLevel, by= 'Id' ,all.x = TRUE)
ggplot(data = hourlyStepu, aes(x=hour(ActivityHour),y= StepTotal ,color =MeanUserActLevel))+
  geom_point()+
  geom_smooth()


hourlyStepu$HourOnly <- hour(hourlyStepu$ActivityHour)

stepsGrouped <- hourlyStepu %>%
  group_by(MeanUserActLevel, HourOnly) %>%
  summarize(MeanHourlyStepsByActivityLevel = mean(StepTotal)) %>%
  ungroup()

# ordering by activity level from sedentary to very active
stepsGrouped$MeanUserActLevel <-
  factor(stepsGrouped$MeanUserActLevel,
         levels = c("sedentary", "lightly active", "fairly active", "very active"))

ggplot(data = stepsGrouped, aes(x = HourOnly, y = MeanHourlyStepsByActivityLevel, fill = MeanUserActLevel))+
  geom_histogram(stat = "identity")+
  facet_wrap(~MeanUserActLevel)+
  labs(x = "Hour", y = "Avg Number of Steps per User")

# activity intensity by hour

hourlyintensityu <- merge(hourlyintensities, userActLevel, by='Id' ,all.x = TRUE)
hourlyintensityu$HourOnly <- hour(hourlyintensityu$ActivityHour)
intensitiesgrped <- hourlyintensityu %>%
  group_by(MeanUserActLevel ,HourOnly)%>%
  summarize(MeanTotalIntensities =mean(TotalIntensity))%>%
  ungroup()

# ORDERING BY ACTIVITY LEVELFROM SEDENATARY TO VERY ACTIVE
intensitiesgrped$MeanUserActLevel <- 
  factor(intensitiesgrped$MeanUserActLevel , levels =c("sedentar","lightly active","fairly active","very active"))

ggplot(data = intensitiesgrped, aes(x =HourOnly ,y=MeanTotalIntensities , fill =MeanUserActLevel))+
  geom_histogram(stat = 'identity')+
  facet_wrap(~MeanUserActLevel)+
  labs(x="Hour",y ="Avg Total Intensity per user")

# How the activity correlate with the calories burned?

dailyactlevel <- dailyActivity
dailyactlevel$DailyActivityLevel = case_when(
  dailyactlevel$TotalSteps <5000 ~ "sedentary",
  dailyactlevel$TotalSteps >=5000 & dailyactlevel$TotalSteps <7500~ "lightly active" ,
  dailyactlevel$TotalSteps >=7500 & dailyactlevel$TotalSteps <10000~ "fairly active" ,
  dailyactlevel$TotalSteps >=10000 ~"very active"
  )
dailyactlevel$DailyActivityLevel <-  factor(dailyactlevel$DailyActivityLevel ,
                                            levels = c("sedentary" ,"lightly active","fairly active","very active"))
ggplot(data = dailyactlevel, aes(x =DailyActivityLevel , y= Calories , fill =DailyActivityLevel))+
  geom_boxplot()+
  labs(x ="daily activity level" ,y="calories burned")

# does the intensity level of activity affect the number of calories burned, too?

activitypercent <- select(dailyactlevel ,Id, ActivityDate ,VeryActiveMinutes ,FairlyActiveMinutes ,LightlyActiveMinutes,
                          SedentaryMinutes, Calories, DailyActivityLevel)
activitypercent$TotalActivityTime <- activitypercent$SedentaryMinutes+ activitypercent$LightlyActiveMinutes+
  activitypercent$FairlyActiveMinutes + activitypercent$VeryActiveMinutes
activitypercent$PercentVeryActive <- activitypercent$VeryActiveMinutes / 
  activitypercent$TotalActivityTime
ggplot(data = activitypercent ,aes(x =PercentVeryActive , y =Calories))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x= "Percent of time very active" ,y="calories burned ")

# How does amount of activity affect quality of sleep?
# define the data frame by joining the activity data with the sleep data
# only using the record of the participants with sleep data
activitysleep <-  merge(x= sleepDay, y=dailyactlevel, by.x = c("Id","SleepDay"), by.y = c("Id","ActivityDate"), all.x = TRUE)
activitysleep$TotalActivityMins <- activitysleep$VeryActiveMinutes + activitysleep$LightlyActiveMinutes + activitysleep$FairlyActiveMinutes
activitysleep$SleepQuality = activitysleep$TotalMinutesAsleep / activitysleep$TotalTimeInBed

ggplot(data = activitysleep, aes(x = TotalActivityMins, y = SleepQuality))+
  geom_point()+
  geom_smooth(method = lm)
ggplot(data = activitysleep, aes(x = TotalActivityMins, y = TotalMinutesAsleep))+
  geom_point()+
  geom_smooth(method = lm)
ggplot(data = activitysleep, aes(x = MeanUserActLevel, y = SleepQuality))+
  geom_boxplot()
ggplot(data = activitysleep, aes(x = MeanUserActLevel, y = TotalMinutesAsleep))+
  geom_boxplot()

## there does not appear to be a correlation between amount of activity and quality of sleep or amount of sleep.


# How does average heart rate compare to user activity level?
heartratehr <- heartratesecond%>%
  mutate(Time =round_date(Time ,"hour"))%>%
  group_by(Id, Time)%>%
  summarize(Heartrate = mean(Value))%>%
  ungroup()
#Grab all heart rate readings for hours in which activity intensity was 0 indicating rest.
#Then merge to get general user activity level.
restingintensities <- filter(hourlyintensities, TotalIntensity ==0)
df <- merge(heartratehr ,restingintensities, by.x =c("Id","Time"),
            by.y = c("Id", "ActivityHour"), all = FALSE)
df2 <- merge(df , userActLevel, by ="Id" ,x.all =TRUE)

df3 <- df2%>%
  group_by(Id)%>%
  summarize(MeanRestingHeartrate =mean(Heartrate) ,
            userActLevel =MeanUserActLevel)%>%
  ungroup()
df3$ userActLevel <- factor(df3$ userActLevel , levels =c("sedentary", "lightly active", "fairly active", "very active"))
box_stats <- function(y, upper_limit = max(df3$MeanRestingHeartrate) * 1.15) {
  return(data.frame(
    y = 0.95 * upper_limit,
    label = paste(
      "Count = ", length(y), "\n",
      "Mean = ", round(mean(y)), "\n",
      "Median = ", round(median(y)), "\n"
    )
  ))
}
ggplot(data = df3, aes(x = userActLevel, y = MeanRestingHeartrate,
                       fill = userActLevel))+
  geom_boxplot()+
  stat_summary(fun.data = box_stats, geom = "text", hjust = 0.5, vjust = 0.9)



