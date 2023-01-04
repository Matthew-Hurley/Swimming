# Load data
swimming <-read.csv("~/Desktop/R files/Activities.csv")

# Load libraries
library(tidyverse)
library(lubridate)
library(ggdark)

# Tidy Data
swimming <- swimming %>% select(Date, Distance, Calories, Time, Avg.HR, Max.HR, 
                                Aerobic.TE, Avg.Pace, Avg.Stroke.Rate, Moving.Time)
swimming$Distance <- as.numeric(gsub(",","",swimming$Distance))
swimming$Date <- ymd_hms(swimming$Date)
swimming$Time <- hms(swimming$Time)
swimming$Moving.Time <- hms(swimming$Moving.Time)
swimming$Avg.Pace <- ms(swimming$Avg.Pace)
swimming$MpS <- 100 / as.numeric(seconds(swimming$Avg.Pace))
swimming$Weekday <- wday(swimming$Date)
swimming$IsWeekend <- swimming$Weekday == 1 | swimming$Weekday == 7
swimming <- swimming %>% arrange(swimming$Date)
swimming$SumDist <- cumsum(swimming$Distance) / 1000

# Analysis

# Plot cumulative distance swam over the year
ggplot(swimming, aes(Date, SumDist))+ 
  geom_area(fill = 'deepskyblue', color = 'deepskyblue3', alpha = 0.3)+
  ylab("Cumulative Distance (km)")+ 
  geom_hline(yintercept =  max(swimming$SumDist), color = 'blue', lty = 2)+
  geom_label(label = paste('Total Distance: ',max(swimming$SumDist), ' km' ), 
             x = ymd_hms('2022-06-01 00:00:00'), y = max(swimming$SumDist))+
  ggtitle('Swim Distance 2022')+
  geom_label(label = paste('Swims Recorded: ',length(swimming$Date)), 
             x = ymd_hms('2022-10-01 00:00:00'), y = 20)+
  geom_label(label = paste('Average Distance: ', round(mean(swimming$Distance), 0),' meters'), 
             x = ymd_hms('2022-10-01 00:00:00'), y = 15)+
  dark_theme_gray()

# Plot distribution of distances swam on weekdays versus weekends
ggplot(swimming, aes(x = IsWeekend, y = Distance))+
  geom_violin()+
  geom_jitter(width = 0.05)+
  geom_hline(yintercept =  mean(swimming$Distance[swimming$IsWeekend==TRUE]), color = 'blue', lty = 2)+
  geom_label(label = paste('Weekend Avg: ',round(mean(swimming$Distance[swimming$IsWeekend==TRUE]),0), 'm' ), x = 1.5, y = mean(swimming$Distance[swimming$IsWeekend==TRUE]))+
  geom_hline(yintercept =  mean(swimming$Distance[swimming$IsWeekend==FALSE]), color = 'red', lty = 2)+
  geom_label(label = paste('Weekday Avg: ',round(mean(swimming$Distance[swimming$IsWeekend==FALSE]),0), 'm' ), x = 1.5, y = mean(swimming$Distance[swimming$IsWeekend==FALSE]))+
  ggtitle('Weekday versus Weekend Distances')+
  ylab('Distance (m)')+
  dark_theme_gray()

# Look at significance of difference with t-test
weekend <- lm(Distance ~ IsWeekend, swimming)
summary(weekend)

# Heart rate
ggplot(swimming, aes(x = Date, y = Avg.HR)) + geom_point() + ylim(c(90,180))+
  stat_smooth(method = 'lm', se = FALSE)+
  ggtitle('Average Heart Rate over time')+ ylab('Average Heart Rate (bpm)')

# Speed
ggplot(swimming, aes(x = Date, y = MpS)) + geom_point() + ylim(c(0,1.4))+
  stat_smooth(method = 'lm', se = FALSE)+
  ggtitle('Average Speed Over Time')+ ylab('Average Speed (m/s)')

#Combine heart rate and speed on same axes
scaleFactor <- mean(swimming$MpS)/mean(swimming$Avg.HR)

ggplot(swimming, aes(x = Date, y = Avg.HR)) + geom_point(col = 'deepskyblue', alpha = 0.4)+
  stat_smooth(method = 'lm', se = FALSE, col = 'deepskyblue3')+
  scale_y_continuous('Heart Rate (bpm)',limits = c(120,170), sec.axis = 
                       sec_axis(~.*scaleFactor, name= 'Speed (m/s)'))+
  geom_point(aes(x = Date, y = (scaleFactor^-1)*MpS),
            col = 'red', alpha = 0.4)+
  stat_smooth(aes(x = Date, y = (scaleFactor^-1)*MpS), method = 'lm', se = FALSE, col ='red')+
  ggtitle('Plot comparing average heart rate versus average speed over 2022')+
  dark_theme_gray()+
  geom_label(label = 'Heart Rate', x = ymd_hms('2022-01-15 00:00:00'), y = 152, fill = 'deepskyblue')+
  geom_label(label = 'Speed', x = ymd_hms('2022-01-15 00:00:00'), y = 148, fill = 'red')

