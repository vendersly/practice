twitch_clean <-function(CSV,channel){
##Cleans a CSV from sully gnome and saves as a csv (to view data on streams) 
##weekdays are replaced with a numeric 1 = sunday
library(tidyverse)
filename = paste('twitchstats_cleaned_',channel,'.csv', sep = "")
channel_stats <- read_csv(CSV)
remove <- c('Stream URL', 'Games')
channel_stats <-channel_stats[, ! names(channel_stats) %in% remove, drop = F]
summary(channel_stats)
day_time <- data.frame(x1 = integer(),weekday=character(), time=character())
summary(day_time)
for (i in 1:nrow(channel_stats)){
  full <- channel_stats$`Stream start time`[i]
  split <- data.frame(str_split(full, ' '))
  time <- toString(slice(split, 0,5))
  weekday <- toString(slice(split, 0,1))
  time <- str_replace(time,':','.')
  time <- as.numeric(time)
  if(weekday == 'Sunday'){
    weekday <- 1
  }else if(weekday == 'Monday'){
    weekday <- 2
  }else if(weekday == 'Tuesday'){
    weekday <- 3
  }else if(weekday == 'Wednesday'){
    weekday <- 4
  }else if(weekday == 'Thursday'){
    weekday <- 5
  }else if(weekday == 'Friday'){
    weekday <- 6
  }else {
    weekday <- 7
  }
  temp <- data.frame(i,weekday,time)
  names(temp) <- c('X1','weekday','time')
  day_time <- rbind(day_time, temp)
}
channel_stats <- merge(channel_stats, day_time, by.x = 'X1')
channel_stats <-channel_stats[, ! names(channel_stats) %in% 'Stream start time', drop = F]
channel_stats <-channel_stats[, ! names(channel_stats) %in% 'X1', drop = F]
write.csv(channel_stats,filename)
}
