library(tidyverse)
library(stargazer)
library(car)
library(olsrr)

data = read.csv("INPUT DATASET HERE")
data = data[-10] # delete weird NA column
colnames(data)[1] = "last_name"
data[data$player_id %in% names(which(table(data$player_id) == 1)), ] # remove players that only appeared once
head(data)

data = data %>% # calculate bb/9
  mutate(bb9 = p_walk / p_formatted_ip * 9,
         adj_year = year - 2016) 

data = data[order(data$player_id, -data$year),] # sort by player id and descending year
head(data) 

n = length(data$last_name)
data$change_bb_per = c()
data$change_fb_velo = c()
data$prior_bb_per = c()
for (i in 1:n){
  if(data$player_id[i] == data$player_id[i+1]){
    data$change_bb_per[i] = data$p_bb_percent[i] - data$p_bb_percent[i+1] 
    data$change_fb_velo[i] = (data$fastball_avg_speed[i] / data$fastball_avg_speed[i+1] - 1)*100
    data$prior_bb_per[i] = data$p_bb_percent[i+1]
  } else{
    data$change_bb_per[i] = NA_integer_
  }
}
head(data)
data = na.omit(data)

data = data %>%
  mutate(bb_binary = if_else(p_bb_percent < 8, 1 , 0 ),
         prior_binary = if_else(prior_bb_per < 7.5, 1, 0)) 

m0 = lm(change_bb_per ~ change_fb_velo + prior_bb_per + player_age, data = data)
summary(m0)

data1 = data %>%
  filter(prior_binary == 1)

m1 = lm(change_bb_per ~ change_fb_velo + prior_bb_per + player_age, data = data1)
summary(m1)
