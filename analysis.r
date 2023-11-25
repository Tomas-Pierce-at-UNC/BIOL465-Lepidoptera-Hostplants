# contains the analysis procedures for our hypothesis

library(dplyr)
library(ggplot2)

# machine learning libs
library(e1071)
library(caret)
library(xgboost)
library(randomForest)

source("utils.r")

hosts2 <- load_hosts()
envelope2 <- load_envelope()
leptraits2 <- load_leptraits()

# ignore hybrids
envelope3 <- envelope2[is.na(envelope2$Plant.SubSpecies1),]

insects_hosts <- left_join(leptraits2,
          hosts2,
          by=c("Insect.Species1" = "Insect.Species", "Genus" = "Insect.Genus"),
          relationship = "many-to-many"
)

insect_hosts_envelopes <- inner_join(insects_hosts,
          envelope3,
          by=c("Hostplant.Genus" = "genus",
               "Hostplant.Species" = "Plant.Species1"),
          relationship = "many-to-many"
)

by_insect <- group_by(insect_hosts_envelopes, verbatimSpecies)

grouped_envelopes <- by_insect %>% summarize(
  temp_max = max(tmax),
  temp_min = min(tmin),
  precip_avg = mean(tp),
  wingspan_upper = mean(WS_U),
  wingspan_lower = mean(WS_L),
  number_hosts = n_distinct(Hostplant.Genus, Hostplant.Species),
  female_ws_upper = mean(WS_U_Fem),
  female_ws_lower = mean(WS_L_Fem),
  male_ws_upper = mean(WS_U_Mal),
  male_ws_lower = mean(WS_L_Mal),
  
)

# testing overall hypothesis part 1: max temperature
model1 <- lm(wingspan_upper ~ temp_max, grouped_envelopes)
p1 <- ggplot(grouped_envelopes, aes(temp_max, wingspan_upper)) + 
  geom_point() + 
  geom_smooth(method='lm') +
  theme_bw() + 
  xlab("Hostplant Maximum Temperature") +
  ylab("Adult Upper Wingspan")
print(p1)
print(summary(model1))

# testing overall hypothesis part 2: min temperature
model1 <- lm(wingspan_upper ~ temp_min, grouped_envelopes)
p1 <- ggplot(grouped_envelopes, aes(temp_min, wingspan_upper)) + 
  geom_point() + 
  geom_smooth(method='lm') +
  theme_bw() + 
  xlab("Hostplant Minimum Temperature") +
  ylab("Adult Upper Wingspan")
print(p1)
print(summary(model1))

# testing overall hypothesis part 3: Average Precipitation
model1 <- lm(wingspan_upper ~ precip_avg, grouped_envelopes)
p1 <- ggplot(grouped_envelopes, aes(precip_avg, wingspan_upper)) + 
  geom_point() + 
  geom_smooth(method='lm') +
  theme_bw() + 
  xlab("Hostplant Average Precipitation") +
  ylab("Adult Upper Wingspan")
print(p1)
print(summary(model1))

# setup for generalist vs specialist ANCOVA
grouped_envelopes$is_specialist <- as.numeric(grouped_envelopes$number_hosts <= 2)

# do specialist versus generalist ANCOVA regarding max temperature
specialization.ancova = lm(wingspan_upper ~ is_specialist + temp_max + temp_max*is_specialist, data=grouped_envelopes)
p2 <- ggplot(grouped_envelopes, aes(temp_max, wingspan_upper, colour = as.factor(is_specialist))) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw()
print(p2)
print(summary(specialization.ancova))

# do specialist versus generalist ANCOVA regarding min temperature
specialization.ancova = lm(wingspan_upper ~ is_specialist + temp_min + temp_min*is_specialist, data=grouped_envelopes)
p2 <- ggplot(grouped_envelopes, aes(temp_min, wingspan_upper, colour = as.factor(is_specialist))) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw()
print(p2)
print(summary(specialization.ancova))

# do specialist versus generalist ANCOVA regarding precipitation
specialization.ancova = lm(wingspan_upper ~ is_specialist + precip_avg + is_specialist*precip_avg, data=grouped_envelopes)
p2 <- ggplot(grouped_envelopes, aes(precip_avg, wingspan_upper, colour = as.factor(is_specialist))) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw()
print(p2)
print(summary(specialization.ancova))

# direct comparison of male and female lepidopterans requires reformatting data
masculine <- grouped_envelopes[,c(2,3,4,7,10,12)]
feminine <- grouped_envelopes[,c(2,3,4,7,8,12)]
masculine$male_sex <- 1
feminine$male_sex <- 0
masculine = masculine %>% rename(sexed_wingspan = male_ws_upper)
feminine = feminine %>% rename(sexed_wingspan = female_ws_upper)
recombined <- base::rbind(masculine, feminine)
#recombined <- recombined[!is.na(recombined$sexed_wingspan),]

# analysis of covariance, max temp
sexed_model <- lm(sexed_wingspan ~ male_sex +  temp_max + male_sex*temp_max, recombined)
print(summary(sexed_model))

# analysis of covariance, min temp
sexed_model <- lm(sexed_wingspan ~ male_sex +  temp_min + male_sex*temp_min, recombined)
print(summary(sexed_model))

# analysis of covariance, precipitation
sexed_model <- lm(sexed_wingspan ~ male_sex + precip_avg + male_sex*precip_avg, recombined)
print(summary(sexed_model))

