# contains the analysis procedures for our hypothesis

library(dplyr)

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
  wingspan_upper = mean(WS_U),
  wingspan_lower = mean(WS_L)
)

model1 <- lm(wingspan_upper ~ temp_max, grouped_envelopes)
print(summary(model1))

plot(grouped_envelopes$temp_max, grouped_envelopes$wingspan_upper)
