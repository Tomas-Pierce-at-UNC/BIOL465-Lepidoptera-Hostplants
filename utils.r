# Contains utilities to help process our data.
# only define functions in this file, do not run commands directly

library(dplyr)
library(tidyr)

load_hosts <- function() {
  hosts <- read.csv("HOSTS/resource.csv")
  hosts2 <- hosts[hosts$Hostplant.Species != "",]
  return(hosts2)
}

load_envelope <- function() {
  envelope <- read.csv("ENVELOPE/data/global_plant_climate_envelopes.csv")
  envelope2 <- separate_wider_delim(envelope, 
                                    species, 
                                    " ", 
                                    names=c("Plant.Genus1", "Plant.Species1", "Plant.SubSpecies1"), 
                                    too_few="align_start",
                                    too_many="merge")
  return(envelope2)
}

load_leptraits <- function() {
  leptraits <- read.csv("LepTraits/consensus/consensus.csv")
  leptraits2 <- separate_wider_delim(leptraits, Species, " ", names=c("Insect.Genus1", "Insect.Species1"))
  return(leptraits2)
}

