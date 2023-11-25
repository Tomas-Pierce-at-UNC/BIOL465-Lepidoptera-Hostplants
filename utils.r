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

# reproduced from Homework 5 assignment
# function to run variance partioning of a dataframe.
# will determine what amount of variation in resp_col_name column
# in dframe is attributable to in1_col_name column, 
# in2_col_name column, their combination, and neither.
# works by fitting linear models and taking the differences of
# their R-squared values
variance_partition <- function(dframe, resp_col_name, in1_col_name, in2_col_name) {
  # extract relevant features
  response = dframe[[resp_col_name]]
  input1 = dframe[[in1_col_name]]
  input2 = dframe[[in2_col_name]]
  
  # create linear models
  shared_model = lm(response ~ input1 + input2)
  input1_model = lm(response ~ input1)
  input2_model = lm(response ~ input2)
  
  #display models
  sm = summary(shared_model)
  in1 = summary(input1_model)
  in2 = summary(input2_model)
  
  print(c(resp_col_name, "=", in1_col_name, "+", in2_col_name))
  print(sm)
  print(c(resp_col_name, "=", in1_col_name))
  print(in1)
  print(c(resp_col_name, "=", in2_col_name))
  print(in2)
  
  # get explained variations
  shared_model.variation = sm$r.squared
  input1_model.variation = in1$r.squared
  input2_model.variation = in2$r.squared
  
  # extract uniquely attributable variations
  unique_input2 <- shared_model.variation - input1_model.variation 
  unique_input1 <- shared_model.variation - input2_model.variation
  not_attributable <- 1 - shared_model.variation
  both_inputs <- input1_model.variation + input2_model.variation - shared_model.variation
  
  # collect data
  variations <- c(unique_input1, unique_input2, both_inputs, not_attributable)
  
  labels <- c(in1_col_name, in2_col_name, "both inputs", "neither input")
  
  variations_with_labels <- list(variations, labels)
  
  #return output data
  return(variations_with_labels)
}

# reproduced from HW 5
plot_variance_partion_pie <- function(vars_with_labels) {
  variations = vars_with_labels[[1]]
  labels = vars_with_labels[[2]]
  pie(variations, labels)
  title("Variance Partion Proportions")
}
