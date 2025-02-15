library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data_music <- read_csv("mxmh_survey_results.csv")
data_music