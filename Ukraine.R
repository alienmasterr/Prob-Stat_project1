library(tidyverse)
#-------------------------------------------------------------------------------
#Introduction and data

#-------------------------------------------------------------------------------

#Hypotheses:

#-------------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df_ua <- read_csv("pooled.csv")
df_ua

df_ua %>% colnames()
df_ua %>% summary()