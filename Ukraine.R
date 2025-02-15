library(tidyverse)
#-------------------------------------------------------------------------------

#Introduction and data:

#Source:
#https://www.kaggle.com/datasets/livochka/youth-opinion-ukraine-2002-vs-2019/data

#Description:
#Statistical analysis and insights from National survey of young people 2002 vs personal (author's, not mine) survey 2019.
#This dataset contains sociopolitical and demographic survey data conducted among Ukrainian youth
#with a focus on public opinions, personal characteristics and political preferences.

# *you may find a very detailed variables analysis by following the link above

#-------------------------------------------------------------------------------

#Research question:

#-------------------------------------------------------------------------------

#Hypotheses:

#h0:
#h1:

#-------------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df_ua <- read_csv("pooled.csv")
df_ua

df_ua %>% colnames()
df_ua %>% summary()

#-------------------------------------------------------------------------------

#Data cleaning

#-------------------------------------------------------------------------------

