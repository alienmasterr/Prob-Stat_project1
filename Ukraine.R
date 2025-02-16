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

# How has the relationship between trust in political institutions (measured by trust_president)
# and life satisfaction (life_satisf) among Ukrainian adults changed between 2002 and 2019,
# and is this relationship moderated by economic status (income categories: very_poor, poor, rich, very_rich)?

#-------------------------------------------------------------------------------

#Hypotheses:

#H0:
#There is no statistically significant difference in the relationship between trust in political institutions
#(trust_president) and life satisfaction (life_satisf) between 2002 and 2019, nor does economic status moderate
#this relationship.


#H1:
#There is a statistically significant difference in the relationship between trust in political institutions
#(trust_president) and life satisfaction (life_satisf) between 2002 and 2019, and economic status moderates
#this relationship.

#-------------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df_ua <- read_csv("pooled.csv")
df_ua

df_ua %>% colnames()

df_ua$year %>% unique()

#-------------------------------------------------------------------------------

#Data cleaning

colnames(df_ua)[1] <- "User_ID"

df_ua_new <- df_ua %>%
  pivot_longer(
    cols = c("underage", "very_young", "young", "preadult", "adult"),
    names_to = "age",
    values_to = "indicator"
  ) %>%
  filter(indicator == 1) %>%  
  select(-indicator)

df_ua_new$age

df_ua_2002 <- df_ua_new %>% 
  filter(year=="2002")
df_ua_2002

df_ua_2019 <- df_ua_new %>% 
  filter(year=="2019")
df_ua_2019

vec1 <- df_ua_2002 %>% colnames()
vec2 <- df_ua_2019 %>% colnames()

vec1==vec2

#-------------------------------------------------------------------------------

