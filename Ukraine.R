library(tidyverse)
#-------------------------------------------------------------------------------

# The reasons and justifications for why we want to work with the data set:

# We chose the data set because it allows us to examine how the level of trust
# towards the political system of Ukraine and general life satisfaction among
# the people of our age changed between 2002 and 2019. Historically the whole 
# vector of development of Ukraine changed throughout these years, so it is very 
# interesting to look how the perception of the young people changed.

#-------------------------------------------------------------------------------

#Introduction and data:

#Source:
#https://www.kaggle.com/datasets/livochka/youth-opinion-ukraine-2002-vs-2019/data

#Description:
#Statistical analysis and insights from National survey of young people 2002 vs
#personal (author's, not ours) survey 2019.
#This data set contains sociopolitical and demographic survey data conducted
#among Ukrainian youth with a focus on public opinions, personal characteristics 
#and political preferences.

# *you may find a very detailed variables analysis by following the link above

#-------------------------------------------------------------------------------

#Research question:
# Among Ukrainian youths aged 18 to 25, is there a statistically significant
# change in trust in political institutions and general life satisfaction in
# 2019 compared to 2002?

# Target Population:
# Ukrainian youths (ages 18–25), “very_young” (18–19), “young” (20–21), and
# “preadult” (24–25).

# Variables:
# Categorical: Youth (18–25)
# Quantitative: Trust in political institutions (trust_president, trust_court,
# trust_police)
# Quantitative: Life satisfaction (life_satisf)
# Time: Year (2002 / 2019)

#-------------------------------------------------------------------------------

#Hypotheses:

#H0:
# Among Ukrainian youths aged 18–25, there is no statistically significant
# change in the level of trust in political institutions or in overall life
# satisfaction between 2002 and 2019

#H1:
# Among Ukrainian youths aged 18–25, there is a statistically significant change
# in the level of trust in political institutions and in overall life
# satisfaction between 2002 and 2019.

#-------------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df_ua <- read_csv("pooled.csv")
df_ua

df_ua %>% colnames()

df_ua$year %>% unique()

#-------------------------------------------------------------------------------

#Data cleaning and preparation

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

df_ua_new <- df_ua_new %>%
  filter(age %in% c("very_young", "young", "preadult")) %>%
  select(User_ID, trust_court, trust_president, trust_police, age, life_satisf, year)

df_ua_new <- df_ua_new %>%
  mutate(political_trust = (trust_president + trust_court + trust_police) / 3) %>%
  select(-trust_court, -trust_president, -trust_police)


df_ua_2002 <- df_ua_new %>% 
  filter(year=="2002")
df_ua_2002

df_ua_2019 <- df_ua_new %>% 
  filter(year=="2019")
df_ua_2019

vec1 <- df_ua_2002 %>% colnames()
vec2 <- df_ua_2019 %>% colnames()

vec1==vec2

df_ua_new %>% colnames()

df_ua_new$age%>% unique()

#-------------------------------------------------------------------------------

# Analyze data distribution

ggplot(df_ua_new, aes(x = as.factor(year), y = political_trust, fill = as.factor(year))) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(title = "Trust in Political Institutions (2002 vs 2019)",
       x = "Year",
       y = "Political Trust",
       fill = "Year") +
  theme_minimal()
#the boxplot shows that 

ggplot(df_ua_new, aes(x = as.factor(year), y = life_satisf, fill = as.factor(year))) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(title = "Life Satisfaction (2002 vs 2019)",
       x = "Year",
       y = "Life Satisfaction",
       fill = "Year") +
  theme_minimal()
#the boxplot shows that 

#-------------------------------------------------------------------------------

mean_summary <- df_ua_new %>%
  group_by(year) %>%
  summarize(
    mean_trust = mean(political_trust, na.rm = TRUE),
    mean_life = mean(life_satisf, na.rm = TRUE)
  )
mean_summary

mean_trust_2002 <- mean_summary %>% filter(year == "2002") %>% pull(mean_trust)
mean_trust_2019 <- mean_summary %>% filter(year == "2019") %>% pull(mean_trust)
mean_life_2002  <- mean_summary %>% filter(year == "2002") %>% pull(mean_life)
mean_life_2019  <- mean_summary %>% filter(year == "2019") %>% pull(mean_life)

perc_growth_trust <- ((mean_trust_2019 - mean_trust_2002) / mean_trust_2002) * 100
perc_growth_life  <- ((mean_life_2019 - mean_life_2002) / mean_life_2002) * 100

cat("Percentage Growth in Trust:", round(perc_growth_trust, 2), "%\n")
cat("Percentage Growth in Life Satisfaction:", round(perc_growth_life, 2), "%\n")

#-------------------------------------------------------------------------------

#Statistics

alpha <- 0.05

t_test_trust <- t.test(political_trust ~ year, data = df_ua_new)
t_test_trust$p.value<alpha #TRUE
# this suggests that the mean level of political trust among Ukrainian youth did
# experience a significant fall between 2002 and 2019

t_test_life <- t.test(life_satisf ~ year, data = df_ua_new)
t_test_life$p.value<alpha #FALSE
# this suggests that the change in the level of mean life satisfaction among the
# Ukrainian youth was not significant between 2002 and 2019

t_test_trust$conf.int #0.08597318 0.43937850
# since the entire interval is > 0, it suggests that the difference in trust is 
# statistically significant

t_test_life$conf.int #-0.33789688  0.07613819
# the interval contains 0, meaning that there is no significant difference in
# life satisfaction between 2002 and 2019

#-------------------------------------------------------------------------------

#Visualizations

df_summary <- df_ua_new %>%
  group_by(year) %>%
  summarise(
    mean_trust = mean(political_trust, na.rm = TRUE),
    mean_life = mean(life_satisf, na.rm = TRUE),
    se_trust = sd(political_trust, na.rm = TRUE) / sqrt(n()),
    se_life = sd(life_satisf, na.rm = TRUE) / sqrt(n())
  )

ggplot(df_summary, aes(x = as.factor(year), y = mean_trust)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = mean_trust - 1.96 * se_trust, ymax = mean_trust + 1.96 * se_trust), width = 0.2, color = "blue") +
  labs(title = "Mean Trust with 95% Confidence Intervals",
       x = "Year",
       y = "Mean Political Trust") +
  theme_minimal()

ggplot(df_summary, aes(x = as.factor(year), y = mean_life)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = mean_life - 1.96 * se_life, ymax = mean_life + 1.96 * se_life), width = 0.2, color = "blue") +
  labs(title = "Mean Life Satisfaction with 95% Confidence Intervals",
       x = "Year",
       y = "Mean Life Satisfaction") +
  theme_minimal()



