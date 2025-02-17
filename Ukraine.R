library(tidyverse)
library(gridExtra)
library(tidyr)
library(ggplot2)

#-------------------------------------------------------------------------------

# The reasons and justifications for why we want to work with the data set:

# We chose the data set because it allows us to examine how the level of trust
# towards the political system of Ukraine and general life satisfaction among
# the people of our age changed between 2002 and 2019. Historically the whole
# vector of development of Ukraine changed throughout these years, so it is very
# interesting to look how the perception of the young people changed.

#-------------------------------------------------------------------------------

# Introduction and data:

# Source:
# https://www.kaggle.com/datasets/livochka/youth-opinion-ukraine-2002-vs-2019/data

# Description:
# Statistical analysis and insights from National survey of young people 2002 vs
# personal (author's, not ours) survey 2019.
# This data set contains sociopolitical and demographic survey data conducted
# among Ukrainian youth with a focus on public opinions, personal characteristics
# and political preferences.

# *you may find a very detailed variables analysis by following the link above

#-------------------------------------------------------------------------------

# Research question:
# Among Ukrainian youths aged 18 to 25, is there a statistically significant
# change in trust in political institutions and general life satisfaction in
# 2019 compared to 2002?

# We find this question to be important as it shows how some of the most
# important life views among the Ukrainian youth changed throughout almost a
# 20-year period.

# Generally, the topic of this particular research focuses on sociopolitical
# views of Ukrainian youths (ages 18–25)

# Target Population:
# Ukrainian youths (ages 18–25), “very_young” (18–19), “young” (20–21), and
# “preadult” (24–25).

# Variables:
# Categorical: Youth (18–25)
# Quantitative: Trust in political institutions (trust_president, trust_court,
# trust_police), Life satisfaction (life_satisf)
# Time: Year (2002 / 2019)

#-------------------------------------------------------------------------------

# Hypotheses:

# H0:
# Among Ukrainian youths aged 18–25, there is no statistically significant
# change in the level of trust in political institutions or in overall life
# satisfaction between 2002 and 2019

# H1:
# Among Ukrainian youths aged 18–25, there is a statistically significant change
# in the level of trust in political institutions and in overall life
# satisfaction between 2002 and 2019.

#-------------------------------------------------------------------------------

# Load and explore data set

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df_ua <- read_csv("pooled.csv")
df_ua

df_ua %>% colnames()

df_ua$year %>% unique()

#-------------------------------------------------------------------------------

# Data cleaning and preparation

colnames(df_ua)[1] <- "User_ID"
# here we rename the column with represents each respondent to have a more 
# understandable name thnn previously

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
  select(User_ID,
         trust_court,
         trust_president,
         trust_police,
         age,
         life_satisf,
         year)

df_ua_new$age %>% unique()
# here we agregate separate columns for ages into one for a more comfortable work
# with data

df_ua_new <- df_ua_new %>%
  mutate(political_trust = (trust_president + trust_court + trust_police) / 3) %>%
  select(-trust_court, -trust_president, -trust_police)
# here we agregate separate colums that we chose to represent political trust 
# into one value for a more comfortable general analysis

df_ua_new %>% colnames()

#-------------------------------------------------------------------------------

# Analyze data distribution

# We decided to build box plots to show how the data is distributed, what are the
# outliers, how the means differ from year to year and generally visualize the
# data from the analytical point of view.

# How the following box plots are built:
# The yellow dots represent the mean values, and the box boundaries capture the
# 25th (lower) and 75th (upper) percentiles, with whiskers extending to 1.5×IQR

bp1 <- ggplot(df_ua_new,
              aes(
                x = as.factor(year),
                y = political_trust,
                fill = as.factor(year)
              )) +
  geom_boxplot(alpha = 0.75, color = c("blue", "green")) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 20,
    size = 3,
    color = "yellow"
  ) +
  scale_fill_manual(values = c("2002" = "lightblue", "2019" = "lightgreen")) +
  labs(title = "Trust in Political Institutions(2002/2019)",
       x = "Year",
       y = "Political Trust",
       fill = "Year") +
  theme_minimal()
# The 2002 box has a higher mean (yellow point) than the 2019 box.
# So on average the trust in political institutions among the Ukrainian youth
# was higher in 2002 than 2019.
# The 2002 box has several high outliers, meaning that some people had very high
# level of trust to political institutions compared to others, which pushes our
# mean value even higher. But we decided not to cut them from the research as
# they still do represent the part of social group we are investigating, even
# though their responds kind of shift the mean research results in 2002 higher.
# The 2019 box is shifted a bit down compared to 2002 box, and also by looking
# at the whiskers we can say that the overall distribution is lower than in 2002.
# It also has less outliers, meaning that most people were answering somewhat
# closer to the mean.
# So in conclusion we can say that the average level of trust in political
# institutions among the Ukrainian youth fell in 2019 compared to 2002.

bp2 <- ggplot(df_ua_new, aes(
  x = as.factor(year),
  y = life_satisf,
  fill = as.factor(year)
)) +
  geom_boxplot(alpha = 0.75, color = c("blue", "green")) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 20,
    size = 3,
    color = "yellow"
  ) +
  scale_fill_manual(values = c("2002" = "lightblue", "2019" = "lightgreen")) +
  labs(title = "Life Satisfaction(2002/2019)",
       x = "Year",
       y = "Life Satisfaction",
       fill = "Year") +
  theme_minimal()
# There is no such significant difference between the levels of life satisfaction
# and the distribution is almost the same, except for the fact that in 2002 we
# can see some major outliers, who responded 7/10, yet still in 2019 the mean
# was slightly higher.

grid.arrange(bp1, bp2, ncol = 2)

df_ua_new  %>%
  arrange(desc(life_satisf))

#-------------------------------------------------------------------------------

# Percentage mean growth

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

cat("Percentage Growth in Trust:",
    round(perc_growth_trust, 2),
    "%\n")
cat("Percentage Growth in Life Satisfaction:",
    round(perc_growth_life, 2),
    "%\n")

#-------------------------------------------------------------------------------

# Visualization

df_long <- df_ua_new %>%
  pivot_longer(
    cols = c(life_satisf, political_trust),
    names_to = "metric",
    values_to = "score"
  )

df_counts <- df_long %>%
  group_by(metric, year, score) %>%
  summarize(count = n(), .groups = "drop")

ggplot(df_counts, aes(x = score, y = count, fill = factor(year), alpha=0.75)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = 1:7, limits = c(1,7)) +
  scale_fill_manual(values = c("2002" = "purple", "2019" = "yellow")) +
  labs(
    title = "Distribution of Life Satisfaction and Political Trust",
    subtitle = "Comparing 2002 vs. 2019",
    x = "Level of trust",
    y = "Count of people",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  )
# This visualization shows the relationship between the number of respondents and
# their responds. It shows show the frequency of each level for both life
# satisfaction and political trust, divided by years (2002 and 2019).
# This plot allows us to clearly compare how the levels of trust and life satisf
# changed throughout the years and how they were distributed among the same year,
# so that's why we chose it to best represent the data we are working with in
# general.

#-------------------------------------------------------------------------------

# Statistics

alpha <- 0.05

# We are using the t-test because we need to check whether there is a
# statistically significant change in the average levels of trust to the
# political institutions and life satisfaction between 2002 and 2019.
# We are also using the Two Sample t-test as our samples in 2002 and 2019 are
# not the same people.

t_test_trust <- t.test(political_trust ~ year, data = df_ua_new)
t_test_trust$p.value < alpha #TRUE
# this suggests that the mean level of political trust among Ukrainian youth did
# experience a significant fall between 2002 and 2019, which does support a part
# of our h1, but to accept it completely and reject h0 we have to also check the
# second part of our hypothesis, which is about life satisfaction

t_test_life <- t.test(life_satisf ~ year, data = df_ua_new)
t_test_life$p.value < alpha #FALSE
# this suggests that the change in the level of mean life satisfaction among the
# Ukrainian youth was not significant between 2002 and 2019, so we are unable to
# reject h0 and support h1, unfortunately

t_test_trust$conf.int #0.08597318 0.43937850
# since the entire interval is > 0, it suggests that the difference in trust is
# statistically significant, which once again does support a part of our h1, but
# to accept it completely and reject h0 we have to also check the
# second part of our hypothesis, which is about life satisfaction

t_test_life$conf.int #-0.33789688  0.07613819
# the interval contains 0, meaning that there is no significant difference in
# life satisfaction between 2002 and 2019, so we still see that we are unable to
# reject h0 and support h1

# So in conclusion of the t-tests we held we can now say that we do not support
# h1 and do not reject h0, because even though there was a statistically
# significant change in trust in political institutions among Ukrainian youths
# aged 18 to 25, there still was no statistically significant change in general
# life satisfaction in 2019 compared to 2002, which does not allow us to support
# h1.

#-------------------------------------------------------------------------------

# Conclusion:

# We held a thorough research on the sociopolitical topic and investigated the 
# evidence of the existence of a statistically significant change in trust in
# political institutions and general life satisfaction in 2019 compared to 2002
# among Ukrainian youths aged 18 to 25, and came to the conclusion that our h1,
# which was formulated as such: 
#"
# Among Ukrainian youths aged 18–25, there is a statistically significant change
# in the level of trust in political institutions and in overall life
# satisfaction between 2002 and 2019.
#"
# could not have been accepted by us in the process of statistical analysis,
# and our h0, which was formulated as such: 
#"
# Among Ukrainian youths aged 18–25, there is no statistically significant
# change in the level of trust in political institutions or in overall life
# satisfaction between 2002 and 2019
#"
# could not have been rejected by us in the process of statistical analysis.
#
# So, in result, we are able to say that there has been a statistically
# significant negative change in trust in political institutions from 2002 to 2019,
# but there has not been a statistically significant change in the level of
# general life satisfaction among Ukrainian youths aged 18–25.
#
# These results bring us to a rather sad conclusion, which says that between 2002
# and 2019, after 2004 Orange revolution, 2008 economic crisis, 2014 Revolution
# of Dignity, 2014-2019 (at that moment) war and a range of other sociopolitical
# changes in the lives of Ukrainian youths, people started to trust the political
# institutions significantly less (11.63 %) and the mean level of their life 
# satisfaction did not experience a significant growth (only 4.91 %).
# Though, the fall in the level of trust to the political institutions can also
# be explained by the fact that in 2019 the youth was more into the politics, so
# understood more and was more disappointed.

#-------------------------------------------------------------------------------
