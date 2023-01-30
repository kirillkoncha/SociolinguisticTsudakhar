library(tidyverse)
df <- read_delim("data.csv", delim=";")
df$`year of birth` <- as.numeric(df$`year of birth`)
df["quran_standard"][df["quran_standard"] == "understand"] <- "read"
df %>% 
  filter(`bio: rec` == 1,
         `year of birth` > 1919,
         `year of birth` < 2006,
         l2_speak != 0,
         l2_speak == "speak" | l2_speak == "no",
         quran_standard == "read" | quran_standard == "no") -> df

df$`year of birth ` <- as.numeric(df$`year of birth`)
df$`year of birth` <- (df$`year of birth` - min(df$`year of birth`)) / (max(df$`year of birth`) - min(df$`year of birth`))

df$gender <- as.factor(df$gender)
df$gender <- relevel(df$gender, ref="ж")

df$zone[df$zone == "Lak"] <- "Лакцы"
df$zone[df$zone == "Dargwa"] <- "Даргинцы"
df$zone <- as.factor(df$zone)
df$zone <- relevel(df$zone, "Даргинцы")

df$l2_speak <- as.factor(df$l2_speak)
df$l2_speak <- relevel(df$l2_speak, ref="no")

df$quran_standard <- as.factor(df$quran_standard)
df$quran_standard <- relevel(df$quran_standard, ref="no")

library(lme4)
library(lmerTest)
df %>%
  glmer(l2_speak ~ zone + `year of birth` + gender + (1|interviewer),
        data = ., family = "binomial",
        control = glmerControl(optimizer = "bobyqa")) -> m1

summary(m1)

df %>%
  glmer(quran_standard ~ zone + `year of birth` + gender + (1|interviewer),
        data = ., family = "binomial",
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))) -> m2

summary(m2)
