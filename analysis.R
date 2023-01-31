library(tidyverse)
library(lingtypology)

df <- read_delim("data.csv", delim=";")
map_data <- read_delim("map_data.csv", delim = ";")


# ZONE MAP

map.feature(
  languages = map_data$lang,
  features = map_data$value,
  title = "Язык села",
  latitude = map_data$latitude,
  longitude = map_data$longitude,
  width = 5,
  shape = TRUE,
  color = c("deepskyblue", "indianred1"),
  label = map_data$village,
  label.fsize = 15,
  label.position = "right",
  label.hide = FALSE,
  minimap = TRUE
)


# BAR PLOT

df %>% 
  filter(zone=="Dargwa") %>% 
  filter(`bio: rec` == 1) %>% 
  filter(lak_standard != 0) %>% 
  group_by(era, lak_standard) %>%
  count() %>% 
  group_by(era) %>%
  mutate(total = sum(n)) %>% 
  mutate(percent = round(n/total * 100, 2)) %>%
  filter(lak_standard == "speak") -> lak_in_dargwa
lak_in_dargwa

df %>% 
  filter(zone=="Lak") %>% 
  filter(`bio: rec` == 1) %>% 
  filter(tsudakhar_standard != 0) %>% 
  group_by(era, tsudakhar_standard) %>%
  count() %>% 
  group_by(era) %>%
  mutate(total = sum(n)) %>% 
  mutate(percent = round(n/total * 100, 2)) %>%
  filter(tsudakhar_standard == "speak") -> dargwa_in_lak
dargwa_in_lak

bar_data = data.frame(
  `Язык`=c("Владение лакским\nсреди цудахарцев",
           "Владение лакским\nсреди цудахарцев",
           "Владение цудахарским\nсреди лакцев",
           "Владение цудахарским\nсреди лакцев"),
  `Период`=c("До 1919", "После 1920",
             "До 1919", "После 1920"),
  `Говорящих (%)`=c(lak_in_dargwa$percent,
                    dargwa_in_lak$percent)
  , check.names = FALSE)
bar_data

p <- ggplot(data=bar_data,
            aes(x=`Язык`, y=`Говорящих (%)`, fill=`Период`)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(limits = c(0, 50))
p


# ANALYSIS

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
