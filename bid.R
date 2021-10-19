library(readxl)
library(ggplot2)
library(tidyverse)

bid <- read_xlsx("base2000.xlsx", col_types = rep("numeric", 13), sheet = "Sheet1")
gender <- read_xlsx("base2000.xlsx", sheet = "Sheet2")
corresp <- read_xlsx("base2000.xlsx", sheet = "Sheet3")

df <- merge(gender, corresp, by = "Q")
bid <- merge(df, bid, by = "ID") 

treat1 <- bid %>% filter(Round < 6)
treat2 <- bid %>% filter(Round > 5) 

summary(treat1)
summary(treat2)

treat1 <- treat1 %>% 
  mutate(diff = Signal - B)

treat2 <- treat2 %>% 
  mutate(diff = Signal - B)

av_treat1 <- treat1 %>%
  group_by(Round) %>%
  summarise(moy = mean(diff), min = min(diff), max = max(diff))

ggplot(av_treat1) +
  aes(x = Round, y = moy) +
  geom_line() +
  geom_line(aes(x = Round, y = min), color = "red") +
  geom_line(aes(x = Round, y = max), color = "blue")

ggplot(treat1) +
  geom_boxplot(aes(x = Round, group = Round, y = diff))

ggplot(treat2) +
  geom_boxplot(aes(x = Round, group = Round, y = diff))

#essai