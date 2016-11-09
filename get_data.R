library(tidyverse)
library(readxl)

mean.income <- read_excel("X:\\programs\\Run912\\run5SaveOpt4\\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx", sheet = "mean income", skip = 2, col_names = TRUE)

# Per capita annuity
per.capita.annuity <- mean.income[, 1:9]

names(per.capita.annuity) <- c("category", "trash", "group", "year2015", "year2025", "year2035", "year2045", "year2055", "year2065")

per.capita.annuity <- per.capita.annuity %>%
  select(-trash) %>%
  filter(!is.na(group)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  filter(!is.na(year2015)) %>%
  mutate(measure = "per capita annuity")


# Per capita cash
per.capita.cash <- mean.income[, 10:18]

names(per.capita.cash) <- c("category", "trash", "group", "year2015", "year2025", "year2035", "year2045", "year2055", "year2065")

per.capita.cash <- per.capita.cash %>%
  select(-trash) %>%
  filter(!is.na(group)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  filter(!is.na(year2015)) %>%
  mutate(measure = "per capita cash")

# Net per capita annuity
net.per.capita.annuity <- mean.income[, 19:27]

names(net.per.capita.annuity) <- c("category", "trash", "group", "year2015", "year2025", "year2035", "year2045", "year2055", "year2065")

net.per.capita.annuity <- net.per.capita.annuity %>%
  select(-trash) %>%
  filter(!is.na(group)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  filter(!is.na(year2015)) %>%
  mutate(measure = "net per capita annuity")

# Net per capita cash
net.per.capita.cash <- mean.income[, 28:36]

names(net.per.capita.cash) <- c("category", "trash", "group", "year2015", "year2025", "year2035", "year2045", "year2055", "year2065")

net.per.capita.cash <- net.per.capita.cash %>%
  select(-trash) %>%
  filter(!is.na(group)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  mutate(category = ifelse(is.na(category), lag(category), category)) %>%
  filter(!is.na(year2015)) %>%
  mutate(measure = "net per capita cash")










