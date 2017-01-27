# Aaron Williams, Urban Institute Program on Retirement Policy

# This script reads five decades of four measures of mean income for older 
# Americans across from 
# "X:\programs\run912\Run5SaveOpt4\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx".
# The script also cleans the data and turns them into a long data frame 
# formatted for ggplot2 

# Library and Source Statements
library(tidyverse)
library(readxl)

# Read unformatted data from Microsoft Excel
mean.income <- read_excel("X:\\programs\\Run912\\run5SaveOpt4\\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx", sheet = "mean income", skip = 2, col_names = TRUE)
scheduled.dollar.change <- read_excel("X:\\programs\\Run912\\run5SaveOpt4\\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx", sheet = "$mean income compare", skip = 2, col_names = TRUE)
scheduled.percent.change <- read_excel("X:\\programs\\Run912\\run5SaveOpt4\\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx", sheet = "%mean income compare", skip = 2, col_names = TRUE)

# Fix imperfections from read_excel

scheduled.dollar.change <- scheduled.dollar.change[, c(1:27, 29:37)]
scheduled.percent.change <- scheduled.percent.change[, c(1:27, 29:37)]

##
## MEAN INCOME
##

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

mean.income <- bind_rows(per.capita.annuity, per.capita.cash, net.per.capita.annuity, net.per.capita.cash)

rm(per.capita.annuity, per.capita.cash, net.per.capita.annuity, net.per.capita.cash)

mean.income <- gather(mean.income, key = year , value,-category, -group, -category, -measure) %>%
               mutate(year = as.numeric(gsub("year", "", year))) %>%
               mutate(chart = "mean.income")

##
## DOLLAR CHANGE
##

# Per capita annuity
per.capita.annuity <- scheduled.dollar.change[, 1:9]

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
per.capita.cash <- scheduled.dollar.change[, 10:18]

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
net.per.capita.annuity <- scheduled.dollar.change[, 19:27]

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
net.per.capita.cash <- scheduled.dollar.change[, 28:36]

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

scheduled.dollar.change <- bind_rows(per.capita.annuity, per.capita.cash, net.per.capita.annuity, net.per.capita.cash)

rm(per.capita.annuity, per.capita.cash, net.per.capita.annuity, net.per.capita.cash)

scheduled.dollar.change <- gather(scheduled.dollar.change, key = year , value,-category, -group, -category, -measure) %>%
  mutate(year = as.numeric(gsub("year", "", year))) %>%
  mutate(chart = "scheduled.dollar.change")

##
## PERCENT CHANGE
##

# Per capita annuity
per.capita.annuity <- scheduled.percent.change[, 1:9]

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
per.capita.cash <- scheduled.percent.change[, 10:18]

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
net.per.capita.annuity <- scheduled.percent.change[, 19:27]

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
net.per.capita.cash <- scheduled.percent.change[, 28:36]

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

scheduled.percent.change <- bind_rows(per.capita.annuity, per.capita.cash, net.per.capita.annuity, net.per.capita.cash)

rm(per.capita.annuity, per.capita.cash, net.per.capita.annuity, net.per.capita.cash)

scheduled.percent.change <- gather(scheduled.percent.change, key = year , value,-category, -group, -category, -measure) %>%
  mutate(year = as.numeric(gsub("year", "", year))) %>%
  mutate(chart = "scheduled.percent.change")

# If data directory does not exist, create data directory
if (!file.exists("data")) {
  dir.create("data")
}

# Combine data sets

income <- bind_rows(mean.income, scheduled.dollar.change, scheduled.percent.change)

write_csv(income, "data//income.csv")

# Write .csv
#write_csv(mean.income, "data//mean.income.csv", row.names = FALSE)
#write_csv(dollar.change, "data//dollar.change.csv", row.names = FALSE)
#write_csv(percent.change, "data//percent.change.csv", row.names = FALSE)