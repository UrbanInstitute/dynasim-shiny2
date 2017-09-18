# Aaron Williams, Urban Institute Program on Retirement Policy

# This script reads mean income data for 64 groups, across 4 measures, in 6 
# different decades, for user-defined reform options, in two scales. It then 
# calculates percent and dollar change against two different baselines. 
# The script also cleans the data and turns them into a long data frame 
# formatted for ggplot2 

# Library, Source, and Options Statements
library(tidyverse)
library(readxl)

options(scipen = 999)

# Read df with links to the Excel sheets with the mean income data
files <- read_csv("options-guide.csv",
                  col_types = cols(
                    option.name = col_character(),
                    option = col_character(),
                    scale = col_character(),
                    link = col_character(),
                    directory = col_character(),
                    file = col_character()
                  )) %>%
  select(option, scale, link)

# define workhouse function
mean_income_scraper <- function(link, option_label, scale_label) {

  #option_label <- enquo(option_label)
  #scale_label <- enquo(scale_label)
  
  mean.income <- read_excel(link, sheet = "mean income", skip = 2, col_names = TRUE)
  
  mean.income <- mean.income[, colSums(is.na(mean.income)) != nrow(mean.income)]

  # Average Annuity Income
  annuity <- mean.income[, 1:9]
  
  names(annuity) <- c("category", "trash", "group", "year2015", "year2025", "year2035", "year2045", "year2055", "year2065")
  
  annuity <- annuity %>%
    select(-trash) %>%
    filter(!is.na(group)) %>%
    mutate(category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category)) %>%
    filter(!is.na(year2015)) %>%
    mutate(measure = "Average annuity income")
  
  # Average Cash Income
  cash <- mean.income[, 10:18]
  
  names(cash) <- c("category", "trash", "group", "year2015", "year2025", "year2035", "year2045", "year2055", "year2065")
  
  cash <- cash %>%
    select(-trash) %>%
    filter(!is.na(group)) %>%
    mutate(category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category)) %>%
    filter(!is.na(year2015)) %>%
    mutate(measure = "Average cash income")
  
  # Average Net Annuity Income
  net.annuity <- mean.income[, 19:27]
  
  names(net.annuity) <- c("category", "trash", "group", "year2015", "year2025", "year2035", "year2045", "year2055", "year2065")
  
  net.annuity <- net.annuity %>%
    select(-trash) %>%
    filter(!is.na(group)) %>%
    mutate(category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category)) %>%
    filter(!is.na(year2015)) %>%
    mutate(measure = "Average net annuity income")
  
  # Average Net Cash Income
  net.cash <- mean.income[, 28:36]
  
  names(net.cash) <- c("category", "trash", "group", "year2015", "year2025", "year2035", "year2045", "year2055", "year2065")
  
  net.cash <- net.cash %>%
    select(-trash) %>%
    filter(!is.na(group)) %>%
    mutate(category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category),
           category = if_else(is.na(category), lag(category), category)) %>%
    filter(!is.na(year2015)) %>%
    mutate(measure = "Average net cash income")
  
  mean.income <- bind_rows(annuity, cash, net.annuity, net.cash)
  
  rm(annuity, cash, net.annuity, net.cash)
  
  mean.income <- gather(mean.income, key = year , value,-category, -group, -category, -measure) %>%
                 mutate(year = as.numeric(gsub("year", "", year)),
                        option = !!option_label,
                        scale = !!scale_label)
  
  return(mean.income)
}


# run function
final.income <- pmap(list(files$link, files$option, files$scale), mean_income_scraper) %>%
  reduce(bind_rows)

# final.income should be 1,536 * 44 = 67,584
stopifnot(nrow(final.income) == 67584)

# Remove "Per Capita " and "Equivalent " so the left_join works
final.income <- final.income %>%
  mutate(category = gsub("Per Capita ", "", category),
         category = gsub("Equivalent ", "", category))

# Rename dataframe for left_join
options <- final.income

# Create a df with the options
baselines <- final.income %>%
  filter(option %in% c("Payable law","Scheduled law")) %>%
  rename(baseline.value = value, baseline.type = option)

# should contain 6,144 rows
stopifnot(nrow(baselines) == 6144)

# left_join the options with the baselines
final.income <- left_join(options, baselines, by = c("category", "group", "measure", "year", "scale"))

# should contain 135,168 rows
stopifnot(nrow(final.income) == 135,168)

# Calculate the dollar and percent changes
final.income <- final.income %>%
  mutate(dollar.change = value - baseline.value,
         percent.change = (value - baseline.value) / baseline.value) %>%
  select(-baseline.value)

# Clean up baselines so it matches final.income
baselines <- baselines %>%
  rename(value = baseline.value) %>%
  rename(option = baseline.type) %>%
  mutate(dollar.change = 0,
         percent.change = 0,
         baseline.type = option)

# Combine the baselines (with zeroes for changes) and the options
final.income <- union(final.income, baselines) %>%
  rename(baseline = baseline.type, level = value) %>%
  gather(level, percent.change, dollar.change, key = "comparison", value = "value") %>%
  spread(key = measure, value = value) %>%
  mutate(group = gsub("1\\.", "", group)) %>%
  mutate(group = gsub("2\\.", "", group)) %>%
  mutate(group = gsub("3\\.", "", group)) %>%
  mutate(group = gsub("4\\.", "", group))

# should contain 101,376 rows
stopifnot(nrow(final.income) == 101376)

rm(files, options, baselines)

# Break into smaller data frames
final.income %>%
  filter(comparison == "level") %>%
  select(-comparison) %>%
  write_csv("data/level.csv")

final.income %>%
  filter(comparison == "dollar.change") %>%
  select(-comparison) %>%
  write_csv("data/dollar-change.csv")

final.income %>%
  filter(comparison == "percent.change") %>%
  select(-comparison) %>%
  write_csv("data/percent-change.csv")
