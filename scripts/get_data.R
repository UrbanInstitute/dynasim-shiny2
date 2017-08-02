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
files <- read_excel("options_guide.xlsx") %>%
  select(option, scale, link)

##
## MEAN INCOME
##

meanIncomeScrapeR <- function(link) {

  mean.income <- read_excel(link, sheet = "mean income", skip = 2, col_names = TRUE)
  
  mean.income <- mean.income[, colSums(is.na(mean.income)) != nrow(mean.income)]

  # Average Annuity Income
  annuity <- mean.income[, 1:9]
  
  names(annuity) <- c("category", "trash", "group", "year2015", "year2025", "year2035", "year2045", "year2055", "year2065")
  
  annuity <- annuity %>%
    select(-trash) %>%
    filter(!is.na(group)) %>%
    mutate(category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category)) %>%
    filter(!is.na(year2015)) %>%
    mutate(measure = "Average Annuity Income")
  
  # Average Cash Income
  cash <- mean.income[, 10:18]
  
  names(cash) <- c("category", "trash", "group", "year2015", "year2025", "year2035", "year2045", "year2055", "year2065")
  
  cash <- cash %>%
    select(-trash) %>%
    filter(!is.na(group)) %>%
    mutate(category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category)) %>%
    filter(!is.na(year2015)) %>%
    mutate(measure = "Average Cash Income")
  
  # Average Net Annuity Income
  net.annuity <- mean.income[, 19:27]
  
  names(net.annuity) <- c("category", "trash", "group", "year2015", "year2025", "year2035", "year2045", "year2055", "year2065")
  
  net.annuity <- net.annuity %>%
    select(-trash) %>%
    filter(!is.na(group)) %>%
    mutate(category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category)) %>%
    filter(!is.na(year2015)) %>%
    mutate(measure = "Average Net Annuity Income")
  
  # Average Net Cash Income
  net.cash <- mean.income[, 28:36]
  
  names(net.cash) <- c("category", "trash", "group", "year2015", "year2025", "year2035", "year2045", "year2055", "year2065")
  
  net.cash <- net.cash %>%
    select(-trash) %>%
    filter(!is.na(group)) %>%
    mutate(category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category),
           category = ifelse(is.na(category), lag(category), category)) %>%
    filter(!is.na(year2015)) %>%
    mutate(measure = "Average Net Cash Income")
  
  mean.income <- bind_rows(annuity, cash, net.annuity, net.cash)
  
  rm(annuity, cash, net.annuity, net.cash)
  
  mean.income <- gather(mean.income, key = year , value,-category, -group, -category, -measure) %>%
                 mutate(year = as.numeric(gsub("year", "", year)))
  
  return(mean.income)
}

# run function

final.income <- tibble()

for (i in 1:nrow(files)) {
 
  mean.income <- meanIncomeScrapeR(as.character(files[i, 3]))
  
  mean.income <- mean.income %>%
    mutate(option = as.character(files[i, 1]),
           scale = as.character(files[i, 2]))
  
  final.income <- bind_rows(final.income, mean.income)
   
}

# TODO(aaron): replace this for loop with with something better
# final.income should be 1,536 * 38 = 58,368 rows

# Remove "Per Capita " and "Equivalent " so the left_join works
final.income <- final.income %>%
  mutate(category = gsub("Per Capita ", "", category),
         category = gsub("Equivalent ", "", category))

# Rename dataframe for left_join
options <- final.income 
# should contain 58,368 rows
# 64 subgroups * 4 measures * 6 years * 17 options * 2 scales

# Create a df with the options
baselines <- final.income %>%
  filter(option == "Payable Law" | option == "Scheduled Law") %>%
  rename(baseline.value = value, baseline.type = option)
# should contain 6,144 rows
# 64*4*6*2*2

# left_join the options with the baselines
final.income <- left_join(options, baselines, by = c("category", "group", "measure", "year", "scale"))
# should be 116,736 observations
# 64 subgroups * 4 measures * 6 years * 19 options * 2 scales * 2 measures

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
  gather(level, percent.change, dollar.change, key = "comparison", value = "value")
# Should be 116,736 observations before gather()
# Should be 350,208 observations after gather()

rm(files, mean.income, options, baselines, i)

# If data directory does not exist, create data directory
if (!dir.exists("data")) {
  dir.create("data")
}

# Combine data sets
write_csv(final.income, "data//new_income.csv")