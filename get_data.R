# Aaron Williams, Urban Institute Program on Retirement Policy

# This script reads mean income data for 64 groups, across 4 measures, in 6 
# different decades, for 15 different reform options, in two scales. It then 
# calculates percent and dollar change against two different baselines. 
# The script also cleans the data and turns them into a long data frame 
# formatted for ggplot2 

# Library, Source, and Options Statements
library(tidyverse)
library(readxl)

options(scipen = 999)

# Create a df with links to the Excel sheets with the mean income data
files <- read_csv("option, scale, base, link
          bpc, per capita, level, X:\\programs\\Run912\\run5SaveOpt4\\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx
          bpc, equivalent, level, X:\\programs\\Run912\\run5SaveOpt4\\BPCtableShellsEquivalentRun5SaveOpt4.xlsx
          0, per capita, scheduled, X:\\programs\\Run912\\opt0\\BPCtableShellsOPT0.xlsx
          0, equivalent, scheduled, X:\\programs\\Run912\\opt0\\BPCtableShellsEquivalentOPT0.xlsx
          0, per capita, payable, X:\\programs\\Run912\\payable\\BPCTableShellsPayableV3.xlsx
          0, equivalent, payable, X:\\programs\\Run912\\payable\\BPCTableShellsEquivalentPayableV3.xlsx
          1, per capita, level, X:\\programs\\Run912\\opt1(MiniPIA)\\BPCtableShellsOPT1.xlsx
          1, equivalent, level, X:\\programs\\Run912\\opt1(MiniPIA)\\BPCtableShellsEquivalentOPT1.xlsx
          2, per capita, level, X:\\programs\\Run912\\opt2(increaseTaxSSB)\\BPCtableShellsOPT2.xlsx
          2, equivalent, level, X:\\programs\\Run912\\opt2(increaseTaxSSB)\\BPCtableShellsEquivalentOPT2.xlsx
          3, per capita, level, X:\\programs\\Run912\\opt3(capSpouse)\\BPCtableShellsOPT3.xlsx
          3, equivalent, level, X:\\programs\\Run912\\opt3(capSpouse)\\BPCtableShellsEquivalentOPT3.xlsx
          4, per capita, level, X:\\programs\\Run912\\opt4(survivorjs75)\\BPCtableShellsOPT4.xlsx
          4, equivalent, level, X:\\programs\\Run912\\opt4(survivorjs75)\\BPCtableShellsEquivalentOPT4.xlsx
          5, per capita, level, X:\\programs\\Run912\\opt5(increaseTAXMAX)\\BPCtableShellsOPT5.xlsx
          5, equivalent, level, X:\\programs\\Run912\\opt5(increaseTAXMAX)\\BPCtableShellsEquivalentOPT5.xlsx
          5b, per capita, level, X:\\programs\\Run912\\opt5B(RaiseFICA)\\BPCtableShellsOPT5B.xlsx
          5b, equivalent, level, X:\\programs\\Run912\\opt5B(RaiseFICA)\\BPCtableShellsEquivalentOPT5B.xlsx
          5c, per capita, level, X:\\programs\\Run912\\opt5c(RaiseFICAonly)\\BPCtableShellsOPT5c.xlsx
          5c, equivalent, level, X:\\programs\\Run912\\opt5c(RaiseFICAonly)\\BPCtableShellsEquivalentOPT5c.xlsx
          6, per capita, level, X:\\programs\\Run912\\opt6(decreaseCOLA)\\BPCtableShellsOPT6.xlsx
          6, equivalent, level, X:\\programs\\Run912\\opt6(decreaseCOLA)\\BPCtableShellsEquivalentOPT6.xlsx
          7, per capita, level, X:\\programs\\Run912\\opt7(IncreaseFRA)\\BPCtableShellsOPT7.xlsx
          7, equivalent, level, X:\\programs\\Run912\\opt7(IncreaseFRA)\\BPCtableShellsEquivalentOPT7.xlsx
          8, per capita, level, X:\\programs\\Run912\\opt8(IncreaseFRAERA)\\BPCtableShellsOPT8.xlsx
          8, equivalent, level, X:\\programs\\Run912\\opt8(IncreaseFRAERA)\\BPCtableShellsEquivalentOPT8.xlsx
          9, per capita, level, X:\\programs\\Run912\\opt9\\BPCtableShellsOPT9v12.xlsx
          9, equivalent, level, X:\\programs\\Run912\\opt9\\BPCtableShellsEquivalentOPT9v12.xlsx
          10, per capita, level, X:\\programs\\Run912\\opt10\\BPCtableShellsOPT10v5.xlsx
          10, equivalent, level, X:\\programs\\Run912\\opt10\\BPCtableShellsEQUIVALENTOPT10v5.xlsx
          12, per capita, level, X:\\programs\\Run912\\opt12\\BPCtableShellsOPT12.xlsx
          12, equivalent, level, X:\\programs\\Run912\\opt12\\BPCtableShellsEquivalentOPT12.xlsx")
        
##
## MEAN INCOME
##

meanIncomeScrapeR <- function(link) {

  mean.income <- read_excel(link, sheet = "mean income", skip = 2, col_names = TRUE)
  
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
                 mutate(year = as.numeric(gsub("year", "", year)))
  
  return(mean.income)
}

# run function

final.income <- tibble()

for (i in 1:nrow(files)) {
 
  mean.income <- meanIncomeScrapeR(as.character(files[i, 4]))
  
  mean.income <- mean.income %>%
    mutate(option = as.character(files[i, 1])) %>%
    mutate(scale = as.character(files[i, 2])) %>%
    mutate(base = as.character(files[i, 3]))
  
  final.income <- bind_rows(final.income, mean.income)
   
}
# final.income should be 1,536 * 32 = 49,152 rows

# Remove "Per Capita " and "Equivalent " so the left_join works
final.income <- final.income %>%
  mutate(category = gsub("Per Capita ", "", category)) %>%
  mutate(category = gsub("Equivalent ", "", category))

# Create a df with the baselines
options <- final.income %>%
  filter(option != "0") %>%
  select(-base)
# should contain 43,008 rows
# 64*4*6*14*2

# Create a df with the options
baselines <- final.income %>%
  filter(option == "0") %>%
  spread(key = option, value = value) %>%
  rename(Option0 = `0`)
# should contain 6,144 rows
# 64*4*6*2*2

# left_join the options with the baselines
final.income <- left_join(options, baselines, by = c("category", "group", "measure", "year", "scale"))
# should be 86,016 observations

# Calculate the dollar and percent changes
final.income <- final.income %>%
  mutate(dollar.change = value - Option0) %>%
  mutate(percent.change = (value - Option0) / Option0) %>%
  select(-Option0)

# Clean up baselines so it matches final.income
baselines <- baselines %>%
  rename(value = Option0) %>%
  mutate(option = "0") %>%
  mutate(dollar.change = 0) %>%
  mutate(percent.change = 0)

# Combine the baselines (with zeroes for changes) and the options
final.income <- union(final.income, baselines)
# Should be 92,160 observations


# If data directory does not exist, create data directory
if (!file.exists("data")) {
  dir.create("data")
}

# Combine data sets
write_csv(final.income, "data//new_income.csv")
