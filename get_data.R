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
scheduled.dollar.change <- read_excel("X:\\programs\\Run912\\run5SaveOpt4\\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx", sheet = "$mean income compare", skip = 2, col_names = TRUE)
scheduled.percent.change <- read_excel("X:\\programs\\Run912\\run5SaveOpt4\\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx", sheet = "%mean income compare", skip = 2, col_names = TRUE)






files <- read_csv("option, scale, link
          bpc, per capita, X:\\programs\\Run912\\run5SaveOpt4\\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx
          bpc, equivalent, X:\\programs\\Run912\\run5SaveOpt4\\BPCtableShellsEquivalentRun5SaveOpt4.xlsx
          0, per capita, X:\\programs\\Run912\\opt0\\BPCtableShellsOPT0.xlsx
          0, equivalent, X:\\programs\\Run912\\opt0\\BPCtableShellsEquivalentOPT0.xlsx
          1, per capita, X:\\programs\\Run912\\opt1(MiniPIA)\\BPCtableShellsOPT1.xlsx
          1, equivalent, X:\\programs\\Run912\\opt1(MiniPIA)\\BPCtableShellsEquivalentOPT1.xlsx
          2, per capita, X:\\programs\\Run912\\opt2(increaseTaxSSB)\\BPCtableShellsOPT2.xlsx
          2, equivalent, X:\\programs\\Run912\\opt2(increaseTaxSSB)\\BPCtableShellsEquivalentOPT2.xlsx
          3, per capita, X:\\programs\\Run912\\opt3(capSpouse)\\BPCtableShellsOPT3.xlsx
          3, equivalent, X:\\programs\\Run912\\opt3(capSpouse)\\BPCtableShellsEquivalentOPT3.xlsx
          4, per capita, X:\\programs\\Run912\\opt4(survivorjs75)\\BPCtableShellsOPT4.xlsx
          4, equivalent, X:\\programs\\Run912\\opt4(survivorjs75)\\BPCtableShellsEquivalentOPT4.xlsx
          5, per capita, X:\\programs\\Run912\\opt5(increaseTAXMAX)\\BPCtableShellsOPT5.xlsx
          5, equivalent, X:\\programs\\Run912\\opt5(increaseTAXMAX)\\BPCtableShellsEquivalentOPT5.xlsx
          5b, per capita, X:\\programs\\Run912\\opt5B(RaiseFICA)\\BPCtableShellsOPT5B.xlsx
          5b, equivalent, X:\\programs\\Run912\\opt5B(RaiseFICA)\\BPCtableShellsEquivalentOPT5B.xlsx
          5c, per capita, X:\\programs\\Run912\\opt5c(RaiseFICAonly)\\BPCtableShellsOPT5c.xlsx
          5c, equivalent, X:\\programs\\Run912\\opt5c(RaiseFICAonly)\\BPCtableShellsEquivalentOPT5c.xlsx
          6, per capita, X:\\programs\\Run912\\opt6(decreaseCOLA)\\BPCtableShellsOPT6.xlsx
          6, equivalent, X:\\programs\\Run912\\opt6(decreaseCOLA)\\BPCtableShellsEquivalentOPT6.xlsx
          7, per capita, X:\\programs\\Run912\\opt7(IncreaseFRA)\\BPCtableShellsOPT7.xlsx
          7, equivalent, X:\\programs\\Run912\\opt7(IncreaseFRA)\\BPCtableShellsEquivalentOPT7.xlsx
          8, per capita, X:\\programs\\Run912\\opt8(IncreaseFRAERA)\\BPCtableShellsOPT8.xlsx
          8, equivalent, X:\\programs\\Run912\\opt8(IncreaseFRAERA)\\BPCtableShellsEquivalentOPT8.xlsx
          9, per capita, X:\\programs\\Run912\\opt9\\BPCtableShellsOPT9v12.xlsx
          9, equivalent, X:\\programs\\Run912\\opt9\\BPCtableShellsEquivalentOPT9v12.xlsx
          10, per capita, X:\\programs\\Run912\\opt10\\BPCtableShellsOPT10v5.xlsx
          10, equivalent, X:\\programs\\Run912\\opt10\\BPCtableShellsEQUIVALENTOPT10v5.xlsx
          12, per capita, X:\\programs\\Run912\\opt12\\BPCtableShellsOPT12.xlsx
          12, equivalent, X:\\programs\\Run912\\opt12\\BPCtableShellsEquivalentOPT12.xlsx")
        
works <- read_excel("X:\\programs\\Run912\\opt1(MiniPIA)\\BPCtableShellsEquivalentOPT1.xlsx", sheet = "mean income", skip = 2, col_names = TRUE)
broken <- read_excel("X:\\programs\\Run912\\opt10\\BPCtableShellsEQUIVALENTOPT10v5.xlsx", sheet = "mean income", skip = 2, col_names = TRUE)

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
                 mutate(year = as.numeric(gsub("year", "", year))) %>%
                 mutate(chart = "mean.income")
  
  return(mean.income)
}

# run function

final.income <- tibble()

for (i in 1:nrow(files)) {
 
  mean.income <- meanIncomeScrapeR(as.character(files[i, 3]))
  
  mean.income <- mean.income %>%
    mutate(option = as.character(files[i, 1])) %>%
    mutate(scale = as.character(files[i, 2]))
  
  final.income <- bind_rows(final.income, mean.income)
   
}

# 46,080 rows
#table(final.income$category)
#table(final.income$measure)
#table(final.income$year)
#table(final.income$option)
#table(final.income$scale)








# Fix imperfections from read_excel
scheduled.dollar.change <- scheduled.dollar.change[, c(1:27, 29:37)]
scheduled.percent.change <- scheduled.percent.change[, c(1:27, 29:37)]


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