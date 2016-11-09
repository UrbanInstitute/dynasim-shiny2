

# Library and Source Statements
library(tidyverse)
library(extrafont)
library(grid)
library(RColorBrewer)


Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")

# Source file for Windows
#source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R')
#source('urban_theme_windows.R')

# Source file for Mac
#source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_ggplot_theme.R')
source('urban_theme_mac.R')

options(scipen=999)



# Load data
#mean.income <- tbl_df(read.csv("data\\mean.income.csv", header = TRUE, stringsAsFactors = TRUE))
#dollar.change <- tbl_df(read.csv("data\\dollar.change.csv", header = TRUE, stringsAsFactors = TRUE))
#percent.change <- tbl_df(read.csv("data\\percent.change.csv", header = TRUE, stringsAsFactors = TRUE))

mean.income <- tbl_df(read.csv("data/mean.income.csv", header = TRUE, stringsAsFactors = TRUE))
dollar.change <- tbl_df(read.csv("data/dollar.change.csv", header = TRUE, stringsAsFactors = TRUE))
percent.change <- tbl_df(read.csv("data/percent.change.csv", header = TRUE, stringsAsFactors = TRUE))





mean.income <- mean.income %>%
  mutate(comparison = "mean.income")

dollar.change <- dollar.change %>%
  mutate(comparison = "dollar.change")

percent.change <- percent.change %>%
  mutate(comparison = "percent.change")

mean.income <- bind_rows(mean.income, dollar.change, percent.change) %>%
  mutate(group = gsub("1\\.", "", group)) %>%
  mutate(group = gsub("2\\.", "", group)) %>%
  mutate(group = gsub("3\\.", "", group)) %>%
  mutate(group = gsub("4\\.", "", group))




mean.income %>%
  filter(measure == "per capita annuity") %>%
  filter(category == "Education") %>%
  ggplot(aes(year, value, color = group)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_line() +
  ggtitle("Per Capita Annuity")


dollar.change %>%
  filter(measure == "per capita annuity") %>%
  filter(category == "Education") %>%
  ggplot(aes(year, value, color = group)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_line() +
  ggtitle("Per Capita Annuity")









