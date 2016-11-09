

# Library and Source Statements
library(tidyverse)
library(extrafont)
library(grid)
library(RColorBrewer)


Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")

# Source file for Windows
#source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R')
source('urban_theme_windows.R')

# Source file for Mac
#source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_ggplot_theme.R')

options(scipen=999)



# Load data
mean.income <- tbl_df(read.csv("data\\mean.income.csv", header = TRUE, stringsAsFactors = TRUE))
dollar.change <- tbl_df(read.csv("data\\dollar.change.csv", header = TRUE, stringsAsFactors = TRUE))
percent.change <- tbl_df(read.csv("data\\percent.change.csv", header = TRUE, stringsAsFactors = TRUE))

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









