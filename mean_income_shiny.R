## Libraries and Source Files
library(shiny)
library(tidyverse)
library(extrafont)
library(grid)
library(RColorBrewer)

options(scipen = 999)

#Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")

# Source file for Windows
#source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R')
#source('urban_theme_windows.R')


# Source file for Mac
#source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_ggplot_theme.R')
source('urban_theme_mac.R')

# Load data
#mean.income <- tbl_df(read.csv("data\\mean.income.csv", header = TRUE, stringsAsFactors = FALSE))
#dollar.change <- tbl_df(read.csv("data\\dollar.change.csv", header = TRUE, stringsAsFactors = FALSE))
#percent.change <- tbl_df(read.csv("data\\percent.change.csv", header = TRUE, stringsAsFactors = FALSE))

mean.income <- tbl_df(read.csv("data/mean.income.csv", header = TRUE, stringsAsFactors = FALSE))
dollar.change <- tbl_df(read.csv("data/dollar.change.csv", header = TRUE, stringsAsFactors = FALSE))
percent.change <- tbl_df(read.csv("data/percent.change.csv", header = TRUE, stringsAsFactors = FALSE))

# 

mean.income <- mean.income %>%
  mutate(comparison = "mean.income") %>%
  mutate(group = gsub("1\\.", "", group)) %>%
  mutate(group = gsub("2\\.", "", group)) %>%
  mutate(group = gsub("3\\.", "", group)) %>%
  mutate(group = gsub("4\\.", "", group)) %>%
  mutate(group = factor(group, levels = group))
  
dollar.change <- dollar.change %>%
  mutate(comparison = "dollar.change") %>%
  mutate(group = gsub("1\\.", "", group)) %>%
  mutate(group = gsub("2\\.", "", group)) %>%
  mutate(group = gsub("3\\.", "", group)) %>%
  mutate(group = gsub("4\\.", "", group)) %>%
  mutate(group = factor(group, levels = group))

percent.change <- percent.change %>%
  mutate(comparison = "percent.change") %>%
  mutate(group = gsub("1\\.", "", group)) %>%
  mutate(group = gsub("2\\.", "", group)) %>%
  mutate(group = gsub("3\\.", "", group)) %>%
  mutate(group = gsub("4\\.", "", group)) %>%
  mutate(group = factor(group, levels = group))

##
## SHINY
##

ui <- fluidPage(
  
  titlePanel("Urban Institute Analysis of BPC Social Security Reforms"),
  
  fluidRow(
    
    column(6,
           plotOutput("chart"))),
    
  fluidRow(
    column(4,
      selectInput(inputId = "comparison",
        label = "Comparison",
        choices = c("Level" = "mean.income",
                    "Percent Change" = "percent.change",
                    "Dollar Change" = "dollar.change")),
    
     selectInput(inputId = "measure",
       label = "Measure",
       choices = c("Per Capita Annuity Income" = "per capita annuity",
                    "Per Capita Cash Income" = "per capita cash",
                   "Net Per Capita Annuity Income" = "net per capita annuity",
                    "Net Per Capita Cash Income" = "net per capita cash")),
    
     selectInput(inputId = "demographic",
       label = "Demographic",
       choices = c("All" = "All",
                    "Sex" = "Sex",
                    "Education" = "Education",
                    "Race Ethnicity" = "Race Ethnicity",
                    "Marital Status" = "Marital Status",
                    "Shared Work Years" = "Shared Work Years",
                    "Own Work Years" = "Own Work Years",
                    "Shared Income Quintile" = "Shared Income Quintile",
                    "Shared Lifetime Earnings" = "Shared Lifetime Earnings",
                    "Homeownership" = "Homeownership",
                    "Family Income Relative to Official Poverty" = "Family Income Relative to Official Poverty",
                    "Per Capita Financial Assets" = "Per Capita Financial Assets ($2015)",
                    "Per Capita Financial + Retirement Account Assets" = 
                     "Per Capita Financial + Retirement Account Assets ($2015)"))))

)

server <- function(input, output){
  
  output$chart <- renderPlot({

    title <- if (input$measure == "per capita annuity") {
                 "Per Capita Annuity Income of People Age 62 and Older"} 
             else if (input$measure == "per capita cash") {
               "Per Capita Cash Income of People Age 62 and Older"}
             else if (input$measure == "net per capita annuity") {
               "Net Per Capita Annuity Income of People Age 62 and Older"
               }
             else if (input$measure == "net per capita cash") {
             "Net Per Capita Cash Income of People Age 62 and Older"}
    
    if (input$comparison == "mean.income") {
      mean.income %>%
        filter(measure == input$measure) %>%
        filter(category == input$demographic) %>%
        ggplot(aes(year, value, color = group)) +
        geom_line() +
        ggtitle(title) +
        xlab("Year") +
        ylab("Mean Annual Income") +
        geom_line(size = 1) +
        scale_y_continuous(labels = scales::dollar)
      } else if (input$comparison == "dollar.change") {
      dollar.change %>%
        filter(measure == input$measure) %>%
        filter(category == input$demographic) %>%
        ggplot(aes(year, value, color = group)) +
        geom_line() +
        ggtitle(title) +
        xlab("Year") +
        ylab("Percent Change in Mean Annual Income") +
        geom_line(size = 1) +
        scale_y_continuous(labels = scales::dollar) +
        expand_limits(y = 0)
    } else if (input$comparison == "percent.change") {
      percent.change %>%
        filter(measure == input$measure) %>%
        filter(category == input$demographic) %>%
        ggplot(aes(year, value, color = group)) +
        geom_line() +
        ggtitle(title) +
        xlab("Year") +
        ylab("Change in Mean Annual Income") +
        geom_line(size = 1) +
        scale_y_continuous(labels = scales::percent) +
        expand_limits(y = 0)
    }
    
  })
  
  }
shinyApp(ui = ui, server = server)


