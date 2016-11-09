## Libraries and Source Files
library(shiny)
library(tidyverse)
library(extrafont)
library(grid)
library(RColorBrewer)

Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")

# Urban theme for Windows
source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R')


# Load data
mean.income <- tbl_df(read.csv("data\\mean.income.csv", header = TRUE, stringsAsFactors = TRUE))
dollar.change <- tbl_df(read.csv("data\\dollar.change.csv", header = TRUE, stringsAsFactors = TRUE))
percent.change <- tbl_df(read.csv("data\\percent.change.csv", header = TRUE, stringsAsFactors = TRUE))

# 

mean.income <- mean.income %>%
  mutate(comparison = "mean.income")
  
dollar.change <- dollar.change %>%
  mutate(comparison = "dollar.change")

percent.change <- percent.change %>%
  mutate(comparison = "percent.change")

mean.income <- bind_rows(mean.income, dollar.change, percent.change)

##
## SHINY
##

ui <- fluidPage(
  
  titlePanel("Urban Analysis of BPC Social Security Reforms"),
  
  verticalLayout(
    
    plotOutput("chart"),
    
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
                  "Per Capita Financial + Retirement Account Assets" = "Per Capita Financial + Retirement Account Assets ($2015)"))

  )
)

server <- function(input, output){
  
  output$chart <- renderPlot({

    mean.income %>%
      filter(comparison == input$comparison) %>%
      filter(measure == input$measure) %>%
      filter(category == input$demographic) %>%
      ggplot(aes(year, value, color = group)) +
      scale_y_continuous(expand = c(0,0)) +
      geom_line() +
      ggtitle("Per Capita Annuity")
  
  })
  
  }
shinyApp(ui = ui, server = server)


