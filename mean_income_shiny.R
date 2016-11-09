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

##
## SHINY
##



ui <- fluidPage(
  
  titlePanel("Urban Analysis of BPC Social Security Reforms"),
  
  verticalLayout(
    
    plotOutput("chart"),
    
    selectInput(inputId = "measure",
      label = "Measure",
      choices = c("Per Capita Annuity Income" = "per capita annuity",
                  "Per Capita Cash Income" = "per capita cash",
                  "Net Per Capita Annuity Income" = "net per capita annuity",
                  "Net Per Capita Cash Income" = "net per capita cash"))
  )
)


#
#
#
#






server <- function(input, output){
  
  output$chart <- renderPlot({

    mean.income %>%
      filter(measure == "per capita annuity") %>%
      filter(category == "Education") %>%
      ggplot(aes(year, value, color = group)) +
      scale_y_continuous(expand = c(0,0)) +
      geom_line() +
      ggtitle("Per Capita Annuity")
  
  })
  
  }
shinyApp(ui = ui, server = server)


