## Libraries and Source Files
library(shiny)
library(tidyverse)
library(extrafont)
library(grid)
library(RColorBrewer)
library(scales)

options(scipen = 999)

# Source file for Windows
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")
#source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R')
source('urban_institute_themes/urban_theme_windows.R')

# Source file for Mac
#source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_ggplot_theme.R')
#source('urban_institute_themes/urban_theme_mac.R')

# Load Data
mean.income <- read_csv("data/mean.income.csv")
dollar.change <- read_csv("data/dollar.change.csv")
percent.change <- read_csv("data/percent.change.csv")

# Remove numbers in certain factor levels and order all levels
mean.income <- mean.income %>%
  mutate(comparison = "mean.income") %>%
  mutate(group = gsub("1\\.", "", group)) %>%
  mutate(group = gsub("2\\.", "", group)) %>%
  mutate(group = gsub("3\\.", "", group)) %>%
  mutate(group = gsub("4\\.", "", group)) %>%
  mutate(group = factor(group, levels = unique(group)))
  
dollar.change <- dollar.change %>%
  mutate(comparison = "dollar.change") %>%
  mutate(group = gsub("1\\.", "", group)) %>%
  mutate(group = gsub("2\\.", "", group)) %>%
  mutate(group = gsub("3\\.", "", group)) %>%
  mutate(group = gsub("4\\.", "", group)) %>%
  mutate(group = factor(group, levels = unique(group)))

percent.change <- percent.change %>%
  mutate(comparison = "percent.change") %>%
  mutate(group = gsub("1\\.", "", group)) %>%
  mutate(group = gsub("2\\.", "", group)) %>%
  mutate(group = gsub("3\\.", "", group)) %>%
  mutate(group = gsub("4\\.", "", group)) %>%
  mutate(group = factor(group, levels = unique(group)))

##
## SHINY
##

ui <- fluidPage(

  theme = "shiny.css",
  
  
  fluidRow(
    
    column(8,
           
           titlePanel("Exploring Social Security Reform Options"),
           
           p("The Social Security trustees project that, by the mid-2030s, the system will no longer be able to pay all scheduled benefits. Which reform option should policymakers pursue to help balance the system?
             Use our interactive tool to compare how different groups would fare, over time, under the following policy options."),
           HTML("<p>Explore the trust fund, <b>by income</b>, by demographics, and <a href='http://www.urban.org/policy-centers/cross-center-initiatives/program-retirement-policy/projects/dynasim-projecting-older-americans-future-well-being/detailed-projections-older-population-through-2065'>the data</a>.</p>"),
           
           br()
           
           
           )
    
  ),
  
  fluidRow(
    
    column(6,
           style = "position:relative",
           plotOutput("chart",
                      hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
           uiOutput("hover_info"))),
    
  fluidRow(
    column(4,
           
      selectInput(inputId = "option",
        label = "Reform Option",
        choices = c("BPC Package" = "bpc",
                    "Mini.PIA" = "mini.pia", 
                    "Tax SSB" = "tax.ssb",
                    "Cap Spouse" = "cap.spouse",
                    "SurvivorJS75" = "survivor.js75",
                    "90% Tax max" = "taxmax90",
                    "90% Tax max and 13.4 FICA" = "taxmax90.fica13.4",
                    "Chained-CPI COLA" = "cola.chaincpi",
                    "Reduce COLA" = "reduce.cola",
                    "Increase FRA" = "increase.fra",
                    "Increase ERA & FRA" = "increase.fra.era",
                    "Tax Max to $150,000" = "taxmax150000",
                    "Tax Max to $180,000" = "taxmax180000",
                    "Eliminate the Tax Max" = "notaxmax",
                    "13.4% FICA" = "fica13.4",
                    "14% FICA" = "fica14",
                    "15% FICA" = "fica15"))),
           
    column(4,
      selectInput(inputId = "comparison",
        label = "Comparison",
        choices = c("Level" = "mean.income",
                    "Percent Change" = "percent.change",
                    "Dollar Change" = "dollar.change")))),
    
      fluidRow(
        column(4,
      
     selectInput(inputId = "measure",
       label = "Measure",
       choices = c("Gross Per Capita Annuity Income" = "per capita annuity",
                   "Gross Per Capita Cash Income" = "per capita cash",
                   "Net Per Capita Annuity Income" = "net per capita annuity",
                   "Net Per Capita Cash Income" = "net per capita cash"))),
    
     column(4,
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
                   "Shared Lifetime Earnings Quintile" = "Shared Lifetime Earnings Quintile",
                   "Homeownership" = "Homeownership",
                   "Family Income Relative to Official Poverty" = "Family Income Relative to Official Poverty",
                   "Per Capita Financial Assets" = "Per Capita Financial Assets ($2015)",
                   "Per Capita Financial + Retirement Account Assets" = 
                   "Per Capita Financial + Retirement Account Assets ($2015)"))))
)

server <- function(input, output){
  
  output$chart <- renderPlot({

    title <- if (input$measure == "per capita annuity") {
                 "Mean Gross Per Capita Annuity Income"} 
             else if (input$measure == "per capita cash") {
               "Mean Gross Per Capita Cash Income"}
             else if (input$measure == "net per capita annuity") {
               "Mean Net Per Capita Annuity Income"}
             else if (input$measure == "net per capita cash") {
               "Mean Net Per Capita Cash Income"}
    
    subtitle <- if (input$demographic == "All") {
                    "Everyone ages 62+, 2015 dollars"} 
                else if (input$demographic == "Sex") {
                  "Ages 62+ by sex, 2015 dollars"}
                else if (input$demographic == "Education") {
                  "Ages 62 and older by education, 2015 dollars"}
                else if (input$demographic == "Race Ethnicity") {
                  "Ages 62 and older by race & ethnicity, 2015 dollars"}
                else if (input$demographic == "Marital Status") {
                  "Ages 62 and older by marital status, 2015 dollars"}
                else if (input$demographic == "Shared Work Years") {
                  "Ages 62 and older by shared work years, 2015 dollars"}
                else if (input$demographic == "Own Work Years") {
                  "Ages 62 and older by own work years, 2015 dollars"}
                else if (input$demographic == "Shared Income Quintile") {
                  "Ages 62 and older by shared income quintile, 2015 dollars"}
                else if (input$demographic == "Shared Lifetime Earnings Quintile") {
                  "Ages 62 and older by shared lifetime earnings quintile, 2015 dollars"}   
                else if (input$demographic == "Homeownership") {
                  "Ages 62 and older by homeownership"}
                else if (input$demographic == "Family Income Relative to Official Poverty") {
                  "Ages 62 and older by family income relative to official poverty, 2015 dollars"}    
                else if (input$demographic == "Per Capita Financial Assets ($2015)") {
                  "Ages 62 and older by per capita financial assets, 2015 dollars"}
                else if (input$demographic == "Per Capita Financial + Retirement Account Assets ($2015)") {
                  "Ages 62 and older by per capita financial + retirement account assets, 2015 dollars"}    

    if (input$comparison == "mean.income") {
      mean.income %>%
        filter(measure == input$measure) %>%
        filter(category == input$demographic) %>%
        ggplot(aes(year, value, color = group)) +
        geom_line() +
        labs(title = title,
             subtitle = subtitle,
             caption = "DYNASIM4") +
        xlab("Year") +
        ylab(NULL) +
        geom_line(size = 1) +
        scale_x_continuous(breaks = c(2015, 2025, 2035, 2045, 2055, 2065)) +
        scale_y_continuous(expand = c(0.3, 0), labels = scales::dollar) +
        theme(axis.ticks.length = unit(0, "points"), 
              axis.text.x = element_text(margin = structure(c(4, 0, 0, 0),
                                                            unit = "pt",
                                                            valid.unit = 8L,
                                                            class = c("margin", "unit"))),
              axis.text.y = element_text(margin = structure(c(0, 2, 0, 0),
                                                            unit = "pt",
                                                            valid.unit = 8L,
                                                            class = c("margin", "unit"))),
              plot.subtitle = element_text(margin = structure(c(5, 0, 2, 0),
                                                              unit = "pt",
                                                              valid.unit = 8L,
                                                              class = c("margin", "unit"))),
              legend.box.margin = margin(6, 0, 0, 0, "points"))
      
      } else if (input$comparison == "dollar.change") {
      dollar.change %>%
        filter(measure == input$measure) %>%
        filter(category == input$demographic) %>%
        ggplot(aes(year, value, color = group)) +
        geom_line() +
        labs(title = title,
             subtitle = subtitle,
             caption = "DYNASIM4") +
        xlab("Year") +
        ylab(NULL) +
        geom_line(size = 1) +
        scale_x_continuous(breaks = c(2015, 2025, 2035, 2045, 2055, 2065)) +
        scale_y_continuous(expand = c(0.3, 0), labels = scales::dollar) +
        expand_limits(y = 0) +
        geom_hline(size = 0.5, yintercept = 0) +
        theme(axis.ticks.length = unit(0, "points"), 
              axis.text.x = element_text(margin = structure(c(4, 0, 0, 0),
                                                            unit = "pt",
                                                            valid.unit = 8L,
                                                            class = c("margin", "unit"))),
              axis.text.y = element_text(margin = structure(c(0, 2, 0, 0),
                                                            unit = "pt",
                                                            valid.unit = 8L,
                                                            class = c("margin", "unit"))),
              plot.subtitle = element_text(margin = structure(c(5, 0, 2, 0),
                                                              unit = "pt",
                                                              valid.unit = 8L,
                                                              class = c("margin", "unit"))),
              legend.box.margin = margin(6, 0, 0, 0, "points"))
        
    } else if (input$comparison == "percent.change") {
      percent.change %>%
        filter(measure == input$measure) %>%
        filter(category == input$demographic) %>%
        ggplot(aes(year, value, color = group)) +
        geom_line() +
        labs(title = title,
             subtitle = subtitle,
             caption = "DYNASIM4") +
        xlab("Year") +
        ylab(NULL) +
        geom_line(size = 1) +
        scale_x_continuous(breaks = c(2015, 2025, 2035, 2045, 2055, 2065)) +
        scale_y_continuous(expand = c(0.3, 0), labels = scales::percent) +
        expand_limits(y = 0) +
        geom_hline(size = 0.5, yintercept = 0) +
        theme(axis.ticks.length = unit(0, "points"), 
              axis.text.x = element_text(margin = structure(c(4, 0, 0, 0),
                                                            unit = "pt",
                                                            valid.unit = 8L,
                                                            class = c("margin", "unit"))),
              axis.text.y = element_text(margin = structure(c(0, 2, 0, 0),
                                                            unit = "pt",
                                                            valid.unit = 8L,
                                                            class = c("margin", "unit"))),
              plot.subtitle = element_text(margin = structure(c(5, 0, 2, 0),
                                                              unit = "pt",
                                                              valid.unit = 8L,
                                                              class = c("margin", "unit"))),
              legend.box.margin = margin(6, 0, 0, 0, "points"))
    }
  })
  
  # Chart
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    
    if (input$comparison == "mean.income") {
      point <- nearPoints(mean.income, hover, threshold = 20, maxpoints = 1)
    } else if (input$comparison == "dollar.change") {
      point <- nearPoints(dollar.change, hover, threshold = 20, maxpoints = 1)
    } else if (input$comparison == "percent.change") {
      point <- nearPoints(percent.change, hover, threshold = 20, maxpoints = 1)
    }
    
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position inside the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    #TODO(awunderground): change CSS colors of pop-up
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; 
                    background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
  
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(
        HTML(
        # TODO(awunderground): set tooltip sensitivity to zero
        paste0(
          "<b> Year: </b>", point$year, "<br/>",
          "<b> Amount: </b>", 
        if (input$comparison == "mean.income") {
        dollar_format()(point$value)
      } else if (input$comparison == "dollar.change") {
        dollar_format()(point$value)
      } else if (input$comparison == "percent.change") {
        percent_format()(point$value)
      }, "<br/>")
        )
      )
    )
  })  
}

shinyApp(ui = ui, server = server)