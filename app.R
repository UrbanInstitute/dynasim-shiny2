## Libraries and Source Files
library(shiny)
library(tidyverse)
library(extrafont)
library(RColorBrewer)
library(scales)
library(stringr)

options(scipen = 999)

# Source file for Windows
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")
source('urban_institute_themes/urban_theme_windows.R')

# Source file for Mac
#source('urban_institute_themes/urban_theme_mac.R')

# Load Data
income <- read_csv("data/incomes.csv",
  col_types = cols(
    category = col_character(),
    group = col_character(),
    measure = col_character(),
    year = col_integer(),
    option = col_character(),
    scale = col_character(),
    baseline = col_character(),
    comparison = col_character(),
    value = col_double()
  )
)

option_text <- read_csv("text/option.csv",
  col_types = cols(
    option = col_character(),
    text = col_character()
    )                      
  )

scale_text <- read_csv("text/scale.csv",
  col_types = cols(
    scale = col_character(),
    text = col_character()
  )
)

baseline_text <- read_csv("text/baseline.csv",
  col_types = cols(
    baseline = col_character(),
    text = col_character()
  )
)

# Remove numbers in certain factor levels and order all levels
income <- income %>%
  mutate(group = gsub("1\\.", "", group)) %>%
  mutate(group = gsub("2\\.", "", group)) %>%
  mutate(group = gsub("3\\.", "", group)) %>%
  mutate(group = gsub("4\\.", "", group)) %>%
  mutate(group = factor(group, levels = c("All",
                                          "Female",
                                          "Male",
                                          "High School Dropout",
                                          "High School Graduate",
                                          "Some College",
                                          "College Graduate",
                                          "Black",
                                          "Hispanic",
                                          "White",
                                          "Other",
                                          "Single",
                                          "Married",
                                          "Divorced",
                                          "Widowed",
                                          "0-9",
                                          "10-14",
                                          "15-19",
                                          "20-24",
                                          "25-29",
                                          "30-34",
                                          "35-39",
                                          "40+",
                                          "bottom quintile",
                                          "2nd quintile",
                                          "3rd quintile",
                                          "4th quintile",
                                          "top quintile",
                                          "Renter",
                                          "Home Owner",
                                          "Less 100% of Poverty",
                                          "100-199% of Poverty",
                                          "200-399% of Poverty",
                                          "400%+ of Poverty",
                                          "<0k",
                                          "0k -  5k",
                                          "5k - 25k",
                                          "25k +",
                                          "62-69",
                                          "70-74",
                                          "75-79",
                                          "80-84",
                                          "85+ "),
                               labels = c("All",
                                          "Female",
                                          "Male",
                                          "HS Dropout",
                                          "HS Graduate",
                                          "Some College",
                                          "College Graduate",      
                                          "Black",
                                          "Hispanic",
                                          "White, Non-Hispanic",
                                          "Other",
                                          "Never Married",
                                          "Married",
                                          "Divorced",
                                          "Widowed",
                                          "0-9", "10-14", "15-19", "20-24",
                                          "25-29", "30-34", "35-39", "40+",
                                          "Bottom Quintile",
                                          "2nd Quintile",
                                          "3rd Quintile",
                                          "4th Quintile",
                                          "Top Quintile",
                                          "Renter", 
                                          "Home Owner",
                                          "<100% of FPL",
                                          "100-199% of FPL",
                                          "200-399% of FPL",
                                          "400%+ of FPL",
                                          "<0k",
                                          "0k-5k",
                                          "5k-25k",
                                          "25k+",
                                          "62-69",
                                          "70-74",
                                          "75-79",
                                          "80-84",
                                          "85+ ")))

##
## SHINY
##

latoCSS <- "http://fonts.googleapis.com/css?family=Lato:300,400,700,900,300italic,400italic,700italic,900italic"

ui <- fluidPage(

  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = latoCSS)),
  
  theme = "shiny.css",
  
  fluidRow(
    
    column(12,
           
           titlePanel("Exploring Social Security Reform Options"),
           
           p("The Social Security trustees project that, by the mid-2030s, the system will no longer be able to pay all scheduled benefits. Which reform option should policymakers pursue to help balance the system?
             Use our interactive tool to compare how different groups would fare, over time, under the following policy options."),
           
           br()
           
           )
    
  ),
  
  fluidRow(
    column(10,
           style = "position:relative",
           
           h4(textOutput("title")),
           h5(textOutput("subtitlea")),
           h5(textOutput("subtitleb")),
           plotOutput("chart",
                      hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
           uiOutput("hover_info"))),
    
  fluidRow(
    column(6,
           
      selectInput(inputId = "option",
        label = "Social Security Reform",
        choices = c("Payable Law" = "Payable Law",
                    "Scheduled Law" = "Scheduled Law",
                    "BPC Option" = "BPC Package",
                    "Annual PIA" = "Annual PIA", 
                    "Increase Benefits Taxation" = "Increase Benefits Taxation",
                    "Cap Spouse Benefits" = "Cap Spouse Benefits",
                    "75% Survivor Benefit" = "75% Survivor Benefit",
                    "90% Tax Max" = "90% Tax Max",
                    "90% Tax Max and 13.4% Payroll Tax" = "90% Tax Max and 13.4% Payroll Tax",
                    "Reduce COLA" = "Reduce COLA",
                    "Chained-CPI COLA" = "Chained-CPI COLA",
                    "Increase FRA" = "Increase FRA",
                    "Increase FRA and EEA" = "Increase FRA and EEA",
                    "$150,000 Tax Max" = "$150,000 Tax Max",
                    "$180,000 Tax Max" = "$180,000 Tax Max",
                    "Eliminate the Tax Max" = "Eliminate the Tax Max",
                    "13.4% Payroll Tax" = "13.4% Payroll Tax",
                    "14.4% Payroll Tax" = "14.4% Payroll Tax",
                    "15.4% Payroll Tax" = "15.4% Payroll Tax"))),
           
    column(6,
      selectInput(inputId = "comparison",
        label = "Comparison",
        choices = c("Level" = "level",
                    "Percent Change" = "percent.change",
                    "Dollar Change" = "dollar.change")))),
    
  fluidRow(
    column(6,
           selectInput(inputId = "baseline",
                       label = "Baseline",
                       choices = c("Current Law Payable" = "Payable Law",
                                   "Current Law Scheduled" = "Scheduled Law"))),
    
     column(6,
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
                   "Financial Assets" = "Financial Assets ($2015)",
                   "Financial + Retirement Account Assets" = 
                   "Financial + Retirement Account Assets ($2015)")))),
  
  fluidRow(
    column(6,
           selectInput(inputId = "measure",
                       label = "Measure",
                       choices = c("Gross Annuity Income" = "Average Annuity Income",
                                   "Gross Cash Income" = "Average Cash Income",
                                   "Net Annuity Income" = "Average Net Annuity Income",
                                   "Net Cash Income" = "Average Net Cash Income"))),
    
    column(6,
      selectInput(inputId = "scale",
        label = "Scale",
        choices = c("Per Capita" = "per capita",
                    "Equivalent" = "equivalent")))
    ),

  fluidRow(
    column(12,
      downloadButton('download_data', 'Download Charted Data')
    )
  ),
    
  fluidRow(
    
    column(12,
           
           # Explanation of Social Security Reform
           
           htmlOutput("text1"))
    
  ),
  
  fluidRow(
    
    column(12,
           
           # Explanation of Scales
           
           htmlOutput("text2"))
    
  ),
  
  fluidRow(
    
    column(12,
           
           # Explanation of Baseline
           
           htmlOutput("text3"))
    
  )  
  
)

server <- function(input, output){
  
  output$title <- renderText({

    # Create title substring for comparison 
    comparison <- if (input$comparison == "level") {"Mean"}
    else if (input$comparison == "percent.change") {"Percent Change in Mean "}
    else if (input$comparison == "dollar.change") {"Change in Mean "}
        
    # Create title string
    paste(comparison, str_to_title(input$scale), sub("Average ", "", input$measure))
  
  })

  output$subtitlea <- renderText({
  
    if (input$comparison == "level") {
      input$option
    } else {
      paste(input$option, "vs.", input$baseline)
    }
      
  })
  
  output$subtitleb <- renderText({
    
    if (input$demographic == "All") {
      "Everyone Ages 62+, 2015 dollars"} 
    else if (input$demographic == "Sex") {
      "Ages 62+ by Sex, 2015 dollars"}
    else if (input$demographic == "Education") {
      "Ages 62 and Older by Education, 2015 dollars"}
    else if (input$demographic == "Race Ethnicity") {
      "Ages 62 and Older by Race & Ethnicity, 2015 dollars"}
    else if (input$demographic == "Marital Status") {
      "Ages 62 and Older by Marital Status, 2015 dollars"}
    else if (input$demographic == "Shared Work Years") {
      "Ages 62 and Older by Shared Work Years, 2015 dollars"}
    else if (input$demographic == "Own Work Years") {
      "Ages 62 and Older by Own Work Years, 2015 dollars"}
    else if (input$demographic == "Shared Income Quintile") {
      "Ages 62 and Older by Shared Income Quintile, 2015 dollars"}
    else if (input$demographic == "Shared Lifetime Earnings Quintile") {
      "Ages 62 and Older by Shared Lifetime Earnings Quintile, 2015 dollars"}   
    else if (input$demographic == "Homeownership") {
      "Ages 62 and Older by Homeownership"}
    else if (input$demographic == "Family Income Relative to Official Poverty") {
      "Ages 62 and Older by Family Income Relative to Official Poverty, 2015 dollars"}    
    else if (input$demographic == "Financial Assets ($2015)") {
      "Ages 62 and Older by Financial Assets, 2015 dollars"}
    else if (input$demographic == "Financial + Retirement Account Assets ($2015)") {
      "Ages 62 and Older by Financial + Retirement Account Assets, 2015 dollars"} 
  
    })
 
  data_subset <- reactive({
    data_subset <- income %>%
      filter(category == input$demographic) %>%
      filter(measure == input$measure) %>%
      filter(option == input$option) %>%
      filter(scale == input$scale) %>%
      filter(baseline == input$baseline) %>%
      filter(comparison == input$comparison)
  })
  
  output$chart <- renderPlot({

    graphr <- function(scale, origin, line.placement, line.color){
  
      ggplot(data_subset(), aes(year, value, color = group)) +
        geom_line() +
        geom_point(size = 2) +
        labs(caption = "DYNASIM3",
             x = "Year",
             y = NULL) +
        geom_line(size = 1) +
        scale_x_continuous(breaks = c(2015, 2025, 2035, 2045, 2055, 2065)) +
        scale_y_continuous(expand = c(0.3, 0), labels = scale) +
        expand_limits(y = origin) +
        geom_hline(size = 0.5, aes(yintercept = line.placement), color = line.color) +
        theme(axis.line = element_blank(),
              plot.margin = margin(t = -5))

    }
    
    if (input$comparison == "level") {
       graphr(scale = scales::dollar, origin = NULL, line.placement = 50000, line.color = NA) 
       } 
    else if (input$comparison == "dollar.change") {
       graphr(scale = scales::dollar, origin = 0, line.placement = 0, line.color = "black")
       } 
    else if (input$comparison == "percent.change") {
       graphr(scale = scales::percent, origin = 0, line.placement = 0, line.color = "black")
       }

  })
  
  # Social Security Reform Descriptions
  output$text1 <- renderText({
    
    as.character(
      option_text %>%
      filter(option == input$option) %>%
      select(text)
    )
    
  })
  
  output$text2 <- renderText({
    
    as.character(
      scale_text %>%
        filter(scale == input$scale) %>%
        select(text)
    )
    
  })
  
  output$text3 <- renderText({
    
    as.character(
      baseline_text %>%
        filter(baseline == input$baseline) %>%
        select(text)
    )
    
  })
  
  # Tooltip
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    
    point <- nearPoints(income, hover, threshold = 20, maxpoints = 1)
    
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
        paste0(
          "<b> Year: </b>", point$year, "<br/>",
          "<b> Amount: </b>", 
        if (input$comparison == "level") {
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
  
  output$download_data <- downloadHandler(
    filename = function() { paste0(input$option, '.csv') },
    content = function(file) {
      write_csv(data_subset(), file)
    }
  )

}

shinyApp(ui = ui, server = server)