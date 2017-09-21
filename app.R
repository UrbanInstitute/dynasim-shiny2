# Libraries and Source Files
library(shiny)
library(tidyverse)
library(scales)

# Set options
options(scipen = 999)
options(shiny.sanitize.errors = TRUE)

# Source file for Windows
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")
source('urban_institute_themes/urban_theme_windows.R')

# Source file for Mac
#source('urban_institute_themes/urban_theme_mac.R')

factor_levels <- c("All", "Female", "Male", "High School Dropout", 
                   "High School Graduate", "Some College", "College Graduate",
                   "Black", "Hispanic", "White", "Other", "Single", "Married", 
                   "Divorced", "Widowed", "0-9", "10-14", "15-19", "20-24", 
                   "25-29", "30-34", "35-39", "40+", "bottom quintile", 
                   "2nd quintile", "3rd quintile", "4th quintile", 
                   "top quintile", "Renter", "Home Owner", 
                   "Less 100% of Poverty", "100-199% of Poverty", 
                   "200-399% of Poverty", "400%+ of Poverty", "<0k", "0k -  5k", 
                   "5k - 25k", "25k +", "62-69", "70-74", "75-79", "80-84", 
                   "85+ ")

factor_labels <- c("All", "Female", "Male", "HS dropout", "HS graduate", 
                   "Some college", "College graduate", "Black", "Hispanic",
                   "White, non-Hispanic", "Other", "Never married", "Married", 
                   "Divorced", "Widowed", "0-9",  "10-14",  "15-19",  "20-24", 
                   "25-29", "30-34", "35-39", "40+", "Bottom quintile", 
                   "2nd quintile", "3rd quintile", "4th quintile", "Top quintile",
                   "Renter", "Homeowner", "<100% of FPL", "100-199% of FPL", 
                   "200-399% of FPL", "400%+ of FPL", "<0k", "0k-5k", "5k-25k", 
                   "25k+", "62-69", "70-74", "75-79", "80-84", "85+")

# Load Data
level <- read_csv("data/level.csv",
  col_types = cols(
    category = col_character(),
    group = col_character(),
    year = col_integer(),
    option = col_character(),
    scale = col_character(),
    baseline = col_character(),
    `Average annuity income` = col_double(),
    `Average cash income` = col_double(),
    `Average net annuity income` = col_double(),
    `Average net cash income` = col_double()
  )
) %>% mutate(group = factor(group, levels = factor_levels, labels = factor_labels))

dollar_change <- read_csv("data/dollar-change.csv",
  col_types = cols(
    category = col_character(),
    group = col_character(),
    year = col_integer(),
    option = col_character(),
    scale = col_character(),
    baseline = col_character(),
    `Average annuity income` = col_double(),
    `Average cash income` = col_double(),
    `Average net annuity income` = col_double(),
    `Average net cash income` = col_double()
  )
) %>% mutate(group = factor(group, levels = factor_levels, labels = factor_labels))

percent_change <- read_csv("data/percent-change.csv",
  col_types = cols(
    category = col_character(),
    group = col_character(),
    year = col_integer(),
    option = col_character(),
    scale = col_character(),
    baseline = col_character(),
    `Average annuity income` = col_double(),
    `Average cash income` = col_double(),
    `Average net annuity income` = col_double(),
    `Average net cash income` = col_double()
  )                           
) %>% mutate(group = factor(group, levels = factor_levels, labels = factor_labels))

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

income_text <- read_csv("text/income.csv",
  col_types = cols(
    income = col_character(),
    text = col_character()
  )
)

demographic_text <- read_csv("text/demographic.csv",
  col_types = cols(
    demographic = col_character(),
    text = col_character()
  )
)

##
## SHINY
##

latoCSS <- "http://fonts.googleapis.com/css?family=Lato:300,400,700,900,300italic,400italic,700italic,900italic"

ui <- fluidPage(

  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = latoCSS)),
  tags$head(tags$script(src = "pym.min.js")),
  
  theme = "shiny.css",
  
  fluidRow(
    
    column(12,
           
           p("Millions of retirees rely on Old-Age Social Security benefits for 
             retirement and many more are counting on these benefits for the 
             future. Use this interactive to compare how Social Security reforms 
             would affect current and future retirees over time based on sex, 
             education, race or ethnicity, marital status, income, and work 
             history.")
           
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
        choices = c("Payable law" = "Payable law",
                    "Scheduled law" = "Scheduled law",
                    "BPC option" = "BPC package",
                    "Annual primary insurance amount" = "Annual primary insurance amount", 
                    "Basic minimum benefit" = "Basic minimum benefit",                    
                    "Increase benefits taxation" = "Increase benefits taxation",
                    "Cap spouse benefits" = "Cap spouse benefits",
                    "75 percent survivor benefit" = "75 percent survivor benefit",
                    "90 percent tax max" = "90 percent tax max",
                    "90% tax max and 13.4% payroll tax" = "90% tax max and 13.4% payroll tax",
                    "Reduce COLA" = "Reduce COLA",
                    "Chained-CPI COLA" = "Chained-CPI COLA",
                    "Cap COLA" = "Cap COLA", 
                    "Increase COLA" = "Increase COLA",
                    "Increase FRA" = "Increase FRA",
                    "Increase FRA and EEA" = "Increase FRA and EEA",
                    "$150,000 tax max" = "$150,000 tax max",
                    "$180,000 tax max" = "$180,000 tax max",
                    "Eliminate the tax max" = "Eliminate the tax max",
                    "13.4 percent payroll tax" = "13.4 percent payroll tax",
                    "14.4 percent payroll tax" = "14.4 percent payroll tax",
                    "15.4 percent payroll tax" = "15.4 percent payroll tax"))),
           
    column(6,
      selectInput(inputId = "comparison",
        label = "Comparison",
        choices = c("Level" = "level",
                    "Percent change" = "percent.change",
                    "Dollar change" = "dollar.change")))),
    
  fluidRow(
    column(6,
           selectInput(inputId = "baseline",
                       label = "Baseline",
                       choices = c("Current law payable" = "Payable law",
                                   "Current law scheduled" = "Scheduled law"))),
    
     column(6,
     selectInput(inputId = "demographic",
       label = "Demographic",
       choices = c("All" = "All",
                   "Sex" = "Sex",
                   "Education" = "Education",
                   "Race or ethnicity" = "Race Ethnicity",
                   "Marital status" = "Marital Status",
                   "Shared work years" = "Shared Work Years",
                   "Own work years" = "Own Work Years",
                   "Shared income quintile" = "Shared Income Quintile",
                   "Shared lifetime earnings quintile" = "Shared Lifetime Earnings Quintile",
                   "Homeownership" = "Homeownership",
                   "Family income relative to official poverty" = "Family Income Relative to Official Poverty",
                   "Financial assets" = "Financial Assets ($2015)",
                   "Financial + retirement account assets" = 
                   "Financial + Retirement Account Assets ($2015)")))),
  
  fluidRow(
    column(6,
           selectInput(inputId = "measure",
                       label = "Measure",
                       choices = c("Gross annuity income" = "`Average annuity income`",
                                   "Gross cash income" = "`Average cash income`",
                                   "Net annuity income" = "`Average net annuity income`",
                                   "Net cash income" = "`Average net cash income`"))),
    
    column(6,
      selectInput(inputId = "scale",
        label = "Scale",
        choices = c("Per capita" = "per capita",
                    "Equivalent" = "equivalent")))
    ),

  fluidRow(
    column(12,
      downloadButton('download_data', 'Download charted data')
    )
  ),
    
  br(),
  
  fluidRow(
    
    column(12,
           
           # Explanation of Social Security Reform
           
           htmlOutput("text_option"))
    
  ),

  fluidRow(
    
    column(12,
           
           # Explanation of Baseline
           
           htmlOutput("text_baseline"))
    
  ),
  
  fluidRow(
    
    column(12,
           
           # Explanation of income
           
           htmlOutput("text_income"))
  ),

  fluidRow(
    
    column(12,
           
           # Explanation of demographic
           
           htmlOutput("text_demographic"))
  ),
      
  fluidRow(
    
    column(12,
           
           # Explanation of Scales
           
           htmlOutput("text_scale"))
    
  ),
  tags$script(src = "activatePym.js")
  
)

server <- function(input, output){
  
  output$title <- renderText({

    # Create title substring for comparison 
    comparison <- if (input$comparison == "level") {"Mean"}
    else if (input$comparison == "percent.change") {"Percent change in mean "}
    else if (input$comparison == "dollar.change") {"Change in mean "}
        
    # Create title string
    paste(comparison, input$scale, sub("Average ", "", gsub("`", "", input$measure)))
  
  })

  output$subtitlea <- renderText({
  
    if (input$comparison == "level") {
      input$option
    } else {
      paste(input$option, "vs.", tolower(input$baseline))
    }
      
  })
  
  output$subtitleb <- renderText({
    
    if (input$demographic == "All") {
      "Everyone ages 62+, 2015 dollars"} 
    else if (input$demographic == "Sex") {
      "Ages 62+ by sex, 2015 dollars"}
    else if (input$demographic == "Education") {
      "Ages 62+ by education, 2015 dollars"}
    else if (input$demographic == "Race Ethnicity") {
      "Ages 62+ by race or ethnicity, 2015 dollars"}
    else if (input$demographic == "Marital Status") {
      "Ages 62+ by marital status, 2015 dollars"}
    else if (input$demographic == "Shared Work Years") {
      "Ages 62+ by shared work years, 2015 dollars"}
    else if (input$demographic == "Own Work Years") {
      "Ages 62+ by own work years, 2015 dollars"}
    else if (input$demographic == "Shared Income Quintile") {
      "Ages 62+ by shared income quintile, 2015 dollars"}
    else if (input$demographic == "Shared Lifetime Earnings Quintile") {
      "Ages 62+ by shared lifetime earnings quintile, 2015 dollars"}   
    else if (input$demographic == "Homeownership") {
      "Ages 62+ by Homeownership"}
    else if (input$demographic == "Family Income Relative to Official Poverty") {
      "Ages 62+ by family income relative to official poverty, 2015 dollars"}    
    else if (input$demographic == "Financial Assets ($2015)") {
      "Ages 62+ by financial assets, 2015 dollars"}
    else if (input$demographic == "Financial + Retirement Account Assets ($2015)") {
      "Ages 62+ by financial + retirement account assets, 2015 dollars"} 
  
    })
 
  data_subset <- reactive({
    
    if (input$comparison == "level") {
      graph_data <- level
    } else if (input$comparison == "dollar.change") {
      graph_data <- dollar_change     
    } else if (input$comparison == "percent.change") {
      graph_data <- percent_change  
    }
    
    graph_data <- graph_data %>% #income %>%
      filter(option == input$option) %>%
      filter(category == input$demographic) %>%
      filter(scale == input$scale) %>%
      filter(baseline == input$baseline) %>%
      select_("category", "group", "year", "option", "scale", "baseline", value = input$measure)
    
  })
  
  output$chart <- renderPlot({

    graphr <- function(scale, origin, line.placement, line.color){
  
      ggplot(data_subset(), aes(year, value, color = group)) +
        geom_line() +
        geom_point(size = 2) +
        labs(caption = "DYNASIM3
                        Urban Institute",
             x = NULL,
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
  output$text_option <- renderText({
    
    as.character(
      option_text %>%
      filter(option == input$option) %>%
      select(text)
    )
    
  })

  output$text_baseline <- renderText({
    
    as.character(
      baseline_text %>%
        filter(baseline == input$baseline) %>%
        select(text)
    )
    
  })
    
  output$text_scale <- renderText({
    
    as.character(
      scale_text %>%
        filter(scale == input$scale) %>%
        select(text)
    )
    
  })
  
  output$text_demographic <- renderText({
    
    as.character(
      demographic_text %>%
        filter(demographic == input$demographic) %>%
        select(text)
    )
    
  })
  
  output$text_income <- renderText({
    
    as.character(
      income_text %>%
        filter(income == gsub("`", "", input$measure)) %>%
        select(text)
    )
    
  })
  
  # Tooltip
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    
    point <- nearPoints(data_subset(), hover, threshold = 100, maxpoints = 1) #todo replace level
    
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