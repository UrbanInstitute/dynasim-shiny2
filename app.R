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

income <- read_csv("data/income.csv")

# Remove numbers in certain factor levels and order all levels
income <- income %>%
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
        label = "Social Security Reform",
        choices = c("BPC Option" = "bpc.option",
                    "Annual PIA" = "mini.pia", 
                    "Increase Benefits Taxation" = "tax.ssb",
                    "Cap Spouse Benefits" = "cap.spouse",
                    "75% Survivor Benefit" = "survivor.js75",
                    "90% Tax max" = "taxmax90",
                    "90% Tax Max and 13.4% Payroll Tax" = "taxmax90.fica13.4",
                    "Full Chained-CPI COLA" = "cola.chaincpi",
                    "Partial Chained-CPI COLA" = "reduce.cola",
                    "Increase FRA" = "increase.fra",
                    "Increase EEA & FRA" = "increase.fra.era",
                    "$150,000 Tax Max" = "taxmax150000",
                    "$180,000 Tax Max" = "taxmax180000",
                    "Eliminate the Tax Max" = "notaxmax",
                    "13.4% Payroll Tax" = "fica13.4",
                    "14.4% Payroll Tax" = "fica14",
                    "15.4% Payroll Tax" = "fica15"))),
           
    column(4,
      selectInput(inputId = "comparison",
        label = "Comparison",
        choices = c("Level" = "mean.income",
                    "Percent Change from Payable Law" = "payable.percent.change",
                    "Percent Change from Scheduled Law" = "scheduled.percent.change",
                    "Dollar Change from Payable Law" = "payable.dollar.change",
                    "Dollar Change from Scheduled Law" = "scheduled.dollar.change")))),
    
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
                   "Per Capita Financial + Retirement Account Assets ($2015)")))),
  
  fluidRow(
    
    column(8,
           
           # Explanation of Social Security Reform
           
           htmlOutput("text1"))
    
  )
  
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

    
    graphr <- function(scale, origin, line.placement, line.color){
  
      income %>%
        filter(chart == input$comparison) %>%
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
        scale_y_continuous(expand = c(0.3, 0), labels = scale) +
        expand_limits(y = origin) +
        geom_hline(size = 0.5, aes(yintercept = line.placement), color = line.color) +
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
    
    
    if (input$comparison == "mean.income") {
       graphr(scale = scales::dollar, origin = NULL, line.placement = 50000, line.color = NA) 
       } 
     else if (input$comparison == "scheduled.dollar.change") {
       graphr(scale = scales::dollar, origin = 0, line.placement = 0, line.color = "black")
       } 
     else if (input$comparison == "scheduled.percent.change") {
       graphr(scale = scales::percent, origin = 0, line.placement = 0, line.color = "black")
       }

  })
  
  output$text1 <- renderText({
    
    if (input$option == "bpc.option") {"<p><h4>BPC Package</h4></p><p>Annual PIA, limit spousal benefits, replace the WEP and GPO with a proportional reduction in OASI benefits based on covered earnings, enhance survivor benefits, increase the progressivity of the benefit formula, increase Social Security tax max to $195,000, payroll tax to 13.4% and FRA to 69, switch to C-CPI-U for COLAs, end 'claim-and-suspend' games, create a basic minimum benefit for all individuals above the FRA eligible for Social Security, and tax 100 percent of Social Security benefits for beneficiaries with annual incomes above $250,000.</p>"}
    
    else if (input$option == "mini.pia") {"<p><h4>Annual PIA</h4></p><p>Eliminates the preferential treatment of workers with short careers by applying Social Security’s progressive benefit formula to the 40 highest years of wage-indexed earnings divided by 37 rather than applying the formula to total wage-indexed earnings received in the top 35 years. It also makes the benefit formula more progressive. This begins with OASI claimants who attain age 62 in 2022.</p>"}
    
    else if (input$option == "tax.ssb") {"<p><h4>Increase Benefits Taxation</h4></p><p>Increases the taxation of Social Security benefits.</p>"}
    
    else if (input$option == "cap.spouse") {"<p><h4>Cap Spouse Benefits</h4></p><p>Caps the spouse benefit at $1,121.68 in 2016 beginning for claimants who turn 60 in 2020 and beyond. Indexes the cap annually by chained CPI-U.</p>"}
    
    else if (input$option == "survivor.js75") {"<p><h4>75% Survivor Benefit</h4></p><p>Increases joint-and-survivors benefits to 75 percent of combined benefits for the couple, from 50 percent of combined benefits, for claimants who turn 62 in 2022 and beyond.</p>"}
    
    else if (input$option == "taxmax90") {"<p><h4>90% Tax Max</h4></p><p>Raises the cap on annual earnings subject to the Social Security payroll tax and that enter the benefits calculation to cover 90 percent of payroll. This increase is phased in over 10 years, beginning in 2016.</p>"}
    
    else if (input$option == "taxmax90.fica13.4") {"<p><h4>90% Tax Max and 13.4% Payroll Tax</h4></p><p>Raises the cap on annual earnings subject to the Social Security payroll tax and that enter the benefits calculation to cover 90 percent of payroll. This increase is phased in over 10 years, beginning in 2016. Also, increase the payroll tax to 13.4% over t10 years beginning in 2016.</p>"}
    
    else if (input$option == "cola.chaincpi") {"<p><h4>Full Chained-CPI COLA</h4></p><p>Ties beneficiaries' annual cost-of-living-adjustment (COLA) to the change in the chained consumer price index (C-CPI-U), which grows more slowly than the standard CPI-U now used to compute COLAs. (Only those NRA or older)</p>"}
    
    else if (input$option == "reduce.cola") {"<p><h4>Partial Chained-CPI COLA</h4></p><p>Ties beneficiaries' annual cost-of-living-adjustment (COLA) to the change in the chained consumer price index (C-CPI-U), which grows more slowly than the standard CPI-U now used to compute COLAs. (All beneficiaries including those under the NRA)</p>"}
    
    else if (input$option == "increase.fra") {"<p><h4>Increase FRA</h4></p><p>Indefinitely raises Social Security's FRA (now set at 67 beginning in 2022) and the age for receiving delayed retirement credits by one month every two years, beginning in 2024.</p>"}
    
    else if (input$option == "increase.fra.era") {"<p><h4>Increase EEA & FRA</h4></p><p>Raises Social Security's early eligibility age (EEA), which is now set at 62, and indefinitely raises Social Security's FRA (now set at 67 beginning in 2022) and the age for receiving delayed retirement credits by one month every two years, beginning in 2024.</p>"}
    
    else if (input$option == "taxmax150000") {"<p><h4>$150,000 Tax Max</h4></p><p>Increase the tax cap to $150,000 between 2016 and 2018 and then increase the tax cap by wage growth plus 0.5 percentage points thereafter.</p>"}
    
    else if (input$option == "taxmax180000") {"<p><h4>$180,000 Tax Max</h4></p><p>Increase the tax cap to $180,000 between 2016 and 2018 and then increase the tax cap by wage growth plus 0.5 percentage points thereafter.</p>"}
    
    else if (input$option == "notaxmax") {"<p><h4>Eliminate the Tax Max</h4></p><p>Eliminates the cap on annual earnings subject to the Social Security payroll tax and that enter the benefits calculation.</p>"}
    
    else if (input$option == "fica13.4") {"<p><h4>13.4% Payroll Tax</h4></p><p>Increase the payroll tax rate to 13.4% over 10 years beginning in 2016.</p>"}
    
    else if (input$option == "fica14") {"<p><h4>14.4% Payroll Tax</h4></p><p>Increase the payroll tax rate to 14.4% over 10 years beginning in 2016.</p>"}
    
    else if (input$option == "fica15") {"<p><h4>15.4% Payroll Tax</h4></p><p>Increase the payroll tax rate to 15.4% over 10 years beginning in 2016.</p>"}
    
    else {"<p><h4></h4></p><p><strong>Current Law Scheduled</strong> assumes that current public policies, business practices, and individual behaviors continue, and that Social Security benefits are paid as promised, even after the trust fund runs out.</p>"}
    
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