## Libraries and Source Files
library(shiny)
library(tidyverse)
library(extrafont)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(scales)

options(scipen = 999)

# Source file for Windows
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")
source('urban_institute_themes/urban_theme_windows.R')

# Source file for Mac
#source('urban_institute_themes/urban_theme_mac.R')

# Load Data
income <- read_csv("data/new_income.csv")

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
        choices = c("Scheduled Law" = "Scheduled Law",
                    "Payable Law" = "Payable Law",
                    "BPC Option" = "BPC Package",
                    "Annual PIA" = "Annual PIA", 
                    "Increase Benefits Taxation" = "Increase Benefits Taxation",
                    "Cap Spouse Benefits" = "Cap Spouse Benefits",
                    "75% Survivor Benefit" = "75% Survivor Benefit",
                    "90% Tax max" = " 90% Tax Max",
                    "90% Tax Max and 13.4% Payroll Tax" = "90% Tax Max and 13.4% Payroll Tax",
                    "Full Chained-CPI COLA" = "Full Chained-CPI COLA",
                    "Partial Chained-CPI COLA" = "Partial Chained-CPI COLA",
                    "Increase FRA" = "Increase FRA",
                    "Increase FRA and EEA" = "Increase FRA and EEA",
                    "$150,000 Tax Max" = "$150,000 Tax Max",
                    "$180,000 Tax Max" = "$180,000 Tax Max",
                    "Eliminate the Tax Max" = "Eliminate the Tax Max",
                    "13.4% Payroll Tax" = "13.4% Payroll Tax",
                    "14.4% Payroll Tax" = "14.4% Payroll Tax",
                    "15.4% Payroll Tax" = "15.5% Payroll Tax"))),
           
    column(4,
      selectInput(inputId = "comparison",
        label = "Comparison",
        choices = c("Level" = "level",
                    "Percent Change" = "percent.change",
                    "Dollar Change" = "dollar.change")))),
    
  fluidRow(
    column(4,
      
     selectInput(inputId = "measure",
       label = "Measure",
       choices = c("Gross Annuity Income" = "Average Annuity Income",
                   "Gross Cash Income" = "Average Cash Income",
                   "Net Annuity Income" = "Average Net Annuity Income",
                   "Net Cash Income" = "Average Net Cash Income"))),
    
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
                   "Financial Assets" = "Financial Assets ($2015)",
                   "Financial + Retirement Account Assets" = 
                   "Financial + Retirement Account Assets ($2015)")))),
  
  fluidRow(
    column(4,
      selectInput(inputId = "baseline",
        label = "Baseline",
        choices = c("Current Law Scheduled" = "Scheduled Law",
                    "Current Law Payable" = "Payable Law"))),
    
    column(4,
      selectInput(inputId = "scale",
        label = "Scale",
        choices = c("Per Capita" = "per capita",
                    "Equivalent" = "equivalent")))
    ),
  
  fluidRow(
    
    column(8,
           
           # Explanation of Social Security Reform
           
           htmlOutput("text1"))
    
  ),
  
  fluidRow(
    
    column(8,
           
           # Explanation of Baselines/Comparisons
           
           htmlOutput("text2"))
    
  )
  
)

server <- function(input, output){
  
  output$chart <- renderPlot({

    title <- if (input$measure == "Average Annuity Income") {
                 "Mean Gross Annuity Income"} 
             else if (input$measure == "Average Cash Income") {
               "Mean Gross Cash Income"}
             else if (input$measure == "Average Net Annuity Income") {
               "Mean Net Annuity Income"}
             else if (input$measure == "Average Net Cash Income") {
               "Mean Net Cash Income"}
    
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
                else if (input$demographic == "Financial Assets ($2015)") {
                  "Ages 62 and older by financial assets, 2015 dollars"}
                else if (input$demographic == "Financial + Retirement Account Assets ($2015)") {
                  "Ages 62 and older by financial + retirement account assets, 2015 dollars"}    

    
    graphr <- function(scale, origin, line.placement, line.color){
  
      income %>%
        filter(category == input$demographic) %>%
        filter(measure == input$measure) %>%
        filter(option == input$option) %>%
        filter(scale == input$scale) %>%
        filter(baseline == input$baseline) %>%
        filter(comparison == input$comparison) %>%
        ggplot(aes(year, value, color = group)) +
        geom_line() +
        labs(caption = "DYNASIM3") +
        xlab("Year") +
        ylab(NULL) +
        geom_line(size = 1) +
        scale_x_continuous(breaks = c(2015, 2025, 2035, 2045, 2055, 2065)) +
        scale_y_continuous(expand = c(0.3, 0), labels = scale) +
        expand_limits(y = origin) +
        geom_hline(size = 0.5, aes(yintercept = line.placement), color = line.color) +
        theme(axis.line = element_blank())

    }
    
    if (input$comparison == "level") {
       graph <- graphr(scale = scales::dollar, origin = NULL, line.placement = 50000, line.color = NA) 
       } 
     else if (input$comparison == "dollar.change") {
       graph <- graphr(scale = scales::dollar, origin = 0, line.placement = 0, line.color = "black")
       } 
     else if (input$comparison == "percent.change") {
       graph <- graphr(scale = scales::percent, origin = 0, line.placement = 0, line.color = "black")
       }
    
    ###
    title.grob <- textGrob(
      label = title,
      x = unit(0.2, "lines"), 
      y = unit(0, "lines"),
      hjust = 0, vjust = 0,
      gp = gpar(fontsize = 18, fontfamily = "Lato"))
    
    subtitle.grob <- textGrob(
      label = subtitle,
      x = unit(0.33, "lines"), 
      y = unit(0.2, "lines"),
      hjust = 0, vjust = 0,
      gp = gpar(fontsize = 14, fontfamily = "Lato"))
    
    margin <- unit(0.5, "line")
    grid.newpage()
    grid.arrange(title.grob, subtitle.grob, graph,
                 heights = unit.c(grobHeight(title.grob) + 1.2 * margin,
                                  grobHeight(subtitle.grob) + margin,
                                  unit(2, "null")))
    ###

  })
  
  # Social Security Reform Descriptions
  output$text1 <- renderText({
    
    if (input$option == "BPC Package") {"<p><h4>BPC Package</h4></p><p>Annual PIA, limit spousal benefits, replace the WEP and GPO with a proportional reduction in OASI benefits based on covered earnings, enhance survivor benefits, increase the progressivity of the benefit formula, increase Social Security tax max to $195,000, payroll tax to 13.4% and FRA to 69, switch to C-CPI-U for COLAs, end 'claim-and-suspend' games, create a basic minimum benefit for all individuals above the FRA eligible for Social Security, and tax 100 percent of Social Security benefits for beneficiaries with annual incomes above $250,000.</p>"}
    
    else if (input$option == "Annual PIA") {"<p><h4>Annual PIA</h4></p><p>Eliminates the preferential treatment of workers with short careers by applying Social Security’s progressive benefit formula to the 40 highest years of wage-indexed earnings divided by 37 rather than applying the formula to total wage-indexed earnings received in the top 35 years. It also makes the benefit formula more progressive. This begins with OASI claimants who attain age 62 in 2022.</p>"}
    
    else if (input$option == "Increase Benefits Taxation") {"<p><h4>Increase Benefits Taxation</h4></p><p>Increases the taxation of Social Security benefits.</p>"}
    
    else if (input$option == "Cap Spouse Benefits") {"<p><h4>Cap Spouse Benefits</h4></p><p>Caps the spouse benefit at $1,121.68 in 2016 beginning for claimants who turn 60 in 2020 and beyond. Indexes the cap annually by chained CPI-U.</p>"}
    
    else if (input$option == "75% Survivor Benefit") {"<p><h4>75% Survivor Benefit</h4></p><p>Increases joint-and-survivors benefits to 75 percent of combined benefits for the couple, from 50 percent of combined benefits, for claimants who turn 62 in 2022 and beyond.</p>"}
    
    else if (input$option == "90% Tax Max") {"<p><h4>90% Tax Max</h4></p><p>Raises the cap on annual earnings subject to the Social Security payroll tax and that enter the benefits calculation to cover 90 percent of payroll. This increase is phased in over 10 years, beginning in 2016.</p>"}
    
    else if (input$option == "90% Tax Max and 13.4% Payroll Tax") {"<p><h4>90% Tax Max and 13.4% Payroll Tax</h4></p><p>Raises the cap on annual earnings subject to the Social Security payroll tax and that enter the benefits calculation to cover 90 percent of payroll. This increase is phased in over 10 years, beginning in 2016. Also, increase the payroll tax to 13.4% over t10 years beginning in 2016.</p>"}
    
    else if (input$option == "Full Chained-CPI COLA") {"<p><h4>Full Chained-CPI COLA</h4></p><p>Ties beneficiaries' annual cost-of-living-adjustment (COLA) to the change in the chained consumer price index (C-CPI-U), which grows more slowly than the standard CPI-U now used to compute COLAs. (Only those NRA or older)</p>"}
    
    else if (input$option == "Partial Chained-CPI COLA") {"<p><h4>Partial Chained-CPI COLA</h4></p><p>Ties beneficiaries' annual cost-of-living-adjustment (COLA) to the change in the chained consumer price index (C-CPI-U), which grows more slowly than the standard CPI-U now used to compute COLAs. (All beneficiaries including those under the NRA)</p>"}
    
    else if (input$option == "Increase FRA") {"<p><h4>Increase FRA</h4></p><p>Indefinitely raises Social Security's FRA (now set at 67 beginning in 2022) and the age for receiving delayed retirement credits by one month every two years, beginning in 2024.</p>"}
    
    else if (input$option == "Increase FRA and EEA") {"<p><h4>Increase EEA & FRA</h4></p><p>Raises Social Security's early eligibility age (EEA), which is now set at 62, and indefinitely raises Social Security's FRA (now set at 67 beginning in 2022) and the age for receiving delayed retirement credits by one month every two years, beginning in 2024.</p>"}
    
    else if (input$option == "$150,000 Tax Max") {"<p><h4>$150,000 Tax Max</h4></p><p>Increase the tax cap to $150,000 between 2016 and 2018 and then increase the tax cap by wage growth plus 0.5 percentage points thereafter.</p>"}
    
    else if (input$option == "$180,000 Tax Max") {"<p><h4>$180,000 Tax Max</h4></p><p>Increase the tax cap to $180,000 between 2016 and 2018 and then increase the tax cap by wage growth plus 0.5 percentage points thereafter.</p>"}
    
    else if (input$option == "Eliminate the Tax Max") {"<p><h4>Eliminate the Tax Max</h4></p><p>Eliminates the cap on annual earnings subject to the Social Security payroll tax and that enter the benefits calculation.</p>"}
    
    else if (input$option == "13.4%  Payroll Tax") {"<p><h4>13.4% Payroll Tax</h4></p><p>Increase the payroll tax rate to 13.4% over 10 years beginning in 2016.</p>"}
    
    else if (input$option == "14.4%  Payroll Tax") {"<p><h4>14.4% Payroll Tax</h4></p><p>Increase the payroll tax rate to 14.4% over 10 years beginning in 2016.</p>"}
    
    else if (input$option == "15.4%  Payroll Tax") {"<p><h4>15.4% Payroll Tax</h4></p><p>Increase the payroll tax rate to 15.4% over 10 years beginning in 2016.</p>"}
    
    else if (input$option == "Scheduled Law") {"<p><h4>Current Law Scheduled</h4></p><p><strong>Current Law Scheduled</strong> assumes that current public policies, business practices, and individual behaviors continue, and that Social Security benefits are paid as promised, even after the trust fund runs out.</p>"}
    
    else if (input$option == "Payable Law") {""}
    
  })
  
  
  # Comparison description
  output$text2 <- renderText({
    
    if (input$comparison == "level") {"<p><h4>Mean Income</h4></p><p>Projected annual income under the specified social security reform option.</p>"}
    
    else if (input$comparison == "percent.change") {"<p><h4>Percent Change from Current Law Payable</h4></p><p>The percentage difference in any year between a scenario where the reform is implemented and current law payable payable, which assumes that current public policies, business practices, and individual behaviors continue, but reduces Social Security benefits by a uniform amount after the trust fund runs out so that all benefits in each year can be paid out of revenues from that year.</p>"}
    
    else if (input$comparison == "scheduled.percent.change") {"<p><h4>Percent Change from Current Law Scheduled</h4></p><p> The percentage difference in any year between a scenario where the reform is implemented and current law scheduled, which assumes that current public policies, business practices, and individual behaviors continue, and that Social Security benefits are paid as promised, even after the trust fund runs out.</p>"}
    
    else if (input$comparison == "dollar.change") {"<p><h4>Dollar Change from Current Law Payable</h4></p><p> The annual dollar difference in any year between a scenario where the reform is implemented and current law payable payable, which assumes that current public policies, business practices, and individual behaviors continue, but reduces Social Security benefits by a uniform amount after the trust fund runs out so that all benefits in each year can be paid out of revenues from that year.</p>"}
    
    else if (input$comparison == "scheduled.dollar.change") {"<p><h4>Dollar Change from Current Law Scheduled</h4></p><p> The annual dollar difference in any year between a scenario where the reform is implemented and current law scheduled, which assumes that current public policies, business practices, and individual behaviors continue, and that Social Security benefits are paid as promised, even after the trust fund runs out.</p>"}
  
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
}

shinyApp(ui = ui, server = server)