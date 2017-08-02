# dynasim-shiny2

Karen Smith used DYNASIM to model many Social Security reforms for the Bipartisan Policy Center's [Report of the Commission on Retirement Security and Personal Savings](http://cdn.bipartisanpolicy.org/wp-content/uploads/2016/06/BPC-Retirement-Security-Report.pdf). The scripts contained in this repo pull data created during this analysis, which are stored at `X:\programs\run912\Run5SaveOpt4\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx`, and build a basic shiny application for interactively visualizing the levels and changes in retirement income for different demographic groups at different income percentiles.

## Scripts

### get_data.R

This script pulls and cleans data from the sheet "mean income" The data were highly formatted in the Excel file, so the script organizes the data into a format that can be used by ggplot2. It also calculates percent and dollar change from current law scheduled and current law payable. 

## /data

incomes.csv contains four income measures for many demographic groups for many policy options.

## /www

The /www subdirectory contains `shiny.css`. Shiny applications automatically look for material in the www subdirectory. The style sheet currently has a single universal css selector which changes the font in the Shiny application to [Lato](https://fonts.google.com/specimen/Lato). 

### themes

The R Shiny graphic is built using the [Urban Institute R theme](https://github.com/UrbanInstitute/urban_R_theme). The theme works better using Mac OSX than Windows so `urban_theme_mac.R` is used when publishing the Shiny graphic and `urban_theme_windows.R` is used for developing edits and new features. 

**Note:** Lines at the top of `mean_income_shiny.R` need to be commented out when switching between operating systems. 
