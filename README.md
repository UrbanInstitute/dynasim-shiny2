# dynasim-shiny2

Karen Smith used DYNASIM to model many Social Security reforms for the Bipartisan Policy Center's [Report of the Commission on Retirement Security and Personal Savings](http://cdn.bipartisanpolicy.org/wp-content/uploads/2016/06/BPC-Retirement-Security-Report.pdf). The scripts contained in this repo pull data created during this analysis, which are stored at `X:\programs\run912\Run5SaveOpt4\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx`, and build a basic shiny application for interactively visualizing the changes in retirement income for different demographic groups.

## Scripts

### get_data.R

This script pulls and cleans data from the sheets "mean income", "%mean income compare", and "$mean income compare" in `X:\programs\run912\Run5SaveOpt4\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx`. The data were highly formatted in the Excel file, so the script organizes the data into a format that can be used by ggplot2. 

### mean_income_shiny.R

This script takes the .csv files created in get_data.R and stored in the data folder, and turns them into an interactive shiny graphic.  

### themes

The R Shiny graphic is built using the [Urban Institute R theme](https://github.com/UrbanInstitute/urban_R_theme). The theme works better using Mac OSX than Windows so `urban_theme_mac.R` is used when publishing the Shiny graphic and `urban_theme_windows.R` is used for developing edits and new features. 

**Note:** Lines at the top of `mean_income_shiny.R` need to be commented out when switching between operating systems. 
