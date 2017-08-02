# dynasim-shiny3

Karen Smith used DYNASIM to model many Social Security reforms for the Bipartisan Policy Center's [Report of the Commission on Retirement Security and Personal Savings](http://cdn.bipartisanpolicy.org/wp-content/uploads/2016/06/BPC-Retirement-Security-Report.pdf). The scripts contained in this repo pull data created during this analysis, which are stored at `X:\programs\run912\Run5SaveOpt4\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx`, and build a basic shiny application for interactively visualizing the levels and changes in retirement income for different demographic groups at different income percentiles.

## Scripts

### get-assets.R

This script pulls and cleans data from the sheet "Total Asset distribution". 
### get-incomes.R

This script pulls and cleans data from the sheet "income Distribution by Source".

### /data

### assets.csv

A .csv file with distributions for 4 measures of wealth for many different policy options and demographic groups. 

### incomes.csv

A .csv file with distributions for 26 measures of income for many different policy options and demographic groups. 

## /www

The /www subdirectory contains `shiny.css`. Shiny applications automatically look for material in the www subdirectory. 

## themes

The R Shiny graphic is built using the [Urban Institute R theme](https://github.com/UrbanInstitute/urban_R_theme). The theme works better using Mac OSX than Windows so `urban_theme_mac.R` is used when publishing the Shiny graphic and `urban_theme_windows.R` is used for developing edits and new features. 

**Note:** Lines at the top of `income_distribution_shiny.R` need to be commented out when switching between operating systems. 
