# dynasim-shiny3

Karen Smith used DYNASIM to model many Social Security reforms for the Bipartisan Policy Center's [Report of the Commission on Retirement Security and Personal Savings](http://cdn.bipartisanpolicy.org/wp-content/uploads/2016/06/BPC-Retirement-Security-Report.pdf). The scripts contained in this repo pull data created during this analysis, which are stored at `X:\programs\run912\Run5SaveOpt4\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx`, and build a basic shiny application for interactively visualizing the levels and changes in retirement income for different demographic groups at different income percentiles.

## Scripts

### get_data_dollar_change.R

This script pulls and cleans data from the sheet "$income Distribution by Sour" in `X:\programs\run912\Run5SaveOpt4\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx`. The Excel data are in 36 tables which are arranged vertically by demographic groups and horizontally by years. Variables (income, taxes, and premiums) are arranged vertically in each table and observations (percentiles) appear horizontally. 

### get_data_levels. R

This script pulls and cleans data from the sheet "income Distribution by Source" in `X:\programs\run912\Run5SaveOpt4\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx`. The Excel data are in 36 tables which are arranged vertically by demographic groups and horizontally by years. Variables (income, taxes, and premiums) are arranged vertically in each table and observations (percentiles) appear horizontally. 

### income_distribution_shiny.R

This script takes the .csv files created in `get_data_levels.R` and `get_data_dollar_change.R` and turns them into an interactive shiny graphic.  

### /data

The /data subdirectory contains .csv files which are created in the get_data scripts and used by `income_distribution_shiny.R`.

### /www

The /www subdirectory contains `shiny.css`. Shiny applications automatically look for material in the www subdirectory. The style sheet currently has a single universal css selector which changes the font in the Shiny application to [Lato](https://fonts.google.com/specimen/Lato). 

### themes

The R Shiny graphic is built using the [Urban Institute R theme](https://github.com/UrbanInstitute/urban_R_theme). The theme works better using Mac OSX than Windows so `urban_theme_mac.R` is used when publishing the Shiny graphic and `urban_theme_windows.R` is used for developing edits and new features. 

**Note:** Lines at the top of `income_distribution_shiny.R` need to be commented out when switching between operating systems. 
