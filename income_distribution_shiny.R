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
levels <- read_csv("data/levels.csv")
dollar.change <- read_csv("data/dollar_change.csv")

# Gather the data

levels <- levels %>%
  gather(-subgroup, -year, -percentile, -group, key = income.source, value = income)

dollar.change <- dollar.change %>%
  gather(-subgroup, -year, -percentile, -group, key = income.source, value = income)

##
## SHINY
##

ui <- fluidPage(
  
  fluidRow(
    
    plotOutput("chart")
  ),
    
  fluidRow(
  
    selectInput(inputId = "income.tax.premium",
                label = "Income, Tax, or Premium",
                choices = c("Annuitized Financial Income" = "Annuitized Financial Income",
                            "DB Pension Income" = "DB Pension Income",
                            "Earned Income" = "Earned Income",
                            "Federal Income Tax" = "Federal Income Tax",
                            "HI Tax" = "HI Tax",
                            "Imputed Rental Income" = "Imputed Rental Income",
                            "Means and Non-Means Tested Benefits" = "Means+Nonmeans Benefits",
                            "Medicare Part B Premium" = "Medicare Part B Premium",
                            "Medicare Surtax" = "Medicare Surtax",
                            "Net Aunnuity Income" = "Net Aunnuity Income",
                            "Net Cash Income" = "Net Cash Income",
                            "OASDI Tax" = "OASDI Tax",
                            "Other Family Member Income" = "Other Family Member Income",
                            "Own Benefit" = "Own Benefit",
                            "Own Earnings" = "Own Earnings",
                            "Per Capita Annuity Income" = "Per Capita Annuity Income",
                            "Per Capita Cash Income" = "Per Capita Cash Income",
                            "Per Capita Dividend Income" = "Per Capita Dividend Income",
                            "Per Capita Interest Income" = "Per Capita Interest Income",
                            "Per Capita IRA Withdrawal" = "Per Capita IRA Withdrawal",
                            "Per Capita Rental Income" = "Per Capita Rental Income",
                            "Social Secuirty Benefits" = "Social Secuirty Benefits",
                            "Spouse Benefit" = "Spouse Benefit",
                            "Spouse Earnings" = "Spouse Earnings",
                            "SSI" = "SSI",
                            "State Income Tax" = "State Income Tax")),
  

    
    selectInput(inputId = "group",
                label = "Group",
                choices = c("All Individuals" = "All Individuals",
                            "Sex" = "Sex",
                            "Race/Ethnicity" = "Race/Ethnicity",
                            "Education" = "Education",
                            "Marital Status" = "Marital Status",
                            "Per Capita Income Quintile" = "Per Capita Income Quintile",
                            "Lifetime Earnings Quintile" = "Lifetime Earnings Quintile")),
    
    selectInput(inputId = "year",
                label = "Year",
                choices = c("2015" = 2015,
                            "2025" = 2025,
                            "2035" = 2035,
                            "2045" = 2045,
                            "2055" = 2055,
                            "2065" = 2065)),
    
    selectInput(inputId = "graph.type",
                label = "Graph Type",
                choices = c("Overlayed Histograms" = "geom_histogram",
                            "Bar" = "geom_bar"))
  )
)

server <- function(input, output){
  
  output$chart <- ({  
  
  levels %>%
    filter(subgroup == "All Individuals") %>%  
    filter(year == 2015) %>%
    ggplot(aes(percentile, income, fill = subgroup)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::dollar)
  
  })
  
  
}

shinyApp(ui = ui, server = server)

