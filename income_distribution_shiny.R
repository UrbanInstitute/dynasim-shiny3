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
  gather(-subgroup, -year, -percentile, -group, key = income.source, value = income) %>%
  mutate(percentile = factor(percentile, levels = c("Mean", "P5", "P10", "P25", "P50", "P75", "P90", "P95", "P99"))) %>%
  mutate(subgroup = factor(subgroup, levels = c("All Individuals",
                                                "Females",
                                                "Males",
                                                "African-Americans",
                                                "Hispanics",
                                                "White, Non-Hispanics",
                                                "Bottom Quintile",
                                                "Quintile 2",
                                                "Quintile 3",
                                                "Quintile 4",
                                                "Top Quintile",
                                                "Never Married Individuals",
                                                "Divorced Individuals",
                                                "Married Individuals",
                                                "Widowed Individuals",
                                                "High School Dropouts",
                                                "High School Graduates",
                                                "Some College",
                                                "College Graduates")))

dollar.change <- dollar.change %>%
  gather(-subgroup, -year, -percentile, -group, key = income.source, value = income) %>%
  mutate(percentile = factor(percentile, levels = c("Mean", "P5", "P10", "P25", "P50", "P75", "P90", "P95", "P99"))) %>%
  mutate(group = factor(group, unique(levels$group))) %>%
  mutate(subgroup = factor(subgroup, levels = c("All Individuals",
                                                "Females",
                                                "Males",
                                                "African-Americans",
                                                "Hispanics",
                                                "White, Non-Hispanics",
                                                "Bottom Quintile",
                                                "Quintile 2",
                                                "Quintile 3",
                                                "Quintile 4",
                                                "Top Quintile",
                                                "Never Married Individuals",
                                                "Divorced Individuals",
                                                "Married Individuals",
                                                "Widowed Individuals",
                                                "High School Dropouts",
                                                "High School Graduates",
                                                "Some College",
                                                "College Graduates")))

##
## SHINY
##

ui <- fluidPage(
  
  theme = "shiny.css",
  
  titlePanel("Distribution of Incomes, Premiums, and Taxes"),
  
  fluidRow(
    column(6,
           style = "position:relative",
           plotOutput("chart",
                      hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
           uiOutput("hover_info"))
  ),
    
  fluidRow(
    column(4,
  
    selectInput(inputId = "income.tax.premium",
                label = "Income, Tax, or Premium",
                choices = c("Annuitized Financial Income" = "Annuitized Financial Income",
                            "DB Pension Income" = "DB Pension Income",
                            "Earned Income" = "Earned Income",
                            "Federal Income Tax" = "Federal Income Tax",
                            "HI Tax" = "HI tax",
                            "Imputed Rental Income" = "Imputed Rental Income",
                            "Means and Non-Means Tested Benefits" = "Means+Nonmeans Benefits",
                            "Medicare Part B Premium" = "Medicare Part B Premium",
                            "Medicare Surtax" = "Medicare Surtax",
                            "Net Annuity Income" = "Net Annuity Income",
                            "Net Cash Income" = "Net Cash Income",
                            "OASDI Tax" = "OASDI tax",
                            "Other Family Member Income" = "Other Family Member Income",
                            "Own Benefit" = "Own Benefit",
                            "Own Earnings" = "Own Earnings",
                            "Per Capita Annuity Income" = "Per Capita Annuity Income",
                            "Per Capita Cash Income" = "Per Capita Cash Income",
                            "Per Capita Dividend Income" = "Per Capita Dividend Income",
                            "Per Capita Interest Income" = "Per Capita Interest Income",
                            "Per Capita IRA Withdrawal" = "Per Capita IRA Withdrawal",
                            "Per Capita Rental Income" = "Per Capita Rental Income",
                            "Social Security Benefits" = "Social Security Benefits",
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
    
    selectInput(inputId = "graph.type",
                label = "Graph Type",
                choices = c("Overlayed Histograms" = "geom_histogram",
                            "Bar" = "geom_bar"))),
    
    column(4, 
    
    selectInput(inputId = "year",
                label = "Year",
                choices = c("2015" = 2015,
                            "2025" = 2025,
                            "2035" = 2035,
                            "2045" = 2045,
                            "2055" = 2055,
                            "2065" = 2065)),
    
    radioButtons(inputId = "comparison",
                label = "Comparison",
                choices = c("Level" = "levels",
                            "Dollar Change" = "dollar.change")))
  )
)

server <- function(input, output){
  
  output$chart <- renderPlot({  
  
    title <- if (input$income.tax.premium == "Annuitized Financial Income") {"Annuitized Financial Income"} else
             if (input$income.tax.premium == "DB Pension Income") {"Defined Benefit Pension Income"} else
             if (input$income.tax.premium == "Earned Income") {"Earned Income"} else
             if (input$income.tax.premium == "Federal Income Tax") {"Federal Income Tax"} else
             if (input$income.tax.premium == "HI Tax") {"Hospital Insurance Tax"} else
             if (input$income.tax.premium == "Imputed Rental Income") {"Imputed Rental Income"} else
             if (input$income.tax.premium == "Means and Non-Means Tested Benefits") {"Means and Non-Means Tested Benefits"} else
             if (input$income.tax.premium == "Medicare Part B Premium") {"Medicare Part B Premium"} else
             if (input$income.tax.premium == "Medicare Surtax") {"Medicare Surtax"} else
             if (input$income.tax.premium == "Net Annuity Income") {"Net Annuity Income"} else
             if (input$income.tax.premium == "Net Cash Income") {"Net Cash Income"} else
             if (input$income.tax.premium == "OASDI Tax") {"OASDI Tax"} else
             if (input$income.tax.premium == "Other Family Member Income") {"Other Family Member Income"} else
             if (input$income.tax.premium == "Own Benefit") {"Own Benefit"} else
             if (input$income.tax.premium == "Own Earnings") {"Own Earnings"} else
             if (input$income.tax.premium == "Per Capita Annuity Income") {"Per Capita Annuity Income"} else
             if (input$income.tax.premium == "Per Capita Cash Income") {"Per Capita Cash Income"} else
             if (input$income.tax.premium == "Per Capita Dividend Income") {"Per Capita Dividend Income"} else
             if (input$income.tax.premium == "Per Capita Interest Income") {"Per Capita Interest Income"} else
             if (input$income.tax.premium == "Per Capita IRA Withdrawal") {"Per Capita IRA Withdrawal"} else
             if (input$income.tax.premium == "Per Capita Rental Income") {"Per Capita Rental Income"} else
             if (input$income.tax.premium == "Social Security Benefits") {"Social Security Benefits"} else
             if (input$income.tax.premium == "Spouse Benefit") {"Spouse Benefit"} else
             if (input$income.tax.premium == "Spouse Earnings") {"Spouse Earnings"} else
             if (input$income.tax.premium == "SSI") {"SSI"} else
             if (input$income.tax.premium == "State Income Tax") {"State Income Tax"}
    
    subtitle <- if (input$group == "All Individuals") {"All Individuals"} else
                if (input$group == "Sex") {"Sex"} else
                if (input$group == "Race/Ethnicity") {"Race/Ethnicity"} else
                if (input$group == "Education") {"Education"} else
                if (input$group == "Marital Status") {"Marital Status"} else
                if (input$group == "Per Capita Income Quintile") {"Per Capita Income Quintile"} else
                if (input$group == "Lifetime Earnings Quintile") {"Lifetime Earnings Quintile"}
    
    if (input$comparison == "levels") {
    
    levels %>%
      filter(group == input$group) %>%  
      filter(year == input$year) %>%
      filter(income.source == input$income.tax.premium) %>%
      ggplot() +
      geom_bar(aes(x = percentile, y = income, fill = subgroup), position = "dodge", stat = "identity") +
      scale_y_continuous(expand = c(0,0), labels = scales::dollar) +
      labs(title = title,
           subtitle = subtitle,
           caption = "DYNASIM4") +
      xlab("Mean and Percentiles") +
      ylab("2015 Dollars")
    } else if (input$comparison == "dollar.change") {
      
      dollar.change %>%
        filter(group == input$group) %>%  
        filter(year == input$year) %>%
        filter(income.source == input$income.tax.premium) %>%
        ggplot() +
        geom_bar(aes(x = percentile, y = income, fill = subgroup), position = "dodge", stat = "identity") +
        scale_y_continuous(labels = scales::dollar) +
        labs(title = title,
             subtitle = subtitle,
             caption = "DYNASIM4") +
        xlab("Mean and Percentiles") +
        ylab("Change in 2015 Dollars")
      
    }
  })
  
    # Chart
    output$hover_info <- renderUI({
      hover <- input$plot_hover
      
      levels.temp <- levels %>%
        filter(group == input$group) %>%
        filter(year == input$year) %>%
        filter(income.source == input$income.tax.premium)

      point <- nearPoints(levels.temp, hover, threshold = 20, maxpoints = 1)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position inside the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      # create style property for tooltip
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
              "<b> Percentile: </b>", point$percentile, "<br/>",
              "<b> Amount: </b>", dollar_format()(point$income), "<br/>")
            )
          )
        )
    })
}
    
shinyApp(ui = ui, server = server)

