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
source('urban_institute_themes/urban_theme_windows.R')

# Source file for Mac
#source('urban_institute_themes/urban_theme_mac.R')

# Load Data
distribution <- read_csv("data/distributions.csv")
scale_text <- read_csv("text/scale.csv")
baseline_text <- read_csv("text/baseline.csv")
income_tax_premium_text <- read_csv("text/income_tax_premium.csv")
option_text <- read_csv("text/option.csv")

# Gather the data
distribution <- distribution %>%
  mutate(group = gsub("Per Capita ", "", group)) %>%
  mutate(subgroup = gsub(" \\(Income\\)", "", subgroup)) %>% 
  mutate(percentile = factor(percentile, levels = c("Mean", "P5", "P10", "P25", "P50", "P75", "P90", "P95", "P99", "Percent with Income Source"))) %>%
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
                                                "College Graduates"),
                                      labels = c("All Individuals",
                                                 "Female",
                                                 "Male",
                                                 "Black",
                                                 "Hispanic",
                                                 "White, Non-Hispanic",
                                                 "Bottom Quintile",
                                                 "2nd Quintile",
                                                 "3rd Quintile",
                                                 "4th Quintile",
                                                 "Top Quintile",
                                                 "Never Married",
                                                 "Divorced",
                                                 "Married",
                                                 "Widowed",
                                                 "HS Dropout",
                                                 "HS Graduate",
                                                 "Some College",
                                                 "College Graduate"))) %>%
  gather(`Annuitized Financial Income`:`State Income Tax`, key = income.tax.premium, value = value)

##
## SHINY
##

latoCSS <- "http://fonts.googleapis.com/css?family=Lato:300,400,700,900,300italic,400italic,700italic,900italic"

ui <- fluidPage(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = latoCSS)),
  
  theme = "shiny.css",
  
  fluidRow(
    
    column(8,
           
           titlePanel("Exploring Social Security Reform Options"),
           
           p("The Social Security trustees project that, by the mid-2030s, the system will no longer be able to pay all scheduled benefits. Which reform option should policymakers pursue to help balance the system?
             Use our interactive tool to compare how different groups would fare, over time, under the following policy options."),
           HTML("<p>Explore the trust fund, by income, <b>by demographics</b>, and <a href='http://www.urban.org/policy-centers/cross-center-initiatives/program-retirement-policy/projects/dynasim-projecting-older-americans-future-well-being/detailed-projections-older-population-through-2065' target='_blank'>the data</a>.</p>"),
           
           br()
           
           
           )
  
  ),
  
  
  fluidRow(
    column(6,
           style = "position:relative",
           
           h4(textOutput("title")),
           h5(textOutput("subtitle")),
           
           plotOutput("chart", width = "100%", height = "400px")
           
           )
  ),
    
  fluidRow(
    column(4, 
           
           sliderInput(inputId = "year", 
                       label = "Year",
                       min = 2015,
                       max = 2065, 
                       step = 10,
                       value = 2015,
                       sep = "",
                       animate = animationOptions(loop = TRUE, interval = 1500))
           ),
    
    column(4,
           
           htmlOutput("text5")
           
           
    )
  ),
  
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
                              "90% Tax Max" = "90% Tax Max",
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
                              "15.4% Payroll Tax" = "15.4% Payroll Tax")),           

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
                              "Net Annuity Income" = "Net Annuity Income",
                              "Net Cash Income" = "Net Cash Income",
                              "OASDI Tax" = "OASDI Tax",
                              "Other Family Member Income" = "Other Family Member Income",
                              "Own Benefit" = "Own Benefit",
                              "Own Earnings" = "Own Earnings",
                              "Annuity Income" = "Annuity Income",
                              "Cash Income" = "Cash Income",
                              "Dividend Income" = "Dividend Income",
                              "Interest Income" = "Interest Income",
                              "IRA Withdrawal" = "IRA Withdrawal",
                              "Rental Income" = "Rental Income",
                              "Social Security Benefits" = "Social Security Benefits",
                              "Spouse Benefit" = "Spouse Benefit",
                              "Spouse Earnings" = "Spouse Earnings",
                              "SSI" = "SSI",
                              "State Income Tax" = "State Income Tax")),
    
      selectInput(inputId = "group",
                  label = "Demographic",
                  choices = c("All Individuals" = "All Individuals",
                              "Sex" = "Sex",
                              "Race/Ethnicity" = "Race/Ethnicity",
                              "Education" = "Education",
                              "Marital Status" = "Marital Status",
                              "Income Quintile" = "Income Quintile",
                              "Lifetime Earnings Quintile" = "Lifetime Earnings Quintile"))),
    
    column(4, 
      selectInput(inputId = "comparison",
                  label = "Comparison",
                  choices = c("Level" = "level",
                              "Dollar Change" = "dollar.change")),
      
      selectInput(inputId = "baseline",
                   label = "Baseline",
                   choices = c("Current Law Payable" = "Payable Law",
                               "Current Law Scheduled" = "Scheduled Law")),
      
      selectInput(inputId = "scale",
                  label = "Scale",
                  choices = c("Per Capita" = "per capita",
                              "Equivalent" = "equivalent")))),
  
  fluidRow(
    
    column(8,
    
    # Explanation of Social Security Reform
    
    htmlOutput("text1"))
    
  ),
  
  fluidRow(
    
    column(8,
           
      # Explanation of Income, Tax, or Premium
      
      htmlOutput("text2"))
    
  ),
  
  fluidRow(
    
    column(8,
           
      # Explanation of Scales
      
      htmlOutput("text3"))
    
  ),
  
  fluidRow(
    
    column(8,
           
           # Explanation of Baseline
           
           htmlOutput("text4"))
    
  )  
  
)

server <- function(input, output) {
  
  options(shiny.sanitize.errors = FALSE)
  
  output$title <- renderText({
    
    if (input$income.tax.premium == "Annuitized Financial Income") {"Annuitized Financial Income ($2015)"} else
    if (input$income.tax.premium == "DB Pension Income") {"Defined Benefit Pension Income ($2015)"} else
    if (input$income.tax.premium == "Earned Income") {"Earned Income ($2015)"} else
    if (input$income.tax.premium == "Federal Income Tax") {"Federal Income Tax ($2015)"} else
    if (input$income.tax.premium == "HI Tax") {"Hospital Insurance Tax ($2015)"} else
    if (input$income.tax.premium == "Imputed Rental Income") {"Imputed Rental Income ($2015)"} else
    if (input$income.tax.premium == "Means and Non-Means Tested Benefits") {"Means and Non-Means Tested Benefits ($2015)"} else
    if (input$income.tax.premium == "Medicare Part B Premium") {"Medicare Part B Premium ($2015)"} else
    if (input$income.tax.premium == "Medicare Surtax") {"Medicare Surtax ($2015)"} else
    if (input$income.tax.premium == "Net Annuity Income") {"Net Annuity Income ($2015)"} else
    if (input$income.tax.premium == "Net Cash Income") {"Net Cash Income ($2015)"} else
    if (input$income.tax.premium == "OASDI Tax") {"OASDI Tax ($2015)"} else
    if (input$income.tax.premium == "Other Family Member Income") {"Other Family Member Income ($2015)"} else
    if (input$income.tax.premium == "Own Benefit") {"Own Benefit ($2015)"} else
    if (input$income.tax.premium == "Own Earnings") {"Own Earnings ($2015)"} else
    if (input$income.tax.premium == "Annuity Income") {"Annuity Income ($2015)"} else
    if (input$income.tax.premium == "Cash Income") {"Cash Income ($2015)"} else
    if (input$income.tax.premium == "Dividend Income") {"Dividend Income ($2015)"} else
    if (input$income.tax.premium == "Interest Income") {"Interest Income ($2015)"} else
    if (input$income.tax.premium == "IRA Withdrawal") {"IRA Withdrawal ($2015)"} else
    if (input$income.tax.premium == "Rental Income") {"Rental Income ($2015)"} else
    if (input$income.tax.premium == "Social Security Benefits") {"Social Security Benefits ($2015)"} else
    if (input$income.tax.premium == "Spouse Benefit") {"Spouse Benefit ($2015)"} else
    if (input$income.tax.premium == "Spouse Earnings") {"Spouse Earnings ($2015)"} else
    if (input$income.tax.premium == "SSI") {"SSI ($2015)"} else
    if (input$income.tax.premium == "State Income Tax") {"State Income Tax ($2015)"}
    
    })
  
  output$subtitle <- renderText({
    
    if (input$group == "All Individuals") {"All Individuals"} else
    if (input$group == "Sex") {"Sex"} else
    if (input$group == "Race/Ethnicity") {"Race/Ethnicity"} else
    if (input$group == "Education") {"Education"} else
    if (input$group == "Marital Status") {"Marital Status"} else
    if (input$group == "Income Quintile") {"Income Quintile"} else
    if (input$group == "Lifetime Earnings Quintile") {"Lifetime Earnings Quintile"}
  
    })
  
  output$chart <- renderPlot({  

    # Calculate the maximum for the y-axis (because of the animation)
    y.max <- distribution %>%
      filter(option == input$option) %>%
      filter(group == input$group) %>%  
      filter(comparison == input$comparison) %>%   
      filter(baseline == input$baseline) %>% 
      filter(scale == input$scale) %>%
      filter(income.tax.premium == input$income.tax.premium) %>%
      summarize(max = max(value))

    # Calculate the maximum for the y-axis (because of the animation)
    y.min <- distribution %>%
      filter(option == input$option) %>%
      filter(group == input$group) %>%  
      filter(comparison == input$comparison) %>%   
      filter(baseline == input$baseline) %>% 
      filter(scale == input$scale) %>%
      filter(income.tax.premium == input$income.tax.premium) %>%
      summarize(min = min(value))
    
    y.min <- min(0, as.numeric(y.min))
    
    print(y.max)
    print(y.min)
    
    graphr <- function(origin, line.placement, line.color){
    
      distribution %>%
        filter(option == input$option) %>%
        filter(group == input$group) %>%  
        filter(year == input$year) %>%
        filter(comparison == input$comparison) %>%   
        filter(baseline == input$baseline) %>% 
        filter(scale == input$scale) %>%
        filter(income.tax.premium == input$income.tax.premium) %>%
        filter(percentile != "Percent with Income Source") %>%
        ggplot() +
          geom_bar(aes(x = percentile, y = value, fill = subgroup), position = "dodge", stat = "identity") +
          scale_y_continuous(limits = c(y.min, as.numeric(y.max)), labels = scales::dollar) +
          labs(caption = "DYNASIM3",
               x = "Mean and Percentiles",
               y = NULL) +
          expand_limits(y = origin) +
          geom_hline(size = 0.5, aes(yintercept = line.placement), color = line.color) +
          theme(axis.ticks.length = unit(0, "points"),
                axis.line = element_blank())
      
      
    }
      
    if (input$comparison == "level") {
      graphr(origin = NULL, line.placement = 0, line.color = "black") 
    } 
    else if (input$comparison == "dollar.change") {
      graphr(origin = 0, line.placement = 0, line.color = "black")
    } 
    
  })  
  
    output$text1 <- renderText({
    
      as.character(
        option_text %>%
          filter(option == input$option) %>%
          select(text)
      )
    
    })
    
    output$text2 <- renderText({
      
      as.character(
        income_tax_premium_text %>%
          filter(income_tax_premium == input$income.tax.premium) %>%
          select(text)
      )
      
    })
    
    output$text3 <- renderText({
      
      as.character(
        scale_text %>%
          filter(scale == input$scale) %>%
          select(text)
      )
  
    })
    
    output$text4 <- renderText({
      
      as.character(
        baseline_text %>%
          filter(baseline == input$baseline) %>%
          select(text)
      )
      
    })
    
    output$text5 <- renderUI({
      
      percent <- distribution %>%
        filter(option == input$option) %>%
        filter(group == "All Individuals") %>%  
        filter(year == input$year) %>%
        filter(comparison == input$comparison) %>%   
        filter(baseline == input$baseline) %>% 
        filter(scale == input$scale) %>%
        filter(income.tax.premium == input$income.tax.premium) %>%
        filter(percentile == "Percent with Income Source") %>% 
        select(value)
      
      HTML(paste("<div class='income-percent'>", as.character(round(percent * 100, 1)), "%", "</div>","<div class='income-text'>", "have", "<b>", input$income.tax.premium, "</b>", "</div>"))
      
    })
    
}
    
shinyApp(ui = ui, server = server)