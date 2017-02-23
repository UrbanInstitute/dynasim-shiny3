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

# Gather the data
distribution <- distribution %>%
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
                                                "College Graduates"))) %>%
  gather(`Annuitized Financial Income`:`State Income Tax`, key = income.tax.premium, value = value)

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
           HTML("<p>Explore the trust fund, by income, <b>by demographics</b>, and <a href='http://www.urban.org/policy-centers/cross-center-initiatives/program-retirement-policy/projects/dynasim-projecting-older-americans-future-well-being/detailed-projections-older-population-through-2065' target='_blank'>the data</a>.</p>"),
           
           br()
           
           
           )
  
  ),
  
  
  fluidRow(
    column(6,
           
           plotOutput("chart")
           
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
                       animate = animationOptions(loop = TRUE))
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
                              "15.4% Payroll Tax" = "15.4% Payroll Tax")),           

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
                  label = "Group",
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
  
  output$chart <- renderPlot({  
  
    title <- if (input$income.tax.premium == "Annuitized Financial Income") {"Annuitized Financial Income ($2015)"} else
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
    
    subtitle <- if (input$group == "All Individuals") {"All Individuals"} else
                if (input$group == "Sex") {"Sex"} else
                if (input$group == "Race/Ethnicity") {"Race/Ethnicity"} else
                if (input$group == "Education") {"Education"} else
                if (input$group == "Marital Status") {"Marital Status"} else
                if (input$group == "Income Quintile") {"Income Quintile"} else
                if (input$group == "Lifetime Earnings Quintile") {"Lifetime Earnings Quintile"}
    
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
        labs(title = title,
             subtitle = subtitle,
             caption = "DYNASIM3") +
        xlab("Mean and Percentiles") +
        ylab(NULL) +
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
      
      else if (input$option == "13.4% Payroll Tax") {"<p><h4>13.4% Payroll Tax</h4></p><p>Increase the payroll tax rate to 13.4% over 10 years beginning in 2016.</p>"}
      
      else if (input$option == "14.4% Payroll Tax") {"<p><h4>14.4% Payroll Tax</h4></p><p>Increase the payroll tax rate to 14.4% over 10 years beginning in 2016.</p>"}
      
      else if (input$option == "15.4% Payroll Tax") {"<p><h4>15.4% Payroll Tax</h4></p><p>Increase the payroll tax rate to 15.4% over 10 years beginning in 2016.</p>"}
      
      else if (input$option == "Payable Law") {"<p><h4>Current Law Payable</h4></p><p>Assumes that current public policies, business practices, and individual behaviors continue, and that Social Security benefits are paid as promised, even after the trust fund runs out. </p>"}
      
      else if (input$option == "Scheduled Law") {"<p><h4>Current Law Scheduled</h4></p><p>Assumes that current public policies, business practices, and individual behaviors continue, and that Social Security benefits are paid as promised, even after the trust fund runs out.</p>"}
      
      })
    
    
    output$text2 <- renderText({
      
      if (input$income.tax.premium == "Annuitized Financial Income") {"<p><h4>Annuitized Financial Income</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "DB Pension Income") {"<p><h4>DB Pension Income</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Earned Income") {"<p><h4>Earned Income</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Federal Income Tax") {"<p><h4>Federal Income Tax</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "HI Tax") {"<p><h4>HI Tax</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Imputed Rental Income") {"<p><h4>Imputed Rental Income</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Means and Non-Means Tested Benefits") {"<p><h4>Means and Non-Means Tested Benefits</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Medicare Part B Premium") {"<p><h4>Medicare Part B Premium</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Medicare Surtax") {"<p><h4>Medicare Surtax</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Net Annuity Income") {"<p><h4>Net Annuity Income</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Net Cash Income") {"<p><h4>Net Cash Income</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "OASDI Tax") {"<p><h4>OASDI Tax</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Other Family Member Income") {"<p><h4>Other Family Member Income</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Own Benefit") {"<p><h4>Own Benefit</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Own Earnings") {"<p><h4>Own Earnings</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Annuity Income") {"<p><h4>Annuity Income</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Cash Income") {"<p><h4>Cash Income</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Dividend Income") {"<p><h4>Dividend Income</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Interest Income") {"<p><h4>Interest Income</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "IRA Withdrawal") {"<p><h4>IRA Withdrawal</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Rental Income") {"<p><h4>Rental Income</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Social Security Benefits") {"<p><h4>Social Security Benefits</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Spouse Benefit") {"<p><h4>Spouse Benefit</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "Spouse Earnings") {"<p><h4>Spouse Earnings</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "SSI") {"<p><h4>SSI</h4></p><p></p>"}
      
      else if (input$income.tax.premium == "State Income Tax") {"<p><h4>State Income Tax</h4></p><p></p>"}
      
      })
    
    output$text3 <- renderText({
      
      if (input$scale == "per capita") {"<p><h4>Per Capita</h4></p><p></p>"}
      
      else if (input$scale == "equivalent") {"<p><h4>Equivalent</h4></p><p></p>"}
      
      })
    
    
    output$text4 <- renderText({
      
      if (input$baseline == "Payable Law") {"<p><h4>Current Law Payable</h4></p><p>Assumes that current public policies, business practices, and individual behaviors continue, but reduces Social Security benefits by a uniform amount after the trust fund runs out so that all benefits in each year can be paid out of revenues from that year.</p>"}
      
      else if (input$baseline == "Scheduled Law") {"<p><h4>Current Law Scheduled</h4></p><p>Assumes that current public policies, business practices, and individual behaviors continue, and that Social Security benefits are paid as promised, even after the trust fund runs out.</p>"}
      
    })
    
    output$text5 <- renderUI({
      
      percent <- distribution %>%
        filter(option == input$option) %>%
        filter(group == input$group) %>%  
        filter(year == input$year) %>%
        filter(comparison == input$comparison) %>%   
        filter(baseline == input$baseline) %>% 
        filter(scale == input$scale) %>%
        filter(income.tax.premium == input$income.tax.premium) %>%
        filter(percentile == "Percent with Income Source") %>% 
        select(value)
      
      HTML(paste("<div class='income-percent'>", as.character(percent * 100), "%", "</div>","<div class='income-text'>", "have", "<b>", input$income.tax.premium, "</b>", "</div>"))
      
    })
    

    
}
    
shinyApp(ui = ui, server = server)