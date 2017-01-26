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
#source('urban_institute_themes/urban_theme_windows.R')
source('urban_institute_themes/urban_ggplot_theme_new_formatting.R')

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
  
  fluidRow(
    
    column(8,
           
           titlePanel("Exploring Social Security Reform Options"),
           
           p("The Social Security trustees project that, by the mid-2030s, the system will no longer be able to pay all scheduled benefits. Which reform option should policymakers pursue to help balance the system?
             Use our interactive tool to compare how different groups would fare, over time, under the following policy options."),
           HTML("<p>Explore the trust fund, by income, <b>by demographics</b>, and <a href='http://www.urban.org/policy-centers/cross-center-initiatives/program-retirement-policy/projects/dynasim-projecting-older-americans-future-well-being/detailed-projections-older-population-through-2065'>the data</a>.</p>"),
           
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
                              "15.4% Payroll Tax" = "fica15")),           

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
                              "Lifetime Earnings Quintile" = "Lifetime Earnings Quintile"))),
    
    column(4, 
    
      sliderInput(inputId = "year", 
                label = "Year",
                min = 2015,
                max = 2065, 
                step = 10,
                value = 2015,
                sep = "",
                animate = animationOptions(loop = TRUE)),
    
    radioButtons(inputId = "comparison",
                label = "Comparison",
                choices = c("Level" = "levels",
                            "Dollar Change" = "dollar.change")),
    
    radioButtons(inputId = "baseline",
                 label = "Baseline",
                 choices = c("Current Law Payable" = "current.law.payable",
                             "Current Law Scheduled" = "current.law.scheduled"))
    )
  ),
  
  fluidRow(
    
    column(8,
    
    # Explanation of Social Security Reform
    
    htmlOutput("text1"))
    
    ),
  
  fluidRow(
    
    column(8,
           
           # Explanation of Baseline
           
           htmlOutput("text2"))
    
  )
  
  
  
  
)

server <- function(input, output) {
  
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
             if (input$income.tax.premium == "Per Capita Annuity Income") {"Per Capita Annuity Income ($2015)"} else
             if (input$income.tax.premium == "Per Capita Cash Income") {"Per Capita Cash Income ($2015)"} else
             if (input$income.tax.premium == "Per Capita Dividend Income") {"Per Capita Dividend Income ($2015)"} else
             if (input$income.tax.premium == "Per Capita Interest Income") {"Per Capita Interest Income ($2015)"} else
             if (input$income.tax.premium == "Per Capita IRA Withdrawal") {"Per Capita IRA Withdrawal ($2015)"} else
             if (input$income.tax.premium == "Per Capita Rental Income") {"Per Capita Rental Income ($2015)"} else
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
                if (input$group == "Per Capita Income Quintile") {"Per Capita Income Quintile"} else
                if (input$group == "Lifetime Earnings Quintile") {"Lifetime Earnings Quintile"}
    
    if (input$comparison == "levels") {
    
      # Calculate the maximum for the y-axis (because of the animation)
      y.max <- levels %>%
        filter(group == input$group) %>%  
        filter(income.source == input$income.tax.premium) %>%
        select(income) %>%
        summarize(max = max(income))
      
      levels %>%
        filter(group == input$group) %>%  
        filter(year == input$year) %>%
        filter(income.source == input$income.tax.premium) %>%
        ggplot() +
        geom_bar(aes(x = percentile, y = income, fill = subgroup), position = "dodge", stat = "identity") +
        scale_y_continuous(limits = c(0, as.numeric(y.max)), expand = c(0,0), labels = scales::dollar) +
        labs(title = title,
             subtitle = subtitle,
             caption = "DYNASIM4") +
        xlab("Mean and Percentiles") +
        ylab(NULL) +
        theme(plot.subtitle = element_text(margin = structure(c(2, 0, 2, 0), 
                                                unit = "pt", 
                                                valid.unit = 8L, 
                                                class = c("margin", "unit"))),
              axis.ticks.length = unit(0, "points"),
              axis.text.x = element_text(margin = structure(c(4, 0, 0, 0), 
                                                            unit = "pt", 
                                                            valid.unit = 8L, 
                                                          class = c("margin", "unit"))))
    } else if (input$comparison == "dollar.change") {
      
      # Calculate the maximum for the y-axis (because of the animation)
      y.max <- dollar.change %>%
        filter(group == input$group) %>%  
        filter(income.source == input$income.tax.premium) %>%
        select(income) %>%
        summarize(max = max(income))
      
      dollar.change %>%
        filter(group == input$group) %>%  
        filter(year == input$year) %>%
        filter(income.source == input$income.tax.premium) %>%
        ggplot() +
        geom_bar(aes(x = percentile, y = income, fill = subgroup), position = "dodge", stat = "identity") +
        scale_y_continuous(limits = c(0, as.numeric(y.max)), expand = c(0,0), labels = scales::dollar) +
        labs(title = title,
             subtitle = subtitle,
             caption = "DYNASIM4") +
        xlab("Mean and Percentiles") +
        ylab(NULL) +
        theme(plot.subtitle = element_text(margin = structure(c(2, 0, 2, 0), 
                                                              unit = "pt", 
                                                              valid.unit = 8L, 
                                                              class = c("margin", "unit"))),
              axis.ticks.length = unit(0, "points"),
              axis.text.x = element_text(margin = structure(c(4, 0, 0, 0), 
                                                            unit = "pt", 
                                                            valid.unit = 8L, 
                                                            class = c("margin", "unit"))))
      
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
    
    output$text2 <- renderText({
      
      if (input$baseline == "current.law.payable") {"<p><h4>Current Law Payable</h4></p><p>Assumes that current public policies, business practices, and individual behaviors continue, but reduces Social Security benefits by a uniform amount after the trust fund runs out so that all benefits in each year can be paid out of revenues from that year.</p>"} else
      if (input$baseline == "current.law.scheduled") {"<p><h4>Current Law Scheduled</h4></p><p>Assumes that current public policies, business practices, and individual behaviors continue, and that Social Security benefits are paid as promised, even after the trust fund runs out.</p>"}
      
    })
    
}
    
shinyApp(ui = ui, server = server)