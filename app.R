# Libraries and Source Files
library(shiny)
library(tidyverse)
library(scales)

# Set options
options(shiny.sanitize.errors = TRUE)
options(scipen = 999)

# Source file for Windows
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")
source('urban_institute_themes/urban_theme_windows.R')

# Source file for Mac
#source('urban_institute_themes/urban_theme_mac.R')

# factor labels and levels
percentile_levels <- c("Mean", "P5", "P10", "P25", "P50", "P75", "P90", "P95", "P99")
subgroup_levels <- c("All Individuals", "Females", "Males",
                     "African-Americans", "Hispanics", "White, Non-Hispanics",
                     "Bottom Quintile", "Quintile 2", "Quintile 3", "Quintile 4",
                     "Top Quintile", "Never Married Individuals",
                     "Divorced Individuals", "Married Individuals",
                     "Widowed Individuals", "High School Dropouts",
                     "High School Graduates", "Some College", "College Graduates")

subgroup_labels <- c("All individuals", "Female", "Male", "Black", "Hispanic",
                     "White, non-Hispanic", "Bottom quintile", "2nd quintile",
                     "3rd quintile", "4th quintile", "Top quintile", 
                     "Never married", "Divorced", "Married", "Widowed",
                     "HS dropout", "HS graduate", "Some college", 
                     "College graduate")

# Load Data

level <- read_csv("data/level.csv", 
  col_types = cols(
    .default = col_double(),
    subgroup = col_character(),
    year = col_integer(),
    percentile = col_character(),
    group = col_character(),
    option = col_character(),
    scale = col_character(),
    baseline = col_character(),
    SSI = col_integer()
  )
) %>%
  mutate(percentile = factor(percentile, levels = percentile_levels)) %>%
  mutate(subgroup = factor(subgroup, levels = subgroup_levels,
                           labels = subgroup_labels))

dollar_change <- read_csv("data/dollar-change.csv", 
  col_types = cols(
    .default = col_double(),
    subgroup = col_character(),
    year = col_integer(),
    percentile = col_character(),
    group = col_character(),
    option = col_character(),
    scale = col_character(),
    baseline = col_character(),
    `Means+Nonmeans Benefits` = col_integer(),
    `Medicare Part B Premium` = col_integer(),
    `Medicare Part D Premium` = col_integer(),
    `Other Family Member Income` = col_integer()
  )
) %>%
  mutate(percentile = factor(percentile, levels = percentile_levels)) %>%
  mutate(subgroup = factor(subgroup, levels = subgroup_levels,
                           labels = subgroup_labels))

percent_with_income <- read_csv("data/percent-with-income.csv",
  col_types = cols(
    .default = col_double(),
    subgroup = col_character(),
    year = col_integer(),
    percentile = col_character(),
    group = col_character(),
    option = col_character(),
    scale = col_character(),
    baseline = col_character(),
    comparison = col_character(),
    `Net Annuity Income` = col_integer()
  )
)

scale_text <- read_csv("text/scale.csv",
  col_types = cols(
    scale = col_character(),
    text = col_character()
  )
)

baseline_text <- read_csv("text/baseline.csv",
  col_types = cols(
    baseline = col_character(),
    text = col_character()
  )
)

income_tax_premium_text <- read_csv("text/income_tax_premium.csv",
  col_types = cols(
    income_tax_premium = col_character(),
    text = col_character()
  )
)

option_text <- read_csv("text/option.csv",
  col_types = cols(
    option = col_character(),
    text = col_character()
  )
)

demographic <- read_csv("text/demographic.csv",
  col_types = cols(
    demographic = col_character(),
    description = col_character()
  )
)

##
## SHINY
##

latoCSS <- "http://fonts.googleapis.com/css?family=Lato:300,400,700,900,300italic,400italic,700italic,900italic"

ui <- fluidPage(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = latoCSS)),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),    
  tags$head(tags$script(src = "pym.min.js")),
  
  theme = "shiny.css",
  
  fluidRow(
    
    column(12,
           
           p("Social Security reform affects low-, middle-, and high-income 
             retirees differently. Use this interactive to explore the impacts 
             of Social Security reforms across the income and wealth 
             distributions for different populations from 2015 to 2065.")
           
           )
  ),
  
  
  fluidRow(
    column(10,
           style = "position:relative",
           
           h4(textOutput("title")),
           h5(textOutput("subtitlea")),
           h5(textOutput("subtitleb")),
           
           plotOutput("chart", width = "100%", height = "400px")
           
           )
  ),
    
  fluidRow(
    column(6, 
           
           sliderInput(inputId = "year", 
                       label = "Year",
                       min = 2015,
                       max = 2065, 
                       step = 10,
                       value = 2015,
                       sep = "",
                       animate = animationOptions(loop = TRUE, interval = 1500))
           ),
    
    column(6,
           
           htmlOutput("text_have_income")
           
    )
  ),
  
  fluidRow(
    
    column(6,
      selectInput(inputId = "option",
                  label = "Social Security Reform",
                  choices = c("Payable law" = "Payable law",
                              "Scheduled law" = "Scheduled law",
                              "BPC package" = "BPC package",
                              "Annual primary insurance amount" = "Annual primary insurance amount", 
                              "Basic minimum benefit" = "Basic minimum benefit",                               
                              "Increase benefits taxation" = "Increase benefits taxation",
                              "Cap spouse benefits" = "Cap spouse benefits",
                              "75 percent survivor benefit" = "75 percent survivor benefit",
                              "90 percent tax max" = "90 percent tax max",
                              "90% tax max and 13.4% payroll tax" = "90% tax max and 13.4% payroll tax",
                              "Reduce COLA" = "Reduce COLA",
                              "Chained-CPI COLA" = "Chained-CPI COLA",
                              "Cap COLA" = "Cap COLA", 
                              "Increase COLA" = "Increase COLA",                              
                              "Increase FRA" = "Increase FRA",
                              "Increase FRA and EEA" = "Increase FRA and EEA",
                              "$150,000 tax max" = "$150,000 tax max",
                              "$180,000 tax max" = "$180,000 tax max",
                              "Eliminate the tax max" = "Eliminate the tax Mmax",
                              "13.4 percent payroll tax" = "13.4 percent payroll tax",
                              "14.4 percent payroll tax" = "14.4 percent payroll tax",
                              "15.4 percent payroll tax" = "15.4 percent payroll tax")),           

      selectInput(inputId = "baseline",
                  label = "Baseline",
                  choices = c("Payable law" = "Payable law",
                              "Scheduled law" = "Scheduled law")),      
      
      selectInput(inputId = "income.tax.premium",
                  label = "Income, Tax, Premium, or Asset",
                  choices = c("Annuitized financial income" = "`Annuitized Financial Income`",
                              "Defined-benefit pension income" = "`DB Pension Income`",
                              "Earned income" = "`Earned Income`",
                              "Federal income tax" = "`Federal Income Tax`",
                              "Hospital Insurance program tax" = "`HI Tax`",
                              "Imputed rental income" = "`Imputed Rental Income`",
                              "Means- and non-means tested benefits" = "`Means+Nonmeans Benefits`",
                              "Medicare Part B premium" = "`Medicare Part B Premium`",
                              "Medicare surtax" = "`Medicare Surtax`",
                              "Net annuity income" = "`Net Annuity Income`",
                              "Net cash income" = "`Net Cash Income`",
                              "OASDI tax" = "`OASDI Tax`",
                              "Other family member income" = "`Other Family Member Income`",
                              "Own benefit" = "`Own Benefit`",
                              "Own earnings" = "`Own Earnings`",
                              "Gross annuity income" = "`Annuity Income`",
                              "Gross cash income" = "`Cash Income`",
                              "Dividend income" = "`Dividend Income`",
                              "Interest income" = "`Interest Income`",
                              "IRA withdrawal" = "`IRA Withdrawal`",
                              "Rental income" = "`Rental Income`",
                              "Social Security benefits" = "`Social Security Benefits`",
                              "Spouse benefit" = "`Spouse Benefit`",
                              "Spouse earnings" = "`Spouse Earnings`",
                              "Supplemental Security Income" = "`SSI`",
                              "State income tax" = "`State Income Tax`",
                              "Financial assets" = "`Financial Assets`",
                              "Retirement account assets" = "`Retirement Account Assets`",
                              "Total assets" = "`Total Assets`"))),

    column(6, 
      selectInput(inputId = "comparison",
                  label = "Comparison",
                  choices = c("Level" = "level",
                              "Dollar change" = "dollar.change")),
      
      selectInput(inputId = "group",
                  label = "Demographic",
                  choices = c("All individuals" = "All Individuals",
                              "Sex" = "Sex",
                              "Race or ethnicity" = "Race/Ethnicity",
                              "Education" = "Education",
                              "Marital status" = "Marital Status",
                              "Shared income quintile" = "Income Quintile",
                              "Shared lifetime earnings quintile" = "Lifetime Earnings Quintile")),
      
      selectInput(inputId = "scale",
                  label = "Scale",
                  choices = c("Per capita" = "per capita",
                              "Equivalent" = "equivalent")))),
  
  fluidRow(
    column(12,
           downloadButton('download_data', 'Download charted data')
    )
  ),
  
  br(),
  
    fluidRow(
    
    column(12,
    
    # Explanation of Social Security Reform
    
    htmlOutput("text_option")
    )
  ),
  
  fluidRow(
    
    column(12,
           
           # Explanation of Baseline
           
           htmlOutput("text_baseline")
    )
  ),
  
  fluidRow(
    
    column(12,
           
      # Explanation of Income, Tax, or Premium
      
      htmlOutput("text_income_tax_premium")
    )
    
  ),
  
  fluidRow(
    
    column(12, 
           
      # Explanation of Demographic
      
      htmlOutput("text_demographic")
    )
  ),
  
  fluidRow(
    
    column(12,
           
      # Explanation of Scales
      
      htmlOutput("text_scales")
    )
    
  ),
  
  br(),
  
  fluidRow(
    column(6,
           h3("About the data"),
           HTML("<p>The Urban Institute’s Dynamic Simulation of Income Model (DYNASIM) projects the size and characteristics (such as financial, health, and disability status) 
                of the US population for the next 75 years. Using the best and most recent data available, it helps sort out how profound social, economic, and demographic 
                shifts will likely affect older adults and their retirement as well astaxpayers, business, and government. The model can also show how outcomes would likely 
                evolve under changes to public policies, business practices, or individual behaviors.</p>"),
           HTML("<p><a href='https://www.urban.org/node/65826'>Read the DYNASIM primer</a></p>"),
           HTML("<p><a href='https://www.urban.org/research/publication/dynamic-simulation-income-model-dynasim-overview'>Review the DYNASIM documentation</a></p>")
           
           ),
    column(6,
           h3("Project Credits"),
           HTML("<p><i>This work was funded by the US Department of Labor’s Employee Benefits Security Administration. 
                We are grateful to them and to all our funders, who make it possible for Urban Institute to advance its mission.</i></p> 
                <p><i>The views expressed are those of the authors and should not be attributed to the Urban Institute, its trustees, 
                or its funders. Funders do not determine research findings or the insights and recommendations of our experts. 
                More information on our funding principles is available <a href='https://www.urban.org/support'>here</a>. 
                Read our terms of service <a href='https://www.urban.org/terms-service'>here</a></i>.</p>"),
           
           h5(HTML("<div class='credit-labels'>RESEARCH")),
           HTML("<div class='credit-names'><p><a href='https://www.urban.org/author/karen-e-smith'>Karen Smith</a></p></div>"),
           h5(HTML("<div class='credit-labels'>DESIGN AND DEVELOPMENT")),
           HTML("<div class='credit-names'><p><a href='https://www.urban.org/author/aaron-r-williams'>Aaron Williams</a>, <a href='https://www.urban.org/author/jerry-ta'>Jerry Ta</a>, and <a href='https://www.urban.org/author/benjamin-chartoff'>Ben Chartoff</a></p></div>"),
           h5(HTML("<div class='credit-labels'>EDITING")),
           HTML("<div class='credit-names'><p><a href='https://www.urban.org/author/michael-marazzi'>Michael Marazzi</a></p></div>"),
           h5(HTML("<div class='credit-labels'>WRITING")),
           HTML("<div class='credit-names'><p><a href = 'https://www.urban.org/author/karen-e-smith'>Karen Smith</a> and <a href='https://www.urban.org/author/aaron-r-williams'>Aaron Williams</a></p></div>"),
           
           HTML("Copyright &copy; <a href='https://www.urban.org/'>Urban Institute</a> 2017. View this project on <a href='https://github.com/urbaninstitute/dynasim-shiny1.git'>GitHub</a>.</p>")
           )
    ),
  
  tags$script(src = "activatePym.js")
)

server <- function(input, output) {
  
  options(shiny.sanitize.errors = FALSE)
  
  output$title <- renderText({
    
    comparison <- ifelse(input$comparison == "level", "", "Change in ")
    
    incomes.taxes <- if (input$income.tax.premium == "Annuitized Financial Income") {"annuitized financial income"} else
    if (input$income.tax.premium == "DB Pension Income") {"defined-benefit pension income"} else
    if (input$income.tax.premium == "Earned Income") {"earned income"} else
    if (input$income.tax.premium == "Federal Income Tax") {"federal income tax"} else
    if (input$income.tax.premium == "HI Tax") {"Hospital Insurance Program tax"} else
    if (input$income.tax.premium == "Imputed Rental Income") {"imputed rental income"} else
    if (input$income.tax.premium == "Means and Non-Means Tested Benefits") {"means- and non-means tested benefits"} else
    if (input$income.tax.premium == "Medicare Part B Premium") {"Medicare Part B premium"} else
    if (input$income.tax.premium == "Medicare Surtax") {"Medicare surtax"} else
    if (input$income.tax.premium == "Net Annuity Income") {"net annuity income"} else
    if (input$income.tax.premium == "Net Cash Income") {"net cash income"} else
    if (input$income.tax.premium == "OASDI Tax") {"OASDI tax"} else
    if (input$income.tax.premium == "Other Family Member Income") {"other family member income"} else
    if (input$income.tax.premium == "Own Benefit") {"own benefit"} else
    if (input$income.tax.premium == "Own Earnings") {"own earnings"} else
    if (input$income.tax.premium == "Annuity Income") {"gross annuity income"} else
    if (input$income.tax.premium == "Cash Income") {"gross cash income"} else
    if (input$income.tax.premium == "Dividend Income") {"dividend income"} else
    if (input$income.tax.premium == "Interest Income") {"interest income"} else
    if (input$income.tax.premium == "IRA Withdrawal") {"IRA withdrawal"} else
    if (input$income.tax.premium == "Rental Income") {"rental income"} else
    if (input$income.tax.premium == "Social Security Benefits") {"Social Security benefits"} else
    if (input$income.tax.premium == "Spouse Benefit") {"spouse benefit"} else
    if (input$income.tax.premium == "Spouse Earnings") {"spouse earnings"} else
    if (input$income.tax.premium == "SSI") {"Supplemental Security Income"} else
    if (input$income.tax.premium == "State Income Tax") {"state income tax"} else
    if (input$income.tax.premium == "Financial Assets") {"financial assets"} else
    if (input$income.tax.premium == "Retirement Account Assets") {"retirement account assets"} else
    if (input$income.tax.premium == "Total Assets") {"total assets"}      
    
    paste(comparison, as.character(input$year), input$scale, incomes.taxes)
    
  })
  
  output$subtitlea <- renderText({
    
    if (input$comparison == "level") {
      input$option
    } else {
      paste(input$option, "vs.", input$baseline)
    }
    
  })
  
  output$subtitleb <- renderText({
    
    if (input$group == "All Individuals") {"Everyone ages 62+, 2015 dollars"} else
    if (input$group == "Sex") {"Ages 62+ by sex, 2015 dollars"} else
    if (input$group == "Race/Ethnicity") {"Ages 62+ by race or ethnicity, 2015 dollars"} else
    if (input$group == "Education") {"Ages 62+ by dducation, 2015 dollars"} else
    if (input$group == "Marital Status") {"Ages 62+ by marital status, 2015 dollars"} else
    if (input$group == "Income Quintile") {"Ages 62+ by shared income quintile, 2015 dollars"} else
    if (input$group == "Lifetime Earnings Quintile") {"Ages 62+ by shared lifetime earnings quintile, 2015 dollars"}
  
    })

  data_subset <- reactive({
    if (input$comparison == "level") {  
      
      level %>%
        filter(option == input$option) %>%
        filter(group == input$group) %>%  
        filter(baseline == input$baseline) %>% 
        filter(scale == input$scale) %>%
        select_("subgroup", value = input$income.tax.premium, "percentile", "year")      
      
    } else if (input$comparison == "dollar.change") {
      
      dollar_change %>%
        filter(option == input$option) %>%
        filter(group == input$group) %>%  
        filter(baseline == input$baseline) %>% 
        filter(scale == input$scale) %>%
        select_("subgroup", value = input$income.tax.premium, "percentile", "year")     
      
    }
  }) 
  
  output$chart <- renderPlot({  

    # Calculate the maximum for the y-axis (because of the animation)
    y.max <- data_subset() %>%
      summarize(max = max(value))

    # Calculate the minimum for the y-axis (because of the animation)
    y.min <- data_subset() %>%
      summarize(min = min(value))
    
    y.min <- min(0, as.numeric(y.min))

    graphr <- function(origin, line.placement, line.color){
    
      filter(data_subset(), year == input$year) %>%  
        ggplot() +
          geom_bar(aes(x = percentile, y = value, fill = subgroup), position = "dodge", stat = "identity") +
          scale_y_continuous(limits = c(y.min, as.numeric(y.max)), labels = scales::dollar) +
          labs(caption = "DYNASIM3
                          Urban Institute",
               x = "Percentile",
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
  
    output$text_option <- renderText({
    
      as.character(
        option_text %>%
          filter(option == input$option) %>%
          select(text)
      )
    
    })
    
    output$text_income_tax_premium <- renderText({
      
      as.character(
        income_tax_premium_text %>%
          filter(income_tax_premium == input$income.tax.premium) %>%
          select(text)
      )
      
    })
    
    output$text_scales <- renderText({
      
      as.character(
        scale_text %>%
          filter(scale == input$scale) %>%
          select(text)
      )
  
    })
    
    output$text_baseline <- renderText({
      
      as.character(
        baseline_text %>%
          filter(baseline == input$baseline) %>%
          select(text)
      )
      
    })
    
    output$text_demographic <- renderText({
      
      as.character(
        demographic %>%
          filter(demographic == input$group) %>%
          select(description)
      )
      
    })
    
    output$text_have_income <- renderUI({
      
      percent <- percent_with_income %>%
        filter(option == input$option) %>%
        filter(year == input$year) %>%
        select_(value = input$income.tax.premium)   
      
      text_income <- as.character(income_tax_premium_text %>%
        filter(income_tax_premium == input$income.tax.premium) %>%
        select(label))
      
      if (input$comparison == "level") {
      HTML(paste("<div class='income-percent'>", as.character(round(percent * 100, 1)), "%", "</div>","<div class='income-text'>", "have", "<b>", text_income, "</b>", "</div>"))
      } else {}
        
    })
    
    output$download_data <- downloadHandler(
      filename = function() { paste0(input$option, '.csv') },
      content = function(file) {
        write_csv(data_subset(), file)
      }
    )
    
}
    
shinyApp(ui = ui, server = server)