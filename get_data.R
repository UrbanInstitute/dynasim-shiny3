# Aaron Williams, Urban Institute Program on Retirement Policy

# This script reads mean income data for 64 groups, across 4 measures, in 6 
# different decades, for user-defined reform options, in two scales. It then 
# calculates percent and dollar change against two different baselines. 
# The script also cleans the data and turns them into a long data frame 
# formatted for ggplot2 

# Library, Source, and Options Statements
library(tidyverse)
library(readxl)

options(scipen = 999)

# Read df with links to the Excel sheets with the mean income data
files <- read_excel("options_guide.xlsx") %>%
  select(option, scale, link)

# Create functions that clean the clunky Excel files
distributionScrapeR <- function(link, bpcpackage) {

  distribution <- read_excel(link, 
                             sheet = "income Distribution by Source", 
                             skip = 4, col_names = FALSE)
  
  # Turn the data into untidy data frames for each year with no missing values
  cleanBPC <- function(column1, column2, column3, year) {
    # Cleans raw data frame from read_excel and returns an untidy data frame
    #
    # Args: column names and year
    #
    # Returns: 
    
    temp <- distribution %>%
      select_(column1, column2, column3)
    
    names(temp) <- c("subgroup", "income.source", temp[1, 3:ncol(temp)])
    
    temp <- temp %>%
      slice(-1) %>%
      mutate(year = year) %>%
      mutate(income.source = gsub("Per Capita ", "", income.source)) %>%
      mutate(income.source = gsub("Equivalent ", "", income.source)) %>%
      mutate(income.source = gsub("tax", "Tax", income.source))
    
    if (bpcpackage == TRUE) {
      temp[566, 1] <- "Bottom Quintile (Income)"
      temp[595, 1] <- "Quintile 2 (Income)"
      temp[624, 1] <- "Quintile 3 (Income)"
      temp[653, 1] <- "Quintile 4 (Income)"
      temp[682, 1] <- "Top Quintile (Income)"
      temp[750, 1] <- "Bottom Quintile (Lifetime Earnings)"
      temp[779, 1] <- "Quintile 2 (Lifetime Earnings)"
      temp[808, 1] <- "Quintile 3 (Lifetime Earnings)"
      temp[837, 1] <- "Quintile 4 (Lifetime Earnings)"
      temp[866, 1] <- "Top Quintile (Lifetime Earnings)"  
    }
    
    if (bpcpackage == FALSE) {
      temp[548, 1] <- "Bottom Quintile (Income)"
      temp[576, 1] <- "Quintile 2 (Income)"
      temp[604, 1] <- "Quintile 3 (Income)"
      temp[632, 1] <- "Quintile 4 (Income)"
      temp[660, 1] <- "Top Quintile (Income)"
      temp[726, 1] <- "Bottom Quintile (Lifetime Earnings)"
      temp[754, 1] <- "Quintile 2 (Lifetime Earnings)"
      temp[782, 1] <- "Quintile 3 (Lifetime Earnings)"
      temp[810, 1] <- "Quintile 4 (Lifetime Earnings)"
      temp[838, 1] <- "Top Quintile (Lifetime Earnings)"    
    }
    
    while (sum(is.na(temp$subgroup)) > 1) {
      
      temp <- temp %>%
        mutate(subgroup = ifelse(is.na(subgroup), lag(subgroup), subgroup))
      
    }
    
    temp <- temp %>%
      filter(!is.na(P10) & P10 != "P10") %>%
      filter(!is.na(income.source)) %>%
      mutate_all(funs(trimws)) %>%
      distinct()
    
    return(temp)
    
  }
  
  # TODO(awunderground): Add check to see if duplicated rows are only the repeated
  # charts in the Excel files
  
  distribution <- bind_rows(
    cleanBPC(column1 = "X0", column2 = "X1", column3 = "X5:X13", year = 2015),
    cleanBPC(column1 = "X15", column2 = "X16", column3 = "X19:X27", year = 2025),
    cleanBPC(column1 = "X29", column2 = "X30", column3 = "X33:X41", year = 2035),
    cleanBPC(column1 = "X43", column2 = "X44", column3 = "X47:X55", year = 2045),
    cleanBPC(column1 = "X57", column2 = "X58", column3 = "X61:X69", year = 2055),
    cleanBPC(column1 = "X71", column2 = "X72", column3 = "X75:X83", year = 2065)
  )
  
  # create tidy data frame
  distribution <- distribution %>%
    gather(key = percentile, value = value, -subgroup, -income.source, -year) %>%
    spread(income.source, value) %>%
    arrange(subgroup, year, percentile)
  
  # Create a variable for groups
  distribution <- distribution %>%
    mutate(subgroup = ifelse(subgroup == "Male", "Males", subgroup)) %>%
    mutate(subgroup = ifelse(subgroup == "High School Graduate", "High School Graduates", subgroup)) %>%
    mutate(group = ifelse(subgroup == "All Individuals", "All Individuals", NA)) %>%
    mutate(group = ifelse(subgroup %in% c("Males", "Females"), "Sex", group)) %>%
    mutate(group = ifelse(subgroup %in% c("High School Dropouts",
                                          "High School Graduates", 
                                          "Some College", "College Graduates"),
                          "Education", group)) %>%
    mutate(group = ifelse(subgroup %in% c("African-Americans", 
                                          "Hispanics",
                                          "White, Non-Hispanics"),
                          "Race/Ethnicity", group)) %>%
    mutate(group = ifelse(subgroup %in% c("Never Married Individuals",
                                          "Divorced Individuals",
                                          "Married Individuals",
                                          "Widowed Individuals"),
                          "Marital Status", group)) %>%
    mutate(group = ifelse(subgroup %in% c("Bottom Quintile (Income)", 
                                          "Quintile 2 (Income)", 
                                          "Quintile 3 (Income)",
                                          "Quintile 4 (Income)", 
                                          "Top Quintile (Income)"), 
                          "Per Capita Income Quintile", group)) %>%
    mutate(group = ifelse(subgroup %in% c("Bottom Quintile (Lifetime Earnings)",
                                          "Quintile 2 (Lifetime Earnings)",
                                          "Quintile 3 (Lifetime Earnings)",
                                          "Quintile 4 (Lifetime Earnings)",
                                          "Top Quintile (Lifetime Earnings)"), 
                          "Lifetime Earnings Quintile", group))
  
  # Mutate numeric variables into class dbl and simplify quintiles
  distribution <- mutate_each(distribution, funs(as.numeric), `Annuitized Financial Income`:`State Income Tax`) %>%
    mutate(subgroup = gsub(" \\(Lifetime Earnings\\)", "", subgroup)) %>%
    mutate(subgroup = gsub(" \\(Per Capita Income\\)", "", subgroup))
  
  # Drop the 99th percentile
  distribution <- distribution %>%
    filter(percentile != "P99")
  
  return(distribution)
}



final.distribution <- tibble()

for (i in 1:36) {

  distribution <- distributionScrapeR(as.character(files[i, 3]), bpcpackage = FALSE)
  
  distribution <- distribution %>%
    mutate(option = as.character(files[i, 1])) %>%
    mutate(scale = as.character(files[i, 2]))
  
  final.distribution <- bind_rows(final.distribution, distribution)
  
}

# Add the bpc packages which have different row numbers because of the basic minimum benefit
for (i in 37:38) {
  
  distribution <- distributionScrapeR(as.character(files[i, 3]), bpcpackage = TRUE)
  
  distribution <- distribution %>%
    mutate(option = as.character(files[i, 1])) %>%
    mutate(scale = as.character(files[i, 2]))
  
  final.distribution <- bind_rows(final.distribution, distribution)
  
}
# Should be 43,776 observations
# 24 subgroups * 6 years * 8 percentiles * 38 options

# Spread the data into long format
final.distribution <- final.distribution %>%
  gather(c(`Annuitized Financial Income`:`State Income Tax`, BMB), key = "incomes.taxes",value = "value")
# Should be 1,225,728 observations
# 24 subgroups * 6 years * 8 percentiles * 38 options * 28 incomes/taxes/premiums

# Create a baseline data frame
baselines <- final.distribution %>%
  filter(option == "Scheduled Law" | option == "Payable Law") %>%
  rename(baseline.value = value, baseline.type = option)
# Should be 129,024 observations
# 1,225,728 * 4 / 38

# Create a options data frame
options <- final.distribution %>%
  filter(option != "Scheduled Law" & option != "Payable Law")
# Should be 1,096,704 observations
# 1,225,728 * 34 / 38

final.distribution <- left_join(options, baselines, by = c("subgroup", "year", "percentile", "group", "scale", "incomes.taxes"))
# should double

# Calculate the dollar and percent changes
final.distribution <- final.distribution %>%
  mutate(dollar.change = value - baseline.value) %>%
  select(-baseline.value)

# Clean up baselines so it matches final.income
baselines <- baselines %>%
  rename(value = baseline.value) %>%
  rename(option = baseline.type) %>%
  mutate(dollar.change = 0) %>%
  mutate(baseline.type = option)

# Combine the baselines (with zeroes for changes) and the options
final.distribution <- union(final.distribution, baselines) %>%
  rename(baseline = baseline.type, level = value) %>%
  gather(level, dollar.change, key = "comparison", value = "value") %>%
  spread(key = incomes.taxes, value = value)
# Should be 248,832
# 24 subgroups * 6 years * 8 percentiles * (38 options - 2) * 2 scales * 2 baselines * 2 comparisons

rm(files, distribution, options, baselines)

# Write tidy data frame
write_csv(final.distribution, "data/distributions.csv")
