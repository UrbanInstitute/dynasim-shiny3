# Aaron Williams, Urban Institute Program on Retirement Policy

# This script reads five decades worth of income projections for different 
# demographic from the Microsoft Excel file
# "X:\programs\run912\Run5SaveOpt4\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx".
# The script also cleans the data and turns them into a long data frame 
# formatted for ggplot2 

# Library and Source Statements
library(tidyverse)
library(readxl)

# Read unformatted data from Microsoft Excel
distribution <- read_excel("X:/programs/Run912/run5SaveOpt4/BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx", sheet = "$income Distribution by Sour", skip = 4, col_names = FALSE)

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
    mutate(year = year)
  
  temp[566, 1] <- "Bottom Quintile (Per Capita Income)"
  temp[595, 1] <- "Quintile 2 (Per Capita Income)"
  temp[624, 1] <- "Quintile 3 (Per Capita Income)"
  temp[653, 1] <- "Quintile 4 (Per Capita Income)"
  temp[682, 1] <- "Top Quintile (Per Capita Income)"
  temp[750, 1] <- "Bottom Quintile (Lifetime Earnings)"
  temp[779, 1] <- "Quintile 2 (Lifetime Earnings)"
  temp[808, 1] <- "Quintile 3 (Lifetime Earnings)"
  temp[837, 1] <- "Quintile 4 (Lifetime Earnings)"
  temp[866, 1] <- "Top Quintile (Lifetime Earnings)"
  
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
  cleanBPC(column1 = "X0", column2 = "X1", column3 = "X6:X13", year = 2015),
  cleanBPC(column1 = "X15", column2 = "X16", column3 = "X20:X27", year = 2025),
  cleanBPC(column1 = "X29", column2 = "X30", column3 = "X34:X41", year = 2035),
  cleanBPC(column1 = "X43", column2 = "X44", column3 = "X48:X55", year = 2045),
  cleanBPC(column1 = "X57", column2 = "X58", column3 = "X62:X69", year = 2055),
  cleanBPC(column1 = "X71", column2 = "X72", column3 = "X76:X83", year = 2065)
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
  mutate(group = ifelse(subgroup %in% c("Bottom Quintile (Per Capita Income)", 
                                        "Quintile 2 (Per Capita Income)", 
                                        "Quintile 3 (Per Capita Income)",
                                        "Quintile 4 (Per Capita Income)", 
                                        "Top Quintile (Per Capita Income)"), 
                        "Per Capita Income Quintile", group)) %>%
  mutate(group = ifelse(subgroup %in% c("Bottom Quintile (Lifetime Earnings)",
                                        "Quintile 2 (Lifetime Earnings)",
                                        "Quintile 3 (Lifetime Earnings)",
                                        "Quintile 4 (Lifetime Earnings)",
                                        "Top Quintile (Lifetime Earnings)"), 
                        "Lifetime Earnings Quintile", group))

# Mutate numeric variables into class dbl
distribution <- mutate_each(distribution, funs(as.numeric), `Annuitized Financial Income`:`State Income Tax`)

# Write tidy data frame
write_csv(distribution, "data/dollar_change.csv")