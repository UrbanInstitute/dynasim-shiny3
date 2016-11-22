# Aaron Williams, Urban Institute Program on Retirement Policy

# This script reads five decades of four measures of mean income for older 
# Americans across from 
# "X:\programs\run912\Run5SaveOpt4\BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx".
# The script also cleans the data and turns them into a long data frame 
# formatted for ggplot2 

# Library and Source Statements
library(tidyverse)
library(readxl)

# Read unformatted data from Microsoft Excel
distribution <- read_excel("X:/programs/Run912/run5SaveOpt4/BPCtableShellsRun5SaveOpt4withSUPERTAX.xlsx", sheet = "$income Distribution by Sour", skip = 4, col_names = FALSE)


distribution2015 <- distribution %>%
  select(c(X0, X1, X6:X13))

names(distribution2015) <- c("group", "income.source", distribution2015[1, 3:ncol(distribution2015)])

distribution2015 <- distribution2015 %>%
  slice(-1) %>%
  mutate(year = 2015)

while (sum(is.na(distribution2015$group)) > 1) {

  distribution2015 <- distribution2015 %>%
    mutate(group = ifelse(is.na(group), lag(group), group))
  
  }

distribution2015 <- distribution2015 %>%
  filter(!is.na(P10) & P10 != "P10")



# boom 











distribution2025 <- distribution %>%
  select(X15:X27)

distribution2035 <- distribution %>%
  select()

distribution2045 <- distribution %>%
  select()

distribution2055 <- distribution %>%
  select()

distribution2065 <- distribution %>%
  select()