# 1. Housekeeping ------------------------------------------------------------ #
# Load libraries
library(tidyverse)       # for general data wrangling
library(readxl)          # to open excel files
library(purrr)           # for the map() functions
library(janitor)         # standard col. names
library(lubridate)       # dates
library(hms)             # times

# List the files
files <- list.files(path = "data-raw/beach-surveys/", 
                    pattern = ".xlsx", 
                    full.names = TRUE) # return the whole file path

files <- files[c(1, 2, 4, 5)]

# List the available sheets in the Excel files
# `files[1]` gets the sheets from the first file entry in `files`, you could
# change this to `files[2]` to do it for the second etc.
sheets <- excel_sheets(files[1]) 

# Which sheet do we want to open?
sheet <- sheets[2] # "Samples"
# ---------------------------------------------------------------------------- #

# 2. Open the data ----------------------------------------------------------- #
# An alternative to using `map()` is to use a for-loop, it takes longer and this
# becomes prohibitive for big datasets, but for this we can get away with it.

# First, we define a `NULL` object that will eventually be our dat
data <- NULL

# Then we initiate the loop - I'll explain what this does in person
for(i in 1:length(files)){
  
  # Each file has different weirdness associated with it, so we need to do some
  # different things to each so they can be joined. We can achieve this with an 
  # if-else statement
  if(i == 1){ # for the first file
    
    # Set the `col_types`
    col_types <- c("date", rep("guess", 5), rep("numeric", 9), 
                   "text", "guess")
    
    # Open the file
    temp <- read_xlsx(path = files[i],
                      sheet = sheet,
                      col_types = col_types)
    
    # Get rid of the 17th column
    temp <- temp %>%
      mutate(Notes = if_else(is.na(Notes), `...17`, Notes)) %>%
      select(-`...17`)
    
  } else if(i == 2) {
    
    col_types <- c("date", rep("guess", 5), rep("numeric", 8), 
                   rep("text", 2))
    
    temp <- read_xlsx(path = files[i],
                      sheet = sheet,
                      col_types = col_types)
    
    # Get rid of the 15th and 16th column
    temp <- temp %>%
      rename(Notes = `...15`) %>%
      mutate(Notes = if_else(is.na(Notes), `...16`, Notes)) %>%
      select(-`...16`)
    
  } else if(i == 3){
    
    col_types <- c("date", rep("guess", 3), rep("numeric", 9), "text")
    
    temp <- read_xlsx(path = files[i],
                      sheet = sheet,
                      col_types = col_types)
    
    # Fix up (recalculate the sail-float ratio)
    temp <- temp %>%
      mutate(`Ratio sail/float` = `Sail Length [cm]` / `Float Length [cm]`) %>%
      rename(Notes = `...14`) %>%
      select(-`...13`)
    
  } else if(i == 4) {
    
    col_types <- c("date", rep("guess", 4), "text", rep("numeric", 8), 
                   rep("text", 2))
    
    temp <- read_xlsx(path = files[i],
                      sheet = sheet,
                      col_types = col_types)
    
    temp <- temp %>% rename(Notes = `...15`) %>% select(-`...16`)
    
  }
  
  # Add `temp` to the empty data frame
  data <- data %>% bind_rows(temp)
  
}

# Make all column names standardized
data <- data %>% clean_names()

# Check formatting of handedness
unique(data$handedness)

# Now fix up capitalization
data <- data %>% mutate(handedness = str_to_sentence(handedness))

# Get rid of "?" - it is as good as an NA, so let's make it one
data <- data %>% mutate(handedness = if_else(handedness == "?", NA, handedness))

# Check dates are date objects and times look how we expect
class(data$date_jj_mm_aaaa)
unique(data$time)

# Remove the NA dates
data <- data %>% rename(date = date_jj_mm_aaaa) %>% filter(!is.na(date))

# We do no need the date (it's wrong too)
data <- data %>% 
  mutate(time = str_sub(time, # the string to manipulate
                        start = 12,
                        end = 20),
         time = as_hms(time))
# ---------------------------------------------------------------------------- #

# 3. Save your work ---------------------------------------------------------- #
write_csv(x = data, 
          file = "data-processed/bluebottle-observations/beach-survey-samples.csv")
# ---------------------------------------------------------------------------- #