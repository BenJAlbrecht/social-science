# Vacancies vs unhoused
# In short, why people are wrong
# we NEED more housing!
library(tidyverse)
library(tidycensus)

# All data uses 2022 ACS 1-year estimates
# reasons:
#         i) 2022 ACS 1-year is the most recent estimate.
#         ii) 2022 ACS 1-year has new detailed tables on
#             housing vacancy.
#########################################################
# Data labels
variable_labels <- load_variables(
  year = 2022,
  dataset = "acs1",
  cache = TRUE
)

##########################################################################################
##################################### MACRO LEVEL #######################################
##########################################################################################
# Starting with the nation as a whole

# ACS B25004 Vacancy Status
vacancy_status_us <- get_acs(
  geography = "us",
  table = "B25004",
  survey = "acs1",
  year = 2022,
  cache_table = TRUE
)

# ACS B25130 Other vacancy status
other_vacancy_status_us <- get_acs(
  geography = "us",
  table = "B25130",
  survey = "acs1",
  year = 2022,
  cache_table = TRUE
)

# Label our data!
vacancy_status_us <- left_join(vacancy_status_us,
                               variable_labels,
                               by = c("variable" = "name"))
other_vacancy_status_us <- left_join(other_vacancy_status_us,
                                     variable_labels,
                                     by = c("variable" = "name"))
# Stack the two datasets
vacancy_us <- bind_rows(vacancy_status_us, other_vacancy_status_us)

# CLEAN #
#########
# drop geoid
vacancy_us <- vacancy_us %>%
  select(-GEOID)

# re-label the total
vacancy_us[1, "label"] <- "Total"

# get rid of common label prefix (Vacancy Status)
vacancy_us$label[2:8] <- gsub("^Estimate!!Total:!!", "", vacancy_us$label[2:8])

# Drop duplicate row (other vacancy status includes 'other vacant' again)
vacancy_us <- vacancy_us %>%
  filter(row_number() != 9)

# Get rid of common label prefix (Other vacancy status)
vacancy_us$label[9:19] <- gsub("^Estimate!!Total:!!", "", vacancy_us$label[9:19])

# ANALYSIS #
############
# With the clean done, if we just looked at vacancies how many would we have?
naive_vacant <- vacancy_us[1, "estimate"]
format(round(naive_vacant / 1000000) * 1000000, scientific = FALSE)

# Remove others since we're removing those from total
vacancy_us <- vacancy_us %>%
  filter(label != "Other vacant")

# let's start removing units
remove_subsets_update_total <- function(df, subsets_to_remove) { # function for this!
  # index of total
  total_index <- which(df$label == "Total")
  # keep track of remaining total
  remaining_total <- df$estimate[total_index]
  
  # Loop through subsets and remove
  for (subset in subsets_to_remove) {
    subset_index <- which(df$label == subset)
    if (length(subset_index) > 0) {
      remaining_total <- remaining_total - df$estimate[subset_index]
      df <- df[-subset_index, , drop = FALSE] # drop the row
    }
  }
  df$estimate[total_index] <- remaining_total
  return(df)
}

# Remove sold/rented not occupied
sold_rented <- c("Rented, not occupied", "Sold, not occupied")
true_vacant <- remove_subsets_update_total(vacancy_us, sold_rented)

# Remove seasional / recreational / occassional, migrant workers
sznl <- c("For seasonal, recreational, or occasional use", "For migrant workers")
true_vacant <- remove_subsets_update_total(true_vacant, sznl)

# remove personal/family, extended absence
family_extended <- c("Personal/Family reasons", "Extended absence")
true_vacant <- remove_subsets_update_total(true_vacant, family_extended)

# remove about to be sold
preparing <- c("Preparing to rent/sell")
true_vacant <- remove_subsets_update_total(true_vacant, preparing)

# Truly unusable housing
unusable <- c("Needs repairs", "Currently being repaired/renovated", "Abandoned/Possibly condemned")
true_vacant <- remove_subsets_update_total(true_vacant, unusable)

# Specific use, legal proceedings
specific <- c("Specific use housing", "Legal proceedings")
true_vacant <- remove_subsets_update_total(true_vacant, specific)

# Sum our suitable units for nationalization
suitable <- c("For rent", "Foreclosure", "Held for storage of furniture", "Other", "For sale only")
seized_homes_us <- true_vacant %>%
  filter(label %in% suitable) %>%
  summarize(seizable_homes = sum(estimate))
true_vacant <- seized_homes_us$seizable_homes

# vacant homes per one person experiencing homelessness
homeless_people <- 582462

# naive vacant per person
naive_vacant / homeless_people

# "true" vacant per person
true_vacant / homeless_people

homeless_people / true_vacant
##########################################################################################
##################################### COUNTY LEVEL #######################################
##########################################################################################
# DATASETS

# ACS B25004 Vacancy Status
vacancy_status_county <- get_acs(
  geography = "county",
  table = "B25004",
  survey = "acs1",
  year = 2022,
  cache_table = TRUE
)

# ACS B25130 Other vacancy status
other_vacancy_status_county <- get_acs(
  geography = "county",
  table = "B25130",
  survey = "acs1",
  year = 2022,
  cache_table = TRUE
)

# Data labels
variable_labels <- load_variables(
  year = 2022,
  dataset = "acs1/subject",
  cache = TRUE
)

# Label our variables

  
  
  
  
  
  
  
  
  