# Election regression analysis
library(tidyverse)
library(tidycensus)

################
# Read in data #
################
election_data <- read_csv("countypres_2000-2020.csv")




#########################
# Clean Election Tibble #
#########################

years <- c(2012, 2016) # years of interest
cands <- c("BARACK OBAMA", "MITT ROMNEY",
           "HILLARY CLINTON", "DONALD TRUMP") # candidates of interest

# process election data tibble
elect <- election_data %>%
  filter(year %in% years) %>%
  filter(candidate %in% cands) %>%
  select(-office, -totalvotes, -version, -mode) %>%
  group_by(year, state_po, county_fips) %>%
  mutate(two_party_vote = sum(candidatevotes)) %>%
  filter(candidate == "BARACK OBAMA" |
           candidate == "HILLARY CLINTON") %>%
  mutate(vote_percent = candidatevotes / two_party_vote) %>%
  select(-candidate, -party, -candidatevotes, -two_party_vote, -state)

# pivot data
elect_pivoted <- elect %>%
  pivot_wider(names_from = "year", values_from = "vote_percent") %>%
  rename(vote2012 = `2012`, vote2016 = `2016`) %>%
  select(county_name, state_po, county_fips, vote2012, vote2016) %>%
  mutate(county_fips = as.numeric(county_fips))

filter(elect_pivoted, county_fips ==46113)
filter(white_educ, geoid ==36000)

##################
# Education data #
##################
education <- get_acs(
  geography = "county",
  table = "S1501",
  survey = "acs5",
  year = 2019,
  cache_table = TRUE
)

# label the data
variables <- load_variables(
  year = 2019,
  dataset = "acs5/subject",
  cache = TRUE
)

education <- left_join(education, variables,
                       by = c("variable" = "name"))

# White alone, bachelor's degree or higher
white_educ <- education %>%
  filter(variable == "S1501_C02_033") %>%
  select(-label, -concept) %>%
  separate(NAME, into = c("county", "state"), sep = ", ") %>%
  rename(geoid = GEOID) %>%
  select(-variable)

# make look-up tibble to add abbreviations to the educ data
state_po_lookup <- tibble(state_po = state.abb, state = state.name)

# add pos to white_educ, rename stuff and clean
white_educ <- left_join(white_educ, state_po_lookup,
                        by = "state")

white_educ <- white_educ %>%
  select(state_po, state, county, geoid, estimate, moe) %>%
  mutate(geoid = as.numeric(geoid))




##############
# Merge Data #
##############
data <- elect_pivoted %>%
  left_join(white_educ, by = c("county_fips" = "geoid")) %>%
  select(state_po.x, county, county_fips, vote2012, vote2016, estimate) %>%
  rename(
    state_po = state_po.x,
    county_name = county,
    white_educ = estimate
  )
mean(data$white_educ)

filter(data, is.na(white_educ))
























