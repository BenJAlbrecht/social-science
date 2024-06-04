## Scraping polls ###
library(tidyverse)  #
library(rvest)      #
#####################

# Make series of URLs to use to scrape #
########################################
pres_elect_yrs <- seq(from = 1976, to = 2020, by = 4)

base_url <- "https://ropercenter.cornell.edu/how-groups-voted-"

urls <- paste0(base_url, pres_elect_yrs)
########################################


# Function to scrape polls #
############################
scrape_exit_polls <- function(link) {
  page <- read_html(link)
  
  exit_polls <- html_element(page, "#main-content > div:nth-child(3) > div > div > div > table") %>%
    html_table()
  
  return(exit_polls)
}
############################


# Apply function #
##################
pres_exit_polls <- list()

for (x in urls) {
  poll <- scrape_exit_polls(x)
  
  pres_exit_polls[[length(pres_exit_polls) + 1]] <- poll
}
##################