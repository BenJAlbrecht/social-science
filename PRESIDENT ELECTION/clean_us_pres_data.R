# SDMO election campaign treatment
library(tidyverse)


# Data cleaning
# Gets two-way vote data
#-------------------------------------------------------------------------------
dat <- read.csv("C:/Users/b-albrecht/Desktop/1976-2020-president.csv") %>% 
  as_tibble() %>% 
  select(
    year, state_po, candidate, candidatevotes, totalvotes, party_simplified, party_detailed 
  ) %>% 
  rename(
    state = state_po
  ) %>% 
  filter(
    state != "DC"
  )

# Only NY does a weird voting thing with multiple parties
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_no_ny <- dat %>% 
  filter(state != "NY",
         party_simplified %in% c("DEMOCRAT", "REPUBLICAN"),
         candidate != "OTHER",
         candidate != "") %>% 
  select(
    year, state, party_simplified, candidate, candidatevotes
  ) %>% 
  rename(
    party = party_simplified,
    votes = candidatevotes
  )

df_ny <- dat %>% 
  filter(state == "NY") %>% 
  group_by(year, state, candidate) %>% 
  mutate(
    votes = sum(candidatevotes)
  ) %>% 
  filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) %>% 
  select(
    year, state, party_simplified, candidate, votes
  ) %>% 
  rename(
    party = party_simplified
  )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Bring them back together
df <- bind_rows(df_no_ny, df_ny) %>% 
  arrange(year, state, party) %>% 
  filter(!(year == 2016 & state == "MD" & votes < 10000)) %>% 
  group_by(year, state) %>% 
  mutate(
    two_way_total = sum(votes)
  ) %>% 
  ungroup() %>% 
  mutate(
    percent = votes / two_way_total,
    win = ifelse(percent > 0.5, 1, 0)
  )

# Calculating margin % (D - R)
data_two_way <- df %>% 
  filter(party == "DEMOCRAT") %>% 
  left_join(
    df %>% filter(party == "REPUBLICAN"), by = c("year", "state"),
    suffix = c("_d", "_r")
  ) %>% 
  select(year, state, votes_d, percent_d, votes_r, percent_r, two_way_total_d, win_d) %>% 
  mutate(
    margin_abs = votes_d - votes_r,
    margin_per = percent_d - percent_r
  ) %>% 
  arrange(state, year) %>% 
  group_by(state) %>% 
  mutate(
    prev_margin_per = lag(margin_per),
    bg_flag = ifelse(prev_margin_per >= -0.04 & prev_margin_per <= 0.04, 1, 0)
  ) %>% 
  filter(
    year >= 2000
  )
# Didn't like the end result, going to look into defining swing-states / BG's
# using popular vote margins
#-------------------------------------------------------------------------------


# Data cleaning
# Gets OVERALL vote data
#-------------------------------------------------------------------------------
df_ <- df %>%
  select(-two_way_total, -percent) %>% 
  left_join(
    dat %>% select(year, state, totalvotes, party_simplified) %>% 
      filter(party_simplified %in% c("REPUBLICAN", "DEMOCRAT")) %>% 
      distinct(year, state, party_simplified, .keep_all = TRUE),
    by = c("year", "state", "party" = "party_simplified")
  ) %>% 
  mutate(
    percent = votes / totalvotes
  )

data_all <- df_ %>% 
  filter(party == "DEMOCRAT") %>% 
  left_join(
    df_ %>% filter(party == "REPUBLICAN"),
    by = c("year", "state"),
    suffix = c("_d", "_r")
  ) %>% 
  select(
    year, state, votes_d, percent_d, votes_r, percent_r, totalvotes_d, win_d
  ) %>% 
  mutate(
    margin_abs = votes_d - votes_r,
    margin_per = percent_d - percent_r
  ) %>% 
  arrange(state, year) %>% 
  group_by(state) %>% 
  mutate(
    prev_margin_per = lag(margin_per),
    bg_flag_last = ifelse(prev_margin_per >= -0.04 & prev_margin_per <= 0.04, 1, 0),
    bg_flag = ifelse(margin_per >= -0.04 & margin_per <= 0.04, 1, 0)
  ) 
#-------------------------------------------------------------------------------


# Quick and dirty SDMO
# Doesn't realllly seem to work...
#-------------------------------------------------------------------------------
sdmo <- data_all %>% 
  group_by(year, bg_flag) %>% 
  summarize(
    Y = mean(margin_per),
    .groups = "drop"
  ) %>% 
  pivot_wider(
    names_from = bg_flag,
    names_glue = "Y{bg_flag}",
    values_from = Y
  ) %>% 
  relocate(
    year, Y1, Y0
  ) %>% 
  mutate(
    SDO = Y1 - Y0
  )

ggplot(sdmo,
       aes(x = year, y = SDO)) +
  geom_bar(stat = "identity")
#-------------------------------------------------------------------------------





















  





?pivot_wider


  