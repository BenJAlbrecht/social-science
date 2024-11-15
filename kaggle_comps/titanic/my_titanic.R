# Titanic dataset Kaggle comp
library(tidyverse)
library(janitor)


# Read in data
#-------------------------------------------------------------------------------
test <- read.csv("C:/Users/b-albrecht/Desktop/titanic/test.csv") %>% 
  as_tibble()
train <- read.csv("C:/Users/b-albrecht/Desktop/titanic/train.csv") %>% 
  as_tibble()
#-------------------------------------------------------------------------------


# EDA for training dataz
#-------------------------------------------------------------------------------
# Drop some columns
df <- train %>% 
  select(-Name) %>% 
  clean_names() %>% 
  mutate(
    fem = case_when( # is 1 if female
      sex == "female" ~ 1,
      sex == "male" ~ 0
    ),
    age = case_when( # make infants age 0
      age < 1 ~ 0,
      TRUE ~ age
    ),
    age_est_flag = case_when( # is the age estimated or not?
      age %% 1 == 0 ~ 0,
      age %% 1 == 0.5 ~ 1
    ),
    age_tru = case_when( # keep integer part of the estimated ages
      age %% 1 == 0.5 ~ floor(age),
      TRUE ~ age
    )
  ) %>% 
  select(-sex)

# How many ages did we estimate?
df %>% group_by(age_est_flag) %>% summarize(n = n())

# Look at summaries of survived / didn't

# Survived #
############
survived <- df %>% 
  filter(survived == 1)

# 5 number summary...
survived %>% summary()

# Look at how many NA
survived %>% 
  summarize(na_ages = sum(is.na(age))) # 52 NA's

# Ticket class:
# 1) 136
# 2) 87
# 3) 119
survived %>% 
  group_by(pclass) %>% 
  summarize(n = n())

# Sex
# men) 109
# women) 233
survived %>% 
  group_by(fem) %>% 
  summarize(n = n())

# Embarked
# na) 2
# C) 93
# Q) 30
# S) 217
survived %>% 
  group_by(embarked) %>% 
  summarize(n = n())

############

# died #
########
died <- df %>% 
  filter(survived == 0)

# 5 number
died %>% summary()

# Ticket class
# 1) 80
# 2) 97
# 3) 372
died %>% 
  group_by(pclass) %>%
  summarize(n = n())
  
# Sex
# men) 468
# women) 81
died %>% 
  group_by(fem) %>% 
  summarize(n = n())

# Embarked
# C) 75
# Q) 47
# S) 427
died %>% 
  group_by(embarked) %>% 
  summarize(n = n())

# Compare distributions here
df %>% 
  ggplot(aes(x = age_tru, fill = as.factor(survived))) +
  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

df %>% 
  ggplot(aes(x = fare, fill = as.factor(survived))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")
#-------------------------------------------------------------------------------


# Additional feature engineering
# Can we fill in ages?
#-------------------------------------------------------------------------------
view(df$ticket) # ticket might be useful...

# Feature engineering for categorical data
df_ <- df %>% 
  select(-age) %>% # We have age_tru now and age_est_flag
  mutate(
    # categoricals for ticket class
    first_class = ifelse(pclass == 1, 1, 0),
    second_class = ifelse(pclass == 2, 1, 0),
    third_class = ifelse(pclass == 3, 1, 0),
    
    # categoricals for port of embarkation
    cherbourg = ifelse(embarked == "C", 1, 0),
    queenstown = ifelse(embarked == "Q", 1, 0),
    southampton = ifelse(embarked == "S", 1, 0)
  ) %>% 
  select(-pclass, -passenger_id)















