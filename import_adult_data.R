library(tidyverse)

adult_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"

adult <- read.csv(adult_url, 
                  strip.white = TRUE, 
                  header = FALSE) %>%
  sample_n(1000) %>% # comment out this line to use the entire dataset
  rename("age" = V1,
         "workclass" = V2,
         "final_weight" = V3,
         "education" = V4,
         "education_num" = V5,
         "marital_status" = V6,
         "occupation" = V7,
         "relationship" = V8,
         "race" = V9,
         "sex" = V10,
         "capital_gain" = V11,
         "capital_loss" = V12,
         "hours_per_week" = V13,
         "native_country" = V14,
         "income" = V15) %>%
  mutate(
    income_bin = case_when(
      income == "<=50K" ~ 0,
      income == ">50K" ~ 1),
    female = case_when(
      sex == "Female" ~ 1,
      sex == "Male" ~ 0),
    white = case_when(
      race == "White" ~ 1,
      TRUE ~ 0
    )
  )

# train/test split
p_train <- 0.8
idx <- 1:nrow(adult)
train_idx <- sample(idx, 
       size= p_train*nrow(adult),
       replace = FALSE)
test_idx <- idx[!(idx %in% train_idx)]

adult_train <- adult[train_idx,]
adult_test <- adult[test_idx,]
