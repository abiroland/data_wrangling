# Summer project 1
# Data cleaning 
# By Roland 

# Prep -------------------------------------------------------------------------
library(tidyverse)
library(janitor)

# importing the dataset --------------------------------------------------------
dat <- read_csv("data/messy_data.csv")

# understanding the structure of the dataset------------------------------------
head(dat)
colnames(dat)
dat1 <- clean_names(dat)
colnames(dat1)

# cleaning and transforming the dataset-----------------------------------------

#transpose the dataset
newdat1 <- dat1 %>%
  t() %>% 
  as.data.frame() %>%
  mutate(
    segment = row.names(.)
  )  %>%
  row_to_names(row_number = 1) %>%
  select(-"Order ID", -"Grand Total") %>%
  remove_rownames(.) %>%
  rename(
    "ship_mode" = `Ship Mode>>`
  )

colnames(newdat1)

# convert columnames to rows
newdat2 <- newdat1 %>%
  pivot_longer(
    cols = c(-ship_mode, -segment),
    names_to = "order_id",
    values_to = "prices",
    names_repair = "minimal"
  ) %>%
  na.omit()

# further cleaning 
newdat3 <- newdat2 %>% 
  mutate(new_segment = 
    case_when(
      segment %in% c('x3', "x4", "x5", "consumer")   ~ "consumer",
      segment %in% c("x8", "x9", "x10", "corporate")  ~ "corporate",
      segment %in% c("x13", "x14", "x15", "home_office") ~ "home_office",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    country = str_extract(order_id, "([A-Z]+)"),
    year = str_extract(order_id, "\\d+"),
    orderid = str_extract(order_id, "\\-\\d\\d\\d\\d\\d\\d"),
    orderid = str_remove_all(orderid, "\\-")
  ) %>%
  select(-order_id, -segment)

newdat3 %>%
  select(ship_mode) %>%
  unique()

