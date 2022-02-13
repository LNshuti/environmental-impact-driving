# Load necessary libraries and functions
library(aws.s3)
library(data.table)
library(tidygraph)
library(gt)
library(tidyverse)

data_bucket <- "harvard-dataverse-trade"

get_aws_files <- function(project_bucket = "harvard-dataverse-trade", prefix = "") {
  # Get AWS File Listing
  get_bucket(project_bucket, prefix = prefix) %>%
    purrr::transpose() %>%
    purrr::pluck("Key") %>%
    unlist() %>%
    tibble::as_tibble()
}

###########################################################
###########################################################
# Define helper function borrowed from John Graves' health-care-markets repo
# This function remove unnecessary axes from shape maps
remove_all_axes <- ggplot2::theme(
  axis.text = ggplot2::element_blank(),
  axis.line = ggplot2::element_blank(),
  axis.ticks = ggplot2::element_blank(),
  panel.border = ggplot2::element_blank(),
  panel.grid = ggplot2::element_blank(),
  axis.title = ggplot2::element_blank(),
  rect = element_blank(), 
  plot.background = element_blank()
)

# select products of interest(sitc_product_code == "transportation")
product2digit <- 
  read_csv("dataverse_files/country_sitcproduct2digit_year.csv")

# Import country identifiers 
countryweb <-
  "https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv"

# Get African country codes
country_codes <- 
  read.csv(countryweb) %>% 
  janitor::clean_names() %>% 
  select(continent_code, country_code = three_letter_country_code) %>%
  rename(continent_name =continent_code)

# Combine trade data from 1962 to 2019
trade_data_all_years <- 
  list.files("dataverse_files/", recursive = TRUE) %>%
  as_tibble() %>% 
  filter(grepl(pattern = "partner_sitcproduct4digit", x = value)) %>%
  mutate(year = str_replace(value, "country_partner_sitcproduct4digit_year_",
                            "")) %>%
  mutate(year = as.numeric(str_replace(year, ".csv",""))) %>%
  filter(year > 2016) %>%
  pull(value)

trade_df_all_years <-
  trade_data_all_years %>% 
  map(~data.table::fread(file = paste0("dataverse_files/",.x)) %>% 
        janitor::clean_names() %>% 
        mutate_all(as.character)
        ) %>% 
  bind_rows()
  
  