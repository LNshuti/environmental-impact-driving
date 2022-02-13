# Load necessary libraries and functions
library(finnts)
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

my_theme <- theme_classic(base_family = "Times") +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "gainsboro")) +
  theme(plot.background = element_rect(fill = "gainsboro")) +
  theme(panel.background = element_rect(fill = "gainsboro"))

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

sitc_xwalk <- 
  readxl::read_xls("dataverse_files/SITCProducts.xls") %>% 
  janitor::clean_names()

# Combine trade data from 1962 to 2019
trade_data_all_years <- 
  list.files("dataverse_files/", recursive = TRUE) %>%
  as_tibble() %>% 
  filter(grepl(pattern = "partner_sitcproduct4digit", x = value)) %>%
  pull(value)

rwanda_trade_df <-
  trade_data_all_years %>% 
  map(~data.table::fread(file = paste0("dataverse_files/",.x)) %>% 
        janitor::clean_names() %>% 
        as_tibble() %>% 
        mutate_all(as.character) %>%
        filter(location_code == "RWA")
        ) %>% 
  bind_rows() %>% 
  as_tibble() %>%
  filter(year > 1998)

saveRDS(object = rwanda_trade_df,
        "GreenAutoImpact.github.io/inputs/rwanda_trade_df_1999_2019.rds")

rwa_sum <-
  #rwanda_trade_df %>% 
  read_rds("GreenAutoImpact.github.io/inputs/rwanda_trade_df_1999_2019.rds") %>%
  left_join(sitc_xwalk, by = c("sitc_product_code"= "product_code")) %>%
  rename(product_name = product_description) %>%
  mutate(product_name = str_to_lower(product_name)) %>%
  mutate(product_name = ifelse(grepl("gold", product_name),"gold non-monetary", product_name)) %>% 
  mutate(product_name = ifelse(grepl("cement", product_name),"cement",product_name)) %>% 
  mutate(product_name = ifelse(grepl("medicaments", product_name),"medicaments",product_name)) %>% 
  mutate(product_name = ifelse(grepl("vaccines", product_name),"medicaments",product_name)) %>% 
  
  mutate(product_name = ifelse(grepl("aircraft", product_name),"aircraft",product_name)) %>%
  mutate(product_name = ifelse(grepl("aircrft", product_name),"aircraft",product_name)) %>%
  mutate(product_name = ifelse(grepl("special transactions", product_name),
                               "special transactions",product_name)) %>%
  mutate(import_value = as.numeric(import_value)) %>%
  group_by(year, product_name) %>%
  summarize(import_value = sum(import_value, na.rm = T)) %>% 
  arrange(desc(import_value)) %>%
  ungroup() %>% 
  mutate(year =  as.Date(as.character(year), format = "%Y"))

rwa_sum_rank <-
  rwa_sum %>%
  mutate(year = lubridate::year(year)) %>%
  filter(year > 2010) %>%
  # select(-sitc_product_code, -nomenclature_code, -tier) %>%
  group_by(year) %>%  
  arrange(year, desc(import_value)) %>%  
  # assign ranking
  mutate(rank = 1:n()) %>%  
  filter(rank <= 10) %>%
  ungroup() %>%
  gt::gt()

library(gt)

gtsave(rwa_sum_rank, "GreenAutoImpact.github.io/plots/rwa_sum_rank.tex")


rwa_sum_rank_plt <- 
  rwa_sum_rank %>%  
  ggplot() +  
  aes(xmin = 0,  xmax = import_value / 1000000) +  
  aes(ymin = rank - .45, ymax = rank + .45, y = rank) +  
  facet_wrap(~ year) +  
  geom_rect(alpha = .7) +  
  aes(fill = product_name) +  
  scale_fill_viridis_d(option = "magma", direction = -1) +
  labs(x = 'Imports (millions)', fill = "Product category") +
  scale_x_continuous(  
    limits = c(0, 800),  
    breaks = c(0, 400,  800)) +
  my_theme

ggsave(rwa_sum_rank_plt,
       filename = "GreenAutoImpact.github.io/plots/rwanda_imports.png",
       width = 6, 
       height = 4)



  

