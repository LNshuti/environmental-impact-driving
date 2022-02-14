#select products of interest(sitc_product_code == "transportation")
product2digit <-
 read_csv("dataverse_files/country_sitcproduct2digit_year.csv")

#Import country identifiers
countryweb <-
 "https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv"

#Get African country codes
country_codes <-
 read.csv(countryweb) %>%
 janitor::clean_names() %>%
 select(continent_code, country_code = three_letter_country_code) %>%
 rename(continent_name =continent_code)

#Combine trade data from 1966 to 2019
trade_data_all_years <-
 list.files("dataverse_files/", recursive = TRUE) %>%
 as_tibble() %>%
 filter(grepl(pattern = "partner_sitcproduct4digit", x = value)) %>%
 pull(value)

rwanda_trade_df <-
 trade_data_all_years %>%
 map(~data.table::fread(file = paste0("dataverse_files/",.x)) %>%
       janitor::clean_names() %>% as_tibble() %>%
       mutate_all(as.character) %>%
       filter(location_code == "RWA")) %>%
 bind_rows() %>% as_tibble() %>% filter(year > 1995)

saveRDS(object = rwanda_trade_df,
       "GreenAutoImpact.github.io/inputs/rwanda_trade_df_1995_2019.rds")