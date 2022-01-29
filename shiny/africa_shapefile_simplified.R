library(aws.s3)
library(tidyverse)
library(spData) # Spatial data from the World Bank
library(sf)


# Personal AWS data store
bucket_name <- "leonce.nshuti.compute"

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

countryweb <-
  "https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv"

# Get country codes
continents_except_oceania <- 
  read.csv(countryweb) %>% 
  janitor::clean_names() %>% 
  mutate(continent_code = ifelse(continent_name == "North America", "NA", continent_code)) %>% 
  filter(continent_code != "OC") %>% 
  select(continent_name, continent_code, two_letter_country_code) %>% 
  # Add country specific demographic information
  left_join(worldbank_df, by = c("two_letter_country_code" = "iso_a2"))

# World map
wolrd_map <-
  world %>% 
  filter(!(continent %in% c("Oceania", "Seven seas (open ocean)", "Antarctica"))) %>% 
  filter(!is.na(iso_a2)) %>% 
  left_join(continents_except_oceania, by = c("iso_a2" = "two_letter_country_code"))

simplified_world_shape <- 
  ggplot(wolrd_map) +
  geom_sf(aes(geometry = geom, fill = continent)) + 
  remove_all_axes +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442")) + 
  labs(fill = 'Continent', caption = "Excluding Oceania, Antarctica and the Seven seas.") + 
  theme(plot.caption = element_text(hjust = 0, face= "italic"))

ggsave(simplified_world_shape,
       filename = file.path(here::here(), "LNSHUTI.github.io/plots/simplified_world_shape.png")
)
