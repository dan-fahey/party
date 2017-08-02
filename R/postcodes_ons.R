# libraries ----
library(tidyverse)
library(janitor)

# http://parlvid.mysociety.org/os/
"http://parlvid.mysociety.org/os/ONSPD_MAY_2017.zip" %>%
download.file("data/ONSPD_MAY_2017.zip")

unz(description = "data/ONSPD_MAY_2017.zip",
                  filename = "Data/ONSPD_MAY_2017_UK.csv") %>%
read_csv() %>%
clean_names() -> onspd_data

# http://geoportal.statistics.gov.uk/datasets/f52c48ea5cf2494b824d35f4be88f2ec_0
# Ward to Westminster Parliamentary Constituency to Local Authority District
# (December 2016) Lookup in the United Kingdom
constituency_codes <-
  read_csv(paste0("https://opendata.arcgis.com/datasets/",
                  "f52c48ea5cf2494b824d35f4be88f2ec_0.csv")) %>%
  clean_names() %>%
  select(pcon = pcon16cd, pcon16nm) %>%
  distinct()

trim_to_sector <- function(postcode) {
  substr(postcode, start = 1,
         stop = stringr::str_locate(postcode, " ")[1] + 1)
}

constituency_postcode_sectors <- onspd_data %>%
  # sample_n(1000) %>% # used for quick testing of the pipeline
  filter(!is.na(pcds) & !is.na(pcon)) %>%
  select(pcds, pcon) %>%
  rowwise() %>%
  mutate(postcode_sector = trim_to_sector(pcds)) %>%
  ungroup() %>%
  select(-pcds) %>%
  select(pcon, postcode_sector) %>%
  distinct() %>%
  arrange(pcon, postcode_sector) %>%
  inner_join(constituency_codes, by = "pcon") %>%
  select(pcon16nm, postcode_sector)

write_csv(constituency_postcode_sectors,
          "data/constituency_postcode_sectors.csv")
