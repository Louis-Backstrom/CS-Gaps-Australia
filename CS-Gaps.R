#### 0. Initialization ---------------------------------------------------------

library(tidyverse)
library(sf)
library(auk)
library(readxl)
library(lubridate)

`%notin%` <- Negate(`%in%`)
sf_use_s2(TRUE)

source("Data-Paths.R") # this will read in all the directories where the datasets are stored locally (not on this repo)

#### 1. Set Up -----------------------------------------------------------------

taxonomy_fixer <- read_xlsx("Data/TaxonomyFixerMaster.xlsx", sheet = 2) # this is the key to resolving taxonomic conflicts
threat <- read_xlsx("Data/ThreatMaster.xlsx") %>% # this contains all APAB listings under the same taxonomy
  select(species_binomial, threat_status)

wlab <- read_xlsx(wlab_v4pt2_path) # Birdlife Australia Working List of Australian Birds v4.2
aus <- read_sf(ne_10m_path) %>% # Natural Earth 10m_cultural (ne_10m_admin_0_countries)
  filter(ADMIN == "Australia") %>% 
  st_transform(crs = 4326)
tsi <- read_sf(tsi_zone_path) %>% # QLD Government Maritime Safety Torres Zone
  st_transform(crs = 4326)
aus <- st_difference(aus, tsi) %>% 
  st_intersection(., st_as_sfc(st_bbox(st_as_sf(data.frame(
    latitude = c(-44, -44, -10, -10), longitude = c(112, 154, 112, 154)), 
    coords = c("longitude", "latitude"), crs = 4326)))) %>% 
  select(geometry)
rm(tsi)

all_species <- taxonomy_fixer %>% 
  filter(CLASS %in% c("Normal", "Shorebird")) %>% 
  pull(OUT) %>% 
  unique()

##### 1.1. eBird ---------------------------------------------------------------

if (file.exists("Data/ebird_cleaned.txt") == TRUE) {
  rebuild_ebird <- askYesNo(msg = "Do you want to rebuild the eBird Dataset?")
} else {
  rebuild_ebird <- TRUE
}

if (rebuild_ebird == TRUE) {
  
  ebird_default <- read_ebd(ebd_path) %>% # eBird dataset for Australia, version Dec-2022
    select("checklist_id", "all_species_reported", "scientific_name", "observation_date", "latitude", "longitude", "exotic_code") %>% 
    filter(exotic_code %notin% c("X"),
           all_species_reported == TRUE) %>%
    left_join(., taxonomy_fixer, by = c("scientific_name" = "IN")) %>% 
    rename("species_binomial" = "OUT") %>% 
    filter(CLASS %in% c("Normal", "Shorebird")) %>% 
    select(-all_species_reported, -scientific_name, -exotic_code, -CLASS, -WLAB, -Population)
  
  ebird_sensitive <- read_ebd(ebd_sensitive_path) %>% # sensitive species (OBP, NP, PW)
    select("checklist_id", "all_species_reported", "scientific_name", "observation_date", "latitude", "longitude", "exotic_code") %>% 
    filter(exotic_code %notin% c("X"),
           all_species_reported == TRUE) %>%
    left_join(., taxonomy_fixer, by = c("scientific_name" = "IN")) %>% 
    rename("species_binomial" = "OUT") %>% 
    filter(CLASS %in% c("Normal", "Shorebird")) %>% 
    select(-all_species_reported, -scientific_name, -exotic_code, -CLASS, -WLAB, -Population)
  
  ebird <- bind_rows(ebird_default, ebird_sensitive) %>% 
    mutate(year = year(observation_date))
  
  loc <- ebird %>% 
    select(latitude, longitude) %>% 
    distinct() %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% 
    mutate(in_aus = as.logical(lengths(st_intersects(., aus)))) %>% 
    st_drop_geometry()
  
  ebird <- ebird %>% 
    left_join(., loc) %>% 
    filter(in_aus == TRUE) %>% 
    select(-in_aus)
  
  rm(ebird_default, ebird_sensitive, loc)
  
  write_tsv(ebird, "Data/ebird_cleaned.txt")
  
} else {
  
  ebird <- read_tsv("Data/ebird_cleaned.txt")
  
} 

##### 1.2. Birdata --------------------------------------------------------------

if (file.exists("Data/birdata_cleaned.txt") == TRUE) {
  rebuild_birdata <- askYesNo(msg = "Do you want to rebuild the Birdata Dataset?")
} else {
  rebuild_birdata <- TRUE
}

if (rebuild_birdata == TRUE) {
  
  birdata <- read_csv(birdata_path) %>% # dataset requested from Birdata team directly
    select("checklist_id" = "Survey.ID", "all_species_reported" = "All.Species.Recorded",
           "scientific_name" = "Scientific.Name", "observation_date" = "Start.Date",
           "latitude" = "Latitude", "longitude" = "Longitude") %>% 
    filter(all_species_reported == "Yes") %>% 
    left_join(., taxonomy_fixer, by = c("scientific_name" = "IN")) %>% 
    rename("species_binomial" = "OUT") %>% 
    filter(CLASS %in% c("Normal", "Shorebird")) %>% 
    select(-all_species_reported, -scientific_name, -CLASS, -WLAB, -Population) %>% 
    mutate(year = year(observation_date))
  
  loc <- birdata %>% 
    select(latitude, longitude) %>% 
    distinct() %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% 
    mutate(in_aus = as.logical(lengths(st_intersects(., aus)))) %>% 
    st_drop_geometry()
  
  birdata <- birdata %>% 
    left_join(., loc) %>% 
    filter(in_aus == TRUE) %>% 
    select(-in_aus)
  
  rm(loc)
  
  write_tsv(birdata, "Data/birdata_cleaned.txt")
  
} else {
  
  birdata <- read_tsv("Data/birdata_cleaned.txt")
  
} 

##### 1.3. ABG ------------------------------------------------------------------

if (file.exists("Data/abg_poly_cleaned.gpkg") == TRUE) {
  rebuild_abg <- askYesNo(msg = "Do you want to rebuild the ABG Maps?")
} else {
  rebuild_abg <- TRUE
}

if (rebuild_abg == TRUE) {
  
  sf_use_s2(FALSE)
  
  abg <- read_sf(abg_path) %>% # provided by Glen Ehmke
    left_join(., filter(wlab, TaxonLevel == "sp"), by = c("sp_id" = "SpID")) %>%
    filter(rnge %in% c(1, 5, 6, 7)) %>% 
    left_join(., taxonomy_fixer, by = c("TaxonScientificName" = "IN")) %>% 
    rename("species_binomial" = "OUT") %>% 
    filter(CLASS %in% c("Normal", "Shorebird")) %>% 
    select(species_binomial) %>% 
    drop_na(species_binomial) %>% 
    group_by(species_binomial) %>% 
    summarise() %>% 
    st_cast() %>% 
    st_transform(crs = 4326) %>% 
    st_intersection(., aus) %>% 
    st_make_valid()
  
  sf_use_s2(TRUE)
  
  write_sf(abg, "Data/abg_poly_cleaned.gpkg")
  
} else {
  
  abg <- read_sf("Data/abg_poly_cleaned.gpkg")
  
}

##### 1.4. BLI ------------------------------------------------------------------

if (file.exists("Data/bli_poly_cleaned.gpkg") == TRUE) {
  rebuild_bli <- askYesNo(msg = "Do you want to rebuild the BLI Maps?")
} else {
  rebuild_bli <- TRUE
}

if (rebuild_bli == TRUE) {
  
  sf_use_s2(FALSE)
  
  bli <- read_sf(bli_path) %>% # Bird species distribution maps of the world version 2020.1
    filter(origin %in% c(1, 2, 3, 5),
           presence == 1) %>% 
    left_join(., taxonomy_fixer, by = c("binomial" = "IN")) %>% 
    rename("species_binomial" = "OUT") %>% 
    filter(CLASS %in% c("Normal", "Shorebird")) %>% 
    select(species_binomial) %>% 
    drop_na(species_binomial) %>% 
    group_by(species_binomial) %>% 
    summarise() %>% 
    st_cast() %>% 
    st_transform(crs = 4326) %>% 
    st_intersection(., aus) %>% 
    st_make_valid()
  
  sf_use_s2(TRUE)
  
  write_sf(bli, "Data/bli_poly_cleaned.gpkg")
  
} else {
  
  bli <- read_sf("Data/bli_poly_cleaned.gpkg")
  
}

#### 2. Analysis ---------------------------------------------------------------

sf_use_s2(FALSE)

add_maps <- bind_rows(abg, bli) %>% 
  group_by(species_binomial) %>% 
  summarise() %>% 
  st_cast() %>% 
  st_make_valid()

add_obs <- birdata %>% 
  mutate(checklist_id = as.character(checklist_id)) %>% 
  bind_rows(., ebird)

add_loc_sf <- add_obs %>% 
  group_by(latitude, longitude) %>% 
  summarise(n_checklists = n_distinct(checklist_id)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
  ungroup() 

add_loc_sf <- add_loc_sf %>% 
  st_intersects(., add_maps, sparse = F) %>% 
  as_tibble() %>% 
  rename_all(~add_maps$species_binomial) %>% 
  bind_cols(add_loc_sf, .) %>% 
  pivot_longer(cols = 5:(4+length(all_species)), names_to = "species_binomial", values_to = "in_range")

all_checklists <- add_loc_sf %>% 
  st_drop_geometry() %>% 
  filter(in_range == TRUE) %>% 
  group_by(species_binomial) %>%
  summarise(n_checklists = sum(n_checklists))

all_observations <- left_join(add_obs, add_loc_sf, by = c("latitude", "longitude", "species_binomial")) %>% 
  filter(in_range == TRUE) %>% 
  group_by(species_binomial) %>% 
  summarise(n_observations = n())

all_areas <- add_maps %>% 
  mutate(area = as.numeric(st_area(.) / 1000000)) %>% 
  st_drop_geometry()

all_data <- reduce(list(all_areas, all_checklists, all_observations, threat), left_join) %>% 
  mutate(reporting_rate = n_observations / n_checklists,
         checklist_density = n_checklists / area,
         observation_density = n_observations / area,
         log_reporting_rate = log10(reporting_rate),
         log_ceiling_reporting_rate = ceiling(log_reporting_rate),
         is_threatened = if_else(threat_status == "LC/NA", FALSE, TRUE),
         quadrant = if_else(
           reporting_rate < 0.01,
           if_else(
             checklist_density > 1,
             "bottom right",
             "bottom left"
           ),
           if_else(
             checklist_density > 1,
             "top right",
             "top left"
           )
         ))

rm(add_loc_sf) # this is usually huge - easier to remove it

#### 3. Plot -------------------------------------------------------------------

ggplot(all_data) + 
  geom_point(aes(x = n_checklists / area, y = n_observations / area, 
                 label = species_binomial, colour = threat_status)) +
  scale_x_log10(labels = scales::comma, limits = c(0.01, 15)) + scale_y_log10(labels = scales::comma, limits = c(0.000001, 15)) +
  scale_colour_manual(values = c("LC/NA" = "#60c659", "NT" = "#cce226", "VU" = "#f9e814", "EN" = "#fc7f3f", "CR" = "#d81e05")) +
  theme_classic() + theme(legend.position = "none") +
  labs(x = "Checklists per km²", y = "Observations per km²") +
  geom_abline(intercept = c(0, -1, -2, -3, -4, -5), alpha = 0.2)

ggsave("RawFigure1.svg", width = 7, height = 5)

#### 4. Extras -----------------------------------------------------------------

all_data %>% 
  pull(log_ceiling_reporting_rate) %>% table()

t.test(x = log10(pull(filter(all_data, is_threatened == TRUE), reporting_rate)), 
       y = log10(pull(filter(all_data, is_threatened == FALSE), reporting_rate)))

t.test(x = log10(pull(filter(all_data, is_threatened == TRUE), checklist_density)), 
       y = log10(pull(filter(all_data, is_threatened == FALSE), checklist_density)))

t.test(x = log10(pull(filter(all_data, is_threatened == TRUE), observation_density)), 
       y = log10(pull(filter(all_data, is_threatened == FALSE), observation_density)))

cor.test(all_data$n_checklists, all_data$n_observations)

#### 5. Checklist Density Map (Supplementary) ----------------------------------

flexi_round <- function (x, to) {
  return (round(x / to) * to)
}

checklist_density <- add_obs %>% 
  mutate(latitude = flexi_round(latitude, 0.25),
         longitude = flexi_round(longitude, 0.25)) %>% 
  group_by(latitude, longitude) %>% 
  summarise(checklist_density = n_distinct(checklist_id)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

bbox <- st_bbox(aus)
bbox[1:2] <- floor(bbox[1:2])-0.125
bbox[3:4] <- ceiling(bbox[3:4])+0.125

aus_grid <- st_make_grid(bbox, cellsize = 0.25) %>% 
  st_sf() %>% 
  rowid_to_column() %>% 
  st_join(., checklist_density) %>% 
  st_intersection(., aus) %>% 
  mutate(checklist_density = replace_na(checklist_density, 0))

rarest_species <- all_data %>% 
  filter(reporting_rate < 0.001) %>% 
  pull(species_binomial)

rarest_centroids <- add_maps %>% 
  st_make_valid() %>% 
  st_centroid() %>% 
  filter(species_binomial %in% rarest_species)

ggplot() +
  geom_sf(data = aus_grid, aes(fill = checklist_density + 1), colour = NA) +
  scale_fill_distiller(trans = "log", palette = "Spectral", limits = c(1, 100000), 
                       breaks = c(1, 10, 100, 1000, 10000, 100000),
                       labels = scales::comma) +
  geom_sf(data = rarest_centroids) +
  theme_classic() +
  theme(legend.position = "bottom", legend.key.width = unit(3, "cm"), legend.title = element_blank())

ggsave("RawFigure2.svg", width = 7, height = 6)

supp_data <- left_join(all_data, wlab, by = c("species_binomial" = "TaxonScientificName")) %>% 
  arrange(TaxonSort) %>% 
  mutate(TaxonName = if_else(species_binomial == "Motacilla flava", "Yellow Wagtail", TaxonName)) %>% # to fix single missing name (taxonomy issue!) 
  mutate(TaxonName = as.character(TaxonName),
         species_binomial = as.character(species_binomial),
         area = formatC(area, format = "e", digits = 2),
         n_observations = formatC(n_observations, format = "e", digits = 2),
         n_checklists = formatC(n_checklists, format = "e", digits = 2),
         observation_density = formatC(observation_density, format = "e", digits = 2),
         checklist_density = formatC(checklist_density, format = "e", digits = 2),
         reporting_rate = formatC(reporting_rate, format = "e", digits = 2),
         quadrant = str_replace_all(quadrant, c(
           "top left" = "Top Left (2)",
           "top right" = "Top Right (3)",
           "bottom left" = "Bottom Left (2)",
           "bottom right" = "Bottom Right (1)")),
         threat_status = str_replace(threat_status, "\\/NA", "")) %>% 
  select("Common Name" = TaxonName,
         "Scientific Name" = species_binomial,
         "Range Size (km²)" = area,
         "# Obs. in Range" = n_observations,
         "# Lists in Range" = n_checklists,
         "Obs. Density" = observation_density,
         "Checklist Density" = checklist_density,
         "Reporting Rate" = reporting_rate,
         "Quadrant (Group)" = quadrant,
         "Threat Status" = threat_status)

write_csv(supp_data, "Data/supplementary_data.csv")

gc()
save.image()
