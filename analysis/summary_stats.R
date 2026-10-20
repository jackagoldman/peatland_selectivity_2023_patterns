library(tidyverse)
library(ggplot2)
library(sf)
library(googledrive)
library(here)
selectivity <- read_csv(here("data", "Dataset_selectivity_jg_0617.csv"))


# Total burned area in peatlands
# Using buffer_ approach - av and bu from same framework

peatland_burned <- selectivity %>%
  # keep only total level peatland classes
  # avoid double counting subclasses
  dplyr::filter(variable %in% c("total_bog",
                                 "total_rich_fen",
                                 "total_poor_fen",
                                 "total_ppc")) %>%
  # get one row per polygon per class
  dplyr::group_by(K_UniqueID, variable) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  # calculate burned area per class per polygon
  dplyr::mutate(burned_area_class_m2 = bu * burned.area) %>%
  # sum across all polygons and classes
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    total_burned_ha = sum(burned_area_class_m2, na.rm=TRUE) / 10000
  ) %>%
  dplyr::bind_rows(
    # add total peatland row
    dplyr::tibble(
      variable        = "total_peatland",
      total_burned_ha = sum(.$total_burned_ha)
    )
  )

print(peatland_burned)

# Also get upland for comparison
upland_burned <- selectivity %>%
  dplyr::filter(variable == "total_upland") %>%
  dplyr::group_by(K_UniqueID) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(burned_area_class_m2 = bu * burned.area) %>%
  dplyr::summarise(
    variable        = "upland",
    total_burned_ha = sum(burned_area_class_m2, na.rm=TRUE) / 10000
  )

print(upland_burned)

colnames(selectivity)
# NfireID by region (n)
nfire_region <- selectivity %>% 
  dplyr::group_by(region) %>%
  dplyr::summarise(nfire = n_distinct(NFIREID.x))

# how many regions are NA
nfire_region_na <- selectivity %>% 
  dplyr::filter(is.na(region)) %>%
  dplyr::summarise(nfire = n_distinct(NFIREID.x))

# count k_unqiueid per fire , 
nfire_kuniqueid <- selectivity %>%
  dplyr::group_by(NFIREID.x) %>%
  dplyr::summarise(n_kuniqueid = n_distinct(K_UniqueID))

#min/max n for nfire_kunqiueid
nfire_kuniqueid %>%
  summarise(min_n = min(n_kuniqueid),
            max_n = max(n_kuniqueid),
            mean_n = mean(n_kuniqueid),
            median_n = median(n_kuniqueid))


######## how many fires burned in spring and summer

selectivity %>%
  dplyr::group_by(season.bin) %>%
  dplyr::summarise(nfire = n_distinct(K_UniqueID))

nfire_season <- selectivity %>%
  dplyr::group_by(season.bin) %>%
  dplyr::summarise(nfire = n_distinct(NFIREID.x))

# why are there more fires in the season.bin than region dataset when they are the same data?
nfire_season_na <- selectivity %>% 
  dplyr::filter(is.na(season.bin)) %>%
  dplyr::summarise(nfire = n_distinct(NFIREID.x))

#duplicate ids in season.bin
nfire_season_dup <- selectivity %>%
  dplyr::group_by(NFIREID.x) %>%
  dplyr::filter(n() > 1) %>%
  dplyr::summarise(n_season = n_distinct(season.bin)) |> 
  dplyr::filter(n_season > 1)


# print get all the first in selectivity which have a matching NFIREID.x in nfire_season_dup
selectivity_dup <- selectivity %>%
  dplyr::filter(NFIREID.x %in% nfire_season_dup$NFIREID.x) |> 
  dplyr::group_by(NFIREID.x) %>%
  dplyr::ungroup()


# look print all dates for NFIREID.x == 123
selectivity_dup %>%
  dplyr::filter(NFIREID.x == 123) %>%
  dplyr::select(NFIREID.x, season.bin, DATE)


# 72 fires burned in both early and late !!


#### Zone Name -> ecozone
counts_per_zone <- selectivity %>%
  dplyr::group_by(ZONE_NAME) %>%
  dplyr::summarise(nfire = n_distinct(NFIREID.x))


# what is the latest date in the dataset?
selectivity %>%
  dplyr::summarise(latest_date = max(DATE, na.rm=TRUE))

selectivity %>%
  dplyr::summarise(earliest_date = min(DATE, na.rm=TRUE))
 


# summary table modification 

table1 <- read_csv(here("results/tables/Table1_area_summary.csv"))

colnames(table1)

# remove column name "study_are_m2", "buffer_area_m2" and "burned_area_m2"
table1 <- table1 %>%
  dplyr::select(-c(study_area_m2, buffer_area_m2, burned_area_m2, ivlev))

# replace canopy == any with "total"
table1 <- table1 %>%
  dplyr::mutate(canopy = ifelse(canopy == "any", "total", canopy))


# export table 
write_csv(table1, here("results/tables/Table1_area_summary_modified.csv"))
