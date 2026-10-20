library(here)
library(terra)
library(sf)
library(dplyr)
library(readr)
library(rnaturalearth)
library(viridisLite)
library(maptiles)
library(ggplot2)
library(tidyterra)
library(treemapify)
library(svglite)

# terra spills large rasters to temp files while plotting/sampling; the
# internal disk is nearly full, so point terra at the external drive instead
# (otherwise a second large raster in the same session can silently fail to
# render if the internal disk runs out of space mid-write)
dir.create("/Volumes/top-secret/PostDoc/peatlands_selectivity/tmp_terra", showWarnings = FALSE)
terraOptions(tempdir = "/Volumes/top-secret/PostDoc/peatlands_selectivity/tmp_terra")

# read in 8bit image
peat_raster = rast("/Volumes/top-secret/PostDoc/peatlands_selectivity/data/PeatlandMap8b_2023_07_17_lamb_not_projected.tif")
# fire progressions
fire_progressions = st_read("/Volumes/top-secret/PostDoc/peatlands_selectivity/data/fire_progression_polygons/landscape_processed_polygons_km_oct18.shp")

# selectivity
selectivity = read_csv(here("data", "Dataset_selectivity_jg_0617.csv"))

# landscape
landscape = st_read("data/landscape_polygons_with_ecozones.shp")

#ecozones
url = "https://agriculture.canada.ca/atlas/data_donnees/nationalEcologicalFramework/data_donnees/geoJSON/ez/nef_ca_ter_ecozone_v2_2.geojson"
ecozones = st_read(url)

# ECOZONE_NAME_EN column filter for "Boreal Shield, Hudson Plains, Atlantic Maritime, Taiga Shield, Taiga Cordillera, Taiga Plain, Boreal Plain,"
ecozones_sel = ecozones %>%
  dplyr::filter(ECOZONE_NAME_EN %in% c("Boreal Shield", "Hudson Plains", "Atlantic Maritime", "Taiga Shield", "Taiga Cordillera", "Taiga Plain", "Boreal Plain"))

# ---- filter fire progressions to fires present in the selectivity data ----
sel_fire_ids = unique(selectivity$NFIREID.x)

fire_progressions_sel = fire_progressions %>%
  dplyr::filter(NFIREID %in% sel_fire_ids) %>%
  sf::st_make_valid()


# ---- one centroid per fire: union all progression polygons for a fire, then take its centroid ----
fire_centroids = fire_progressions_sel %>%
  dplyr::group_by(NFIREID) %>%
  dplyr::summarise(.groups = "drop") %>%
  sf::st_centroid() %>%
  sf::st_transform(crs(peat_raster))

# ---- peat class palette (GEE display palette) ----
peat_classes = c(
  "Bog", "Rich Fen", "Poor Fen", "PPC",
  "Mineral Wetlands", "Water", "Upland ", "Agriculture", "Urban"
)
peat_palette = c(
  "#4C0073", "#FFFF00", "#E64C00", "#727272",
  "#F4C2C2", "#0070FF", "#4C7300", "#623131", "#000000"
)


### V2 lambert

peat_raster_lambert = rast("/Volumes/top-secret/PostDoc/peatlands_selectivity/data/PeatlandMap8b_2023_07_17_lamb.vrt")

NAflag(peat_raster_lambert) = 0  # reprojection fill value outside source extent, not a real class

fire_centroids_lambert = sf::st_transform(fire_centroids, crs(peat_raster_lambert))



canada_lambert = sf::st_transform(
  ne_states(country = "Canada", returnclass = "sf"),
  crs(peat_raster_lambert)
)

peat_palette_named = peat_palette
names(peat_palette_named) = peat_classes

peat_sample = terra::spatSample(peat_raster_lambert, size = 1e6, method = "regular",
                                 xy = TRUE, values = TRUE, na.rm = TRUE)
names(peat_sample)[3] = "class_id"
peat_sample$class = factor(peat_classes[peat_sample$class_id], levels = peat_classes)


p_basemap = ggplot() +
  geom_sf(data = canada_lambert, fill = "grey95", color = "grey30", linewidth = 0.3) +
  geom_tile(data = peat_sample, aes(x = x, y = y, fill = class)) +
  geom_sf(data = fire_centroids_lambert, shape = 21, fill = "red", color = "black",
          size = 1.8, stroke = 0.4) +
  scale_fill_manual(values = peat_palette_named, name = "Peat Class", drop = FALSE) +
  coord_sf(crs = st_crs(canada_lambert), expand = FALSE, datum = NA) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        legend.position  = "right")

ggsave(here("plots", "map", "study_area_progression_panel_basemap.png"),
       p_basemap, width = 12, height = 9, dpi = 300)



# MAKE PROGRESSION MAP

# ---- fire identified by its dNBR raster: <K_FireID>_dnbr.tif ----
target_fire_id  = 79
dnbr_path       = paste0(
  "/Volumes/top-secret/PostDoc/peatlands_selectivity/data/dnbr_rasters/",
  target_fire_id, "_dnbr.tif"
)
dnbr_raster = rast(dnbr_path)

# ---- subset progression polygons to this fire, ordered earliest -> latest ----
progressions_fire = fire_progressions %>%
  dplyr::filter(K_FireID == target_fire_id) %>%
  sf::st_make_valid() %>%
  sf::st_transform(crs(dnbr_raster)) %>%
  dplyr::arrange(DATE) %>%
  dplyr::mutate(
    progression_date  = as.Date(as.character(DATE), format = "%Y%m%d"),
    progression_order = dplyr::row_number()
  )

# ---- colour ramp: one colour per progression, earliest -> latest ----
n_progressions   = nrow(progressions_fire)
progression_cols = viridisLite::viridis(n_progressions)

# ---- map: all progressions for this fire overlaid on the dNBR raster ----
# layout: map panel + dedicated legend panel, so the date legend never
# overlaps or gets clipped by the map itself


# ---- satellite basemap for the fire extent ----
satellite_basemap = maptiles::get_tiles(
  progression_union,
  provider = "Esri.WorldImagery",
  crop     = TRUE
)

p_map = ggplot() +
  geom_spatraster_rgb(data = satellite_basemap) +
  geom_sf(data = progressions_fire,
          aes(fill = format(progression_date, "%Y-%m-%d")),
          color = "black", linewidth = 0.5, alpha = 0.6) +
  scale_fill_manual(values = progression_cols, name = "Progression\nDate") +
  coord_sf(expand = FALSE, datum = NA) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        legend.position  = "right")


ggsave(here("plots", "map", "workflow_map", paste0("fire_", target_fire_id, "_progressions.svg")),
    width = 11, height = 9,  dpi = 300)


# ---- classify dNBR into burned / unburned using the standard 0.1 threshold ----
# Miller and Thode (2007) - matches get_burned_raster() in 02_functions.R
burn_threshold = 0.1
burn_class     = terra::classify(
  dnbr_raster,
  rcl = matrix(c(-Inf, burn_threshold, 1,
                 burn_threshold,  Inf, 2), ncol = 3, byrow = TRUE)
)
levels(burn_class) = data.frame(id = c(1, 2), class = c("Unburned", "Burned"))



burn_cols = c("Unburned" = "grey85", "Burned" = "#D7301F")


# ---- clip burned class to the combined extent of the progression perimeters ----
# any burned pixel outside the union of all progression polygons is forced
# back to "Unburned" so red only ever appears within the fire's mapped extent
progression_union = vect(sf::st_union(progressions_fire))
inside_progressions = terra::rasterize(progression_union, burn_class, field = 1, background = 0)
burn_class_clipped = burn_class
burn_class_clipped[inside_progressions == 0] = 1

# burned pixels only (within progression extent), as NA elsewhere so the
# satellite basemap shows through everywhere that isn't burned
burned_only = terra::ifel(inside_progressions == 1 & dnbr_raster > burn_threshold, 1, NA)


burned_df = as.data.frame(burned_only, xy = TRUE, na.rm = TRUE)
names(burned_df)[3] = "value"

p_map_burned =  ggplot() +
  geom_spatraster_rgb(data = satellite_basemap) +
  geom_raster(data = burned_df, aes(x = x, y = y), fill = "#D7301F", alpha = 0.75) +

  geom_sf(data = progression_union,
          fill = NA, color = "#FDE725FF", linewidth = 0.5, alpha = 0.75) +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        legend.position  = "right")

p_map_burned

ggsave(here("plots", "map", "workflow_map", paste0("fire_", target_fire_id, "_burned.svg")),
    width = 11, height = 9,  dpi = 300)


## PROGRESSION MAP V2: Land Cover Class

# ---- land cover class raster, cropped to this fire's progression extent ----
landcover_path    = "/Volumes/top-secret/nickp_products/Peat_Canopy_2023_11_06.tif"
landcover_raster  = rast(landcover_path)
landcover_fire    = terra::crop(landcover_raster, progression_union)

# pixel value -> wetland class, by structure (see get_class_name() in 02_functions.R)
# only bog / rich fen / poor fen / ppc are kept; everything else becomes NA
wetland_rcl = matrix(c(
  5,  1,   # open_bog
  13, 2,   # forested_bog
  18, 3,   # treed_bog
  3,  4,   # open_rich_fen
  12, 5,   # forested_rich_fen
  17, 6,   # treed_rich_fen
  2,  7,   # open_poor_fen
  14, 8,   # forested_poor_fen
  21, 9,   # treed_poor_fen
  4,  10,  # open_ppc
  8,  11,  # forested_ppc
  16, 12   # treed_ppc
), ncol = 2, byrow = TRUE)

wetland_class = terra::classify(landcover_fire, rcl = wetland_rcl, others = NA)

wetland_labels = c(
  "Bog - Open",      "Bog - Forested",      "Bog - Treed",
  "Rich Fen - Open", "Rich Fen - Forested", "Rich Fen - Treed",
  "Poor Fen - Open", "Poor Fen - Forested", "Poor Fen - Treed",
  "PPC - Open",      "PPC - Forested",      "PPC - Treed"
)
levels(wetland_class) = data.frame(id = 1:12, class = wetland_labels)

wetland_cols = c(
  "#E0CCFF", "#4C0073", "#9966CC",   # Bog: open, forested, treed
  "#FFFFB3", "#999900", "#E6E600",   # Rich Fen
  "#FFD9B3", "#994D00", "#FF8C1A",   # Poor Fen
  "#A6D8E8", "#1B6684", "#4A9BB8"    # PPC
)
names(wetland_cols) = wetland_labels

# clip to the union of progression perimeters so land cover only shows
# within the fire's mapped extent; everywhere else falls back to satellite
wetland_class = terra::mask(wetland_class, progression_union)



# Convert wetland raster to a data frame, no resampling
wetland_df = as.data.frame(wetland_class, xy = TRUE, na.rm = TRUE)
names(wetland_df)[3] = "class_id"

# Map class id to labels (adjust to match however wetland_labels/wetland_cols are indexed)
wetland_df$class = factor(wetland_labels[wetland_df$class_id], levels = wetland_labels)

# Named colour vector for scale_fill_manual
wetland_cols_named = setNames(wetland_cols, wetland_labels)

p_map_wet = ggplot() +
  geom_spatraster_rgb(data = satellite_basemap) +
  geom_raster(data = wetland_df, aes(x = x, y = y, fill = class)) +
  geom_sf(data = progression_union, fill = NA, color = "#FDE725FF", linewidth = 0.5, alpha = 0.75) +
  scale_fill_manual(values = wetland_cols_named, name = "Land Cover Class", drop = TRUE) +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.grid = element_blank(),
        legend.position = "right")

print(p_map_wet)


# ggsave
ggsave(here("plots", "map", "workflow_map", paste0("fire_", target_fire_id, "_landcover.svg")),
    width = 11, height = 9,  dpi = 300)

wetland_df %>% count(class)

#wetland bar 


p_bar = ggplot(wetland_df, aes(x = class, fill = class)) +
  geom_bar() +
  scale_fill_manual(values = wetland_cols_named, drop = TRUE) +
  labs(x = "Land Cover Class", y = "Pixel Count", title = "Pixel Count by Wetland Class") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(p_bar)

wetland_counts = wetland_df %>% count(class) %>% filter(n > 0)

ggplot(wetland_counts, aes(area = n, fill = class, label = class)) +
  geom_treemap() +
  geom_treemap_text(colour = "black", place = "centre", size = 10) +
  scale_fill_manual(values = wetland_cols_named) +
  theme(legend.position = "none")



wet_pix_n <- wetland_df %>%
  count(class) %>%
  filter(n > 0) %>%
  ggplot(aes(x = reorder(class, -n), y = n, fill = class)) +
  geom_col() +
  geom_text(aes(label = scales::comma(n)), vjust = -0.4, size = 3) +
  scale_fill_manual(values = wetland_cols_named) +
  scale_y_log10(labels = scales::comma, expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = "Land Cover Class", y = "Pixel Count (log scale)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2))
print(wet_pix_n)

# ggsave
ggsave(here("plots", "map", "workflow_map", paste0("fire_", target_fire_id, "_plot_lcclass.png")),
    width = 11, height = 9,  dpi = 300)

## UPDATE BASEMAP  with bbox

progression_union_sf = sf::st_as_sf(progression_union)

progression_union_lambert = st_transform(progression_union_sf, crs(canada_lambert))

# Get the bounding box of progression_union
bbox = st_bbox(progression_union_lambert)


# Find the center and the larger of width/height, so the box is square
x_center = mean(c(bbox["xmin"], bbox["xmax"]))
y_center = mean(c(bbox["ymin"], bbox["ymax"]))

width  = bbox["xmax"] - bbox["xmin"]
height = bbox["ymax"] - bbox["ymin"]

half_side = max(width, height) / 2


# Optional: add a buffer/padding around the box (e.g., 10% of the side length)
padding = half_side * 0.8
half_side = half_side + padding

square_bbox = st_bbox(c(
  xmin = x_center - half_side,
  xmax = x_center + half_side,
  ymin = y_center - half_side,
  ymax = y_center + half_side
), crs = st_crs(progression_union_lambert))

# Convert to an sf polygon so it can be added with geom_sf()
square_box_sf = st_as_sfc(square_bbox) |> st_sf()



p_basemap = ggplot() +
  geom_sf(data = canada_lambert, fill = "grey95", color = "grey30", linewidth = 0.3) +
  geom_tile(data = peat_sample, aes(x = x, y = y, fill = class)) +
  geom_sf(data = fire_centroids_lambert, shape = 21, fill = "red", color = "black",
          size = 1.8, stroke = 0.4) +
  geom_sf(data = square_box_sf, fill = NA, color = "black", linewidth = 1) +
  scale_fill_manual(values = peat_palette_named, name = "Peat Class", drop = FALSE) +
  coord_sf(crs = st_crs(canada_lambert), expand = FALSE, datum = NA) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        legend.position  = "right")

print(p_basemap)

ggsave(here("plots", "map", "study_area_progression_panel_basemap_bbox.svg"),
       p_basemap, width = 12, height = 9, dpi = 300)






