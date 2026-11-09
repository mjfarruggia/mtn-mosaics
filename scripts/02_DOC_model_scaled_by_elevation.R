#DOC scaled by elevation

source("scripts/02_DOC_model.R", local = FALSE)



# MJF oct 2026 - can we scale by elevation? 
# janice comment: The data only appears to not match the model because of 
# different starting elevations. If we treat the base of the mountain as ground 
# zero and not actual masl, the data amd model will match more closely.

library(magrittr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lmerTest)

mtn_colors <- c(     #this is an NCEAS scicomm colorblind friendly recommended palette https://www.nceas.ucsb.edu/sites/default/files/2022-06/Colorblind%20Safe%20Color%20Schemes.pdf
  
  "Yukon" = "#332288",
  "Northern Rockies" = "#117733",
  "Columbia" = "#44AA99",
  "Olympic" = "#88CCEE",
  "Cascades" = "#DDCC77",
  "Siskiyou" = "#AA4499",
  "Southern Rockies" = "#CC6677",
  "Sierra Nevada" = "#882255",
  "Alps and Pyrenees" = "#999933"
)
#reclassify mountain ranges to a common grouping (see colors above)
ValData2 <- ValData %>%
  mutate(Region = stringr::str_trim(as.character(Region)))

# Assign GMBA polygon IDs using the original, more specific region names
range_ids <- tibble::tribble(
  ~Region,              ~GMBA_V2_ID,
  "Yukon",              15783,
  "Canadian Rockies",   12199,
  "N. Rocky",           12199,
  "Northern Rockies",   12199,
  "Columbia",           12422,
  "Olympic",            11688,
  "Cascades",           11202,
  "Siskiyou",           12930,
  "Rocky Mountains",    12208,
  "S. Rocky",           12208,
  "Southern Rockies",   12208,
  "Uintas",             12026,
  "Wind River",         12060,
  "Yellowstone",        17501,
  "Sierra Nevada",      11928,
  "Alps",               10001,
  "Pyrenees",           11756
)

ValData2 <- ValData2 %>%
  dplyr::left_join(range_ids, by = "Region")

# Check for site regions without a GMBA polygon ID
ValData2 %>%
  dplyr::filter(is.na(GMBA_V2_ID)) %>%
  dplyr::count(Region, name = "n_sites")

#reclassify mountain ranges to a common grouping (see colors above)
ValData2 <- ValData2 %>%
  mutate(Range = case_when(
    Region == "Canadian Rockies"   ~ "Northern Rockies",
    Region == "N. Rocky"           ~ "Northern Rockies",
    Region == "Rocky Mountains"    ~ "Southern Rockies",
    Region == "S. Rocky"           ~ "Southern Rockies",
    Region == "Uintas"             ~ "Southern Rockies",
    Region == "Wind River"         ~ "Southern Rockies",
    Region == "Yellowstone"        ~ "Southern Rockies",
    Region == "Cascades"           ~ "Cascades",
    Region == "Columbia"           ~ "Columbia",
    Region == "Sierra Nevada"      ~ "Sierra Nevada",
    Region %in% c("Alps", "Pyrenees", "Alps and Pyrenees") ~ "Alps and Pyrenees",
    TRUE                           ~ Region
  ))

ValData2$Range <- factor(ValData2$Range, levels = c("Yukon", "Northern Rockies", "Columbia", 
                                                  "Olympic", "Cascades", "Siskiyou", "Southern Rockies", "Sierra Nevada", "Alps and Pyrenees"))



ValRel <- ValData2 %>%
  group_by(Range) %>%
  mutate(Relief = max(Elevation) - min(Elevation)) %>%
  mutate(Elev_S = 100 * (Elevation - min(Elevation)) / Relief) %>%
  ungroup()

# Reference relief used to map the 0-100 scale onto the model's meter axis. The model has no range of its own, so this is a choice.
H_ref <- ValRel %>%
  distinct(Range, Relief) %>%
  pull(Relief) %>%
  mean()

# rule = 2 clamps to the nearest model value outside the model's elevation grid
model_at <- function(x) approx(DOCRunMod2$elevation, DOCRunMod2$DOCRunmean, xout = x, rule = 2)$y

ValRel <- ValRel %>%
  mutate(Pred_abs = model_at(Elevation)) %>%
  mutate(Pred_S = model_at(Elev_S / 100 * H_ref))

FitSummary <- ValRel %>%
  summarise(n = n(), RMSE_abs = sqrt(mean((DOC - Pred_abs)^2)), RMSE_S = sqrt(mean((DOC - Pred_S)^2)), Bias_abs = mean(Pred_abs - DOC), Bias_S = mean(Pred_S - DOC))

print(FitSummary)

# Premise check: if position within the range is what matters, range relief should not explain DOC once Elev_S is in the model
summary(lmer(DOC ~ Elev_S + Relief + (1 | Range), data = ValRel))

model_S <- DOCRunMod2 %>%
  mutate(mx = 100 * elevation / H_ref)

ggplot() +
  geom_line(data = model_S, aes(x = mx, y = DOCRunmean), size = 1.5) +
  geom_point(data = ValRel, aes(x = Elev_S, y = DOC, color = Range)) +
  geom_smooth(data = ValRel, aes(x = Elev_S, y = DOC, color = Range), method = "lm", se = FALSE, size = 0.75) +
  scale_color_manual(values=mtn_colors, breaks=levels(ValRel$Range), name="Region")+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +
  labs(x = "Scaled elevation (by region)", y = "DOC (mg/L)")



# Across-region scaling ------------------------------------------------------------

elev_min_all <- min(ValData$Elevation)
elev_range_all <- max(ValData$Elevation) - min(ValData$Elevation)

ValRel <- ValRel %>%
  mutate(Elev_S_all = 100 * (Elevation - elev_min_all) / elev_range_all) %>%
  mutate(Pred_S_all = model_at(Elev_S_all / 100 * elev_range_all))

FitSummary_all <- ValRel %>%
  summarise(n = n(), RMSE_abs = sqrt(mean((DOC - Pred_abs)^2)), RMSE_S_region = sqrt(mean((DOC - Pred_S)^2)), RMSE_S_all = sqrt(mean((DOC - Pred_S_all)^2)), Bias_abs = mean(Pred_abs - DOC), Bias_S_region = mean(Pred_S - DOC), Bias_S_all = mean(Pred_S_all - DOC))

print(FitSummary_all)

model_S_all <- DOCRunMod2 %>%
  mutate(mx = 100 * elevation / elev_range_all)

ggplot() +
  geom_line(data = model_S_all, aes(x = mx, y = DOCRunmean), size = 1.5) +
  geom_point(data = ValRel, aes(x = Elev_S_all, y = DOC, color = Range)) +
  geom_smooth(data = ValRel, aes(x = Elev_S_all, y = DOC, color = Range), method = "lm", se = FALSE, size = 0.75) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +
  scale_color_manual(values=mtn_colors, breaks=levels(ValRel$Range), name="Region")+
  labs(x = "Scaled elevation (across regions)", y = "DOC (mg/L)")








#get transects for mtn ranges via a code-reproduceable approach --------------------------------------------------

# install.packages("remotes"); remotes::install_github("GMBA-biodiversity/gmbaR")
library(gmbaR)
library(sf)
library(elevatr)

# Reads the inventory and creates gmba_inv(). Run it without assigning the result (gmbaR requirement).
# Large download; use source = "local", local = "path/to/shapefile.shp" after downloading once from https://www.earthenv.org/mountains
gmba_read("web", timeout = 900)


# 1. Find the polygons for each range  ---------------------------------------------
# Run this block, read the printed candidates, then fill GMBA_V2_ID in section 2.

search_terms <- c("Sierra Nevada", "Cascade", "Olympic", "Siskiyou", "Columbia", "Uinta", "Wind River", "Yellowstone", "Rocky", "Yukon", "Alps", "Pyrenees")

gmba_results <- gmba_inv() %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(grepl(paste(search_terms, collapse = "|"), DBaseName, ignore.case = TRUE)) %>%
  dplyr::select(GMBA_V2_ID, DBaseName, Hier_Lvl, Area, Countries, Elev_Low, Elev_High) %>%
  dplyr::arrange(DBaseName)


 
# 2. region table ---------------------------------------------------------------
region_ids <- tibble::tribble(
  ~Region, ~GMBA_V2_ID,
  "Yukon", 15783,
  "Canadian Rockies", 12199,
  "N. Rocky", 12199,
  "Columbia", 12422,
  "Olympic", 11688,
  "Cascades", 11202,
  "Siskiyou", 12930,
  "Rocky Mountains", 12208,
  "S. Rocky", 12208,
  "Uintas", 12026,
  "Wind River", 12060,
  "Yellowstone", 17501,
  "Sierra Nevada", 11928,
  "Alps", 10001,
  "Pyrenees", 11756
)

# Keep only the ranges that are in your data
# region_ids <- region_ids %>%
#   dplyr::filter(Range %in% unique(ValData$Region))

# 3. Base elevation per range ---------------------------------------------------------------
# gmba_inv() is filtered directly rather than with gmba_subset(), which compares the whole ID vector with == inside if() and may error for more than one ID.
# Each GMBA polygon is matched to its Range using GMBA_V2_ID. Multiple ranges may intentionally share the same polygon.

gmba_selected <- gmba_inv() %>%
  dplyr::mutate(GMBA_V2_ID = as.character(GMBA_V2_ID)) %>%
  dplyr::filter(GMBA_V2_ID %in% as.character(region_ids$GMBA_V2_ID))

region_poly <- gmba_selected %>%
  dplyr::inner_join(region_ids %>% dplyr::mutate(GMBA_V2_ID = as.character(GMBA_V2_ID)), by = "GMBA_V2_ID") %>%
  sf::st_make_valid() %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(do_union = TRUE, .groups = "drop")

# Samples the DEM every spacing_m along the outer boundary. z = 7 is ~1 km pixels: fine for a mean, and keeps the tile downloads manageable.
boundary_elev <- function(geom, spacing_m = 20000, z = 7) {
  xy <- geom %>%
    sf::st_transform(6933) %>%
    sf::st_boundary() %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_line_sample(density = 1 / spacing_m) %>%
    sf::st_cast("POINT") %>%
    sf::st_transform(4326) %>%
    sf::st_coordinates()
  
  xy <- xy[is.finite(xy[, "X"]) & is.finite(xy[, "Y"]), , drop = FALSE]
  if (nrow(xy) == 0) return(c(Base_mean = NA_real_, Base_median = NA_real_, n_pts = 0))
  
  elev <- elevatr::get_elev_point(data.frame(x = xy[, "X"], y = xy[, "Y"]), prj = "EPSG:4326", src = "aws", z = z)$elevation
  
  c(Base_mean = mean(elev, na.rm = TRUE), Base_median = median(elev, na.rm = TRUE), n_pts = sum(!is.na(elev)))
}

RegionBase <- lapply(seq_len(nrow(region_poly)), function(i) {
  b <- boundary_elev(sf::st_geometry(region_poly)[i])
  data.frame(Region = region_poly$Region[i], Base_mean = b[["Base_mean"]], Base_median = b[["Base_median"]], n_pts = b[["n_pts"]])
}) %>%
  dplyr::bind_rows()

# Published lowest GMTED2010 elevation inside the polygon(s), for comparison. It is a single lowest cell, not an average base.
# Use distinct polygon IDs before joining to avoid duplicate rows when multiple analysis ranges share an ID.

ElevLow <- gmba_selected %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(GMBA_V2_ID, Elev_Low) %>%
  dplyr::right_join(region_ids %>% dplyr::mutate(GMBA_V2_ID = as.character(GMBA_V2_ID)), by = "GMBA_V2_ID") %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(Elev_Low = min(Elev_Low, na.rm = TRUE), .groups = "drop")

RegionBase <- RegionBase %>%
  dplyr::left_join(ElevLow, by = "Region")

# write.csv(RegionBase, file = "data/processed/RegionBase_gmba.csv", row.names = FALSE)


#combine alps and pyrenees. just avg them?
RegionBase <- RegionBase %>%
  dplyr::bind_rows(
    RegionBase %>%
      dplyr::filter(Region %in% c("Alps", "Pyrenees")) %>%
      dplyr::summarise(
        dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(Region = "Alps and Pyrenees") %>%
      dplyr::select(names(RegionBase))
  )

# 4. Height above base for each site ---------------------------------------------------------

ValRel <- ValRel %>%
  dplyr::select(-dplyr::any_of(c("Base_mean", "Base_median", "n_pts", "Elev_Low", "Elev_J", "Pred_J"))) %>%
  dplyr::left_join(RegionBase %>% dplyr::select(Region, Base_mean), by = "Region") %>%
  dplyr::mutate(Elev_J = Elevation - Base_mean) %>%
  dplyr::mutate(Pred_J = model_at(Elev_J))

stopifnot(!anyNA(ValRel$Base_mean))

# Sites below their range's mean base get clamped to the model's lowest value by model_at(), so check how many there are
print(sum(ValRel$Elev_J < 0))
ValRel %>%
  dplyr::filter(Elev_J < 0) %>%
  dplyr::select(Region, Range, Elevation, Base_mean, Elev_J) %>%
  print(n = Inf)


#plot
ggplot() +
  geom_line(data = DOCRunMod2, aes(x = elevation, y = DOCRunmean), linewidth = 1.5) +
  geom_point(data = ValRel, aes(x = Elev_J, y = DOC, color = Range)) +
  geom_smooth(data = ValRel, aes(x = Elev_J, y = DOC, color = Range, group = Range), method = "lm", se = FALSE, linewidth = 0.75) +
  scale_color_manual(values = mtn_colors, breaks = names(mtn_colors), name = "Mountain range", drop = T) +
  coord_cartesian(xlim = c(-600, 4000), ylim = c(0, 30)) +
  labs(x = "Height above range base (m)", y = "DOC (mg/L)")

