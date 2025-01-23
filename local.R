source("runtime.R")
library("ggplot2")
library("readr")
library("jsonlite")
library("raster")
library("sf")

# Create habitat layers
reef <- raster("data/reef_ras.tif")
seagrass <- raster("data/seagrass_ras.tif")
seagrass_reef_hires <- raster("data/seagrass_reef_ras.tif");
seagrass_reef <- resample(seagrass_reef_hires, 
                          seagrass, 
                          method = "bilinear") 

reef_ras <- as.data.frame(reef, xy = TRUE, na.rm = FALSE)
seagrass_ras <- as.data.frame(seagrass, xy = TRUE, na.rm = FALSE)
seagrass_reef_ras <- as.data.frame(seagrass_reef, xy = TRUE, na.rm = FALSE)

write.csv(reef_ras, "data/reef_ras.csv", row.names = F)
write.csv(seagrass_ras, "data/seagrass_ras.csv", row.names = F)
write.csv(seagrass_reef_ras, "data/seagrass_reef_ras.csv", row.names = F)

## Local setup
years <- 100
seasons <- 1
time_step <- 1 / seasons
snapper_diffusion <- 1 # km^2/year
lobster_diffusion <-  0.5 # km^2/year
max_hab_mult = 20

colnames(reef_ras) <- c("x","y","layer")
colnames(seagrass_ras) <- c("x","y","layer")
colnames(seagrass_reef_ras) <- c("x","y","layer")

reef_ras <- reef_ras %>%
  filter(y < 18.225, y > 15.985) %>%
  mutate(
    layer = ifelse(is.na(layer), 0, layer),
    layer = layer/max(layer)
  )
seagrass_ras <- seagrass_ras %>%
  filter(y < 18.225, y > 15.985) %>%
  mutate(
    layer = ifelse(is.na(layer), 0, layer),
    layer = layer/max(layer)
  )
seagrass_reef_ras <- seagrass_reef_ras %>%
  filter(y < 18.225, y > 15.985) %>%
  mutate(
    layer = ifelse(is.na(layer), 0, layer),
    layer = layer/max(layer)
  )

reef_habitat <- reef_ras %>%
  pivot_wider(names_from = y, values_from = layer) %>%
  as.matrix() 
juvenile_habitat <- seagrass_ras %>%
  pivot_wider(names_from = y, values_from = layer) %>%
  dplyr::select(-x) %>%
  as.matrix()
lobster_habitat <- seagrass_reef_ras %>%
  pivot_wider(names_from = y, values_from = layer) %>%
  dplyr::select(-x) %>%
  as.matrix()

x_id = reef_habitat %>% 
  as.data.frame() %>% 
  mutate(id.x = 1:nrow(reef_habitat)) %>% 
  dplyr::select(x, id.x)
y_id = data.frame(y = unique(reef_ras$y),
                  id.y = 1:32)

reef_habitat <- reef_ras %>%
  pivot_wider(names_from = y, values_from = layer) %>%
  dplyr::select(-x) %>%
  as.matrix() 

ports <-  data.frame(x =  c(15, 15),
                     y = c(11, 11),
                     fleet = c(1, 2))

# Get spatial parameters from habitat layers
patches <- nrow(juvenile_habitat)*ncol(juvenile_habitat)
patch_area <- 57 # km2
simulation_area <- patch_area * patches #km2
resolution <- c(nrow(juvenile_habitat), ncol(juvenile_habitat))

# Define species
# Mutton snapper, Lutjanus analis
snapper <- create_critter(
  query_fishlife = FALSE,
  linf = 87.4,
  vbk = 0.16,
  t0 = -1.32,
  cv_len = 0.1,
  m=0.11,
  length_units = 'cm',
  min_age = 0,
  max_age = 40,
  weight_a = 0.114,
  weight_b = 2.53,
  weight_units = 'kg',
  fec_form = "weight",
  fec_expo = 1,
  length_50_mature = 40,
  length_95_mature = 50,
  delta_mature = .1,
  habitat = reef_habitat,
  recruit_habitat = juvenile_habitat,
  adult_diffusion = snapper_diffusion,
  recruit_diffusion = simulation_area ,
  density_dependence = "pre_dispersal",
  seasons = seasons,
  resolution = resolution,
  init_explt = 0.125, #F/Fmsy much faster
  ssb0 = 4000,
  max_hab_mult = max_hab_mult,
  patch_area = patch_area
)

# Spiny lobster, Panulirus argus
lobster <- create_critter(
  query_fishlife = FALSE,
  linf = 18.36,
  vbk = 0.24,
  t0 = -0.446,
  cv_len = 0.1,
  m = 0.34,
  length_units = 'cm',
  min_age = 0,
  max_age = NA,
  weight_a = 0.0036,
  weight_b = 2.66,
  weight_units = 'kg',
  fec_form = "weight",
  fec_expo = 1,
  length_50_mature = 8.5,
  length_95_mature = 9.1,
  delta_mature = .1,
  habitat = lobster_habitat,
  recruit_habitat = juvenile_habitat,
  adult_diffusion = lobster_diffusion,
  recruit_diffusion = simulation_area,
  density_dependence = "pre_dispersal",
  seasons = seasons,
  resolution = resolution,
  init_explt = 0.16,
  ssb0 = 4000,
  max_hab_mult = max_hab_mult,
  patch_area = patch_area
)

fauna <-
  list(
    "snapper" = snapper,
    "lobster" = lobster
  )


# Create fleets
snapper_fleet = create_fleet(
  list(
    snapper = Metier$new(
      critter = fauna$snapper,
      price = 20,
      sel_form = "logistic",
      sel_start = .5,
      sel_delta = 1,
      p_explt = 1
    ),
    lobster = Metier$new(
      critter = fauna$lobster,
      price = 0,
      sel_form = "logistic",
      sel_start = 8,
      sel_delta = 1,
      p_explt = 0
    )
  ),
  ports = ports[1, ],
  cost_per_unit_effort = 1,
  cost_per_distance = 5,
  responsiveness = 0.5,
  cr_ratio = 1,
  resolution = resolution,
  mpa_response = "stay",
  fleet_model = "constant effort",
  spatial_allocation = "ppue"
)

lobster_fleet = create_fleet(
  list(
    lobster = Metier$new(
      critter = fauna$lobster,
      price = 50,
      sel_form = "logistic",
      sel_start = .5,
      sel_delta = 1,
      p_explt = 1
    ),
    snapper = Metier$new(
      critter = fauna$snapper,
      price = 0,
      sel_form = "logistic",
      sel_start = 40,
      sel_delta = 1,
      p_explt = 0
    )
  ),
  ports = ports[2, ],
  cost_per_unit_effort = 1,
  cost_per_distance = 5,
  responsiveness = 0.5,
  cr_ratio = 1,
  resolution = resolution,
  mpa_response = "stay",
  fleet_model = "constant effort",
  spatial_allocation = "ppue"
)

fleets <- list("lobster_fleet" = lobster_fleet,
               "snapper_fleet" = snapper_fleet)

fleets <- tune_fleets(fauna, fleets)

# Simulate baseline dynamic, without any MPAs
print("Simulate baseline")
no_mpa_sim <- simmar(fauna = fauna,
                     fleets = fleets,
                     years = 100)

prs_nompa <- process_marlin(no_mpa_sim, keep_age = FALSE)

patch_noMPA <-
  map_df(no_mpa_sim, ~ map_df(.x, ~ tibble(
    catch = rowSums(.x$c_p_fl),
    biomass = rowSums(.x$b_p_a),
    ssb = rowSums(.x$ssb_p_a),
    patch = 1:nrow(.x$ssb_p_a)
  ), .id = "critter"), .id = "step") %>% 
  separate(step, "_", into = c("year", "season")) %>% 
  mutate(year = as.double(year) - 45) %>%
  filter(year >= 0) %>%
  group_by(year, critter) %>%
  summarise(catch = sum(catch),
            biomass = sum(biomass),
            ssb = sum(ssb)) %>%
  mutate(scenario = "No MPA")

write(toJSON(patch_noMPA), "data/noMPA.json")


# Simulate existing MPAs
mpa <- st_read("data/Existing-MPAs.geojson.json")
mpa_union <- st_union(mpa)
existing_mpa_sf <- st_as_sf(seagrass_ras, coords = c("x", "y"), crs = st_crs(mpa))
existing_mpa_sf <- existing_mpa_sf %>%
  mutate(
    mpa = rowSums(st_within(geometry, mpa_union, sparse = FALSE)) > 0
  )
mpa_spatial <- existing_mpa_sf %>%
  mutate(x = st_coordinates(geometry)[, 1],
         y = st_coordinates(geometry)[, 2]) %>%
  st_drop_geometry()

mpa_locations <- mpa_spatial %>% 
  left_join(x_id) %>% 
  left_join(y_id) %>% 
  dplyr::select(id.x, id.y, mpa) %>% 
  rename(y=id.y,
         x=id.x) %>% 
  dplyr::select(x, y, mpa)

existing_mpa_sim <- simmar(
  fauna = fauna,
  fleets = fleets,
  manager = list(mpas = list(
    locations = mpa_locations,
    mpa_year = 50
  )),
  years = 100
)

prs_existing <- process_marlin(existing_mpa_sim, keep_age = FALSE)

patch_existing_MPA <-
  map_df(existing_mpa_sim, ~ map_df(.x, ~ tibble(
    catch = rowSums(.x$c_p_fl),
    biomass = rowSums(.x$b_p_a),
    ssb = rowSums(.x$ssb_p_a),
    patch = 1:nrow(.x$ssb_p_a)
  ), .id = "critter"), .id = "step") %>% 
  separate(step, "_", into = c("year", "season")) %>% 
  mutate(year = as.double(year) - 45) %>%
  filter(year >= 0) %>%
  group_by(year, critter) %>% 
  summarise(catch = sum(catch),
            biomass = sum(biomass),
            ssb = sum(ssb)) %>%
  mutate(scenario = "Existing MPA")

write(toJSON(patch_existing_MPA), "data/existingMPAs.json")


# Simulate dynamics with existing MPAs plus proposed MPAs
# This now happens in lambda
# Create inputs for lambda
write(toJSON(seagrass_ras %>% select(x,y)), "data/coordinates.json")

existingmpa  <- st_read("data/Existing-MPAs.geojson.json")
mpa_in <- read_file("data/sketch.geojson.json")
mpa <- st_read(mpa_in)
mpa_union <- st_union(mpa, existingmpa)
seagrass_sf <- st_as_sf(seagrass_ras, coords = c("x", "y"), crs = st_crs(mpa_union))
seagrass_sf <- seagrass_sf %>%
  mutate(
    mpa = rowSums(st_within(geometry, mpa_union, sparse = FALSE)) > 0
  )

mpa_spatial <- seagrass_sf %>%
  mutate(x = st_coordinates(geometry)[, 1],
         y = st_coordinates(geometry)[, 2]) %>%
  st_drop_geometry()

mpa_locations <- mpa_spatial %>% 
  left_join(x_id, by = "x") %>% 
  left_join(y_id, by = "y") %>% 
  dplyr::select(id.x, id.y, mpa) %>% 
  rename(y=id.y,
         x=id.x) %>% 
  dplyr::select(x, y, mpa)

print(toJSON(mpa_locations))
returned <- run_marlin(toJSON(mpa_locations))
print(returned)
patch_MPA <- fromJSON(as.data.frame(returned)$body)

combined_df <- bind_rows(patch_noMPA, patch_existing_MPA, patch_MPA)

combined_df <- combined_df %>%
  pivot_longer(
    cols = c("catch", "biomass", "ssb"),
    names_to = "metric",
    values_to = "value"
  )

combined_df %>%
  filter(metric == "catch") %>%
  ggplot(aes(x = year, y = value, color = scenario)) +
  facet_wrap(~ critter) +
  geom_line(size = 1) +
  labs(title = "Catch Over Time") +
  theme_minimal()

combined_df %>%
  filter(metric == "biomass") %>%
  ggplot(aes(x = year, y = value, color = scenario)) +
  facet_wrap(~ critter) +
  geom_line(size = 1) +
  labs(title = "Biomass Over Time") +
  theme_minimal()

combined_df %>%
  filter(metric == "ssb") %>%
  ggplot(aes(x = year, y = value, color = scenario)) +
  facet_wrap(~ critter) +
  geom_line(size = 1) +
  labs(title = "Spawning Stock Biomass Over Time") +
  theme_minimal()

