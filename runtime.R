# Import necessary libraries
library(marlin)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(raster)
library(readr)

# Run_marlin is the function running in lambda
run_marlin <- function(number) {
  # Ensure piping is working
  print(paste("input =", number))
  
  # Create baseline parameters
  years <- 100
  seasons <- 1
  time_step <- 1 / seasons
  
  # Set diffusion rates
  snapper_diffusion <- 1 # km^2/year
  lobster_diffusion <-  0.5 # km^2/year
  max_hab_mult = 20
  
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
  
  # Map habitats
  belize <- ne_countries(country = "Belize", returnclass = "sf")
  
  ggplot() +
    geom_tile(data = seagrass_ras, aes(x = x, y = y, fill = layer)) +
    labs(title = "Seagrass / Juvenile Habitat") +
    geom_sf(data = belize, fill = "gray90", color = "gray50") 
  
  ggplot() +
    geom_tile(data = reef_ras, aes(x = x, y = y, fill = layer)) +
    labs(title = "Reef / Reef Habitat") +
    geom_sf(data = belize, fill = "gray90", color = "gray50") 
  
  ggplot() +
    geom_tile(data = seagrass_reef_ras, aes(x = x, y = y, fill = layer)) +
    labs(title = "Seagrass Reef / Lobster Habitat") +
    geom_sf(data = belize, fill = "gray90", color = "gray50") 

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
  
  write_rds(
    list(fauna = fauna, fleets = fleets),
    file = file.path("data/fauna_and_fleets.rds")
  )

  
  
  # Simulate baseline dynamic, without any MPAs
  no_mpa_sim <- simmar(fauna = fauna,
                     fleets = fleets,
                     years = 100)
  
  prs_nompa <- process_marlin(no_mpa_sim, keep_age = FALSE)
  
  plot_marlin(prs = prs_nompa, plot_var = "ssb")
  plot_marlin(prs = prs_nompa, plot_var = "ssb", plot_type = "space")
  
  patch_noMPA <-
    map_df(no_mpa_sim, ~ map_df(.x, ~ tibble(
      catch = rowSums(.x$c_p_fl),
      biomass = rowSums(.x$b_p_a),
      ssb = rowSums(.x$ssb_p_a),
      patch = 1:nrow(.x$ssb_p_a)
    ), .id = "critter"), .id = "step") %>% 
    separate(step, "_", into = c("year", "season")) %>% 
    mutate(year = as.double(year)) %>% 
    group_by(year, critter) %>%
    summarise(catch_noMPA = sum(catch),
              biomass_noMPA = sum(biomass),
              ssb_noMPA = sum(ssb))
  
  
  # Simulate dynamics with only existing MPAs
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
  
  ggplot() +
    geom_sf(data = belize, fill = "gray90", color = "gray50") +
    geom_tile(data = mpa_spatial, aes(x = x, y = y, fill = mpa)) +
    geom_sf(data = mpa, fill = NA, color = "gray50") +
    labs(title = "MPA Locations") 
  
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
  
  plot_marlin(prs = prs_existing, plot_var = "ssb")
  
  patch_existing_MPA <-
    map_df(existing_mpa_sim, ~ map_df(.x, ~ tibble(
      catch = rowSums(.x$c_p_fl),
      biomass = rowSums(.x$b_p_a),
      ssb = rowSums(.x$ssb_p_a),
      patch = 1:nrow(.x$ssb_p_a)
    ), .id = "critter"), .id = "step") %>% 
    separate(step, "_", into = c("year", "season")) %>% 
    mutate(year = as.double(year)) %>% 
    group_by(year, critter) %>% 
    summarise(catch_existingMPA = sum(catch),
              biomass_existingMPA = sum(biomass),
              ssb_existingMPA = sum(ssb))
  
  ## Plot catch
  plot_marlin(prs_existing, max_scale = TRUE, plot_var = "c", plot_type = "space")
  
  existing_catch <- prs_existing$fleets |>
    filter(step == max(step)) |>
    group_by(x,y) |>
    summarise(catch = sum(catch), .groups = "drop") %>%
    rename(id.x = x, id.y = y)
  
  catch_located <- existing_catch %>%
    left_join(x_id, by = "id.x") %>%
    left_join(y_id, by = "id.y") %>%
    dplyr::select(x, y, catch)
  
  ggplot() +
    geom_tile(data = catch_located, aes(x,y, fill = catch)) +
    geom_sf(data = belize, fill = "gray90", color = "gray50") +
    geom_sf(data = mpa, fill = NA, color = "gray50") +
    labs(title = "Snapper Catch") 
  
  
  
  # Simulate dynamics with existing MPAs plus proposed MPAs
  mpa <- st_read("data/sketch.geojson.json")
  existingmpa  <- st_read("data/Existing-MPAs.geojson.json")
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
  
  ggplot() +
    geom_sf(data = belize, fill = "gray90", color = "gray50") +
    geom_tile(data = mpa_spatial, aes(x = x, y = y, fill = mpa)) +
    geom_sf(data = mpa, fill = NA, color = "gray50") +
    geom_sf(data = existingmpa, fill = NA, color = "gray50") +
    labs(title = "MPA Locations") 
  
  mpa_locations <- mpa_spatial %>% 
    left_join(x_id, by = "x") %>% 
    left_join(y_id, by = "y") %>% 
    dplyr::select(id.x, id.y, mpa) %>% 
    rename(y=id.y,
           x=id.x) %>% 
    dplyr::select(x, y, mpa)
  
  mpa_sim <- simmar(
    fauna = fauna,
    fleets = fleets,
    manager = list(mpas = list(
      locations = mpa_locations,
      mpa_year = 50
    )),
    years = 100
  )
  
  prs_sketch <- process_marlin(mpa_sim, keep_age = FALSE)
  
  ## Plot catch
  plot_marlin(prs_sketch, max_scale = TRUE, plot_var = "c", plot_type = "space")
  
  sketch_catch <- prs_sketch$fleets |>
    filter(step == max(step)) |>
    group_by(x,y) |>
    summarise(catch = sum(catch), .groups = "drop") %>%
    rename(id.x = x, id.y = y)
  
  catch_located <- sketch_catch %>%
    left_join(x_id, by = "id.x") %>%
    left_join(y_id, by = "id.y") %>%
    dplyr::select(x, y, catch)
  
  ggplot() +
    geom_tile(data = catch_located, aes(x,y, fill = catch)) +
    geom_sf(data = belize, fill = "gray90", color = "gray50") +
    geom_sf(data = mpa, fill = NA, color = "gray50") +
    geom_sf(data = existingmpa, fill = NA, color = "gray50") +
    labs(title = "Snapper Catch") 
  
  
  patch_MPA <-
    map_df(mpa_sim, ~ map_df(.x, ~ tibble(
      catch = rowSums(.x$c_p_fl),
      biomass = rowSums(.x$b_p_a),
      ssb = rowSums(.x$ssb_p_a),
      patch = 1:nrow(.x$ssb_p_a)
    ), .id = "critter"), .id = "step") %>% 
    separate(step, "_", into = c("year", "season")) %>% 
    mutate(year = as.double(year)) %>% 
    group_by(year, critter) %>% 
    summarise(catch_MPA = sum(catch),
              biomass_MPA = sum(biomass),
              ssb_MPA = sum(ssb))
  
  # Compare outcomes
  plot_marlin(
    `MPA: Existing` = prs_existing,
    `No MPA` = prs_nompa,
    `MPA: Existing + Sketch` = prs_sketch,
    steps_to_plot = 50:100,
    plot_var = "c",
    max_scale = FALSE
  )
  
  ## Bar chart
  patch_dta = patch_noMPA %>% 
    left_join(patch_existing_MPA) %>% 
    left_join(patch_MPA) %>% 
    filter(year == 100) %>% 
    reshape2::melt(id.vars = c("year", "critter")) %>% 
    separate(variable, "_", into = c("variable","is_MPA")) %>% 
    as.data.frame()
  
  ggplot(data = patch_dta, aes(x=variable, y=value, fill = is_MPA)) +
    facet_wrap(~critter) +
    geom_col(position = "dodge")
  
  ## Over time
  patch_noMPA <- patch_noMPA %>%
    rename(
      year = year,                    
      catch    = catch_noMPA,
      biomass  = biomass_noMPA,
      ssb  = ssb_noMPA
    ) %>%
    mutate(scenario = "No MPA")
  
  patch_existing_MPA <- patch_existing_MPA %>%
    rename(
      year = year,
      catch    = catch_existingMPA,
      biomass  = biomass_existingMPA,
      ssb  = ssb_existingMPA
    ) %>%
    mutate(scenario = "Existing MPA")
  
  patch_MPA <- patch_MPA %>%
    rename(
      year = year,
      catch    = catch_MPA,
      biomass  = biomass_MPA,
      ssb  = ssb_MPA
    ) %>%
    mutate(scenario = "Proposed MPA")
  
  combined_df <- bind_rows(
    patch_noMPA,
    patch_existing_MPA,
    patch_MPA
  )
  
  # Plot catch
  ggplot(combined_df %>% filter(year >= 45), aes(x = year, y = catch, color = scenario)) +
    facet_wrap(~critter) +
    geom_line(size = 1) +
    labs(title = "Catch over Time",
         x = "Year",
         y = "Catch") +
    theme_minimal()
  
  # Plot BIOMASS
  ggplot(combined_df %>% filter(year >= 45), aes(x = year, y = biomass, color = scenario)) +
    facet_wrap(~critter) +
    geom_line(size = 1) +
    labs(title = "Biomass over Time",
         x = "Year",
         y = "Biomass") +
    theme_minimal()
  
  # Plot Spawning stock biomass
  ggplot(combined_df %>% filter(year >= 45), aes(x = year, y = ssb, color = scenario)) +
    facet_wrap(~critter) +
    geom_line(size = 1) +
    labs(title = "SSB over Time",
         x = "Year",
         y = "SSB") +
    theme_minimal()
  
  # Plot ssb heatmap
  ssb_summary <- prs_sketch$fauna |>
    filter(step == max(step)) |>
    group_by(critter,x,y) |>
    summarise(ssb = sum(ssb)) 
  
  ssb_located = ssb_summary %>% 
    rename(id.x = x) %>% 
    rename(id.y = y) %>% 
    left_join(x_id, by="id.x") %>% 
    left_join(y_id, by="id.y") 
  
  ggplot() +
    geom_sf(data = belize, fill = "gray90", color = "gray50") +
    geom_tile(data = ssb_located, aes(x = x, y = y, fill = ssb)) +
    facet_wrap(~critter) +
    scale_fill_viridis_c() +
    geom_sf(data = mpa_union, fill = NA, color = "gray50")
  
  # Plot catch heatmap
  catch_summary <- prs_sketch$fleets |>
    filter(step == max(step)) |>
    group_by(fleet,x,y) |>
    summarise(catch = sum(catch)) 
  
  catch_located = catch_summary %>% 
    rename(id.x = x) %>% 
    rename(id.y = y) %>% 
    left_join(x_id, by="id.x") %>% 
    left_join(y_id, by="id.y") 
  
  ggplot() +
    geom_sf(data = belize, fill = "gray90", color = "gray50") +
    geom_tile(data = catch_located, aes(x = x, y = y, fill = catch)) +
    facet_wrap(~fleet) +
    scale_fill_viridis_c() +
    geom_sf(data = mpa_union, fill = NA, color = "gray50")
  
  
  
  fleet_summary <- prs_sketch$fleets |>
    filter(step == max(step)) |>
    group_by(fleet,x,y) |>
    summarise(catch = sum(catch),
              effort = sum(effort)) |>
    mutate(cpue = catch / effort)
  
  

  fleet_summary |>
    ggplot(aes(x,y,fill = cpue)) +
    geom_tile() +
    facet_wrap(~fleet) +
    scale_fill_viridis_c()
  
  belize <- ne_countries(country = "Belize", returnclass = "sf")
  geo_located = fleet_summary %>% 
    rename(id.x = x) %>% 
    rename(id.y = y) %>% 
    left_join(x_id) %>% 
    left_join(y_id) 
  
  ggplot() +
    geom_sf(data = belize, fill = "gray90", color = "gray50") +
    geom_tile(data = geo_located, aes(x = x, y = y, fill = cpue)) +
    facet_wrap(~fleet) +
    scale_fill_viridis_c() 
  
  fleet_summary |>
    ggplot(aes(x,y,fill = cpue)) +
    geom_tile() +
    facet_wrap(~fleet) +
    scale_fill_viridis_c() + 
    geom_sf(data = belize, fill = "gray90", color = "gray50", inherit.aes = FALSE) 

  cpue = geo_located %>% 
    filter(fleet == "snapper_fleet") %>%
    ungroup() %>%
    dplyr::select(x, y, cpue) 
  
  r <- rast(
    cpue,
    type = "xyz",          # indicates the columns are x, y, and the value
    crs  = "EPSG:4326"     # set this to the correct CRS for your data
  )
  
  # (Optional) Rename the single layer to something more descriptive
  names(r) <- "cpue"
  
  # Write out as a GeoTIFF
  writeRaster(r, "cpue.tif", overwrite = TRUE)
  
  fleet_summary |>
    ggplot(aes(x,y,fill = cpue)) +
    geom_tile() +
    
  
  
  return(fauna$bigeye$max_age)
}

lambdr::start_lambda()
