# Import necessary libraries
library(marlin)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(mvtnorm)

# Run_marlin is the function running in lambda
run_marlin <- function(mpa_locations_in) {
  if (is.list(mpa_locations_in)) {
    mpa_locations <- mpa_locations_in 
  } else if (is.character(mpa_locations_in)) {
    mpa_locations <- fromJSON(mpa_locations_in)
  } else {
    stop("Invalid input format")
  }
  
  if (!is.data.frame(mpa_locations)) {
    mpa_locations <- do.call(rbind, lapply(mpa_locations, as.data.frame))
  }
  
  print("MPA locations:")
  print(mpa_locations, row.names = FALSE)
  
  # Create baseline parameters
  print("Creating baseline parameters")
  years <- 100
  seasons <- 1
  time_step <- 1 / seasons
  snapper_diffusion <- 1 # km^2/year
  lobster_diffusion <-  0.5 # km^2/year
  max_hab_mult = 20
  
  print("Load habitat layers")
  reef_ras <- read.csv("data/reef_ras.csv")
  seagrass_ras <- read.csv("data/seagrass_ras.csv")
  seagrass_reef_ras <- read.csv("data/seagrass_reef_ras.csv")
  
  colnames(reef_ras) <- c("x","y","layer")
  colnames(seagrass_ras) <- c("x","y","layer")
  colnames(seagrass_reef_ras) <- c("x","y","layer")
  
  reef_ras <- reef_ras %>%
    filter(y <= 18.225, y > 15.985) %>%
    mutate(
      layer = ifelse(is.na(layer), 0, layer),
      layer = layer/max(layer)
    )
  seagrass_ras <- seagrass_ras %>%
    filter(y <= 18.225, y > 15.985) %>%
    mutate(
      layer = ifelse(is.na(layer), 0, layer),
      layer = layer/max(layer)
    )
  seagrass_reef_ras <- seagrass_reef_ras %>%
    filter(y <= 18.225, y > 15.985) %>%
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
  print("Gather spatial parameters")
  patches <- nrow(juvenile_habitat)*ncol(juvenile_habitat)
  patch_area <- 57 # km2
  simulation_area <- patch_area * patches #km2
  resolution <- c(nrow(juvenile_habitat), ncol(juvenile_habitat))
  
  # Define species
  # Mutton snapper, Lutjanus analis
  print("Define species")
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
  print("Define fleets")
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
  
  
  print("Run MPA simulation")
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
  
  patch_MPA <-
    map_df(mpa_sim, ~ map_df(.x, ~ tibble(
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
    mutate(scenario = "Proposed MPA")
  
  print("Returning")
  print(toJSON(patch_MPA))
  json_body <- toJSON(patch_MPA, auto_unbox = TRUE)
  return(
    list(
      statusCode = 200,
      headers = list("Content-Type" = "application/json"),
      body = json_body
    )
  )
}

lambdr::start_lambda()
