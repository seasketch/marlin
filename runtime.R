library(marlin)
library(jsonlite)

run_marlin <- function(number) {
  print(paste("input =", number))
  resolution <- c(5,10) 
  years <- number
  seasons <- 4
  time_step <- 1 / seasons
  steps <- years * seasons
  
  print("Creating marlin fauna object for bigeye tuna")
  fauna <- 
    list(
      "bigeye" = create_critter(
        common_name = "bigeye tuna",
        adult_diffusion = 10,
        density_dependence = "post_dispersal",
        seasons = seasons,
        fished_depletion = .25,
        resolution = resolution,
        steepness = 0.6,
        ssb0 = 42,
        m = 0.4
      )
    )
  print(fauna)
  
  print("Returning max age of bigeye tuna")
  return(fauna$bigeye$max_age)
}

lambdr::start_lambda()
