# helper functions for visualisation

# Graphic Theme for all maps
# map_theme <- function(base_size = 12){
#   theme(
#     axis.text =          element_blank(),
#     axis.title =         element_blank(),
#     panel.background =   element_blank(),
#     panel.grid.major =   element_blank(),
#     panel.grid.minor =   element_blank(),
#     axis.ticks.length =  unit(0, "cm"),
#     complete = TRUE
#   )
# }


theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      ...
    )
}

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
