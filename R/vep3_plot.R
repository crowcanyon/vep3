vep3_plot <- function(
  # legend = TRUE
){
  list(
    add_hillshade(),
    ggplot2::labs(x = NULL, y = NULL),
    vep3_theme_map(),
    ggplot2::coord_equal()
  )
}
