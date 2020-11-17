add_hillshade <- function(){

  # Plot the hillshade using the "alpha hack"
  list(
    ggplot2::geom_raster(data = vep3_hillshade_drained %>%
                           get_df(),
                         mapping = aes(x = x,
                                       y = y,
                                       alpha = ID),
                         na.rm = TRUE),
    scale_alpha(range = c(0.8, 0),
                na.value = 0,
                limits = c(0,255),
                guide = "none")
  )
}
