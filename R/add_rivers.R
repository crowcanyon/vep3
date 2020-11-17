add_rivers <- function(){
  # Plot the county boundaries
  geom_sf(data =
            vep3_nhd$Flowline %>%
            dplyr::filter(GNIS_NAME %in% c("Mancos River",
                                           "Dolores River",
                                           "San Juan River",
                                           "McElmo Creek",
                                           "Montezuma Creek",
                                           "Recapture Creek",
                                           # "Cottonwood Creek",
                                           "West Dolores River",
                                           "La Plata River",
                                           "Animas River",
                                           "San Miguel River",
                                           "Cowboy Wash",
                                           "Marble Wash"
            )),
          size = 0.25
  )
}
