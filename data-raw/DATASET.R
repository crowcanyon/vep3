library(sf)
library(tigris)
library(mapview)
library(magrittr)
library(raster)

options(scipen = 999)

as_proj <- function(x){
  x %>%
    {paste0("+",names(.),"=",., collapse = " ")}
}

vep3_projection <-
  list(proj = "omerc",
       lat_0 = 36.998981,
       lonc = -109.045189,
       alpha = 0,
       gamma = 0,
       k_0 = 1,
       x_0 = 0,
       y_0 = 0
  ) %>%
  as_proj() %>%
  sf::st_crs()

usethis::use_data(vep3_projection, overwrite = TRUE)

vepii_cmv_boundary <-
  villager::vepii_cmv_boundary %>%
  sf::st_transform(vep3_projection)

usethis::use_data(vepii_cmv_boundary, overwrite = TRUE)




simple_hydro_read <- function(x){

  out <-
    x %>%
    sf::read_sf(layer = "WBDHU12") %>%
    dplyr::select(HUC12, Name) %>%
    dplyr::rename(`Hydrologic_Unit` = HUC12) %>%
    magrittr::set_names(c("Hydrologic_Unit","Subwatershed","geometry")) %>%
    sf::st_set_geometry("geometry")

  hydrologic_units <-
    c("Subregion" = 4,
      "Basin" = 6,
      "Subbasin" = 8,
      "Watershed" = 10)

  for(i in seq(4,10,2)){
    out %<>%
      dplyr::mutate(HUC = stringr::str_sub(`Hydrologic_Unit`, end = i)) %>%
      dplyr::left_join(sf::read_sf(x,
                                   layer = paste0("WBDHU",i)) %>%
                         sf::st_drop_geometry() %>%
                         dplyr::rename(HUC = paste0("HUC",i)) %>%
                         dplyr::select(HUC, Name) %>%
                         magrittr::set_names(c("HUC",
                                               names(hydrologic_units)[hydrologic_units == i])) %>%
                         dplyr::distinct()) %>%
      dplyr::select(-HUC)
  }
  out %>%
    dplyr::select(`Hydrologic_Unit`, Subregion:Watershed, Subwatershed, geometry) %>%
    sf::st_transform(4326)
}

extract_river <- function(x, river){
  x %>%
    sf::read_sf(layer = "NHDFlowline") %>%
    sf::st_zm() %>%
    dplyr::filter(GNIS_Name == river) %>%
    sf::st_union() %>%
    sf::st_line_merge()
}

cut_hydro <- function(x, river, side){
  x %>%
    sf::st_intersection(x %>%
                          sf::st_union() %>%
                          lwgeom::st_split(river) %>%
                          sf::st_collection_extract("POLYGON") %>%
                          sf::st_make_valid() %>%
                          magrittr::extract(side))
}


# Read hydrography data

vep3_areas <-
  list(
    `Upper Dolores` = c("1403000201",
                        "1403000202",
                        "1403000203",
                        "1403000204",
                        "1403000206"),
    `Mancos` = c("14080107",
                 # "140801051005",
                 "140802010205"),
    `Marble/Cowboy` = c("1408020102"),
    `McElmo` = c("14080202",
                 "140802010502",
                 "140802010504"),
    `Montezuma` = c("14080203",
                    "140802010507",
                    "140802010508",
                    "140802010509"),
    `Recapture` = c("1408020103")
  )

# San Juan basin
httr::GET(
  url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHDPlusHR/Beta/GDB/NHDPLUS_H_1408_HU4_GDB.zip",
  httr::write_disk(tempfile(fileext = ".zip")),
  httr::progress()
) %$%
  content %>%
  magrittr::extract2(1) %>%
  unzip(exdir = tempdir())

nhdplus_1408 <- paste0(tempdir(),"/NHDPLUS_H_1408_HU4_GDB.gdb/")

san_juan_river <-
  extract_river(nhdplus_1408, "San Juan River")

san_juan <-
  simple_hydro_read(nhdplus_1408)

san_juan %<>%
  dplyr::filter(!(stringr::str_starts(`Hydrologic_Unit`, "1408010101")),
                !(stringr::str_starts(`Hydrologic_Unit`, "1408010102"))) %>%
  cut_hydro(san_juan_river, 2)

# Dolores basin
httr::GET(
  url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHDPlusHR/Beta/GDB/NHDPLUS_H_1403_HU4_GDB.zip",
  httr::write_disk(tempfile(fileext = ".zip")),
  httr::progress()
) %$%
  content %>%
  magrittr::extract2(1) %>%
  unzip(exdir = tempdir())
nhdplus_1403 <- paste0(tempdir(),"/NHDPLUS_H_1403_HU4_GDB.gdb/")

dolores_river <-
  extract_river(nhdplus_1403, "Dolores River")

dolores <-
  simple_hydro_read(nhdplus_1403)


subsample <-
  function(x,watersheds){
    x %>%
      dplyr::filter(
        HUC12 %>%
          purrr::map_lgl(
            function(y){
              watersheds %>%
                purrr::map_lgl(
                  function(z){
                    stringr::str_starts(y,z)
                  }) %>%
                any()
            })
      )
  }

vep3_wbd <-
  rbind(san_juan,
        dolores) %>%
  dplyr::mutate(`Study_Area` = NA) %>%
  dplyr::select(`Study_Area`, dplyr::everything())

vep3_areas %>%
  purrr::iwalk(function(x,i){
    x %>%
      purrr::walk(function(area){
        vep3_wbd <<- vep3_wbd %>%
          dplyr::mutate(`Study_Area` =
                          ifelse(
                            stringr::str_starts(`Hydrologic_Unit`,area) & is.na(`Study_Area`),
                            i,
                            `Study_Area`)
          )
      })
  })

vep3_wbd %<>%
  dplyr::filter(!is.na(`Study_Area`)) %>%
  sf::st_cast("MULTIPOLYGON") %>%
  dplyr::arrange(Hydrologic_Unit, Study_Area, Hydrologic_Unit) %>%
  sf::st_transform(vep3_projection)

usethis::use_data(vep3_wbd,
                  overwrite = TRUE)


# Surface Land Management
httr::GET(
  url = "https://gis.blm.gov/EGISDownload/LayerPackages/BLM_National_Surface_Management_Agency.zip",
  httr::write_disk(tempfile(fileext = ".zip")),
  httr::progress()
) %$%
  content %>%
  magrittr::extract2(1) %>%
  unzip(exdir = tempdir())

vep3_surface_management <-
  paste0(tempdir(),
         "/BLM_National_Surface_Management_Agency/sma_wm.gdb") %>%
  sf::read_sf(layer = "SurfaceManagementAgency") %>%
  dplyr::filter(ADMIN_ST %in% c("AZ","CO","NM","UT"),
                ADMIN_AGENCY_CODE != "OTHFE",
                !(ADMIN_DEPT_CODE %in% c("PVT", "UND"))) %>%
  sf::st_cast("MULTIPOLYGON") %>%
  dplyr::select(Name = ADMIN_UNIT_NAME,
                Agency = ADMIN_AGENCY_CODE,
                Department = ADMIN_DEPT_CODE) %>%
  dplyr::filter(.,
                sf::st_intersects(.,
                                  vep3_wbd %>%
                                    sf::st_union() %>%
                                    sf::st_transform(3857),
                                  sparse = FALSE)[,1]) %>%
  sf::st_make_valid() %>%
  sf::st_intersection(vep3_wbd %>%
                        sf::st_union() %>%
                        sf::st_transform(3857)) %>%
  dplyr::mutate(Agency = ifelse(Name %in% c("Navajo Reservation",
                                            "Ute Mountain Reservation",
                                            "Southern Ute Reservation"),
                                "Tribal Land",
                                Agency),
                Department = ifelse(Name %in% c("Navajo Reservation",
                                                "Ute Mountain Reservation",
                                                "Southern Ute Reservation"),
                                    "Tribal Land",
                                    Department),
                Name = ifelse(Agency %in% c("LG","ST"),
                              NA,
                              Name),
                Agency  = ifelse(Agency %in% c("LG","ST"),
                                 "Local/State",
                                 Agency),
                Department  = ifelse(Department %in% c("LG","ST"),
                                     "Local/State",
                                     Department),
                Agency  = ifelse(Agency %in% c("USBR", "FWS"),
                                 "USFS",
                                 Agency),
                Department  = ifelse(Agency %in% c("USFS"),
                                     "USDA",
                                     Department)) %>%
  dplyr::filter(!(Agency %in% c("BIA","DOE"))) %>%
  dplyr::group_by(Name, Agency, Department) %>%
  dplyr::summarise() %>%
  dplyr::ungroup() %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_transform(vep3_projection)

usethis::use_data(vep3_surface_management,
                  overwrite = TRUE)

# Bears Ears
# Current
httr::GET(
  url = "https://prod-hub-indexer.s3.amazonaws.com/files/feaa4faae66747a792b6d664c1159fc7/0/full/4326/feaa4faae66747a792b6d664c1159fc7_0_full_4326.gdb.zip",
  httr::write_disk(tempfile(fileext = ".zip")),
  httr::progress()
) %$%
  content %>%
  magrittr::extract2(1) %>%
  unzip(exdir = paste0(tempdir(),"/bears-ears-current"))

# Historic d5560a571a86436fbb3a664c3edd5488_0
httr::GET(
  url = "https://opendata.arcgis.com/api/v3/datasets/d5560a571a86436fbb3a664c3edd5488_0/downloads/data?format=fgdb&spatialRefId=3857",
  httr::write_disk(tempfile(fileext = ".zip")),
  httr::progress()
) %$%
  content %>%
  magrittr::extract2(1) %>%
  unzip(exdir = paste0(tempdir(),"/bears-ears-historic"))



vep3_bears_ears <-
  list.files(paste0(tempdir(),"/bears-ears-current"),
             pattern = ".gdb",
             full.names = TRUE) %>%
  sf::read_sf() %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326) %>%
  dplyr::filter(stringr::str_starts(NLCS_NAME, "Bears Ears")) %>%
  dplyr::mutate(NLCS_NAME = "Bears Ears National Monument — Current Designation") %>%
  dplyr::select(Name = NLCS_NAME) %>%
  dplyr::group_by(Name) %>%
  dplyr::summarise() %>%
  dplyr::ungroup() %>%
  rbind(
    list.files(paste0(tempdir(),"/bears-ears-historic"),
               pattern = ".gdb",
               full.names = TRUE)  %>%
      sf::read_sf() %>%
      sf::st_cast("MULTIPOLYGON") %>%
      sf::st_make_valid() %>%
      sf::st_transform(4326) %>%
      dplyr::filter(NAME == "Bears Ears National Monument") %>%
      # sf::st_intersection(counties) %>%
      dplyr::select(Name = NAME) %>%
      dplyr::group_by(Name) %>%
      dplyr::summarise() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Name = "Bears Ears National Monument — Original Designation")
  ) %>%
  dplyr::arrange(.,-sf::st_area(.)) %>%
  sf::st_transform(vep3_projection)

usethis::use_data(vep3_bears_ears,
                  overwrite = TRUE)

# Native Nations in TIGRIS
vep3_native_nations <-
  tigris::native_areas(class = "sf") %>%
  dplyr::select(Nation = NAME) %>%
  dplyr::filter(sf::st_intersects(.,
                                  vep3_wbd %>%
                                    sf::st_union() %>%
                                    sf::st_transform(4269),
                                  sparse = FALSE)[,1]) %>%
  # sf::st_intersection(nsj_dolores_colorado %>%
  #                       sf::st_union() %>%
  #                       sf::st_transform(4269)) %>%
  sf::st_cast("MULTIPOLYGON") %>%
  dplyr::group_by(Nation) %>%
  dplyr::summarise() %>%
  sf::st_transform(vep3_projection)

usethis::use_data(vep3_native_nations,
                  overwrite = TRUE)

# Digital Elevation Model and Hillshade

vep3_buffer <-
  vep3_wbd %>%
  sf::st_union() %>%
  sf::st_transform(vep3_projection) %>%
  sf::st_buffer(10000)

vep3_ned <-
  FedData::get_ned(sf::st_as_sf(vep3_buffer),
                   label = "vep3")

vep3_ned_drained <-
  demdrainer::drain_dem(x = vep3_ned,
                        label = "vep3")

raster::writeRaster(vep3_ned,
                    paste0(tempdir(),"/vep3_ned.tif"),
                    datatype = "FLT4S",
                    options = c(
                      "COMPRESS=DEFLATE",
                      "ZLEVEL=9",
                      "INTERLEAVE=BAND"
                    ),
                    overwrite = TRUE,
                    setStatistics = FALSE
)

raster::writeRaster(vep3_ned_drained,
                    paste0(tempdir(),"/vep3_ned_drained.tif"),
                    datatype = "FLT4S",
                    options = c(
                      "COMPRESS=DEFLATE",
                      "ZLEVEL=9",
                      "INTERLEAVE=BAND"
                    ),
                    overwrite = TRUE,
                    setStatistics = FALSE)

system(paste0("gdaldem hillshade ",
              tempdir(), "/vep3_ned.tif ",
              tempdir(), "/vep3_hillshade.tif",
              " -z 2 -s 111120 -multidirectional -co 'COMPRESS=DEFLATE' -co 'ZLEVEL=9'"))

system(paste0("gdaldem hillshade ",
              tempdir(), "/vep3_ned_drained.tif ",
              tempdir(), "/vep3_hillshade_drained.tif",
              " -z 2 -s 111120 -multidirectional -co 'COMPRESS=DEFLATE' -co 'ZLEVEL=9'"))

## Generate a hillshade for statewide mapping
aggregate_longlat <-
  function(x, res, fun = 'mean'){
    scale.x <- geosphere::distGeo(c(xmin(x),mean(ymin(x),ymax(x))),
                                  c(xmax(x),mean(ymin(x),ymax(x)))) %>%
      magrittr::divide_by(ncol(x))

    factor.x <- (res/scale.x) %>%
      floor()

    scale.y <- geosphere::distGeo(c(mean(xmin(x),xmax(x)),ymin(x)),
                                  c(mean(xmin(x),xmax(x)),ymax(x))) %>%
      magrittr::divide_by(nrow(x))

    factor.y <- (res/scale.y) %>%
      floor()

    x.vx <- velox::velox(x)

    x.vx$aggregate(factor = c(factor.x, factor.y),
                   aggtype = fun)

    if(is(x,"RasterBrick")) return(x.vx$as.RasterBrick())
    if(is(x,"RasterStack")) return(x.vx$as.RasterStack())
    return(x.vx$as.RasterLayer(band=1))
  }

vep3_hillshade <-
  raster::raster(paste0(tempdir(), "/vep3_ned.tif ")) %>%
  aggregate_longlat(res = 100) %>%
  raster::projectRaster(crs = vep3_projection$input) %>%
  raster::crop(vep3_buffer %>%
                 as("Spatial"),
               snap = 'in') %>%
  round() %>%
  raster::trim(padding = -1)

vep3_hillshade[]=as.integer(vep3_hillshade[])

raster::dataType(vep3_hillshade) <- "INT1U"

vep3_hillshade_drained <-
  raster::raster(paste0(tempdir(), "/vep3_hillshade_drained.tif")) %>%
  aggregate_longlat(res = 100) %>%
  raster::projectRaster(crs = vep3_projection$input) %>%
  raster::crop(vep3_buffer %>%
                 as("Spatial"),
               snap = 'in') %>%
  round() %>%
  raster::trim(padding = -1)

vep3_hillshade_drained[]=as.integer(vep3_hillshade_drained[])

raster::dataType(vep3_hillshade_drained) <- "INT1U"

vep3_ned %<>%
  raster::projectRaster(vep3_hillshade) %>%
  round(1)

vep3_ned_drained %<>%
  raster::projectRaster(vep3_hillshade) %>%
  round(1)

usethis::use_data(vep3_ned,
                  overwrite = TRUE)

usethis::use_data(vep3_ned_drained,
                  overwrite = TRUE)

usethis::use_data(vep3_hillshade,
                  overwrite = TRUE)

usethis::use_data(vep3_hillshade_drained,
                  overwrite = TRUE)

vep3_nhd <-
  FedData::get_nhd(vep3_ned,
                   label = "vep3",
                   force.redo = TRUE) %>%
  purrr::map(sf::st_transform,
             vep3_projection)

usethis::use_data(vep3_nhd,
                  overwrite = TRUE)


# vep3_ssurgo <-
#   FedData::get_ssurgo(label = "vep3")
# vep3_ssurgo$spatial %<>%
#   sf::st_transform(vep3_projection)
#
# usethis::use_data(vep3_ssurgo,
#                   overwrite = TRUE)





# library(ggplot2)
#
# ggplot2::ggplot() +
#   vep3_plot() +
#   geom_sf(data =
#             vep3_wbd %>%
#             dplyr::group_by(Study_Area) %>%
#             dplyr::summarise(),
#           aes(fill = Study_Area),
#           color = "black",
#           alpha = 0.5,
#           show.legend = FALSE) +
#   add_rivers()


