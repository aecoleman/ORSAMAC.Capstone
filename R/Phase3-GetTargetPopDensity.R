#' Get Target Population Density
#'
#' Reads in shapefiles describing the geometry of the region, and some data
#' files that describe the population of each geometry. Finds the geometry
#' containing each target's spatialPoint. Note that not all spatialPoints will
#' belong to a geometry.
#'
#' @return data.table containing list of targets and zctas
#' @export
#'
#' @import data.table
GetTargetPopDensity <- function(){

  ## Read in data ################################################################

  zcta.pop <-
    readRDS(
      system.file( 'data/2016_acs_zcta_population.RDS',
                   package = 'ORSAMAC.Capstone' ) )

  ## Read in shapefiles ##########################################################

  attica.polys <-
    rgdal::readOGR( dsn = system.file('extData/shapefiles/ply.phase4.atticaDivisions',
                               package = 'ORSAMAC.Capstone'),
             layer = 'Attica_Ph4')

  zcta.polys <-
    rgdal::readOGR(
      dsn = system.file('extData/shapefiles/census/cb_2016_us_zcta510_500k',
                        package = 'ORSAMAC.Capstone'),
      layer = 'cb_2016_us_zcta510_500k',
      stringsAsFactors = FALSE)

  ## Transform Attica polygons to CRS used for zctas #############################

  crsTransform <- zcta.polys@proj4string

  attica.region.polys <- spTransform(attica.polys, crsTransform)

  rm(crsTransform)

  ## Subset zcta.polys by removing all zctas not within Attica ###################

  attica.zcta.polys <- zcta.polys[attica.region.polys,]

  rm( attica.polys, zcta.polys )

  ## Format Population Data for joining with Spatial Data ########################

  dt.zcta.pop <-
    data.table(
      'ZCTA5CE10' = zcta.pop@geography['zipcodetabulationarea'],
      'POP5ACS16' = zcta.pop@estimate )

  setnames( dt.zcta.pop,
            names(dt.zcta.pop),
            gsub('^(.*)\\..*$', '\\1', names(dt.zcta.pop) ) )

  # So we can merge with the geo information by ZCTA
  setkey(dt.zcta.pop, ZCTA5CE10)

  dt.zcta.geo <-
    data.table( attica.zcta.polys@data )

  setkey(dt.zcta.geo, ZCTA5CE10)

  # Merge pop and geo data for ZCTAs
  dt.zcta.pop <- dt.zcta.pop[dt.zcta.geo]

  rm( dt.zcta.geo )

  dt.zcta.pop[,DENSITY := POP5ACS16 / ( (as.numeric(ALAND10) + as.numeric(AWATER10) ) ) ]

  dt.zcta.pop[ DENSITY >= 1000,
               'CLASSIFICATION' := 'Urban' ]

  dt.zcta.pop[ DENSITY >= 500 & is.na(CLASSIFICATION), 'CLASSIFICATION' := 'Suburban']

  ## Get target locations ########################################################

  tgt.locations <-
    rgdal::readOGR(
      dsn = system.file('extData/shapefiles/pt.phase3.target',
                        package = 'ORSAMAC.Capstone'),
      layer = 'Phase_3_Targets',
      stringsAsFactors = FALSE)

  crsTransform <- attica.zcta.polys@proj4string

  tgt.locations <- spTransform(tgt.locations, crsTransform)

  rm(crsTransform)

  tgt.zctas <- over(tgt.locations, attica.zcta.polys, returnList = TRUE)

  dt.tgt.zcta <-
    data.table( 'tgt.id' = tgt.locations@data$Location,
                'hasZCTA' = sapply( tgt.zctas, FUN = function(x){ length(x$ZCTA5CE10) > 0 }, USE.NAMES = FALSE ) )

  dt.tgt.zcta[hasZCTA == TRUE, 'ZCTA5CE10' := unlist( lapply(tgt.zctas, FUN = function(x){ x$ZCTA5CE10 } ) )]

  rm( tgt.zctas )

  setkeyv( dt.tgt.zcta, c('tgt.id', 'ZCTA5CE10') )

  dt.tgt.zcta <- dt.zcta.pop[dt.tgt.zcta, on = 'ZCTA5CE10'][!grepl('^.+ AFB$', tgt.id)]

  return( dt.tgt.zcta )

}
