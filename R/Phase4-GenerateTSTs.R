#' GenerateTSTs
#'
#' @return
#' @export
#' @import sp
#' @import rgdal
#' @import acs
#' @import data.table
GenerateTSTs <- function(){

  ## Get Event Times
  eventTimes <- rexp(101, 0.5)

  eventTimes <- 2928 * eventTimes / sum(eventTimes) # number of hours in 122 days

  ## Targets to Generate #########################################################

  targetRequirements <-
    fread( system.file('extData/target_requirements.csv', package = 'ORSAMAC.Capstone' ) )

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

  attica.region.polys <- sp::spTransform(attica.polys, crsTransform)

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

  dt.zcta.pop[,DENSITY := POP5ACS16 / ( (as.numeric(ALAND10) + as.numeric(AWATER10) ) / 1000^2 ) ]

  dt.zcta.pop[ DENSITY >= 1000/2.59,
               'CLASSIFICATION' := 'Dense Urban' ]

  dt.zcta.pop[ DENSITY >= 500/2.59 & is.na(CLASSIFICATION),
               'CLASSIFICATION' := 'Urban Sprawl']

  dt.zcta.pop[ 'CLASSIFICATION' := 'Unpopulated']


}
