#' GenerateTSTs
#'
#' @return
#' @export
#' @import sp
#' @import rgdal
#' @import acs
#' @import data.table
GenerateTSTs <- function(){

  ## Generate the Event Times

  # Generate 101 inter-event times. The rate won't matter, because we will be
  # normalizing this in a moment so that the events are squeezed into 122 days.
  # We generate 101 instead of 100 because the 101th event ends up being the end
  # of the 122 day period.
  eventTimes <- rexp(101, 0.5)

  # Here we squeeze the events into a 122 day timeframe.
  eventTimes <- 2928 * eventTimes / sum(eventTimes) # number of hours in 122 days

  # Now we replace each value with the sum of all the values before it, and
  # throw away the zero. The extra value we used earlier is now gone.
  eventTimes <- sapply(1:100, FUN = function(x){sum(eventTimes[1:x])})

  ## Targets to Generate #########################################################

  # Read requirements from a csv, discarding any rows where the required
  # quantity is zero
  targetRequirements <-
    fread(
      system.file('extData/target_requirements.csv',
                  package = 'ORSAMAC.Capstone' )
      )[quantity > 0]

  ## Create Empty Target Tables ################################################
  ##
  ## NOTE: We first create two target tables, one for each range set. Then,
  ## after selecting 100 targets for each from their respective tables, we
  ## select 20 rows that will come from the 500km table. The other 80 rows come
  ## from the 300km table. The unused rows are then discarded. This allows us to
  ## ensure that 20% of targets are in the 300km to 500km range.

  # Create data.table with one row for each target. This will hold the randomly
  # selected 300km range ZCTAs that the targets will be generated in.
  targets <- data.table(
    rbindlist(
      apply(targetRequirements,
            MARGIN = 1,
            FUN = function(x){
              data.frame( 'type' = rep(x['type'], x['quantity']),
                    'stationary' = rep(x['stationary'], x['quantity']),
                          'area' = rep(x['area'], x['quantity']),
                         'ZCTA5CE10.300km' = '',
                         'ZCTA5CE10.500km' = '',
                         'ZCTA5CE10' = '',
                              'MLRS' = '',
                          'MLRS.RANGE' = 0,
                    'F15E' = '',
                    'F15E.RANGE' = '',
                    'DENSITY.M2' = 0,
                          stringsAsFactors = FALSE) } ) ) )
#
#   # Create data.table with one row for each target. This will hold the randomly
#   # selected 300km-500km range ZCTAs that the targets will be generated in.
#   targets.500km <-
#     data.table(
#       rbindlist(
#         apply(targetRequirements,
#               MARGIN = 1,
#               FUN = function(x){
#                 data.frame( 'type' = rep(x['type'], x['quantity']),
#                       'stationary' = rep(x['stationary'], x['quantity']),
#                             'area' = rep(x['area'], x['quantity']),
#                             stringsAsFactors = FALSE) } ) ) )

  ## Read in ZCTA polygons #####################################################

  # # These polygons were made in QGIS, such that the ZCTAs are within the alotted
  # # ranges of the determined MLRS locations.
  #
  # poly.zcta.300km <- rgdal::readOGR(dsn = system.file('extData/shapefiles/ply.phase4.atticaDivisions',
  #                                               package = 'ORSAMAC.Capstone'),
  #                            layer = 'Phase_4-MLRS_300km_Range_ZCTAs',
  #                            stringsAsFactors = FALSE)
  #
  # poly.zcta.500km <- rgdal::readOGR(dsn = system.file('extData/shapefiles/ply.phase4.atticaDivisions',
  #                                              package = 'ORSAMAC.Capstone'),
  #                            layer = 'Phase_4-MLRS_500km_Range_ZCTAs',
  #                            stringsAsFactors = FALSE)
  #
  # ## Save polygon data in a data.table
  # dt.zcta.300km <- data.table( poly.zcta.300km@data )
  #
  # dt.zcta.500km <- data.table( poly.zcta.500km@data )
  #
  # dt.zcta.300km[, `:=`('MLRS' = Phase_4.ML, 'MLRS.RANGE' = Phase_4._1, 'F15E' = Airfield_H, 'F15E.RANGE' = Airfield_1, 'KM' = 300)]
  # dt.zcta.500km[, `:=`('MLRS' = HubName, 'MLRS.RANGE' = HubDist, 'F15E' = AirfieldHu, 'F15E.RANGE' = Airfield_1, 'KM' = 500)]
  #
  # dt.zcta <-
  #   rbindlist(
  #     list(
  #       dt.zcta.300km[,.(ZCTA5CE10, ALAND10, AWATER10,
  #                        POPULATION = POP, DENSITY.KM2 = Density, MLRS, MLRS.RANGE,
  #                        F15E, F15E.RANGE, KM) ],
  #       dt.zcta.500km[,.(ZCTA5CE10, ALAND10, AWATER10,
  #                        POPULATION = POP, DENSITY.KM2 = Density, MLRS, MLRS.RANGE,
  #                        F15E, F15E.RANGE, KM) ] ) )
  #
  # dt.zcta[, DENSITY.M2 := DENSITY.KM2 / (1000^2) ]
  #
  # dt.zcta <- dt.zcta[!is.na(MLRS)]

  #
  # rm(poly.zcta.300km, poly.zcta.500km)

## Read Pre-Processed Polygon Data.Table #######################################

  # MUCH faster than loading the shapefiles in each time

  dt.zcta <-
    readRDS( system.file( 'data/Phase4_ZCTA_Data.RDS',
                          package = 'ORSAMAC.Capstone' ) )

  ## Classify ZCTAs ############################################################
#
#   dt.zcta.300km[ Density >= 1000/2.59,
#                  'CLASSIFICATION' := 'Dense Urban' ]
#
#   dt.zcta.300km[ Density >= 500/2.59 & is.na(CLASSIFICATION),
#                  'CLASSIFICATION' := 'Urban Sprawl']
#
#   dt.zcta.300km[ is.na(CLASSIFICATION),
#                  'CLASSIFICATION' := 'Unpopulated']
#
#   dt.zcta.500km[ Density >= 1000/2.59,
#                  'CLASSIFICATION' := 'Dense Urban' ]
#
#   dt.zcta.500km[ Density >= 500/2.59 & is.na(CLASSIFICATION),
#                  'CLASSIFICATION' := 'Urban Sprawl']
#
#   dt.zcta.500km[ is.na(CLASSIFICATION),
#                  'CLASSIFICATION' := 'Unpopulated']

    dt.zcta[ DENSITY.KM2 >= 1000/2.59,
                   'CLASSIFICATION' := 'Dense Urban' ]

    dt.zcta[ DENSITY.KM2 >= 500/2.59 & is.na(CLASSIFICATION),
                   'CLASSIFICATION' := 'Urban Sprawl']

    dt.zcta[ is.na(CLASSIFICATION),
                   'CLASSIFICATION' := 'Unpopulated']


  ## Sample Targets from ZCTAs #################################################

  targets[, 'ZCTA5CE10.300km' := sapply(area,
                                        FUN = function(x){
                                          sample(dt.zcta[CLASSIFICATION == x & KM == 300, ZCTA5CE10],
                                                 size = 1) } ) ]

  targets[, 'ZCTA5CE10.500km' := sapply(area,
                                        FUN = function(x){
                                          sample(dt.zcta[CLASSIFICATION == x & KM == 500, ZCTA5CE10],
                                                 size = 1) } ) ]
  rand.500km <- sample(1L:100L, 20)

  rand.300km <- which(! 1L:100L %in% rand.500km )

  targets[rand.500km, ZCTA5CE10 := ZCTA5CE10.500km
          ][rand.300km, ZCTA5CE10 := ZCTA5CE10.300km]

  targets[, 'MLRS' := sapply(ZCTA5CE10,
                             FUN = function(x){
                               dt.zcta[ZCTA5CE10 == x, MLRS ][[1]] } ) ]

  targets[, 'MLRS.RANGE' := sapply(ZCTA5CE10,
                             FUN = function(x){
                               dt.zcta[ZCTA5CE10 == x, MLRS.RANGE ][[1]] } ) ]
  targets[, 'F15E' := sapply(ZCTA5CE10,
                             FUN = function(x){
                               dt.zcta[ZCTA5CE10 == x, F15E ][[1]] } ) ]
  targets[, 'F15E.RANGE' := sapply(ZCTA5CE10,
                             FUN = function(x){
                               dt.zcta[ZCTA5CE10 == x, F15E.RANGE ][[1]] } ) ]

  targets[, 'KM' := sapply(ZCTA5CE10,
                             FUN = function(x){
                               dt.zcta[ZCTA5CE10 == x, KM ][[1]] } ) ]

  targets[, 'DENSITY.M2' := sapply(ZCTA5CE10,
                           FUN = function(x){
                             dt.zcta[ZCTA5CE10 == x, DENSITY.M2 ][[1]] } ) ]

  targets[, 'TIME' := eventTimes[sample(1L:100L, replace = FALSE)] ]

  targets[type == 'Technical',
          'tgt.category' := 'Light-Skinned Vehicle']
  targets[type == 'High-Payoff Target' & stationary == FALSE,
          'tgt.category' := 'Light-Skinned Vehicle' ]



  return( targets )

}

