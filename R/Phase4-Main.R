## Setup #######################################################################

Phase4.Main <- function(){

  ## Initialize Variables ########################################################

  targetSets <- 30

  numReplications <- 30

  ## Define Needed Functions #####################################################

  # GenerateTargets <- function( numTargets, maxRange, sigma ){
  #
  #   theta <- runif(numTargets, min = 0, max = 2 * pi)
  #
  #   r <- rnorm( numTargets, 9 * maxRange / 32, sigma )
  #
  #   outOfBounds <- which(r < 0 | r > maxRange / 2)
  #
  #   while( length(outOfBounds) > 0 ){
  #
  #     r[outOfBounds] <- rnorm( length(outOfBounds), maxRange / 4, sigma )
  #
  #     outOfBounds <- which(r < 0 | r > maxRange)
  #
  #   }
  #
  #   targetLocations <-
  #     data.frame( 'target' = 0:numTargets,
  #                 'x.rel' = c(0, r * cos( theta )),
  #                 'y.rel' = c(0, r * sin( theta )) )
  #
  #   return( targetLocations )
  #
  # }




  PrbHit <- function(cep.r, target.r){

    return( 1 - exp( - 0.693147 * target.r^2 / cep.r^2 ) )

  }

  ## F-15E Attributes ############################################################

  attr.F15 <-
    data.table( 'reliability.takeoff' = 0.98,
                'reliability.flight' = 0.95,
                'range.km' = 3840,
                'speed.kmph' = 3017.52,
                'speed.alt' = 944.52,
                'ordnance' = 8)

  attr.MLRS <-
    data.table( 'speed.kmph' = 3704 )

  prb.glimpse <- 0.9784

  prb.hit <- PrbHit(cep.r = 7, target.r = 10)


  ## Target Selection Function ###################################################

  SelectTargets <- function( remaining.tgts ){

    tgtSelection <- vector( mode = 'integer', length = 10 )

    i <- 1L

    tgtSelection[i] <- 0L

    rangeRemaining <- attr.F15$range.km

    tgtOptions <- targetRanges[depart == tgtSelection[i] & arrive %in% remaining.tgts & `distance (km)` + `egress (km)` <= rangeRemaining,
                               .(depart, arrive, `distance (km)`, `egress (km)`)
                               ][which.max(`distance (km)`), .(arrive, `distance (km)`)]

    i <- i + 1L

    tgtSelection[i] <- tgtOptions$arrive[1]

    rangeRemaining <- rangeRemaining - tgtOptions$`distance (km)`[1]

    while( tgtSelection[i] != 0L & i <= 9L ){

      remaining.tgts <- remaining.tgts[which(!remaining.tgts %in% tgtSelection[2:i])]

      tgtOptions <- targetRanges[depart == tgtSelection[i] & arrive %in% remaining.tgts & `distance (km)` + `egress (km)` <= rangeRemaining,
                                 .(depart, arrive, `distance (km)`, `egress (km)`)
                                 ][which.min(`distance (km)`), .(arrive, `distance (km)`)]

      if( dim(tgtOptions)[1] == 0 ){

        tgtOptions <- targetRanges[depart == tgtSelection[i] & arrive == 0 & `distance (km)` + `egress (km)` <= rangeRemaining,
                                   .(depart, arrive, `distance (km)`, `egress (km)`)
                                   ][which.min(`distance (km)`), .(arrive, `distance (km)`)]

      }

      i <- i + 1L

      tgtSelection[i] <- tgtOptions$arrive[1]

      rangeRemaining <- rangeRemaining - tgtOptions$`distance (km)`[1]

    }

    return( tgtSelection )

  }

  ## Fly Sortie ##################################################################

  output <- data.frame()

  tpb <- txtProgressBar(min = 0, max = targetSets * numReplications, style = 3 )

  for( j in 1:targetSets){

    generatedTargets <-
      data.table( GenerateTargets( numTargets = totalTargets,
                                   maxRange = attr.F15$range.km,
                                   sigma = attr.F15$range.km / 11 ) )

    targetRanges <-
      data.table(
        expand.grid('depart' = 0:totalTargets,
                    'arrive' = 0:totalTargets),
        key = c('depart', 'arrive') )

    targetRanges <- targetRanges[depart != arrive]

    targetRanges <- merge(targetRanges, generatedTargets, by.x = 'arrive', by.y = 'target')

    targetRanges <- merge(targetRanges, generatedTargets, by.x = 'depart', by.y = 'target', suffixes = c('','.dep') )

    targetRanges[, `:=`('ingress (km)' = sqrt(x.rel.dep^2 + y.rel.dep^2),
                        'egress (km)' = sqrt(x.rel^2 + y.rel^2) ) ]

    targetRanges[, `:=`(x.rel = x.rel - x.rel.dep, y.rel = y.rel - y.rel.dep)]

    targetRanges[, `:=`(x.rel.dep = NULL, y.rel.dep = NULL) ]

    targetRanges[, 'distance (km)' := sqrt(x.rel^2 + y.rel^2) ]

    targetRanges[, 'Operable' := TRUE ]

    for( i in 1:numReplications ){

      planesInMaintenance <- 0L

      sortiesFlown <- 0L

      aircraftLost <- 0L

      targetRanges[, Operable := TRUE]

      totalDistFlown <- 0

      sensorFailures <- 0L

      noDetect <- 0L

      miss <- 0L

      while( length(targetRanges[arrive != 0 & Operable, unique(arrive)]) != 0 ){

        sortiesFlown <- sortiesFlown + 1L

        selectedTargets <- SelectTargets( targetRanges[arrive != 0 & Operable, unique(arrive)] )

        if( runif(1) <= attr.F15$reliability.takeoff){

          i <- 2L

          detectFailures <- 0L

          shotDown <- FALSE


          while( i <= 9 & detectFailures <= 3 & shotDown == FALSE & selectedTargets[i] != 0 ){



            if( rgeom(1,0.005) > targetRanges[arrive == selectedTargets[i] & depart == selectedTargets[i-1L], c(`distance (km)`) ] / 100){

              totalDistFlown <- totalDistFlown + targetRanges[arrive == selectedTargets[i] & depart == selectedTargets[i-1L], c(`distance (km)`) ]

              if( runif(1) <= prb.glimpse){

                if( runif(1) <= prb.hit ){

                  targetRanges[arrive == selectedTargets[i], Operable := FALSE]

                } else {

                  miss <- miss + 1L

                }

              } else {

                detectFailures <- detectFailures + 1L

                noDetect <- noDetect + 1L

                if( detectFailures >= 3 ){

                  sensorFailures <- sensorFailures + 1L



                }

              }

              i <- i + 1L

            } else {

              shotDown <- TRUE

              aircraftLost <- aircraftLost + 1L

              totalDistFlown <- totalDistFlown + targetRanges[arrive == selectedTargets[i] & depart == selectedTargets[i-1L], c(`distance (km)`) ]/2

            }

          }

          if( !shotDown ){



            if(rgeom(1,0.005) < targetRanges[arrive == selectedTargets[i] & depart == selectedTargets[i-1L], c(`distance (km)`) ] / 100 ){

              aircraftLost <- aircraftLost + 1L

              totalDistFlown <- totalDistFlown + targetRanges[arrive == selectedTargets[i] & depart == selectedTargets[i-1L], c(`distance (km)`) ] / 2

            } else {

              totalDistFlown <- totalDistFlown + targetRanges[arrive == selectedTargets[i] & depart == selectedTargets[i-1L], c(`distance (km)`) ]

            }

          }

        } else {

          planesInMaintenance <- planesInMaintenance + 1

        }

      }

      output <- rbindlist( list( output, data.frame( 'targetSet' = j,
                                                     'lost' = aircraftLost,
                                                     'maint' = planesInMaintenance,
                                                     'noDetects' = noDetect,
                                                     'sensorFailures' = sensorFailures,
                                                     'flown' = sortiesFlown,
                                                     'missedTarget' = miss,
                                                     'distFlown' = totalDistFlown) ) )

      setTxtProgressBar(tpb, getTxtProgressBar(tpb) + 1 )

    }

  }

  close( tpb )

  # fwrite(output, file = file.path(dirPath, paste0('model-',totalTargets,'-',targetSets,'-',numReplications,'.csv') ) )

  outputSummary <- output[,.(avg.sorties = mean(flown), avg.lost = mean(lost), avg.maint = mean(maint))]

  outputSummary.byTgtSet <- output[,.(avg.sorties = mean(flown), avg.lost = mean(lost), avg.maint = mean(maint)), by = 'targetSet']


}
