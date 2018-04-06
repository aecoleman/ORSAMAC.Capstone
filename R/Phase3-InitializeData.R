#' Initialize Data
#'
#' @return
#' @export

Phase3.InitializeData <- function(){

  library(data.table)

  mun.attr <-
    fread( system.file( 'extData/munition_attributes.csv',
                        package = 'ORSAMAC.Capstone' ) )

  tgt.attr <-
    fread( system.file( 'extData/target_attributes.csv',
                        package = 'ORSAMAC.Capstone') )

  tgt.mun.feas <-
    fread( system.file( 'extData/target_munition_feasibility.csv',
                        package = 'ORSAMAC.Capstone') )

  tgt.mun.eff <-
    fread( system.file( 'extData/target.category-munition_effectiveness.csv',
                        package = 'ORSAMAC.Capstone') )

  return(
    list( 'mun.attr' = mun.attr,
          'tgt.attr' = tgt.attr,
          'tgt.mun.feas' = tgt.mun.feas,
          'tgt.mun.eff' = tgt.mun.eff
          )
    )

}

#' Cumulative Probability of Kill
#'
#' @param mer Mean Effective Radius
#' @param cep Circular Error Probable
#' @param tgt.n number of targets (assumed to be 1 unless otherwise specified)
#' @param tgt.r radius of target (assumed to be 0 unless otherwise specified)
#' @param tot.r total radius containing randomly distributed identical targets (assumed to be 0 unless otherwise specified)
#' @param m the number of munitions of the given type fired on the target
#'
#' @return probability of total kill
#' @export
#'
pKB <- function(mer, cep, tgt.n = 1, tgt.r = 0, tot.r = 0, m = 1){

  if( tgt.n > 1 & tgt.r > 0 & tot.r > tgt.r){

    p <- 1 - (1 - pweibull( mer, shape = 2, scale = cep / sqrt(2 * log(2) ) ) * tgt.n / tot.r^2 ) ^ m

  } else {


    p <- 1 - ( 1 - pweibull( mer, shape = 2, scale = cep / sqrt(2 * log(2) ) ) ) ^ m

    # p.k <- 1 - exp( log(0.5) * ( mer^2 / cep^2 ) )

  }

  return( p )

}

#' Quantity of Munitions to Achieve pKB
#'
#' @param p probability of total kill
#' @param mer Mean Effective Radius of munition against target
#' @param cep Circular Error Probable of munition
#' @param tgt.n number of targets
#' @param tgt.r radius of each target
#' @param tot.r radius of area containing the identical, randomly distributed targets.
#'
#' @return a number of shots that would need to be fired to achieve the desired pKB
#' @export
#'
qKB <- function(p, mer, cep, tgt.n = 1, tgt.r = 0, tot.r = 0 ){

  if( tgt.n > 1 & tgt.r > 0 & tot.r > tgt.r){

    q <- log1p(1 - p) / log1p(1 - pweibull( mer, shape = 2, scale = cep / sqrt(2 * log(2) ) ) * tgt.n / tot.r^2 )

  } else {

    q <- log1p(1 - p) /  log1p( 1 - pweibull( mer, shape = 2, scale = cep / sqrt(2 * log(2) ) ) )

    # p.k <- 1 - exp( log(0.5) * ( mer^2 / cep^2 ) )

  }

  return( q )

}
