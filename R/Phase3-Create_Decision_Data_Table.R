#' Create Linear Program Data Table
#'
#' @param db
#'
#' @return data.table
#' @export
#'
CreateLPDT <- function(db){


  # Get all munition/target combinations, then remove infeasible combinations
  # and merge with the various attributes.
  decision.var <-
    merge(
      data.table(
        expand.grid('mun.id' = db$mun.attr[, mun.id],
                    'tgt.id' = db$tgt.attr[, tgt.id],
                    'alloc.qty' = 0,
                    stringsAsFactors = FALSE) ),
      db$tgt.mun.feas,
      by = c('mun.id', 'tgt.id'),
      all = TRUE
    )[ is.na(feasible) | feasible == TRUE, .(mun.id, tgt.id, alloc.qty)
       ][ db$mun.attr[,.(mun.id, weapon.sys, `cep (m)`, `range (km)`,
                         `cost ($K)`, `reliability`, `avail.qty`) ],
          on = 'mun.id'
          ][ db$tgt.attr[,.(tgt.id, tgt.category, tgt.type, `radius (m)`, `length (m)`,
                            `width (m)`, `nearest.afb (km)`, DENSITY, Latitude, Longitude)],
             on = 'tgt.id'
             ][ db$tgt.mun.eff[,.(mun.id, tgt.category, `MER (m)`)],
                on = c('mun.id', 'tgt.category') ]

  # If the weapon system isn't F-15E, we don't need to use the distance to the
  # nearest AFB as a proxy for AF risk when that munition is used.
  decision.var[weapon.sys != 'F-15E', `nearest.afb (km)` := 0]

  # Two of the bombs had NA for their range, these are changed to zero to
  # prevent errors arrising from arithmetic.
  decision.var[ is.na(`range (km)`), `range (km)` := 0]

  # The af.risk for F-15E will be the distance to the nearest AFB, minus the
  # range to the target, divided by the number of total munitions that can be
  # taken. If time allows, we may implement an integer variable to count the
  # number of sorties dispatched instead.
  decision.var[, 'af.risk' := ifelse( weapon.sys == 'F-15E',
                                      pmax(0, (`nearest.afb (km)` - `range (km)`)/2 ),
                                      0 ) ]

  # For the GBU-39 SDB, the number of munitions per fighter is 8 instead of 2,
  # so this corrects that.
  decision.var[, 'af.risk' := ifelse( mun.id == 'GBU-39',
                                      pmax(0, (`nearest.afb (km)` - `range (km)`)/8 ),
                                      af.risk) ]

  # All of the joins may have created some rows that don't correspond to
  # targets, we remove those here.
  decision.var <- decision.var[!is.na(tgt.id)]

  return( decision.var )

}
