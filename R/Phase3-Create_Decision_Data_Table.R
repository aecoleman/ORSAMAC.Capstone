#' Create Linear Program Data Table
#'
#' @param db
#'
#' @return data.table
#' @export
#'
CreateLPDT <- function(db){

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
                            `width (m)`, `nearest.afb (km)`, DENSITY)],
             on = 'tgt.id'
             ][ db$tgt.mun.eff[,.(mun.id, tgt.category, `MER (m)`)],
                on = c('mun.id', 'tgt.category') ]

  decision.var[ db$tgt.mun.eff[,.(mun.id, tgt.category, `MER (m)`)],
    on = c('mun.id', 'tgt.category') ]


  decision.var[weapon.sys != 'F-15E', `nearest.afb (km)` := 0]

  decision.var[ is.na(`range (km)`), `range (km)` := 0]

  decision.var[, 'af.risk' := ifelse( weapon.sys == 'F-15E',
                                      pmax(0, (`nearest.afb (km)` - `range (km)`)/2 ),
                                      0 ) ]


  decision.var[, 'af.risk' := ifelse( mun.id == 'GBU-39',
                                      pmax(0, (`nearest.afb (km)` - `range (km)`)/8 ),
                                      af.risk) ]

  decision.var <- decision.var[!is.na(tgt.id)]

  return( decision.var )

}
