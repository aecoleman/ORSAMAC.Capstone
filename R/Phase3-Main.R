#' Phase 3 Main
#'
#' @return
#' @export
#'
Phase3.Main <- function(opRisk.wt, colDmg.wt, cost.wt, probDestructRequired = 0.3){

  db <- Phase3.InitializeData()

  setkeyv( db$mun.attr, c('weapon.sys', 'mun.id') )

  setkeyv( db$tgt.attr, c('tgt.id', 'tgt.category', 'tgt.type') )

  setkeyv( db$tgt.mun.feas, c('tgt.id', 'mun.id', 'feasible') )

  tgt.pop.density <- GetTargetPopDensity()

  tgt.pop.density[,tgt.id := gsub('^MRMB (.*)$', 'MRBM \\1', tgt.id)]

  tgt.pop.density[is.na(DENSITY), DENSITY := 0]

  db$tgt.attr <- db$tgt.attr[tgt.pop.density[,.(tgt.id, DENSITY) ], on = 'tgt.id' ]

  rm( tgt.pop.density )

  ## Set up decision variable data.table...
  ## written for speed rather than readability right now.

  decision.var <- CreateLPDT(db)

  ## Set Objective Function Coefficients #######################################

  # First two are zero to allow for introduction of binary variables to ensure
  # that at most one of MGM-168 and ZMGM-168B is used.

  decision.var <-
    decision.var[db$tgt.mun.eff[tgt.category == 'Personnel', .(mun.id, 'civ.MER (m)' = `MER (m)`) ],
                 on = 'mun.id' ]

  obj.fun.opRisk <-
      c( decision.var[, af.risk], 0, 0 )

  obj.fun.colDmg <-
    c( decision.var[, ifelse( tgt.type == 'area', 0, pi * `civ.MER (m)`^2 * DENSITY ) ], 0, 0 )

  obj.fun.cost <-
    c( decision.var[,`cost ($K)`], 0, 0 )

  obj.fun <-
    opRisk.wt * obj.fun.opRisk + colDmg.wt * obj.fun.colDmg + cost.wt * obj.fun.cost


## Constraints #################################################################


  # data.table to hold constraints for the linear program. The first row holds
  # the RHS of the constraint. The second row holds the direction of the
  # inequality, with -1 for <=, 0 for ==, and  1 for >=.
  constraint.var <-
    data.table(
      'variable' = c( 'RHS', 'dir', decision.var[,paste0(mun.id,':',tgt.id)], 'bin.MGM-168', 'bin.ZMGM-168B' ),
      'oneATACMS.1' = c( 1, -1, rep(0, nrow(decision.var) ), 1, 1),

      'qty.MGM-168' =
        c( 0, -1, decision.var[, as.numeric(mun.id == 'MGM-168')],
                  (-1) * db$mun.attr[mun.id == 'MGM-168', avail.qty], 0),

      'qty.ZMGM-168B' =
        c( 0, -1, decision.var[, as.numeric(mun.id == 'ZMGM-168B')],
                  0, (-1) * db$mun.attr[mun.id == 'ZMGM-168B', avail.qty] ),

      'qty.AGM-158A' =
        c( db$mun.attr[mun.id == 'AGM-158A', avail.qty], -1, decision.var[, as.numeric(mun.id == 'AGM-158A')],
                  0, 0 ),

      'qty.AGM-158B' =
        c( db$mun.attr[mun.id == 'AGM-158B', avail.qty], -1, decision.var[, as.numeric(mun.id == 'AGM-158B')],
                  0, 0 ) )

  ## All targets must be destroyed

  decision.var[tgt.type == 'point',
               est.mun.rqmt := 1 / (reliability * pKB(mer = `MER (m)`, cep = `cep (m)` ) )]

  decision.var[ grepl('^SAM ', tgt.id), est.mun.rqmt := est.mun.rqmt * 2 ]

  decision.var[tgt.type == 'area',
               `:=`('mun.wide' = `width (m)` / (sqrt(2) * `MER (m)`), 'mun.long' = `length (m)` / (sqrt(2) * `MER (m)`) ) ]

  decision.var[tgt.type == 'area',
               `:=`('est.mun.rqmt' = ceiling( probDestructRequired * pmax(1, mun.wide) * pmax(1, mun.long) / reliability) ) ]

  ## To get numbers for the estimated munitions to disable aircraft at airfields

  tgts.airfield <-
    data.table(
    foreign::read.dbf(
      system.file(
        'extData/shapefiles/pt.phase3.target/Phase_3_Airfield_Bounding_Boxes.dbf',
        package = 'ORSAMAC.Capstone'), as.is = TRUE ) )

  tgts.airfield[, `:=`('width (m)' = pmin(width, height) * 100000, 'length (m)' = pmax(width, height) * 100000 ) ]

  tgts.airfield[, 'area' := `width (m)` * `length (m)`]

  tgts.mun.airfield <-
    data.table(
      expand.grid('mun.id' = db$tgt.mun.eff[,unique(mun.id)],
                  'tgt.sid' = tgts.airfield[,unique(tgt.sid)],
                  stringsAsFactors = FALSE)
      )[tgts.airfield[,.(tgt.id, tgt.sid, `length (m)`, `width (m)`)], on = 'tgt.sid'
        ][db$mun.attr[,.(mun.id, `cep (m)`, reliability)], on = 'mun.id'
          ][db$tgt.mun.eff[tgt.category == 'Light-Skinned Vehicle', .(mun.id, `MER (m)`) ], on = 'mun.id' ]

  rm(tgts.airfield)

  tgts.mun.airfield[,`:=`('mun.wide' = `width (m)` / (sqrt(2) * `MER (m)`),
                          'mun.long' = `length (m)` / (sqrt(2) * `MER (m)`) ) ]

  tgts.mun.airfield[,`:=`('est.mun.rqmt' = ceiling( pmax(1, mun.wide) * pmax(1, mun.long) / reliability) ) ]

  tgt.airfield <- tgts.mun.airfield[,.('est.mun.rqmt' = sum(est.mun.rqmt)), by = c('tgt.id', 'mun.id') ]

  rm(tgts.mun.airfield)

  for( i in seq( nrow(tgt.airfield) ) ){

    decision.var[tgt.id == tgt.airfield[i,tgt.id] & mun.id == tgt.airfield[i,mun.id],
                   est.mun.rqmt := tgt.airfield[i,est.mun.rqmt] ]

  }

  rm(tgt.airfield)

  for( tgt in decision.var[,tgt.id] ){

    if( grepl('^SAM ', tgt ) ){

      constraint.var[, eval(paste0('ffe.',tgt)) := c(1, 1, decision.var[, (tgt.id == tgt) / est.mun.rqmt ], 0, 0 ) ]

    } else {

      constraint.var[, eval(paste0('ffe.',tgt)) := c(1, 1, decision.var[, (tgt.id == tgt) / est.mun.rqmt ], 0, 0 ) ]

    }

  }



  const.mat <- as.matrix(constraint.var[3:nrow(constraint.var), j = -1])

  const.dir <- unlist(lapply(c(constraint.var[variable == 'dir',-1]), FUN = function(x){ ifelse( x == -1, return('<='), ifelse( x == 1, return('>='), return('==') ) ) } ), use.names = FALSE )

  const.rhs <- unlist(c(constraint.var[variable == 'RHS',-1], use.names = FALSE ) )

  # lp(direction = 'min',
  #    objective.in = obj.fun,
  #    const.mat = const.mat,
  #    const.dir = const.dir,
  #    const.rhs = const.rhs,
  #    binary.vec = c(674, 675),
  #    transpose.constraints = FALSE)

  lpSolution <-
    Rglpk::Rglpk_solve_LP(obj = obj.fun,
                          mat = t(const.mat),
                          dir = const.dir,
                          rhs = const.rhs,
                          types = c( rep('C',673),'B','B'),
                          verbose = TRUE)

  decision.var[,'lp.soln' := lpSolution$solution[1:673] ]

  out <- list( 'mun.used' = decision.var[ lp.soln > 0, .(tgt.id, mun.id, lp.soln)],
               'lp' = lpSolution,
               'obj' = lpSolution$solution * obj.fun,
               'obj.risk' = sum(lpSolution$solution * obj.fun.opRisk),
               'obj.colDmg' = sum(lpSolution$solution * obj.fun.colDmg),
               'obj.cost' = sum(lpSolution$solution * obj.fun.cost) )

}
