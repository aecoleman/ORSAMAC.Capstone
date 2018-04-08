#' Output Looker
#'
#' Generates output for a couple of options using successive goal optimization.
#'
#' @return list of lists
#' @export
#'
OutputLooker <- function(){

  out1 <- Phase3.Main(opRisk.wt = 1,
                      colDmg.wt = 0,
                        cost.wt = 0)

  out1.1 <- Phase3.Main(opRisk.wt = 0,
                        colDmg.wt = 1,
                          cost.wt = 0,
                     opRisk.const = out1$obj.risk)

  out1.1.1 <- Phase3.Main(opRisk.wt = 0,
                          colDmg.wt = 0,
                            cost.wt = 1,
                       opRisk.const = out1$obj.risk,
                       colDmg.const = out1.1$obj.colDmg)

  out2 <- Phase3.Main(opRisk.wt = 0,
                      colDmg.wt = 1,
                        cost.wt = 0)

  out2.1 <- Phase3.Main(opRisk.wt = 1,
                        colDmg.wt = 0,
                          cost.wt = 0,
                     colDmg.const = out2$obj.colDmg)

  out2.1.1 <- Phase3.Main(opRisk.wt = 0,
                          colDmg.wt = 0,
                            cost.wt = 1,
                       opRisk.const = out2.1$obj.risk,
                       colDmg.const = out2$obj.colDmg)

  outputAnalysis <- list( out1.1.1, out2.1.1 )

  return( outputAnalysis)

}
