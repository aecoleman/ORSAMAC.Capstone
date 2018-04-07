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

  out3 <- Phase3.Main(0,0,1)

  outputAnalysis <-
    data.table( 'optimized' = c('risk', 'collateral', 'cost'),
                'risk' = c(out1$obj.risk, out2$obj.risk, out3$obj.risk),
                'collateral' = c(out1$obj.colDmg, out2$obj.colDmg, out3$obj.colDmg),
                'cost' = c(out1$obj.cost, out2$obj.cost, out3$obj.cost) )

  return( outputAnalysis)

}
