OutputLooker <- function(){

  out1 <- Phase3.Main(1,0,0)
  out2 <- Phase3.Main(0,1,0)
  out3 <- Phase3.Main(0,0,1)

  outputAnalysis <-
    data.table( 'optimized' = c('risk', 'collateral', 'cost'),
                'risk' = c(out1$obj.risk, out2$obj.risk, out3$obj.risk),
                'collateral' = c(out1$obj.colDmg, out2$obj.colDmg, out3$obj.colDmg),
                'cost' = c(out1$obj.cost, out2$obj.cost, out3$obj.cost) )

  return( outputAnalysis)

}
