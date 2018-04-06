Fk <- function(Ph, n, entity.area, target.radius, m = NULL, fk = NULL){
  
  if( is.null(fk) ){
    
    fk <- 1 - (1 - Ph * (n * entity.area) / (pi * target.radius ^ 2) ) ^ m
    
    return( fk )
    
  } else if( is.null(m) & fk < 1 ){
    
    m <- 1
    
    while( 1 - (1 - Ph * (n * entity.area) / (pi * target.radius ^ 2) ) ^ m < fk ){
      
      m <- m + 1
      
    }
   
    return(m)
     
  }
}  



RandomShot <- function(n, cep){

  out <- rweibull(n, shape = 2, scale = cep / sqrt( log(2) ) )

}


randMunition <- function(cep, n){
  
  sigma <- cep / sqrt(2 * log(2) )
  
  out <- sqrt(-2 * sigma ^ 2 * log( runif(n) ) )
   
}
