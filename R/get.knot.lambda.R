###### Get selected knots for one lambda #######
get.knot.lambda <- function(lambda_i, out_trend, ord, D, x_min,x_max, xselec){
  coeffGen <- coef(out_trend,lambda=lambda_i)$beta
  b <- round(as.matrix(D)%*%coeffGen, digits = 6)
  idx <- which(b != 0) + 1   
  
  knots_i <- unique(xselec[idx])
  knotSelecToReturn<- knots_i[!knots_i %in% c(x_min,x_max)]
  
  return(knotSelecToReturn)
}
