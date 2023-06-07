###############################################################################################################################
########## --- GENLASSO 1D --- #######################
###############################################################################################################################
genlasso.fun.1d <- function(x, y, x_min, x_max, ord, parallel = FALSE, nb.Cores = 1){
  ### matrix D ##
  D <- D.weight(x, ord = (ord+1))
  
  ### Genlasso ###
  out_trend <- genlasso(as.matrix(y), D = as.matrix(D))
  sum_out <- summary(out_trend)
  lambda <- out_trend$lambda
  
  knot.List <- list() ;
  
  if (parallel == T) {knot.List <- mclapply(lambda, function(lambda_i) get.knot.lambda(lambda_i, out_trend, ord, D, x_min, x_max, x),  mc.preschedule = T, mc.cores = nb.Cores)
  }else{
    knot.List <- list() ;
    idx_i = 1
    for (lambda_i in lambda){
      knot.List[[idx_i]] <- get.knot.lambda(lambda_i, out_trend, ord, D, x_min, x_max, x)
      idx_i =  idx_i + 1 
    }
  }
  
  return(list(knots = knot.List, lambda = lambda))
}
