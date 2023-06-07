###############################################################################################################################
########## --- GENLASSO 2D --- #######################
###############################################################################################################################
genlasso.fun.2d <- function(list_x, yfun, x_min, x_max, dim_i, ord, x, parallel = F, nb.Cores = 1){
  
  xfixed <- list_x[[dim_i]]
  lambdaList = list() ; indices_not_null = c() ; 
  bigListeOfKnots = lapply(1:length(xfixed), function(i) NULL) ; knotSelec <- list() ;
  idi = 1
  nbtemp = length(xfixed)
  
  for (x_i in xfixed){
    
    y_i <- yfun[x[,dim_i] == x_i,]
    xchanging <- sort(unique(x[x[,dim_i] == x_i, -dim_i]))
    
    OutFun <- genlasso.fun.1d(xchanging, y_i, x_min[dim_i], x_max[dim_i], ord, parallel = parallel, nb.Cores = nb.Cores)
    bigListeOfKnots[[idi]] <- OutFun[[1]] ; lambda <- OutFun[[2]]
    lambdaList[[idi]] <- lambda
    
    if (length(lambda) > 1) indices_not_null <- c(indices_not_null, idi)
    idi = idi + 1
  }
  
  end_iteration <- min(sapply(indices_not_null, function(id) 
    length(lambdaList[[id]])))
  knots_axis_LIST <- lapply(1:end_iteration, function(i) NULL) ; lambda_axis_LIST <- list() ;
  
  for (idj in 1:end_iteration){
    for (idi in indices_not_null){
      knots_axis_LIST[[idj]] <- sort(unique(c(knots_axis_LIST[[idj]], bigListeOfKnots[[idi]][[idj]])))
    }
  }
  return(list(knots = knots_axis_LIST, lambda = lambda))
}

