###################################################################################################################
### ---              Function to build basis function for each penalization parameter lambda                --- ###
###################################################################################################################
projection.lambda.2d <- function(knotSelec, ord, x, y, x_min, x_max, d = 2){
  
  nbKnots <- length(knotSelec)
  nb_pt <- length(y)
  nb_dim_i <- sqrt(nb_pt)
  ## Matrice 
  MatEval = lapply(1:d, function(i) bsplineS(sort(unique(x[,i])), norder = (ord+1), breaks=c(x_min[i], knotSelec[[i]], x_max[i]), returnMatrix = T))
  basis <- lapply(1:d, function(i) create.bspline.basis(rangeval=c(x_min[i], x_max[i]), norder = (ord+1), breaks=c(x_min[i], knotSelec[[i]], x_max[i])))
  
  ## Nouvelle matrice X
  newTabX <- kronecker(MatEval[[1]],MatEval[[2]])
  nameSplines <- c(t(outer(basis[[1]]$names, basis[[2]]$names, paste)))
  colnames(newTabX) <- nameSplines
  
  ## Lm ##
  data_fit <- as.data.frame(cbind(y, as.matrix(newTabX)))
  colnames(data_fit) <- c('Y', colnames(newTabX))
  res <- lm(Y~.- 1, data =data_fit)
  
  ## EBIC
  ebic_calculated <- sum((y - res$fitted.values)**2) + (ncol(newTabX))*log(nb_pt) + 2*log(choose((ord + nb_dim_i + 1)**2, (ncol(newTabX)))) ;
  
  return(ebic_calculated)
}