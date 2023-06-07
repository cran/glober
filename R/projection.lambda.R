###################################################################################################################
### ---              Function to build basis function for each penalization parameter lambda                --- ###
###################################################################################################################
projection.lambda <- function(i, knots_axis_LIST, ord, x, y, x_min, x_max){
  
  knotSelec = knots_axis_LIST[[i]] 
  nbKnots <- length(knotSelec)
  ## Matrice 
  newTabX = bsplineS(x, norder = (ord+1), breaks=c(x_min, knotSelec, x_max), returnMatrix = T)
  basis <- create.bspline.basis(rangeval=c(x_min, x_max), norder = (ord+1), breaks=c(x_min, knotSelec, x_max))
  
  ## Nouvelle matrice X
  nameSplines <- basis$names
  colnames(newTabX) <- nameSplines
  
  ## Lm ##
  data_fit <- as.data.frame(cbind(y, as.matrix(newTabX)))
  colnames(data_fit) <- c('Y', colnames(newTabX))
  res <- lm(Y~.- 1, data =data_fit)
  
  ## nombre de points
  nb_pt <- length(x)
  
  ## EBIC
  ebic_calculated <- sum((y - res$fitted.values)**2) + (ord + nbKnots + 1)*log(nb_pt) + 2*log(choose((ord + nb_pt + 1), (ord + nbKnots + 1))) ;
  
  return(ebic_calculated)
}
