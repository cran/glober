############################################
#### --- Combination of every knots --- ####
############################################
combinaison.knots <- function(i, knots_axis1_LIST, knots_axis2_LIST, x, y, ord, x_min, x_max){
  nb_pt <- length(y); knotSelec <- list() ; 
  nb_dim_i <- nb_pt**(1/2)
  ebic_calculated_list <- c(); knots_combination <- list() ; 
  idij = 1
  
  for (j in 1:length(knots_axis2_LIST)){
    
    knotSelec[[1]] <- knots_axis1_LIST[[i]]
    knotSelec[[2]] <- knots_axis2_LIST[[j]]
    nbKnots <- sum(lengths(knotSelec))
    ebic_calculated <- projection.lambda.2d(knotSelec, ord, x, y, x_min, x_max)
    ebic_calculated_list <- c(ebic_calculated_list, ebic_calculated) ; 
    
    knots_combination[[idij]] <- knotSelec
    
    idij <- idij + 1
  }
  
  return(list(ebic = ebic_calculated_list, knots = knots_combination))
}

