######## Check for unique vector of knots ##########
checkForUniqueVector <- function(list_knots_est){
  
  new_list_of_knots_est <- list() ; len_list <- length(list_knots_est) ; result <- list()
  
  if (len_list > 1){
    for (ind_i in 1:(len_list-1)){
      result[[ind_i]] <- sapply((ind_i+1):len_list, function(jx) 
        sum(list_knots_est[[ind_i]] %in%  list_knots_est[[jx]]) == length(list_knots_est[[ind_i]]) & length(list_knots_est[[ind_i]]) == length(list_knots_est[[jx]]))
    }
    result[[len_list]] <- FALSE
    
    for (ind_i in 1:length(result)){
      if (!(TRUE %in% result[[ind_i]])) new_list_of_knots_est[[(length(new_list_of_knots_est)+1)]] <- list_knots_est[[ind_i]]
    }
  }else{new_list_of_knots_est <- list_knots_est}
  
  return(new_list_of_knots_est)
}
