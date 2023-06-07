#####################################################
### --- Estimation on new x -- ######################
#####################################################
estimation.1d <- function(xpred, knotSelec, ord, x_min, x_max, res.lm){
  
  ##### --- prediction 
  nameSplines = attr(terms(res.lm), "term.labels")
  newTabXBig = bsplineS(xpred, norder = (ord+1), breaks=c(x_min, knotSelec, x_max), returnMatrix = T)
  colnames(newTabXBig) <- nameSplines
  predict_f <- as.data.frame(predict(res.lm, newdata = as.data.frame(newTabXBig), interval = 'prediction'))
  fhat <- predict_f$fit
  
  return(fhat)
}