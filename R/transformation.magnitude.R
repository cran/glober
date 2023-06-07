transformation.magnitude <- function(y){
  medval = median(y)
  logmed = floor(log10(abs(medval))) ; ytrans = y
  logmax = floor(log10(abs(max(y))))
  
  if ((logmed>= 0 & logmed < 2) == F & logmed == logmax){ fact = -logmed+ 1
  }else if ((logmed>= 0 & logmed < 2) == F & logmed < logmax){ fact = -logmed 
  }else{fact = 0}
  ytrans = y * 10^fact
  
  return(list(ytrans = ytrans, fact = fact))
}

