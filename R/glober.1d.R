glober.1d <-
  function(x, y, xpred, ord=3, parallel = FALSE, nb.Cores = 1){
    ord <-  ord-1
    x_min <-  min(c(x,xpred)) ; x_max <- max(c(x,xpred))
    
    transformation <- transformation.magnitude(y)
    y <- transformation$ytrans ; fact <- transformation$fact 
    
    
    genlasso.res <- genlasso.fun.1d (x, y, x_min, x_max, ord, parallel = parallel, nb.Cores = nb.Cores)
    
    knot.preSelected <- genlasso.res$knots ; #lambda.preSelected = genlasso.res$lambda
    knot.Selected <- checkForUniqueVector(knot.preSelected)
    
    if (parallel == T){
      out.Parallel <- mclapply(1:length(knot.Selected), function(i) projection.lambda(i, knot.Selected, ord, x, y, x_min, x_max), mc.preschedule = T, mc.cores =nb.Cores)
      ebic_calculated_list <- unlist(out.Parallel)
    }else{
      ebic_calculated_list <- c()
      for (i in 1:length(knot.Selected)){
        ebic_calculated_list <- c(ebic_calculated_list, projection.lambda(i, knot.Selected, ord, x, y, x_min, x_max))
      }
    }
    
    ########## EBIC CRITERION ################
    idx.ebic <- which.min(ebic_calculated_list)[1]
    knotSelec <- knot.Selected[[idx.ebic]]
    
    newTabX <- bsplineS(x, norder = (ord+1), breaks=c(x_min, knotSelec, x_max), returnMatrix = T)
    basis <- create.bspline.basis(rangeval=c(x_min, x_max), norder = (ord+1), breaks=c(x_min, knotSelec, x_max))
    nameSplines <- basis$names
    colnames(newTabX) <- nameSplines
    
    data_fit <- as.data.frame(cbind(y, as.matrix(newTabX)))
    colnames(data_fit) <- c('Y', colnames(newTabX))
    res.lm <- lm(Y~.- 1, data =data_fit)
    
    rss <- sum((y - res.lm$fitted.values)^2) ; tss <- sum((y - mean(y))^2)
    rsquared <- 1-(rss/tss)
    fhat <- estimation.1d(xpred, knotSelec, ord, x_min, x_max, res.lm)
    
    fhat <- fhat * 10^(-fact)
    return(list(festimated = fhat, Selected.knots = knotSelec, rss = rss, rsq = rsquared))
  }
