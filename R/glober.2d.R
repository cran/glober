glober.2d <-
  function(x, y, xpred, ord=3, parallel = FALSE, nb.Cores = 1){
    
    d <-2
    transformation <- transformation.magnitude(y)
    y <- transformation$ytrans ; fact = transformation$fact 
    
    ord <- ord - 1
    list_x <- list(sort(unique(x[,1])), sort(unique(x[,2]))) ; list_xpred <- list(sort(unique(xpred[,1])), sort(unique(xpred[,2]))) 
    x_min <- matrix(apply(rbind(x,xpred), 2, min), ncol = 2) ; x_max <- matrix(apply(rbind(x,xpred), 2, max), ncol = 2)
    
    genlasso.res.1 <- genlasso.fun.2d(list_x, y, x_min, x_max, 2, ord, x, parallel = parallel, nb.Cores = nb.Cores)
    genlasso.res.2 <- genlasso.fun.2d(list_x, y, x_min, x_max, 1, ord, x, parallel = parallel, nb.Cores = nb.Cores)
    
    knot.preSelected.1 <- genlasso.res.1$knots ; knot.preSelected.2 <- genlasso.res.2$knots ; #lambda.preSelected = genlasso.res$lambda
    knot.Selected.1 <- checkForUniqueVector(knot.preSelected.1) ; knot.Selected.2 <- checkForUniqueVector(knot.preSelected.2)  
    
    if (parallel == T){
      outFunParallel <- mclapply(1:length(knot.Selected.1), function(i) combinaison.knots(i, knot.Selected.1, knot.Selected.2, x, y, ord, x_min, x_max), mc.preschedule = T, mc.cores =nb.Cores)
      ebic_calculated_list <- unlist(lapply(outFunParallel, `[[`, 1))
      noeuds_double_boucle <- unlist(lapply(outFunParallel, `[[`, 2), recursive = F);
      
    }else{
      ebic_calculated_list <- c() ; noeuds_double_boucle <- list()
      for (i in 1:length(knot.Selected.1)){
        fun.combination <- combinaison.knots(i, knot.Selected.1, knot.Selected.2, x, y, ord, x_min, x_max)
        ebic_calculated_list <- c(ebic_calculated_list, fun.combination$ebic)
        noeuds_double_boucle <- c(noeuds_double_boucle, fun.combination$knots)
      }
    }
    idx.ebic <- which.min(ebic_calculated_list)[1]
    knotSelec <- noeuds_double_boucle[[idx.ebic]]
    
    MatEval <- lapply(1:d, function(i) bsplineS(list_x[[i]], norder = (ord+1), breaks=c(x_min[i], knotSelec[[i]], x_max[i]), returnMatrix = T))
    basis <- lapply(1:d, function(i) create.bspline.basis(rangeval=c(x_min[i], x_max[i]), norder = (ord+1), breaks=c(x_min[i], knotSelec[[i]], x_max[i])))
    
    newTabX <- kronecker(MatEval[[1]],MatEval[[2]])
    nameSplines <- c(t(outer(basis[[1]]$names, basis[[2]]$names, paste)))
    colnames(newTabX) <- nameSplines
    
    data_fit <- as.data.frame(cbind(y, as.matrix(newTabX)))
    colnames(data_fit) <- c('Y', colnames(newTabX))
    res.lm <- lm(Y~.- 1, data =data_fit)
    
    rss <- sum((y - res.lm$fitted.values)^2) ; tss <- sum((y - mean(y))^2)
    rsquared <- 1-(rss/tss)
    ##### --- prediction 
    MatEstX <- lapply(1:d, function(i) cbind( bsplineS(list_xpred[[i]], norder = (ord+1), breaks=c(x_min[i], knotSelec[[i]], x_max[i]), returnMatrix = T)))
    
    newTabXBig <- kronecker(MatEstX[[1]],MatEstX[[2]] )
    colnames(newTabXBig) <- nameSplines
    
    predict_y <- as.data.frame(predict(res.lm, newdata = as.data.frame(newTabXBig), interval = 'prediction'))
    fhat <- predict_y$fit
    fhat <- fhat * 10^(-fact)
    return(list(festimated = fhat, Selected.knots = knotSelec, rss = rss, rsq = rsquared))
  }
