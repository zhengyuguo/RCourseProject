myPGSEA <- function (exprs, cl, range=c(25,500), ref=NULL,center=TRUE, p.value=0.005, weighted=TRUE, enforceRange=TRUE,...) {
    
  exprs <- scale(exprs,scale=FALSE)
  results <- matrix(NA,length(cl),ncol(exprs))
  rownames(results) <- names(cl)
  colnames(results) <- colnames(exprs)
  mode(results) <- "numeric"
  if(is.logical(p.value))
    p.results <- results
  
  for (i in 1:length(cl)) {
    clids <- cl[[i]]
    ix <- match(clids,rownames(exprs))
    ix <- unique(ix[!is.na(ix)])
    present <- sum(!is.na(ix))
    if(present < range[1]) {
      next
    }
    if(present > range[2]) {
      next
    }
    
    texprs <- exprs[ix,]
    if(any(is.na(texprs))) cat("Warning - 'NA' values within expression data, enrichment scores are estimates only.\n")
    if(!is.matrix(texprs)) texprs <-as.matrix(texprs)
    
    if(!weighted) stat <- try(apply(texprs, 2, t.test,...))
    else {
      try(mod <- (length(ix) ^ (1/2)) / apply(exprs, 2, sd, na.rm=TRUE))
      try(m <- apply(texprs, 2, mean,na.rm=TRUE) - apply(exprs,2,mean,na.rm=TRUE))
      stat2 <- m * mod
      p.val <- 2*pnorm(-abs(stat2))
      stat <- list()
      for(q in 1:length(stat2)){
        stat[[q]] <- list(statistic=stat2[q],p.value=p.val[q])
      }
      names(stat) <- names(stat2)
      
    }
    if (is.list(stat)) {
      ps <- unlist(lapply(stat, function(x) x$p.value))
      stat <- unlist(lapply(stat, function(x) x$statistic))
      if (!is.na(p.value)) {
        if(is.numeric(p.value)) {
          stat[ps > p.value] <- NA
        } else {
          p.results[i,] <- ps
        }
      }
    }
    results[i,] <- as.numeric(stat)
    if(enforceRange) {
      for(w in 1:ncol(texprs)) {
        if(sum(!is.na(texprs[,w])) < range[1] | sum(!is.na(texprs[,w])) > range[2] ) 
          results[i,w] <- NA
      }
    }
  }
  if(is.logical(p.value) & !is.na(p.value)) return(list(results=results,p.results=p.results))
  return(results)
}


