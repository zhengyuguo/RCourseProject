myScorePGSEA2 <-
  function(MergingSet,SignatureLength){
    #ScoringDistance=match.arg(ScoringDistance,c("avg","max"))
    #PRLs=exprs(MergingSet)
    PRLs=MergingSet
    n=ncol(PRLs)
    m=nrow(PRLs)
    pgscores=matrix(0,1,n)
    for (i in 1:n){
      prls=as.matrix(PRLs[,i])
      up=which(prls<=SignatureLength)
      down=which(prls>=m-SignatureLength+1)
      #upsmc=new("smc",ids=up)
      #downsmc=new("smc",ids=down)
      uppgscore=myPGSEA(MergingSet,up,p.value=NA)
      downpgscore=myPGSEA(MergingSet,down,p.value=NA)
      pgscore=(uppgscore-downpgscore)/2
      pgscores=rbind(pgscores,pgscore)
      
    }
    pgscores=as.matrix(pgscores)
    pgscores=pgscores[-1,]
    Mvalue=max(abs(pgscores))
    pgscores=pgscores/max(abs(pgscores))  
    if (pgscores[1,1]>0){
      pgscores=pgscores
    }
    else{
      pgscores=-pgscores
    } 
    rownames(pgscores)=colnames(pgscores)
    distances=1-(pgscores+t(pgscores))/2
    return(distances)
  }