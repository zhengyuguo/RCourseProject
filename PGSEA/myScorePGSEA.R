myScorePGSEA <-
  function(MergingSet,SignatureLength,ScoringDistance=c("avg", "max")){
    ScoringDistance=match.arg(ScoringDistance,c("avg","max"))
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
      uppgscore=PGSEA(MergingSet,up,p.value=NA)
      downpgscore=PGSEA(MergingSet,down,p.value=NA)
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
    if (ScoringDistance=="avg"){
      distances=1-(pgscores+t(pgscores))/2
    }
    else{
      distances=pmin(1-pgscores,t(1-pgscores))/2
    }
    return(distances)
  }