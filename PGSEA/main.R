library("GeneExpressionSignature")
library("PGSEA")

data(exampleSet)
MergingSet=RankMerging(exampleSet,"Spearman")
print(ScorePGSEA(MergingSet,250, ScoringDistance="avg"))
print(myScorePGSEA(exprs(MergingSet),250, ScoringDistance="avg"))
