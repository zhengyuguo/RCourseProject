extremecosine = function( disease, drug) {
  xset = intersect( rownames(drug), rownames(disease))
  xsetdrug = subset( drug, subset = rownames(drug) %in% xset )
  xsetdis = subset( disease, subset = rownames(disease) %in% xset )
  sapply(xsetdrug,function(x){
    y = as.vector(unlist(xsetdis))
    s = max(c(x,y))
    x = x / s
    y = y / s
    sum(x*y)/sqrt(sum(x^2) * sum(y^2))
})

}
