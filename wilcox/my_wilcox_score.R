my_wilcox_score = function (a, b)
{
  abs_diff = abs(a-b)
  sign_diff = a>b
  sign_diff[sign_diff==F]=-1
  sign_diff = sign_diff[abs_diff!=0]
  abs_diff = abs_diff[abs_diff!=0]
  rank_abs_diff = rank(abs_diff)
  W = rank_abs_diff * sign_diff
  nr = length(rank_abs_diff)
  sigma = sqrt((nr*(nr+1)*(2*nr+1))/6)
  pvalue=(sum(W)-0.5)/sigma
}