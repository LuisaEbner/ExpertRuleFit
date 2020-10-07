#' @import gbm
#' @import inTrees


genrulesGBM = function(X, y, nt, S, L) {
  N = dim(X)[1]
  sf = min(1, (11*sqrt(N)+1)/N)
  ns = S
  dist = ifelse(is.numeric(y), "gaussian", "bernoulli")
  if (is.numeric(y)==F){
    y = as.numeric(y)-1
  }
  model1 = gbm.fit(x = X, y=y, bag.fraction = sf, n.trees =nt, interaction.depth = L
                   , shrinkage = 0.01, distribution = dist, verbose = F, n.minobsinnode = ns)

  treelist = GBM2List(model1, X)
  rules    = lapply(1:L, function(d)as.character(extractRules(treeList=treelist, X=X, ntree=nt, maxdepth=d)))
  rules    = unique(unlist(rules))
  rules    = rules[take1(length(rules))]
  rulesmat = matrix(rules)
  colnames(rulesmat) = "condition"
  metric = getRuleMetric(rulesmat,X,y)
  pruned = pruneRule(metric, X, y, 0.025, typeDecay = 1)
  unique(pruned[,4])
}
