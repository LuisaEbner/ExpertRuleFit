#' @importFrom randomForest randomForest
#' @importFrom randomForest combine
#' @import inTrees

genrulesRF = function(X, y, nt,S ,L){
  N      = dim(X)[1]
  sf     = min(1, (11*sqrt(N)+1)/N)
  mn     = L*2
  ns     = S
  forest = randomForest(x = X, y=y, sampsize = sf*N, replace=F, ntree =nt, maxnodes=mn, nodesize = ns)

  treelist = RF2List(forest)
  rules    = lapply(1:L, function(d)as.character(extractRules(treeList=treelist, X=X, ntree=nt, maxdepth=d)))
  rules    = unique(unlist(rules))
  rules    = rules[take1(length(rules))]
  rulesmat = matrix(rules)
  colnames(rulesmat) = "condition"
  metric   = getRuleMetric(rulesmat,X,y)
  pruned   = pruneRule(metric, X, y, 0.025, typeDecay = 1)
  unique(pruned[,4])
}
