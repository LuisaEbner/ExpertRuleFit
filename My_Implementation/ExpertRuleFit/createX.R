createX = function(X, rules, t, corelim=1){
  Xr = matrix(0, nrow=dim(X)[1], ncol=length(rules))
  for (i in 1:length(rules)){
    Xr[eval(parse(text = rules[i])),i] = 1
  }

  Nr = dim(Xr)[2]
  ind = 1:Nr
  if(dim(X)[1]<200){
  t= 0.05
  }

  sup  = apply(Xr, 2, mean)
  elim = which((sup<t)|(sup>(1-t)))

  if(length(elim)>0){
    ind = ind[-elim]
  }

  cMat      = abs(cor(Xr[,ind])) >= (corelim)
  whichKeep = which(rowSums(lower.tri(cMat) * cMat) == 0)

  ind = ind[whichKeep]
  Xr = Xr[,ind]
  rules = rules[ind]
  list(data.matrix(Xr), rules)
}
