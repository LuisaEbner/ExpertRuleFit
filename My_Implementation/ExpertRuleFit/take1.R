take1 = function(len) {
  draw = c()
  for(i in 1:(len/2)){
    draw[[i]] = sample(1:2)
  }

  keep  = which(unlist(draw) == 1)
  keep
}
