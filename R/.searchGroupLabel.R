# ancillary funs - find pattern in groups list labels, returns a matrix with link of group
.searchGroupLabel <- function(groups,pattern,...){
  
  indices <- which(sapply(groups[["groups"]], function(x) grepl(x=x[["label"]], pattern=pattern, ...)), arr.ind = T)
  
  return(groups[["groups"]][indices])
}
