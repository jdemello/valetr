#' @export
.searchGroupLabel <- function(groups,pattern,dots){

  indices <- which(sapply(groups[["groups"]], function(x) do.call("grepl", append(list(pattern=pattern, x=x[["label"]]), dots))),
                   arr.ind = T)

  return(groups[["groups"]][indices])
}

#' @export
.dotsHierarchy <- function(dots, dotsnames, validArgs){
  els <- validArgs

  if(any(grepl(x=dotsnames, pattern="date"))) return(dots[grepl(x=dotsnames, pattern="date")])

  return(dots[els[els %in% dotsnames]])
}

#' @export
.drillDown <- function(x, classObj){
  if (!inherits(x, classObj)) lapply(x, .drillDown, classObj) else TRUE}
