.dotsHierarchy <- function(dots, dotsnames, validArgs){
  els <- validArgs
  
  if(any(grepl(x=dotsnames, pattern="date"))) return(dots[grepl(x=dotsnames, pattern="date")])
  
  return(dots[els[els %in% dotsnames]])
}