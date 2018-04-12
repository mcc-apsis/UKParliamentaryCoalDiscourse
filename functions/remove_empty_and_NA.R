remove_empty_and_NA <- function(x) {
  
  out <- x[which(x != "" & !is.na(x))]
  
  return(out)
}