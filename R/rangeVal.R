# rangeVal extracts the minimum and maximum from a range of input values

rangeVal <- function(x) {


  firstval <- unlist(x[1])
  minValue <- min(firstval)
  maxValue <- max(firstval)
  for(i in 2:length(x)){
    checkval <- unlist(x[i])
    if(min(checkval) < minValue) {
       minValue <- min(checkval)
    }
    if(max(checkval) > maxValue) {
      maxValue <- max(checkval)
    }
  }
  outList <- list("min" =  minValue, "max" = maxValue)
  return(outList)
}