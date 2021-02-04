#' NumFmt formats a numberic variable to a whold number, comma separated value
#'
#' @param inval is the input data value
#' @export
NumFmt <- function(inval){
  outval <- format(round(inval ,digits=0),  big.mark=",")
  return(outval)
}