singlecor <- function(data,x,y,bayes_factor=F){
  #' Single correlation with r, p value and optionally a Bayes factor
  #'
  #' Correlates two columns of a data frame, and returns Pearson's r with its p
  #' value, and Kendall's tau as a rank based alternative. Optionally, it adds a
  #' Bayes factor (using the BayesFactor package).
  #'
  #' @param data A data frame with the two columns to correlate.
  #' @param x Name of the first column.
  #' @param y Name of the second column.
  #' @param bayes_factor Calculate a Bayes factor? Needs the BayesFactor package.
  #' @return A list with `r`, `p` (both rounded to 3 places), `bf` (the Bayes factor
  #'   for the correlation, rounded down to a whole number, or 0 if not requested)
  #'   and `k.tau` (Kendall's tau).
  #' @export
  #' @seealso [plotscat()], [exclorr()]
  #' @examples
  #' singlecor(mtcars, "mpg", "hp")



  sigs <- cor.test(x=data[[x]],y=data[[y]])

  rankscore <- cor.test(x=data[[x]],y=data[[y]],method = "kendall")

  if (bayes_factor){
    bf <- data.frame( BayesFactor::regressionBF(data= na.omit(data.frame(x=data[[x]],y=data[[y]])),
                                                formula=x~y,progress=FALSE) )[1]
    bf <- floor(as.numeric(bf))
  }else{bf <- 0}

  return(list(r=round(sigs$estimate,3),p=round(sigs$p.value,3),bf=bf,k.tau=round(rankscore$estimate,3)))
}
