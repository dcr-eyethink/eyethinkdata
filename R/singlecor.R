singlecor <- function(data,x,y,bayes_factor=F){
  #' single correlation with r, p value and optionally a bayes factor
  #' @param data data frame with two cols to be correlated
  #' @param x name of x column
  #' @param y name of y column
  #' @param bayes_factor do you want bayes factor calculated, needs BayesFactor package

  #' @export


  sigs <- cor.test(x=data[[x]],y=data[[y]])

  rankscore <- cor.test(x=data[[x]],y=data[[y]],method = "kendall")

  if (bayes_factor){
    bf <- data.frame( BayesFactor::regressionBF(data= na.omit(data.frame(x=data[[x]],y=data[[y]])),
                                                formula=x~y,progress=FALSE) )[1]
    bf <- floor(as.numeric(bf))
  }else{bf <- 0}

  return(list(r=round(sigs$estimate,3),p=round(sigs$p.value,3),bf=bf,k.tau=round(rankscore$estimate,3)))
}
