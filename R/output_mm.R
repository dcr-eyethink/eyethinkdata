output_mm  <- function (mm,t="",outp="analysis",cond1=NULL,cond2=NULL){
  #' Outputs result of a mixed model
  #'
  #' Turns an mixed model into a text output of means,
  #' and stats
  #'
  #' @param mm mixed model with two factors
  #' @param t title for plot and text files
  #' @param outp name of data output folder
  #' @param cond1 first condition, guessed if ommitted
  #' @param cond2 second condition, guessed if ommitted

  #' @export
  dmm <- data.frame(nice(mm))

  if(is.null(cond1)){ cond1 <- dmm$Effect[1]}
  if(is.null(cond2)){ cond2 <- dmm$Effect[2]}
  if(is.na(cond2)){ cond2 <- cond1}

  if (is.character(outp)){dir.create(file.path(outp),showWarnings = FALSE)}

  if (is.character(outp)){sink(paste0(outp,"/",t,"_",cond1,"X",cond2,"_MIXED.txt"), append=FALSE, split=TRUE)}

  print (mm)
  cat("\n\n\n")
  print(summary(mm))

  print(pairs(emmeans::emmeans(mm,specs=c(cond1,cond2))))

  if (is.character(outp)){sink()}

  return
}
