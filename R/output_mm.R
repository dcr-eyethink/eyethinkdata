mixedmodel <- function(formula,data,outp="analysis",...){
  #' Outputs result of a mixed model
  #'
  #' Turns an mixed model into a text output of means,
  #' and stats
  #'

  #run model
  mm <- afex::mixed(formula = formula,data=data)

  # get the terms
  mterms <-  insight::find_terms(mm$full_model)
  conditions <- mterms$conditional
  groups <- mterms$random
  dvname <- mterms$response

  title <- paste(dvname,"by",paste(conditions,collapse = "+"))

  ## output analysis to text
  if (is.character(outp)){dir.create(file.path(outp),showWarnings = FALSE)}
  if (is.character(outp)){sink(paste0(outp,"/",title,"_MIXED.txt"), append=FALSE, split=TRUE)}

  print(summary(mm))


  plotmm <-    function(mm,c,dvname){
  r <- list()

  md <- emmeans::emmeans(mm,specs=c(conditions))
  mdc <- pairs(emmeans::emmeans(mm,specs=c(conditions)))

  xlabs <- data.table( data.frame(mdc),dv=dvname)

  if (length(c)==2){
    xlabs[,c1:=tstrsplit(contrast,split =  " ")[2],by=p.value]
    xlabs[,c2:=tstrsplit(contrast,split =  " ")[5],by=p.value]
    xlabs <- xlabs[c1==c2]
    xlabs[[c[2]]] <- xlabs$c1
    xlabs$lab <-  ifelse(xlabs$p<.001,"p<.001",paste0("p=",round(xlabs$p,3)))
    cat("\n\nInteractions of ", c,"\n")
  }else{
    cat("\n\nMain effects of ", c,"\n")
  }

  print(int)
  print(int_contrast)

  p <- do.call(mypirate,resolve.args(...,mm$data,dv=dvname,plot_condition=conditions,bars=T,dots=F,violin=F,error_bars=F,xlabs = xlabs))
  #mypirate(mm$data,dv=dvname,colour_condition = c,bars=T,dots=F,violin=F,error_bars=F,xlabs = xlabs)

  r[[paste0("means_",c)]] <- md
  r[[paste0("plot_",c)]] <- p
  return(r)
  }

  clist <- as.list(conditions)
  if (length(conditions)==2){ clist[[3]] <- conditions }
  results <- list()
  for (c in  clist){
   append(results, plotmm(mm=mm,c=c,dvname=dvname))
  }


#
#
#   if (length(conditions)==2){clist <- list(conditions)
#
#   for (c in  conditions){
#     cat("\n\nMain effects of ", c,"\n")
#     mfx <- emmeans::emmeans(mm,specs=c(c))
#     mfx_contrast <- pairs(emmeans::emmeans(mm,specs=c(c)))
#     print(mfx)
#     print(mfx_contrast)
#     mm$data[[c]] <- as.factor(mm$data[[c]] )
#
#     xlabs <- data.table( p=data.frame(mfx_contrast)$p.value,dv=dvname)
#
#     xlabs$lab <-  ifelse(xlabs$p<.001,"p<.001",paste0("p=",round(xlabs$p,3)))
#
#     p <- do.call(mypirate,resolve.args(...,data=mm$data,dv=dvname,colour_condition = c,bars=T,dots=F,violin=F,error_bars=F,xlabs = xlabs))
#     #mypirate(mm$data,dv=dvname,colour_condition = c,bars=T,dots=F,violin=F,error_bars=F,xlabs = xlabs)
#
#     results[[paste0("mfx_",c,"_means")]] <- mfx
#     results[[paste0("mfx_",c,"_plot")]] <- p
#   }
#
#   #interactions plot - just does 2x2 for now
# if (length(conditions)==2){
#
#     cat("\n\nInteractions for ", conditions,"\n")
#
#   }


  if (is.character(outp)){sink()}

  return(results)

  }




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
  dmm <- data.frame(afex::nice(mm))

  if(is.null(cond1)){ cond1 <- dmm$Effect[1]}
  if(is.null(cond2)){ cond2 <- dmm$Effect[2]}
  if(is.na(cond2)){ cond2 <- cond1}

  if (is.character(outp)){dir.create(file.path(outp),showWarnings = FALSE)}
  if (is.character(outp)){sink(paste0(outp,"/",t,"_",cond1,"X",cond2,"_MIXED.txt"), append=FALSE, split=TRUE)}

  print (mm)
  cat("\n\n\n")
  print(summary(mm))

    cat("\n\nMain effects of ", cond1,"\n")
  mfx1 <- emmeans::emmeans(mm,specs=c(cond1))
  mfx1_contrast <- pairs(emmeans::emmeans(mm,specs=c(cond1)))

  mypirate(data.frame(mmr$fx1),dv="emmean",colour_condition = "adr",bars=T)


  cat("\n\nMain effects of ", cond2,"\n")
  mfx2 <- emmeans::emmeans(mm,specs=c(cond2))
  mfx2_contrast <- pairs(emmeans::emmeans(mm,specs=c(cond2)))

  cat("\n\nInteractions of ",cond1, cond2,"\n")
  int <- emmeans::emmeans(mm,specs=c(cond1,cond2))
  int_contrast <- pairs(emmeans::emmeans(mm,specs=c(cond1,cond2)))

  mypirate(data.frame(mmr$int),dv="emmean",x_condition = "choice",colour_condition = "adr",bars=T)

  if (is.character(outp)){sink()}

  return(list(mfx1=mfx1,mfx2=mfx2,mfx2_contrast=mfx2_contrast,int=int,int_contrast=int_contrast))
}
