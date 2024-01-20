mixedmodel <- function(formula,data,outp="analysis",...){
  #' Runs a mixed model and out puts results and plots
  #'
  #' Runs an mixed model and gives a text output of means,
  #' and stats
  #' @param formula same as LME
  #' @param data
  #' @param outp folder for saving. Set blank if not required
  #'
  #' @export

  #run model
  mm <- afex::mixed(formula = formula,data=data)

  # get the terms
  mterms <-  insight::find_terms(mm$full_model)
  conditions <- mterms$conditional
  dvname <- mterms$response

  title <- paste(dvname,"by",paste(conditions,collapse = "+"))

  ## output analysis to text
  if (is.character(outp)){dir.create(file.path(outp),showWarnings = FALSE)}
  if (is.character(outp)){sink(paste0(outp,"/",title,"_MIXED.txt"), append=FALSE, split=TRUE)}

  print(summary(mm))


  plotmm <-    function(mm,ct,dvname){
    r <- list()

    md <- emmeans::emmeans(mm,specs=c(ct))
    mdc <- pairs(emmeans::emmeans(mm,specs=c(ct)))

    xlabs <- data.table( data.frame(mdc),dv=dvname)

    if (length(ct)==2){
      xlabs[,c1:=tstrsplit(contrast,split =  " ")[2],by=p.value]
      xlabs[,c2:=tstrsplit(contrast,split =  " ")[5],by=p.value]
      xlabs <- xlabs[c1==c2]
      xlabs[[ct[2]]] <- xlabs$c1

      cat("\n\nInteractions of ", ct,"\n")
    }else{
      cat("\n\nMain effects of ", ct,"\n")
    }

    print(md)
    print(mdc)
    xlabs$lab <-  ifelse(xlabs$p<.001,"p<.001",paste0("p=",round(xlabs$p,3)))
    p <- do.call(mypirate,resolve.args(...,data=mm$data,dv=dvname,plot_condition=ct,bars=T,dots=F,violin=F,error_bars=F,xlabs = xlabs))
    #mypirate(mm$data,dv=dvname,colour_condition = c,bars=T,dots=F,violin=F,error_bars=F,xlabs = xlabs)

    r[[paste0("means_",paste0(ct,collapse="_"))]] <- md
    r[[paste0("plot_",paste0(ct,collapse="_"))]] <- p
    return(r)
  }

  clist <- as.list(conditions)
  if (length(conditions)==2){ clist[[3]] <- conditions }

   results <- list()
  for (ct in  clist){
    r <- plotmm(mm=mm,ct=ct,dvname=dvname)
    results <-  append(results,r )
  }

  if (is.character(outp)){sink()}

  return(results)

}
