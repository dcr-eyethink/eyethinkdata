plot_model <- function(mod,outp="analysis",error_type="SE",posthocs=T,...){
  #' Plots the output of an anova or mixed model
  #'
  #' Plots the main effects and interactionsof an afex anova or mixed model
  #' And shows posthocs contrasts for the effects of condition 1 across levels of condition 2
  #' Will pass on arguments to mypirate
  #' @param mod output from an afex model
  #' @param outp folder for saving. Set blank if not required
  #' @param error_type Error bars: none, SE or CL
  #' @param posthocs do you want to show posthocs on plot?
  #' @export

  #run model
  #mod <- afex::mixed(formula = formula,data=data)

  # get the terms
   if ("aov" %in% names(mod)){
     mtype="aov"
     mterms <-  insight::find_terms(mod)
     mdata <- mod$data$long
   }else{
     mtype="mix"
     mterms <-  insight::find_terms(mod$full_model)
     mdata <- mod$data
   }


  conditions <- mterms$conditional
  dvname <- mterms$response

  title <- paste(dvname,"by",paste(conditions,collapse = "+"))

  ## output analysis to text
  if (is.character(outp)){dir.create(file.path(outp),showWarnings = FALSE)}
  if (is.character(outp)){sink(paste0(outp,"/",title,"_MIXED.txt"), append=FALSE, split=TRUE)}

  print(summary(mod))


  plotmod <-    function(mod,ct,dvname){
    r <- list()

    md <- emmeans::emmeans(mod,specs=c(ct))
    mdc <- pairs(emmeans::emmeans(mod,specs=c(ct)))

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

    if (error_type=="none"){
      error_bars=F
      emd <- data.table()
    }else{
      emd <- data.table(data.frame(md))
      error_bars=T
 if (error_type=="CL")  {
    emd$ymin <- emd$lower.CL
    emd$ymax <- emd$upper.CL
    }else{
      emd$ymin <- emd$emmean-emd$SE
      emd$ymax <- emd$emmean+emd$SE
    }
      }

    if (paste0(ct,collapse = ":") %in% rownames(mod$anova_table)){
    pvalue <- mod$anova_table[paste0(ct,collapse = ":"),"Pr(>F)"]
    pvalue <-  ifelse(pvalue<.001," (p<.001)",paste0(" (p=",round(pvalue,3),")"))
}else{pvalue <- ""}
    title = paste0(dvname," by ", paste0(ct,collapse=" and "),pvalue)

    p <- do.call(mypirate,resolve.args(...,data=mdata,dv=dvname,plot_condition=ct,bars=T,title=title,
                                       dots=F,violin=F,xlabs = xlabs,error_bar_data=emd,error_bars=error_bars))
    #mypirate(mdata,dv=dvname,colour_condition = c,bars=T,dots=F,violin=F,error_bars=F,xlabs = xlabs)

    r[[paste0("means_",paste0(ct,collapse="_"))]] <- md
    r[[paste0("plot_",paste0(ct,collapse="_"))]] <- p
    return(r)
  }

  clist <- as.list(conditions)
  if (length(conditions)==2){ clist[[3]] <- conditions }

   results <- list()
  for (ct in  clist){

    ant <- data.frame(mod$anova_table)

       r <- plotmod(mod=mod,ct=ct,dvname=dvname)
    results <-  append(results,r )
  }

  if (is.character(outp)){sink()}

  return(results)

}
