plot_model <- function(mod,outp="analysis",error_type="SE",posthocs=T,
                       plot_conditions=NULL,...){
  #' Plots the output of an anova or mixed model
  #'
  #' Plots the main effects and interactionsof an afex anova or mixed model
  #' And shows posthocs contrasts for the effects of condition 1 across levels of condition 2
  #' Will pass on arguments to pirateye
  #' @param mod output from an afex model
  #' @param outp folder for saving. Set blank if not required
  #' @param error_type Error bars: none, SE or CL
  #' @param posthocs do you want to show posthocs on plot?
  #' @param plot_conditions a list of main effects and interactions eg list("cond1", "cond1:cond2")
  #' @param ... plotting arguments to pass to pirateye
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
  if (is.character(outp)){dir.create(file.path(outp),showWarnings = FALSE)
  sink(paste0(outp,"/",title,"_RESULTS.txt"), append=FALSE, split=TRUE)}

  print(summary(mod))


  plotmod <-    function(mod,ct,dvname){
    r <- list()

    md <- emmeans::emmeans(mod,specs=c(ct))
    mdc <- pairs(emmeans::emmeans(mod,specs=c(ct)))


    if (length(ct)==2){

      ## INTERACTIONS

           cat("\n\nInteractions of ", ct,"\n")
      xlabs <- data.table( data.frame(mdc),dv=dvname)
      xlabs[,c1:=data.table::tstrsplit(contrast,split =  " ")[2],by=p.value]
      xlabs[,c2:=data.table::tstrsplit(contrast,split =  " ")[5],by=p.value]
      xlabs <- xlabs[c1==c2]
      xlabs[[ct[2]]] <- xlabs$c1
      xlabs$lab <-  ifelse(xlabs$p<.001,"p<.001",paste0("p=",round(xlabs$p,3)))

      ## get pvalue for title
      if (paste0(ct,collapse = ":") %in% rownames(mod$anova_table) |
          (paste0(rev(ct),collapse = ":") %in% rownames(mod$anova_table))) {

        pvalue <- na.omit(c(mod$anova_table[paste0(ct,collapse = ":"),"Pr(>F)"],
                        mod$anova_table[paste0(rev(ct),collapse = ":"),"Pr(>F)"]))
        pvalue <-  ifelse(pvalue<.001," (p<.001)",paste0(" (p=",round(pvalue,3),")"))
      }else{pvalue <- ""}

    }else{

     ## MAIN FXs
      cat("\n\nMain effects of ", ct,"\n")
      xlabs <- NULL
      if (ct %in% rownames(mod$anova_table))  {
        pvalue <- mod$anova_table[ct,"Pr(>F)"]
        pvalue <-  ifelse(pvalue<.001," (p<.001)",paste0(" (p=",round(pvalue,3),")"))
      }else{pvalue <- ""}
    }


    print(md)
    print(mdc)


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

    title = paste0(dvname," by ", paste0(ct,collapse=" and "),pvalue)

    p <- do.call(pirateye,resolve.args(...,data=mdata,dv=dvname,plot_condition=ct,bars=T,title=title,
                                       dots=F,violin=F,xlabs = xlabs,error_bar_data=emd,error_bars=error_bars))
    #pirateye(mdata,dv=dvname,colour_condition = c,bars=T,dots=F,violin=F,error_bars=F,xlabs = xlabs)

    r[[paste0("means_",paste0(ct,collapse="_"))]] <- md
    r[[paste0("plot_",paste0(ct,collapse="_"))]] <- p
    return(r)
  }


  results <- list(model_summary=summary(mod))

  # ## parse the plot_conditions into a list of items
  # ## or just read from the model
  if (!is.null(plot_conditions)){
    clist <- list()
    for (ct in plot_conditions){
      ci <- strsplit(ct,split = ":")
      clist <- append(clist,ci)
    }
  }else{
    clist <- as.list(conditions)
    if (length(conditions)==2){ clist[[3]] <- conditions }

  }


  for (ct in  clist){

    ant <- data.frame(mod$anova_table)

    r <- plotmod(mod=mod,ct=ct,dvname=dvname)
    results <-  append(results,r )
  }

  if (is.character(outp)){sink()}

  return(results)

}
