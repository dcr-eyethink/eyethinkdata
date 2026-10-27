plot_model <- function(mod,outp="analysis",error_type="SE",posthocs=T,
                       plot_conditions=NULL,...){
  #' Plot the output of an ANOVA or mixed model
  #'
  #' Plots the means for the main effects and interactions in a model made with
  #' the afex package (`aov_ez`, `aov_car`, `aov_4` or `mixed`), and shows the
  #' post hoc contrasts. It also prints the model summary, and the estimated
  #' means and contrasts for each effect, and saves them to a text file.
  #'
  #' @details
  #' For interactions of two factors, the first factor named is shown in colour and
  #' the second along the x axis. The post hoc p-values (shown above the x axis)
  #' compare the levels of the first factor at each level of the second.
  #' The p-value of the effect from the model is shown in the title.
  #'
  #' The plots are drawn with [pirateye()], and you can pass any of its arguments
  #' to change them.
  #'
  #' @param mod A model from the afex package.
  #' @param outp Name of the folder in which to save the plots (pdf) and the results
  #'   (a text file); it is created if it does not exist. Set to `NULL` or "" to save
  #'   nothing.
  #' @param error_type The error bars: "SE" for standard errors (the default), "CL"
  #'   for confidence limits, or "none".
  #' @param posthocs Not currently used. (The post hoc results are always shown.)
  #' @param plot_conditions Which effects to plot, as a list or vector of text, eg
  #'   `c("cond1", "cond1:cond2")`. By default all the main effects and the
  #'   interaction (if there are two factors) are plotted.
  #' @param ... Further arguments passed on to [pirateye()].
  #' @return A list with the `model_summary`, and for each effect plotted, the
  #'   estimated means (`means_<effect>`) and the plot (`plot_<effect>`).
  #' @export
  #' @seealso [pirateye()]
  #' @examples
  #' \dontrun{
  #' anv <- afex::aov_ez(data = iat_data, id = "pid", dv = "rt",
  #'                     between = "firstblock", within = "congruency",
  #'                     fun_aggregate = mean)
  #' res <- plot_model(anv, outp = NULL)
  #' res$plot_congruency
  #' }

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
  nsink <- sink.number()
  on.exit(while (sink.number() > nsink) sink())   # never leave output diverted if there is an error
  if (is.character(outp) && nzchar(outp)){dir.create(file.path(outp),showWarnings = FALSE)
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

  if (is.character(outp) && nzchar(outp)){sink()}

  return(results)

}
