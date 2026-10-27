plotscat <- function(data,x,y,z=NULL,cl=2,condcols=NULL,cleand=FALSE,
                     results_title=TRUE,title_text=NULL,
                     outp="analysis",w=6,h=6,label=NULL,bayes_factor=F){
  #' Plot a scatterplot of two or three variables, optionally split by group
  #'
  #' Plots `x` against `y`, with a regression line, and puts the correlation
  #' statistics in the plot title. What happens depends on `z`:
  #'
  #' * no `z`: just `y` against `x`.
  #' * `z` is a number: `y` and `z` are both plotted against `x`, in different
  #'   colours, and the two correlations are compared, using cocor (Hittner et al.,
  #'   2003).
  #' * `z` is a factor or text: `y` against `x` is plotted separately for each group
  #'   in `z` (only the first two groups are used), and the two correlations are
  #'   compared (see `plotscat_split`).
  #'
  #' @param data A data frame with the columns to be plotted, one row per
  #'   participant.
  #' @param x Name of the column for the x axis.
  #' @param y Name of the column for the y axis.
  #' @param z Optional name of a third column, either numeric or a grouping factor.
  #' @param cl For a grouping factor: 2 draws a regression line for each group, 1
  #'   draws one line for everyone.
  #' @param condcols Colours for the groups when `z` is a grouping factor. By default
  #'   they are chosen by [replacecondcols()].
  #' @param cleand If `TRUE`, remove rows with missing values first.
  #' @param results_title Show the statistics in the plot title?
  #' @param title_text Optional extra text for the top of the title.
  #' @param outp Name of the folder in which to save the plot as a pdf (it is
  #'   created if needed). Set to `NULL` or "" to not save.
  #' @param w Width of the saved plot, in inches.
  #' @param h Height of the saved plot, in inches.
  #' @param label Optional name of a column to use to label each point.
  #' @param bayes_factor Calculate Bayes factors? This needs the BayesFactor
  #'   package. The Bayes factors are shown in the plot title.
  #' @return A list containing the plot (`p`) and the statistics: r, p value, Bayes
  #'   factor and Kendall's tau for each correlation, the p value for the difference
  #'   between the correlations (`cc`), and the interaction Bayes factor (`intbf`),
  #'   when relevant.
  #' @export
  #' @seealso [singlecor()], [exclorr()]
  #' @examples
  #' \dontrun{
  #' plotscat(mtcars, x = "mpg", y = "hp", outp = NULL)
  #' plotscat(mtcars, x = "mpg", y = "hp", z = "am", outp = NULL)
  #' }

  data <- data.table::data.table(data)

  if(!is.null(title_text)){
    t <- paste(title_text,"
")
  }else{t <- ""}

  ## assuming data is wide, one row per subject


  if (is.null(z)) {
    ## we are just plotting two variables

    if (cleand==TRUE){
      data <- na.omit(data[,.(get(x),get(y))])
      colnames(data) <- c(x,y)
      }

    sigy <- singlecor(data,x,y,bayes_factor=bayes_factor)




    p <- ggplot(data=data,aes_string(x=x,y=y))+
      geom_point(shape=19, color="red",position = "jitter") +
      geom_smooth(method=lm)  +
      theme_bw()

    if (!is.null(label)){
      p <- p+ geom_label(aes_string(label=label))
    }


    if (results_title){
      bftext <- if (bayes_factor){paste0("  BF=",signif(sigy$bf,digits=3))}else{""}
      t <- paste0(t,x," and ",y,"
r=",sigy$r,"  p=",sigy$p,bftext, "  k.tau=",sigy$k.tau)}

    p <- p + ggtitle(t)

    results <- list(p=p, sigy.r=sigy$r,sigy.p=sigy$p,sigy.bf=sigy$bf,k.tau=sigy$k.tau)

  } else if ( is.factor(data[[z]]) | is.character(data[[z]])) {
    results=plotscat_split(data=data,x=x,y=y,z=z,cl=cl,
                           condcols=condcols,results_title=results_title,title_text=t,
                           bayes_factor=bayes_factor)
    } else{
    ## we have a third variable, so will plot x-y and x-z

    if (cleand==TRUE){
      data <- na.omit(data[,.(get(x),get(y),get(z))])
      colnames(data) <- c(x,y,z)}

    sigy <- singlecor(data,x,y,bayes_factor=bayes_factor)
    cc <- cocor::cocor(as.formula(paste("~",x,"+",y,"|",x,"+",z)), data = data)
    sigz  <- singlecor(data,x,z,bayes_factor=bayes_factor)


    data <-  melt.data.table(data=data, measure.vars = c(y,z))
    data[levels(data$variable)[1]==data$variable, g:= 0]
    data[levels(data$variable)[2]==data$variable, g:= 1]

    if (bayes_factor){
    ## Bayes for the full model against one with interaction omitted
    full <- BayesFactor::lmBF(data= na.omit(data.frame(x=data[[x]],y=data$value,g=data$g)),
                              formula=x~y + g + y:g, progress=FALSE)
    nointer <- BayesFactor::lmBF(data= na.omit(data.frame(x=data[[x]],y=data$value,g=data$g)),
                                 formula=x~y + g , progress=FALSE)
    bf <- data.frame( full/nointer )[1]
    bf <- floor(as.numeric(bf))
    }else{bf <- 0}

    p <- ggplot(data=data,aes_string(x=x,y="value"))+
      geom_point(shape=19, position = "jitter", alpha =0.8, aes(color=variable) ) +
      geom_smooth(method=lm, aes(color=variable))  +
      theme_bw()

    if (!is.null(label)){
      p <- p+ ggrepel::geom_label_repel(aes_string(label=label))
    }


    if (results_title) {

    if (bayes_factor){
    t <- paste(t,x,"
",y," r=",sigy$r,"  p=",sigy$p,"  BF=",sigy$bf, "  k.tau=",sigy$k.tau,"
",z," r=",sigz$r,"  p=",sigz$p,"  BF=",sigz$bf, "  k.tau=",sigz$k.tau,"
reject equal r values p=",round(cc@hittner2003$p.value,3),"  BF inter=",bf )

    }else{
      t <- paste(t,x,"
",y," r=",sigy$r,"  p=",sigy$p,"  k.tau=",sigy$k.tau,"
",z," r=",sigz$r,"  p=",sigz$p,"  k.tau=",sigz$k.tau,"
reject equal r values p=",round(cc@hittner2003$p.value,3))
      }

      p <- p + ggtitle(t)+theme(plot.title = element_text(size = 10))

    results <- list(p=p, sigy.r=sigy$r,sigy.p=sigy$p,sigy.bf=sigy$bf,
                    sigz.r=sigz$r,sigz.p=sigz$p,sigz.bf=sigz$bf,
                    cc=round(cc@hittner2003$p.value,3), intbf=bf)

  }

    }


  if (is.character(outp) && nzchar(outp)){
    dir.create(file.path(outp),showWarnings = FALSE)
    ggsave(paste0(outp,"/ScatPlot ",x,y,z,".pdf"), width = w, height = h)
  }


  return(results)
}



plotscat_split <- function(data,x,y,z,cl=2,condcols=NULL,results_title=TRUE,title_text=NULL,outp="",bayes_factor=F){
    #' Plot a scatterplot of two variables, split by a group
    #'
    #' This is called by [plotscat()] when `z` is a grouping variable. It plots `x`
    #' against `y` for the first two levels of `z`, and tests whether the
    #' correlations differ (cocor, Fisher 1925).
    #'
    #' @param data A data frame, one row per participant.
    #' @param x Name of the x column.
    #' @param y Name of the y column.
    #' @param z Name of the column splitting the groups.
    #' @param cl 2 plots a line for each sub group, 1 plots just the main line.
    #' @param condcols Colours for the groups. By default they are chosen by
    #'   [replacecondcols()].
    #' @param results_title Show the statistics in the plot title?
    #' @param title_text Optional text for the title.
    #' @param outp Not used.
    #' @param bayes_factor Calculate the Bayes factor for the difference between
    #'   groups?
    #' @return A list with the plot (`p`) and the statistics. The results for the
    #'   first and second group are `sigyz1.*` and `sigyz2.*`.
    #' @keywords internal
    ###### BayesFactor regressionBF


    data <- setDT(data)
data[[z]] <- as.factor(data[[z]] )
data <- droplevels(data)
    ## assuming data is wide, one row per subject

    ## plot x y
    sigy <- singlecor(data,x,y,bayes_factor=bayes_factor)

    ## split by z

    cc <- cocor::cocor(as.formula(paste("~",x,"+",y,"|",x,"+",y)),
                       data = list(data[get(z)==levels(data[[z]])[1]],data[get(z)==levels(data[[z]])[2]]))

    sigyz1  <- singlecor(data[get(z)==levels(data[[z]])[1]],x,y,bayes_factor=bayes_factor)
    sigyz2  <- singlecor(data[get(z)==levels(data[[z]])[2]],x,y,bayes_factor=bayes_factor)

    ## Bayes for the full model against one with interaction omitted
    if (bayes_factor){
    full <- BayesFactor::lmBF(data= na.omit(data),
                              formula=as.formula(paste0(x,"~",y, "+", z, "+", y,":",z)), progress=FALSE)
    nointer <- BayesFactor::lmBF(data= na.omit(data),
                                 formula=as.formula(paste0(x,"~",y," +", z)) , progress=FALSE)
    bf <- data.frame( full/nointer )[1]
    bf <- floor(as.numeric(bf))
  }else{bf <- 0}

    if (cl==2){
      p <- ggplot(data=data,aes_string(x=x,y=y,colour=z))+
        geom_point(shape=19,  alpha =0.8 ) + # you could put jitter in here position = "jitter",
        geom_smooth(method=lm)  +
        theme_bw()
    }else{
      p <- ggplot(data=data,aes_string(x=x,y=y))+
        geom_point(aes_string(colour=z),  shape=19, position = "jitter", alpha =0.8 ) +
        geom_smooth(method=lm,colour="black")  +
        theme_bw()
    }

    if (is.null(condcols)){condcols <- replacecondcols(condlevels = levels(data[[z]]))}
    if(!is.null(condcols)){
      p <- p + scale_colour_manual(values=condcols) }




    if (bayes_factor){
    if (results_title) {p <- p + ggtitle(paste(title_text,x,"
                         ",y," r=",sigy$r,"  p=",sigy$p,"  BF=",sigy$bf,"
                         ",y,z,"=",levels(data[[z]])[1],":  r=",sigyz1$r,"  p=",sigyz1$p,"  BF=",sigyz1$bf,"
                         ",y,z,"=",levels(data[[z]])[2],":  r=",sigyz2$r,"  p=",sigyz2$p,"  BF=",sigyz2$bf,"
                         reject equal r values p=",round(cc@fisher1925$p.value,3),"  BF inter=",bf ))+
      theme(plot.title = element_text(size = 10))}
    }else{
      if (results_title) {p <- p + ggtitle(paste(title_text,x,"
                         ",y," r=",sigy$r,"  p=",sigy$p,"
                         ",y,z,"=",levels(data[[z]])[1],":  r=",sigyz1$r,"  p=",sigyz1$p,"
                         ",y,z,"=",levels(data[[z]])[2],":  r=",sigyz2$r,"  p=",sigyz2$p,"
                         reject equal r values p=",round(cc@fisher1925$p.value,3)))+
        theme(plot.title = element_text(size = 10))}
    }
    results <- list(p=p, sigy.r=sigy$r,sigy.p=sigy$p,sigy.bf=sigy$bf,
                    sigyz1.r=sigyz1$r,sigyz1.p=sigyz1$p,sigyz1.bf=sigyz1$bf,
                    sigyz2.r=sigyz2$r,sigyz2.p=sigyz2$p,sigyz2.bf=sigyz2$bf,
                    sigz.p=sigyz1$p,sigz.bf=sigyz1$bf,   # old names, kept for back compatibility
                    cc=round(cc@fisher1925$p.value,3), intbf=bf)


    return(results)
  }
