plotscat <- function(data,x,y,z=NULL,cl=2,condcols=NULL,cleand=FALSE,
                     results_title=TRUE,title_text=NULL,
                     outp="analysis",w=6,h=6,label=NULL,bayes_factor=F){
  #' Plot scattergram for two or three variables, optionally split by group
  #'
  #' this plots x  against y if there are two;
  #' if z is given and is a number, plots x against z as well
  #' Or z can be is a between subjects factor, will plot x y for the groups given in z
  #' and tests for slope differences
  #' @param data data frame with two cols to be correlated
  #' @param x name of x column
  #' @param y name of y column
  #' @param z name of column, either numeric DV or a factor for splitting groups
  #' @param outp name of folder to save plot in, set to blank if no save needed
  #' @param bayes_factor do you want bayes factor calculated, needs BayesFactor package
  #' @param results_title do you want all the stats in the title?
  #' @param title_text optional text for title
  #' @export

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

    sigy <- singlecor(data,x,y)




    p <- ggplot(data=data,aes_string(x=x,y=y))+
      geom_point(shape=19, color="red",position = "jitter") +
      geom_smooth(method=lm)  +
      theme_bw()

    if (!is.null(label)){
      p <- p+ geom_label(aes_string(label=label))
    }


    if (results_title){    t <- paste0(t,x," and ",y,"
r=",sigy$r,"  p=",sigy$p,"  BF=",signif(sigy$bf,digits=3), "  k.tau=",sigy$k.tau)}

    p <- p + ggtitle(t)

    results <- list(p=p, sigy.r=sigy$r,sigy.p=sigy$p,sigy.bf=sigy$bf,k.tau=sigy$k.tau)

  } else if ( is.factor(data[[z]]) | is.character(data[[z]])) {
    results=plotscat_split(data=data,x=x,y=y,z=z,cl=cl,
                           condcols=condcols,results_title=results_title,title_text=t )
    } else{
    ## we have a third variable, so will plot x-y and x-z

    if (cleand==TRUE){
      data <- na.omit(data[,.(get(x),get(y),get(z))])
      colnames(data) <- c(x,y,z)}

    sigy <- singlecor(data,x,y)
    cc <- cocor::cocor(as.formula(paste("~",x,"+",y,"|",x,"+",z)), data = data)
    sigz  <- singlecor(data,x,z)


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


  if (is.character(outp)){
    dir.create(file.path(outp),showWarnings = FALSE)
    ggsave(paste0(outp,"/ScatPlot ",x,y,z,".pdf"), width = w, height = h)
  }


  return(results)
}



plotscat_split <- function(data,x,y,z,cl=2,condcols=NULL,results_title=TRUE,title_text=NULL,outp="",bayes_factor=F){
    #' Plot scattergram for two variables, split by a group
    #'
    #' this plots x against y, split by z
    #' and it will plot x y for the two groups
    #' and tests for slope differences
    #' @param data data frame with two cols to be correlated
    #' @param x name of x column
    #' @param y name of y column
    #' @param z name of column, a factor splitting groups
    #' @param cl =2 plots the sub groups, =1 plots just main.
    #' @param condcols colours for two groups
    ###### BayesFactor regressionBF


    data <- setDT(data)
data[[z]] <- as.factor(data[[z]] )
    ## assuming data is wide, one row per subject

    ## plot x y
    sigy <- singlecor(data,x,y)

    ## split by z

    cc <- cocor::cocor(as.formula(paste("~",x,"+",y,"|",x,"+",y)),
                       data = list(data[get(z)==levels(data[[z]])[1]],data[get(z)==levels(data[[z]])[2]]))

    sigyz1  <- singlecor(data[get(z)==levels(data[[z]])[1]],x,y)
    sigyz2  <- singlecor(data[get(z)==levels(data[[z]])[2]],x,y)

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

    condcols <- replacecondcols(condlevels = levels(data[[z]]))
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
                    sigyz1.r=sigyz1$r,sigz.p=sigyz1$p,sigz.bf=sigyz1$bf,
                    sigyz2.r=sigyz2$r,sigz.p=sigyz2$p,sigz.bf=sigyz2$bf,
                    cc=round(cc@fisher1925$p.value,3), intbf=bf)


    return(results)
  }
