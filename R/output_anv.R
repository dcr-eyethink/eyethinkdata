output_anv  <- function (mm,t="",outp="analysis",cond1=NULL,cond2=NULL){
  #' Outputs result of a anova using mypirate
  #'
  #' Turns an ANOVA into a text output of means,
  #' and stats
  #'
  #' NOT IMPLEMENTED YET
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


plot22  <- function (anv,t="",outp="analysis",cond1=NULL,cond2=NULL,cols=NULL,
                     pirate=F,barchart=F,maineffects=T){
  #' Outputs result of a two way ANOVA, with bar plots
  #'
  #' Turns an anova model into a text output of means,
  #' anova table and bar plot interactions
  #'
  #' @param anv anova model, two factors
  #' @param t title for plot and text files
  #' @param outp name of data output folder
  #' @param cond1 first condition, guessed if ommitted
  #' @param cond2 second condition, guessed if ommitted
  #' @param cols plot colours
  #' @param pirate produce pirate plot
  #' @param barchart produce barchart

  #' @export

  if (is.character(outp)){dir.create(file.path(outp),showWarnings = FALSE)}

  if (is.null(cond1)) {cond1 <- colnames(anv$data[["long"]])[2]}

  if(dim(anv$data[["long"]])[2]==3){
    # it's only a one way
    cond2 <- cond1
  }

  if (is.null(cond2)) {cond2 <- colnames(anv$data[["long"]])[3]}
  if(is.na(cond2)){ cond2 <- cond1}
  dv <- tail(colnames(anv$data[["long"]]),1)
  if (t==""){t=dv}


  #### ANOVA results
  # direct output to a file

  if (is.character(outp)){sink(paste0(outp,"/",t,"_",cond1,"X",cond2,"_ANOVA.txt"), append=FALSE, split=TRUE)}

  cat("ANOVA on",t,"plotted by",cond1,"X",cond2,"\n\n")
  cat("ANOVA table\n")
  print (anv)
  means_a <- emmeans::emmeans(anv,specs=c(cond1,cond2))
  inter <- emmeans::emmeans(anv, cond1, by = cond2)

  cat("\n\nMeans table\n")
  print(means_a)


  mfx <- data.table()
  for(c in c(cond1,cond2)){
    mainfx <-  emmeans::emmeans(anv, c)
    cat("\n\nPosthoc main effect contrasts within levels: ",c,"\n")
    print(pairs(mainfx) )
    smfx <- data.table(condition=c,summary(mainfx))
    setnames(smfx,old=c,new="level")
    mfx <- rbind(mfx,smfx)
  }


  cat("\n\nPosthoc contrasts within levels\n")
  print(pairs(inter) )

  if (is.character(outp)){sink()}


  if (pirate){

    if (is.character(outp)){pdf(file = paste0(outp,"/",t,"_",cond1,"X",cond2,"_Pirateplot.pdf"))}
    d <- anv$data[["long"]]
    f <- as.formula(paste(last(colnames(d)),"~",cond1, "+", cond2))
    (yarrr::pirateplot(formula = f,
                       data = d,
                       theme = 2,
                       point.o = .5,point.cex = .5,jitter.val = 0.075,
                       bean.f.o = .4,avg.line.o = .5, avg.line.lwd = 2,
                       bean.b.o = .2,inf.b.o = .3,inf.lwd = .5,inf.method="hdi",
                       bar.f.o = .5))

    if (is.character(outp)){ dev.off()}

  }

  maineffects_plot <- ""

  if (maineffects) {


    d <- anv$data[["long"]]

    dd <- rbind(data.table(condition=cond1,level=d[[cond1]],d),
                data.table(condition=cond2,level=d[[cond2]],d))
    #
    # f <- as.formula(paste(last(colnames(dd)),"~level+MainFX"))
    # (yarrr::pirateplot(formula = f,
    #                    data = dd,
    #                    theme = 2,
    #                    point.o = .5,point.cex = .5,jitter.val = 0.075,
    #                    bean.f.o = .4,avg.line.o = .5, avg.line.lwd = 2,
    #                    bean.b.o = .2,inf.b.o = .3,inf.lwd = .5,inf.method="hdi",
    #                    bar.f.o = .5))


    maineffects_plot <-

      ggplot(data =dd, aes_string(x = "condition", y = dv,colour="level"))+
      geom_violin(position = position_dodge(width=.8),alpha=.6)+
      geom_point(alpha=0.2, aes(x=condition),
                 position=position_jitterdodge(dodge.width = .8),show.legend = FALSE) +
      geom_crossbar(data=mfx,inherit.aes = F,
                    aes(x=condition,colour=level,
                        y=emmean, ymin=emmean,ymax=emmean),width=.8,fatten=3,
                    position=position_dodge(width=.8),show.legend = FALSE)+
      geom_errorbar(data=mfx,inherit.aes = F,
                    aes(x=condition,colour=level,
                        ymin=lower.CL,ymax=upper.CL),width=.4,
                    position=position_dodge(width=.8),show.legend = FALSE)+
      theme_minimal()+labs(title=t,subtitle="Main Effects")+
      labs(colour=cond2)



    if (is.character(outp)){
      ggsave(paste0(outp,"/",t,"_MainFX_plot.pdf"), width = 8, height = 4)
    }

  }

  anvplot <- ""

  if (barchart){
    plotdata <- as.data.frame(summary(means_a ))
    pd <- position_dodge(.7)
    anvplot <- ggplot(plotdata, aes_string(x=cond2,y="emmean",fill=cond1)) +
      geom_bar(stat="identity", position=pd) + theme_classic() + labs(title=t,y=dv) +
      geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1,position=pd)

    # if (!is.null(cols)){
    # anvplot <- anvplot + scale_fill_manual(values=cols)}
    #
    cols <- arecondcols(cols,condlevels = levels(plotdata[[cond1]]))
    if(!is.null(cols)){
      anvplot <- anvplot + scale_fill_manual(values=cols) }

    anvplot
    if (is.character(outp))
    {ggsave(paste0(outp,"/",t,"_",cond1,"X",cond2,"_plot.pdf"), width = 4, height = 4)}
  }else{

    ## do a RDI of interactions

    d <- anv$data[["long"]]

    plotdata <- as.data.frame(summary(means_a ))

    anvplot <- ggplot(data =d, aes_string(x = cond2, y = dv,colour=cond1))+
      geom_violin(position = position_dodge(width = 0.8),alpha=.6 )+
      geom_point(alpha=0.2,position=position_jitterdodge(dodge.width = .8)) +
      geom_crossbar(data=plotdata,inherit.aes = F,
                    aes_string(x=cond2,colour=cond1,
                               y="emmean", ymin="emmean",ymax="emmean"),width=.8,
                    position=position_dodge(width=.8))+
      geom_errorbar(data=plotdata,inherit.aes = F,
                    aes_string(x=cond2,colour=cond1,
                               ymin="lower.CL",ymax="upper.CL"),width=.4,
                    position=position_dodge(width=.8))+
      theme_minimal()+labs(title=t,subtitle="Interactions")




    if (is.character(outp)){
      ggsave(paste0(outp,"/",t,"_",cond1,"X",cond2,"_Inter_plot.pdf"), width = 6, height = 4)}

  }



  return(list(anvplot=anvplot,maineffects_plot=maineffects_plot,means=means_a))
}


