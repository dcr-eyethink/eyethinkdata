gorilla_iatanalysis <- function(filename=NULL,gather=TRUE,outputfolder="analysis",e
                        xclude_pids=NULL,exclude_items=NULL,
                        datafull=NULL,full=T,
                        hypoth="",cl = c("#009E73", "#D55E00",... )){
  #' processes IAT data from gorilla
  #'
  #' Runs a full analysis on IAT data from a gorilla experiment
  #' that is based on standard template.
  #' It takes a filename of an individual .csv data file from gorilla
  #' or if filename is blank, it asks for a csv file.
  #' If there is data from more than one experiment/version
  #' then set gather to T and it'll get all neighbouring task files
  #' outputs into working directory unless outputfolder set.
  #' You can pass arguments to mypirate that is drawing the main plot
  #'
  #' @param filename data file from gorilla, if blank requests it
  #' @param outputfolder foldername, if blank uses working directory
  #' @param hypoth a string of expected results or notes to add to output
  #' @param cl line colours for density plots
  #' @param gather are there more than 1 experiment versions?
  #' @return full stats and plots, also saved to files

  #' @export

  if (is.null(datafull)){

    if (is.null( filename)){  filename <- file.choose()  }


    if (gather){
      filename_list <- list.files(dirname(filename),
                                  pattern="task", full.names=TRUE, ignore.case=TRUE)
    }else{filename_list <-filename }

    # if passed more than one, read them all in and string together
    datafull <- data.frame()

    for (f in filename_list){
      dbit <- read.csv(f)
      #datafull <- rbind(datafull,dbit)
      datafull <- rbindlist(list(datafull,dbit), fill = TRUE)
    }
  }

  # if (is.null( outputfolder)){    outputfolder <- dirname(filename)  }

  if (is.character(outputfolder)){
    outputfolder <- paste0(dirname(filename[1]),"/",outputfolder[1])
    dir.create(file.path(outputfolder),showWarnings = FALSE)}


  ##  make data
  data <- data.table(p=datafull$Participant.Private.ID,d=datafull$UTC.Date,
                     rt=as.numeric(as.character(datafull$Reaction.Time)),
                     trial=datafull$Trial.Number,attempt=datafull$Attempt,
                     a=datafull$Correct,
                     condition=tolower(datafull$metadata),
                     item=paste0(na.omit(datafull$ImageCentre),na.omit(datafull$TextCentre)))
  ## get only the trials with attempt 1
  data <-  data[((condition=="congruent" | condition=="incongruent") & attempt==1)]

  ## exclude pids
  if(!is.null(exclude_pids)){
    data <-   data[!p %in% exclude_pids]

  }

  ## exclude items
  if(!is.null(exclude_items)){
    data <-   data[!item %in% exclude_items]

  }

  ## label which condition came first
  setkey(data,p)
  counterb <- data[,.(firstblock=.SD[c(1)]$condition), by=p]
  setkey(counterb,p)
  data <- data[counterb]

  subjmeans <- data[a==1 & rt<=3000,
                    .(meanRT=mean(rt)),by=.(p,condition,firstblock)]

  ## put data with one row per participant, and exclude missing
  ParticipantMeans <- dcast(subjmeans,formula = p+ firstblock ~ condition)
  ParticipantMeans <- na.omit(ParticipantMeans)
  ParticipantMeans$IATscore <- ParticipantMeans$incongruent - ParticipantMeans$congruent

  subjmeans <- data.table(melt(ParticipantMeans,id.vars = c("p","firstblock"),
                               measure.vars = c("congruent","incongruent"),variable.name = "condition",value.name = "meanRT"))

  ### pirate plot of participant means

  mp <- do.call(mypirate,resolve.args(...,data=subjmeans,dv="meanRT",cond="condition",outp = outputfolder,cols=cl,bars = T)

  n <- dim(ParticipantMeans)[1]

  sink(paste0(outputfolder,"/IAT_analysis.txt"), append=FALSE, split=TRUE)

  writeLines(hypoth)
  cat("\n")
  cat("You ran ", n, "participants successfully")
  cat("\n")

  tt <- t.test(data=subjmeans, meanRT~condition, paired=TRUE)
  print(tt)


  cat("\n\nDescriptive Statistics for Congruent and Incongruent conditions")
  cat("\n")
  condition_means <- (subjmeans[,psych::describe(meanRT),by=condition])
  print(condition_means)

  srt <- ggplot2::ggplot(subjmeans,ggplot2::aes(x=meanRT,colour=condition))+ggplot2::geom_density(size=1) +
    ggplot2::geom_vline(data=condition_means, aes(xintercept=mean,  colour=condition), linetype="dashed", size=1) +
    ggplot2::theme_bw() + ggplot2::scale_colour_manual(values=cl)
  srt
  ggplot2::ggsave(paste0(outputfolder,"/density_plot.pdf"), width=6, height = 4)


  if (length(unique(subjmeans$firstblock))==1){
    cat("\n\n You only seem to have one condition for the firstblock")
    anvplot <- ""
    anv <- ""
  }else{

    if (full){
      ## these analyses can be disabled for first years who haven't done AVNOVAs

      cat("\n\nDescriptive Statistics for Congruent and Incongruent conditions, split by firstblock")
      cat("\n")
      print(subjmeans[,psych::describe(meanRT),by=.(condition,firstblock)])

      cat("\n\nANOVA on condition and firstblock")
      cat("\n")
      anv <- afex::aov_ez(id="p",dv="meanRT",between="firstblock",within="condition", data = subjmeans)
      print (anv)

      means_a <- emmeans::lsmeans(anv,specs=c("firstblock","condition"))
      plotdata <- as.data.frame(summary(means_a))
      pd <- ggplot2::position_dodge(.7)
      anvplot <- ggplot2::ggplot(plotdata, ggplot2::aes(x=firstblock,y=lsmean,fill=condition)) +
        ggplot2::geom_bar(stat="identity", position=pd) + ggplot2::theme_classic() +
        labs(title="IAT by condition and first mapping", y="mean RT (ms)") +
        ggplot2::geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1,position=pd) +
        ggplot2::scale_fill_manual(values=cl)
      anvplot
      ggplot2::ggsave(paste0(outputfolder,"/IAT_ANOVA_plot.pdf"), width = 5, height = 4)
      cat("\n\nMeans Estimates from ANOVA")
      cat("\n")
      print(means_a)
    }
  }



  sink()

  write.csv(x = ParticipantMeans ,
            file = paste0(outputfolder,"/ParticipantMeans.csv"))
  if (full){
    return(list(srt=srt, anvplot=anvplot,mypirate=mp, anv=anv,n=n, pv=tt$p.value, ParticipantMeans=ParticipantMeans,rawdata=data))
  }else{
    return(list(srt=srt,mypirate=mp,n=n, pv=tt$p.value, ParticipantMeans=ParticipantMeans,rawdata=data))
  }
}
