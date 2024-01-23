gorilla_iatanalysis <- function(data=NULL, outp="analysis",
                                exclude_pids=NULL,exclude_items=NULL, ...){
  #' processes IAT data from gorilla
  #'
  #' Runs a full analysis on IAT data from a gorilla experiment
  #' that is based on standard template.
  #' You can pass arguments to mypirate that is drawing the main plot
  #'
  #' @param data data list from gorilla import, if missing, I'll ask fora folder of gorilla downloads
  #' @param outp output foldername

  #' @export

if (is.null(data)){
  cat("Give me a folder of gorilla downloads")
  data <- data_collator_gorilla()
}

  iat_data <-  data$data_task[metadata %in% c("congruent","incongruent") & Attempt==1 & display=="trials",  # row selection
                              .(pid, trial=Trial.Number, condition=metadata,accuracy=Correct,rt,                # columns
                                item=paste0(na.omit(ImageCentre),na.omit(TextCentre)))]                   # stimulus idea


  data.table::setkey(iat_data,pid,trial)
  iat_data[,firstblock:=.SD[c(1)]$condition,by=pid]



  ## exclude pids
  if(!is.null(exclude_pids)){
    iat_data <-   iat_data[!p %in% exclude_pids]

  }

  ## exclude items
  if(!is.null(exclude_items)){
    iat_data <-   iat_data[!item %in% exclude_items]

  }


  subjmeans <- iat_data[accuracy==1 & rt<=3000,
                    .(meanRT=mean(rt)),by=.(pid,condition,firstblock)]

  ## put data with one row per participant, and exclude missing
  ParticipantMeans <- data.table::dcast(subjmeans,formula = pid+ firstblock ~ condition)
  ParticipantMeans <- na.omit(ParticipantMeans)
  ParticipantMeans$IATscore <- ParticipantMeans$incongruent - ParticipantMeans$congruent

  subjmeans <- data.table(data.table::melt(ParticipantMeans,id.vars = c("pid","firstblock"),
                               measure.vars = c("congruent","incongruent"),variable.name = "condition",value.name = "meanRT"))

  ## output analysis to text
  if (is.character(outp)){dir.create(file.path(outp),showWarnings = FALSE)
    sink(paste0(outp,"/IAT_ttest.txt"), append=FALSE, split=TRUE)}

  n <- dim(ParticipantMeans)[1]


  cat("You ran ", n, "participants successfully")
  cat("\n")

  tt <- t.test(data=subjmeans, meanRT~condition, paired=TRUE)
  print(tt)

  sink()

  anv <- afex::aov_ez(id="pid",dv="meanRT",between="firstblock",within="condition", data = subjmeans)
  anv_res <- do.call(plot_model,resolve.args(mod=anv,outp=outp,...))



  write.csv(x = ParticipantMeans ,
            file = paste0(outp,"/ParticipantMeans.csv"))

    return(list(anv_res, ParticipantMeans=ParticipantMeans,rawdata=iat_data))

}

