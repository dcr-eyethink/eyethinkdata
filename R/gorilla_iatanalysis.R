gorilla_iatanalysis <- function(data=NULL, outp="analysis",
                                exclude_pids=NULL,exclude_items=NULL, ...){
  #' Process and analyse IAT data from gorilla
  #'
  #' Runs a full analysis on data from an Implicit Association Test (IAT) that was
  #' run in gorilla using the standard template. It picks out the trial data,
  #' calculates each person's mean reaction time in the congruent and incongruent
  #' blocks, and tests the difference.
  #'
  #' @details
  #' The analysis follows these steps:
  #'
  #' 1. Trials are selected where `metadata` is "congruent" or "incongruent", at the
  #'    first attempt, in the display called "trials". Incorrect trials and those
  #'    with reaction times over 3000 ms are removed.
  #' 2. Mean reaction times are calculated for each person, and participants with a
  #'    missing condition are dropped. The IAT score is incongruent minus congruent.
  #' 3. A paired t-test is run and saved in `IAT_ttest.txt` in `outp`.
  #' 4. A mixed ANOVA is run with block order (`firstblock`) between participants
  #'    and condition within participants. It is plotted and printed using
  #'    [plot_model()].
  #' 5. The participant means are saved as `ParticipantMeans.csv` in `outp`.
  #'
  #' @param data The list of data from a gorilla import. If missing, you will be
  #'   asked to choose a folder of gorilla downloads.
  #' @param outp Name of the folder to save results in (it is created if needed).
  #'   Set to `NULL` or "" to not save anything, and just return the results.
  #' @param exclude_pids Vector of participant ids to leave out.
  #' @param exclude_items Vector of items (stimuli) to leave out.
  #' @param ... Further arguments passed to [plot_model()], and from there to
  #'   [pirateye()], to change the plots.
  #' @return A list. The first (unnamed) item is the output of [plot_model()], then
  #'   `ParticipantMeans` (one row per person, with the IAT score) and `rawdata`
  #'   (the selected trial data).
  #' @export
  #' @seealso [plot_model()], [data_collator_gorilla()]
  #' @examples
  #' \dontrun{
  #' data <- data_collator_gorilla("gorilla_data")
  #' iat <- gorilla_iatanalysis(data, exclude_pids = c(1234567))
  #' iat$ParticipantMeans
  #' }


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
    iat_data <-   iat_data[!pid %in% exclude_pids]

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
  saving <- is.character(outp) && nzchar(outp)
  nsink <- sink.number()
  on.exit(while (sink.number() > nsink) sink())   # never leave output diverted if there is an error
  if (saving){dir.create(file.path(outp),showWarnings = FALSE)
    sink(paste0(outp,"/IAT_ttest.txt"), append=FALSE, split=TRUE)}

  n <- dim(ParticipantMeans)[1]


  cat("You ran ", n, "participants successfully")
  cat("\n")

  tt <- t.test(data=subjmeans, meanRT~condition, paired=TRUE)
  print(tt)

  if (saving){sink()}

  anv <- afex::aov_ez(id="pid",dv="meanRT",between="firstblock",within="condition", data = subjmeans)
  anv_res <- do.call(plot_model,resolve.args(mod=anv,outp=outp,...))



  if (saving){
    write.csv(x = ParticipantMeans ,
              file = paste0(outp,"/ParticipantMeans.csv"))}

    return(list(anv_res, ParticipantMeans=ParticipantMeans,rawdata=iat_data))

}

