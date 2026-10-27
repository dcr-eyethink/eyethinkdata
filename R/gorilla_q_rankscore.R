gorilla_q_rankscore <- function(data,rankkey="rankpredict",keepcols=F,respid=NULL){
  #' Score ranked items
  #'
  #' Takes the output from a ranking question in a gorilla questionnaire (QB1) and
  #' returns the data in wide format, with one column for each item that was
  #' ranked, containing the rank given to it by each participant.
  #'
  #' @param data The list of data from a gorilla import, or just the questionnaire
  #'   data (`data_q`).
  #' @param rankkey The key that was used for the ranking question in gorilla.
  #'   Columns whose names contain this text are used, and their names should end
  #'   with the rank number (eg `rankpredict_1`).
  #' @param keepcols If `TRUE`, merge the ranks into the full original data.
  #' @param respid Text to put at the start of the new column names, which are
  #'   otherwise called `rank_<item>`.
  #' @return A data.table with the rank of each item.
  #' @export


  if (inherits(data, "list")){
    qd <- data$data_q
  }else{qd <- data}
  qdata <- qd   # original table, used if keepcols=TRUE



  rankcols <- grep(colnames(qd),pattern = rankkey,value = T)

  idv <- colnames(qd)[  !(colnames(qd) %in% rankcols)]

  qd <-   melt(qd,id.vars = idv)



  qd[,rank:=as.numeric(tail(strsplit(as.character(variable) ,split = "_")[[1]],1)),by=.(pid,variable) ]

  qd <- dcast(qd,formula = paste0( idv,"~value"),value.var = "rank")

  items <- colnames(qd)[!(colnames(qd) %in% idv)]

  if (is.null(respid)){
    setnames(qd,old = items,new=paste0("rank_",items)   )
  }else{
    setnames(qd,old = items,new=paste0(respid,"_", "rank_",items))
  }
  if (keepcols){qd <- pid_merge(qdata,qd)}
  return(qd)

}
