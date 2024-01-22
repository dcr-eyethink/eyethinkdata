gorilla_q_rankscore <- function(data,rankkey="rankpredict",keepcols=F,respid=NULL){
  #' Scores ranked items
  #'
  #' Takes output from a ranking questionniare (QB1) in gorilla and returns data in a wide format with one column per ranked item
  #' @param data data list or just the data_q
  #' @param rankkey what key was used in QB1 for this item
  #' @param respid word to append to column names
  #' @export


  if (inherits(data, "list")){
    qd <- data$data_q
  }else{qd <- data}



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
