data_merger_gorilla <- function(datafile=NULL,ending="csv",...){
  #' Merge and load in data from a gorilla download
  #'
  #' It asks for one of the files in the un zipped DATA folder and
  #' loads in the task and the questionnaire data seperately

  #' @param ending string at the end of the filename, ie filetype.
  #' @param datafile example file. if this is supplied then don't ask user
  #' @return A list of two or three data tables of gorilla data.

  #' @export
  if (is.null(datafile)){
    print("Give me an example data file from folder downloaded from gorilla")
    datafile <- file.choose()}

  td <- data.table::data.table(do.call(data_merger,resolve.args(datafile = datafile, ending = ending,
                               contains = "task",...)))
  qd <- data.table::data.table(do.call(data_merger,resolve.args(datafile = datafile,ending = ending,
                               contains = "quest",...)))

  dd <- data.table::data.table()

  uploads <- NULL
  etd <- data.table::data.table()
  etf <- paste0(dirname(datafile),"/uploads")
  if (file.exists(etf)){
    uploads <-data.table::data.table(filename=list.files(etf,  full.names=F),filepath=list.files(etf,  full.names=T))
    #etd <- data.table(data_merger(ending="xlsx",datafile = dir(etf,full.names = T)[1]))
    etd <- data.table::data.table(data_merger(ending="csv",datafile = dir(etf,full.names = T)[1]))
    if (dim(etd)[1]>0){
      if ("participant_id" %in% colnames(etd)){
        etd[,pid:=as.factor(participant_id),by=participant_id]
        etd[,sid:=spreadsheet_row+screen_index/10,by=.(spreadsheet_row,screen_index)]
      }else{
        etd[,pid:=as.factor(Participant.Private.ID),by=Participant.Private.ID]
        etd[,sid:=Spreadsheet.Index+Screen.Index/10,by=.(Spreadsheet.Index,Screen.Index)]
      }

      ## return list of other uploaded files
    }
  }

  if(dim(td)[1]>0){

    td[,pid:=as.factor(Participant.Private.ID),by=Participant.Private.ID]
    td[,lid:=as.factor(Participant.Public.ID),by=Participant.Public.ID]

    data.table::setcolorder(td,neworder = c("pid","lid"))

    # DUPLICATED DATA NOT IMPLEMENTED
    # if ("Spreadsheet.Row" %in% colnames(td)){
    #   dd <- duplicated_gorilla(td)
    #   td[,sid:=Spreadsheet.Row+Screen.Number/10]}

  }

  if(dim(qd)[1]>0){

    qd[,pid:=as.factor(Participant.Private.ID),by=Participant.Private.ID]
    qd[,lid:=as.factor(Participant.Public.ID),by=Participant.Public.ID]

    data.table::setcolorder(qd,neworder = c("pid","lid"))


    if("Object.Name" %in% colnames(qd)){
      qd[!is.na(Object.Name),Question.Key:=Object.Name,by=Object.Name]
    }

    data.table::setcolorder(qd,"pid")
    ## get rid of - in quantised
    if ("Question.Key" %in% colnames(qd)){
      qd[,Question.Key:=gsub(Question.Key,pattern = "-",replacement = "_"),by=Question.Key]}

  }

  return(list(data_task=td,
              data_q=qd,data_continuous=etd,uploads=uploads))
}
