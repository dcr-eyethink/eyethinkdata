data_merger_gorilla <- function(datafile=NULL,ending="csv",...){
  #' Load in and merge the data from one gorilla download
  #'
  #' Reads one unzipped gorilla download folder. The task data and questionnaire
  #' data are loaded and merged separately, and any uploaded files (for example
  #' mouse or eye tracking data) are loaded too. You will usually use
  #' [data_collator_gorilla()], which calls this function on every download in a
  #' folder.
  #'
  #' @details
  #' You can give any file from the unzipped download folder as `datafile`, and the
  #' rest are found from that. If you give nothing, you will be asked to choose a
  #' file. All the files with "task" in their name are merged to make the task
  #' data, and all those with "quest" in their name make the questionnaire data.
  #'
  #' Adds a column `pid` (the anonymised participant id, as a factor, from
  #' `Participant.Private.ID`) and `lid` (from `Participant.Public.ID`). The
  #' questionnaire data is checked to see if it is in long or wide format.
  #' In the questionnaire data, hyphens in `Question.Key` are changed to underscores.
  #'
  #' @param datafile An example file from the unzipped download folder. If `NULL`,
  #'   you will be asked to choose one.
  #' @param ending The file type to read, default "csv".
  #' @param ... Further arguments passed on to [data_merger()], eg
  #'   `gorilla_clean = TRUE`.
  #' @return A list with items `data_task`, `data_q`, `data_qtype` ("long", "wide"
  #'   or "none"), `data_continuous` (tracking data from the uploads folder) and
  #'   `uploads` (a table of other uploaded files). See [data_collator_gorilla()]
  #'   for details.
  #' @export
  #' @seealso [data_collator_gorilla()], [data_merger()]
  #' @examples
  #' \dontrun{
  #' data <- data_merger_gorilla("gorilla_data/my_download/data_exp_1.csv")
  #' }


  if (is.null(datafile)){
    print("Give me an example data file from folder downloaded from gorilla")
    datafile <- file.choose()}

  td <- data.table::data.table(do.call(data_merger,resolve.args(datafile = datafile, ending = ending,
                               contains = "task",...)))
  qd <- data.table::data.table(do.call(data_merger,resolve.args(datafile = datafile,ending = ending,
                               contains = "quest",gorilla_checkwideq=T,...)))

  data_qtype <- "none"
  if (!is.null(qd)){
  if ( "Question.Key" %in% colnames(qd) |  "Object.Name" %in% colnames(qd)){
    data_qtype <- "long"
  }else{
    data_qtype <- "wide"
  }}

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

    if ("Participant.Public.ID" %in% colnames(td)){
    td[,lid:=as.factor(Participant.Public.ID),by=Participant.Public.ID]
    data.table::setcolorder(td,neworder = c("pid","lid"))}else{
      data.table::setcolorder(td,neworder = c("pid"))
    }

    # DUPLICATED DATA NOT IMPLEMENTED
    # if ("Spreadsheet.Row" %in% colnames(td)){
    #   dd <- duplicated_gorilla(td)
    #   td[,sid:=Spreadsheet.Row+Screen.Number/10]}

  }

  if(dim(qd)[1]>0){

    qd[,pid:=as.factor(Participant.Private.ID),by=Participant.Private.ID]
    if ("Participant.Public.ID" %in% colnames(qd)){
    qd[,lid:=as.factor(Participant.Public.ID),by=Participant.Public.ID]
      data.table::setcolorder(qd,neworder = c("pid","lid"))
    }else{
      data.table::setcolorder(qd,neworder = c("pid"))
    }




    if("Object.Name" %in% colnames(qd)){
      qd[!is.na(Object.Name),Question.Key:=Object.Name,by=Object.Name]
    }

    data.table::setcolorder(qd,"pid")
    ## get rid of - in quantised
    if ("Question.Key" %in% colnames(qd)){
      qd[,Question.Key:=gsub(Question.Key,pattern = "-",replacement = "_"),by=Question.Key]}

  }

  return(list(data_task=td,
              data_q=qd,data_qtype=data_qtype,
              data_continuous=etd,uploads=uploads))
}
