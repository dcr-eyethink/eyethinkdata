data_collator_gorilla <- function(datafolder=NULL,...){
  #' Merge and load in data from multiple gorilla downloads
  #'
  #' Asks for a folder that can contain multiple data folders, zipped or un zipped, downloaded from gorilla.
  #' It will try and figure out if your questionnaire data has been downloaded wide or long format,
  #' and collate it accordingly
  #' Note that the functions for processing questionnaire data in this package assume long

  #' @export
  #' @return Returns a list of data.tables for each type (task, questionnaire, continuous)
  #' @param datafolder the folder with a collection of gorilla downloads


  if (is.null(datafolder)){
    rstudioapi::showDialog(message = "Select a folder of gorrila zip data files or unzipped data folders",title = "datafolder location?")
    datafolder <- rstudioapi::selectDirectory()}

  if (!dir.exists(datafolder)){stop(paste0("I can't find ",datafolder))}

  ## unzip and remove archive
  zf <- list.files(datafolder,pattern=".zip",full.names = T)
  if (length(zf)>0){
    for (zff in zf){
      newf <- gsub(zff,pattern = ".zip",replacement = "")
      try(dir.create(newf),silent = TRUE)

      utils::unzip(zff,exdir = newf)
      file.remove(zff)}
  }


  ## run through reg data import
  dirf <- list.dirs(datafolder,full.names = T,recursive=F)
  retdata <- list()

  for(dfold in dirf){
    datafile <- list.files(dfold,pattern=".csv",full.names = T)[1]
    impdata <- do.call(data_merger_gorilla,resolve.args(datafile = datafile,...))

    if(length(retdata)==0){
      retdata <- impdata
    }else{
      #combine lists
      for (d in names(impdata)){
        retdata[[d]] <- rbind(retdata[[d]],impdata[[d]],fill=T)
      }
    }

  }

  retdata$data_qtype <- NULL
  data_qtype <- "none"
  if (!is.null(retdata$data_q)){
    if ( "Question.Key" %in% colnames(retdata$data_q) |  "Object.Name" %in% colnames(retdata$data_q)){
      data_qtype <- "long"
    }else{
      data_qtype <- "wide"
    }}

  retdata$data_qtype <- data_qtype

  return(retdata)

}
