data_collator_gorilla <- function(datafold=NULL,...){
  #' Merge and load in data from multiple gorilla downloads
  #'
  #' Asks for a folder that can contain multiple data folders, zipped or un zipped, downloaded from gorilla

  #' @export
  #' @return Returns a list of data.tables for each type (task, questionnaire, continuous)
  #' @param datafold the folder with a collection of gorilla downloads

  if (is.null(datafold)){datafold <- rstudioapi::selectDirectory()}

  ## unzip and remove archive
  zf <- list.files(datafold,pattern=".zip",full.names = T)
  if (length(zf)>0){
    for (zff in zf){
      newf <- gsub(zff,pattern = ".zip",replacement = "")
      try(dir.create(newf),silent = TRUE)

      utils::unzip(zff,exdir = newf)
      file.remove(zff)}
  }


  ## run through reg data import
  dirf <- list.dirs(datafold,full.names = T,recursive=F)
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

  return(retdata)

}
