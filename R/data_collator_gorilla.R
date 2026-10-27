data_collator_gorilla <- function(datafolder=NULL,...){
  #' Merge and load in data from multiple gorilla downloads
  #'
  #' Reads in the raw data from one or more downloads from the gorilla experiment
  #' platform and collates them into a single list of data.tables. This is usually
  #' the first function to run in an analysis of gorilla data.
  #'
  #' @details
  #' `datafolder` should be a folder that contains the downloads from gorilla.
  #' Each download can either be a .zip file, or an already unzipped folder that
  #' contains the gorilla .csv files. The data from all of the downloads are
  #' stacked together.
  #'
  #' **Note:** any .zip files found in `datafolder` are unzipped into a folder with
  #' the same name, and the .zip file is then deleted.
  #'
  #' Each download folder is processed by [data_merger_gorilla()], and `...` is
  #' passed on to that function (and from there to [data_merger()]).
  #'
  #' The questionnaire data can be downloaded from gorilla in wide (one row per
  #' participant) or long format. This function works out which you have and
  #' reports it in `data_qtype`. The questionnaire functions in this package
  #' (`gorilla_q_...`) need long format.
  #'
  #' The names of the tasks and questionnaires found are printed to the console,
  #' so that you can copy them into later analysis code.
  #'
  #' @param datafolder The folder containing the gorilla downloads. If left as
  #'   `NULL`, you will be asked to choose a folder.
  #' @param ... Further arguments passed on to [data_merger_gorilla()], for
  #'   example `gorilla_clean = TRUE` to drop some of the less useful gorilla
  #'   columns.
  #' @return A list with these items:
  #' \describe{
  #'   \item{data_task}{data.table of all the task (experiment) data, with added
  #'   columns `pid` (anonymous participant id, a factor), `lid` (public id), and
  #'   `rt` (numeric reaction time)}
  #'   \item{data_q}{data.table of the questionnaire data}
  #'   \item{data_qtype}{"long", "wide" or "none"}
  #'   \item{data_continuous}{data.table of mouse or eye tracking data from the
  #'   `uploads` folder, if there is any}
  #'   \item{uploads}{table of the names and paths of any other uploaded files}
  #'   \item{task_names}{the names of the tasks found}
  #'   \item{q_names}{the names of the questionnaires found}
  #' }
  #' @export
  #' @seealso [data_merger_gorilla()], [data_merger()], [pid_merge()]
  #' @examples
  #' \dontrun{
  #' data <- data_collator_gorilla("gorilla_data")
  #' head(data$data_task)
  #' }



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

  retdata$task_names <- unique( retdata$data_task$Task.Name)
 cat("\nTask names:\n",paste0('"',retdata$task_names,'", '),"\n")

 retdata$q_names <- unique( retdata$data_q$Task.Name)
 cat("\nQ names:\n",paste0('"',retdata$q_names,'", '),"\n")

  return(retdata)

}
