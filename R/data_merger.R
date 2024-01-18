data_merger <- function(ending="txt",contains="",d=",",h=TRUE,
                        datafile=NULL,gorilla_clean=FALSE,datafolder=NULL){

  #' load in and merge data from multiple files into one data.table
  #'
  #' It asks for one of the files in the DATA folder and merges
  #' all the files in there strung together in one dataframe

  #' @param ending string at the end of the filename, ie filetype.
  #' @param contains string to identify datafile, eg 'task'
  #' @param d delimiter for data, default comma
  #' @param h are there headers in data files?
  #' @param datafile example file. if this is supplied then don't ask user
  #' @param datafolder give the file location of a folder instead of eg file
  #' @return A data table of all datafiles ending in \code{ending} containing \code{contains}.

  #' @export

  if (is.null(datafolder)){
    if (is.null(datafile)){
      print("Give me an example data file, and I'll get all of those in the same folder")
      datafolder <- dirname(file.choose())}else{
        datafolder <- dirname(datafile)
      }}

  p <- paste0(".*",contains,".*",ending,"$")
  file.sources = list.files(datafolder, pattern=p, full.names=TRUE, ignore.case=TRUE)

  ## get rid of those ~$ excel files
  if (ending=="xlsx" | ending=="xls"){
    file.sources <- file.sources[grep(x=file.sources,pattern="~$",fixed = TRUE,invert=TRUE)]
  }

  # get rid of those less than 4 bytes
  info = file.info(file.sources)
  file.sources = rownames(info[info$size > 4, ])

  alldata <- data.frame()

  for (f in file.sources){

    if (ending=="xlsx" | ending=="xls"){
      data <-  (readxl::read_excel (f, sheet = 1))
      colnames(data) <- sub(colnames(data),pattern = " ",replacement = ".")
    }else{
      data <- read.csv(f, na.strings="",sep = d,header=h)

      # data <- try(read.csv(f, na.strings="",sep = d,header=h),silent=TRUE)
      # if (inherits(df, 'try-error')){data <- NULL}

    }


    if (dim(data)[1]>0) {
      newdata <- data.frame(filename=basename(f),data)
      if(dim(alldata)[1]>0){
        ## this is new code to create new col names
        ## if they exist only in new data set
        ## these are uniquely in newdata
        nn <- colnames(newdata)[!colnames(newdata) %in% colnames(alldata)]
        if(length(nn)>0){alldata[,nn] <- NA}
        ## these are uniquely in alldata
        nn <- colnames(alldata)[!colnames(alldata) %in% colnames(newdata)]
        if(length(nn)>0){newdata[,nn] <- NA}}

      alldata <- rbind(alldata,newdata)
    }
    setDT(alldata)
    if (gorilla_clean){
      ## discard some of the gorilla columns to make more manageable
      discard <- c("Post.Processed","Local.Timestamp", "Local.Timezone",
                   "Local.Date", "Cohort.Name","Cohort.Version" ,
                   "Repeat.Key","Schedule.ID",
                   "Participant.Starting.Group", "Participant.Status","Participant.Completion.Code",
                   "Participant.External.Session.ID", "Participant.Device.Type",
                   "Participant.Device", "Participant.OS" ,"Participant.Browser" ,
                   "Participant.Viewport.Size","Checkpoint","Task.Version",
                   "Dishonest","X.Coordinate","Y.Coordinate","Timed.Out",
                   "randomise_blocks" ,"randomise_trials")
      suppressWarnings( alldata[, eval(discard) := NULL])
    }
    if ("Reaction.Time" %in% colnames(alldata)){
      suppressWarnings( alldata[, rt := as.numeric(as.character(Reaction.Time))])}


  }
  return(alldata)
}
