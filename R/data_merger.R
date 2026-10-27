data_merger <- function(ending="txt",contains="",d=",",h=TRUE,
                        datafile=NULL,gorilla_clean=FALSE,gorilla_checkwideq=F,datafolder=NULL){

  #' Load in and merge data from multiple files into one data.table
  #'
  #' Reads all of the files of one type in a folder and stacks them into one
  #' data.table, adding a `filename` column to say where each row came from. It is
  #' a general function, but has some options for tidying up gorilla data.
  #'
  #' @details
  #' You can either name the folder (`datafolder`) or give an example file from that
  #' folder (`datafile`). If you give neither, you will be asked to choose an
  #' example file, and the function will use all of the matching files in the same
  #' folder.
  #'
  #' Files are picked out if their name contains the text in `contains` and ends
  #' with `ending` (upper and lower case are ignored; `contains` can be a regular
  #' expression). Empty files (4 bytes or less) and the temporary `~$` files that
  #' Excel makes are skipped. For Excel files, only the first sheet is read.
  #'
  #' If files have different columns, the missing columns are filled with `NA`.
  #'
  #' @param ending The end of the file names, ie the file type. Default is "txt";
  #'   use "csv", "xlsx" or "xls" as needed.
  #' @param contains Text that must appear in the file name, eg "task". Default is
  #'   "" which accepts any name.
  #' @param d The delimiter (separator) used in the data files. Default is a comma.
  #' @param h Do the data files have a header row of column names? Default `TRUE`.
  #' @param datafile One example file from the folder you want to read. If supplied,
  #'   you will not be asked to choose a file.
  #' @param gorilla_clean If `TRUE`, discard a set of gorilla columns that are
  #'   rarely needed (timestamps, device information, etc.) to make the data more
  #'   manageable.
  #' @param gorilla_checkwideq For gorilla questionnaire data. If `TRUE`, files that
  #'   turn out to be wide format questionnaire data are merged side by side
  #'   (matched on `Participant.Private.ID`) rather than stacked.
  #' @param datafolder The folder containing the files. If given, this is used
  #'   instead of `datafile`.
  #' @return A data.table of all of the files. If the data has a `Reaction.Time`
  #'   column, a numeric copy of it called `rt` is added.
  #' @export
  #' @seealso [data_merger_gorilla()], [data_collator_gorilla()]
  #' @examples
  #' \dontrun{
  #' all_csvs <- data_merger(ending = "csv", contains = "task", datafolder = "my_folder")
  #' }



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


    }


    if (dim(data)[1]>0) {
      newdata <- data.frame(filename=basename(f),data)

         if(dim(alldata)[1]==0){
        ## this is the first
        alldata <- newdata
      }else{

          if(gorilla_checkwideq & ( !"Question.Key" %in% colnames(newdata) &
                                    !"Object.Name" %in% colnames(newdata))){
          #  we have a wide question data set

            alldata <- pid_merge(data.table(alldata),data.table(newdata),link="Participant.Private.ID")

          }else{
        #  we have a long data set

        ## this is  code to create new col names
        ## if they exist only in new data set
        ## these are uniquely in newdata
        nn <- colnames(newdata)[!colnames(newdata) %in% colnames(alldata)]
        if(length(nn)>0){alldata[,nn] <- NA}
        ## these are uniquely in alldata
        nn <- colnames(alldata)[!colnames(alldata) %in% colnames(newdata)]
        if(length(nn)>0){newdata[,nn] <- NA}

        alldata <- rbind(alldata,newdata)
}
    }
}
  } # now gone through files

  data.table::setDT(alldata)
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
    suppressWarnings( alldata$rt <- as.numeric(as.character(alldata$Reaction.Time)))}


  return(alldata)
}
