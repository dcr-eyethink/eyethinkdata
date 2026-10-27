readin_datalist <- function(folder="processed",flist=NULL, data=list(),drp=F,token=NULL){
  #' Read in a set of data files and store them in a list
  #'
  #' Reads all of the .csv files in a folder (or chosen ones) into a list of
  #' data.tables, named after the files. It reverses [saveout_datalist()].
  #'
  #' @details
  #' If a list is given in `data` that already has an item with the same name as a
  #' file, the new data is added to the bottom of the existing item. This is useful
  #' for adding more participants.
  #'
  #' @param folder Folder (or vector of folders) to read from. Default is
  #'   "processed". If it does not exist, you will be asked to choose one.
  #' @param flist Names of the files to read (including .csv). Default is all of
  #'   the files in the folder.
  #' @param data An existing list to add the data to.
  #' @param drp Read the files from a dropbox folder? This needs the rdrop2 package.
  #' @param token The dropbox token, if using `drp`.
  #' @return A list of data.tables, named after the files (without .csv).
  #' @export
  #' @seealso [saveout_datalist()]

    if (any(!file.exists(folder))){
     cat("Show me a folder of processed data\n")
      folder <- rstudioapi::selectDirectory()
   }

  for (fitem in folder){



    if(length(flist)==0){fitem_files <- list.files(fitem)}else{fitem_files <- flist}
    #flist <- list.files(fitem)


    for (f in fitem_files){

      if (drp){
        nd <- data.table::data.table(rdrop2::drop_read_csv(file = paste0(fitem,"/",f) ,
                                       dtoken = token))
        # print(str(nd))
        data[[gsub(f,pattern=".csv",replacement="")]] <- rbind(data[[gsub(f,pattern=".csv",replacement="")]],
                                                               nd,fill=T)
      }else{
        #nd <- data.table::data.table(read.csv(file = paste0(fitem,"/",f)))

        nd <- data.table::fread(file = paste0(fitem,"/",f))

        # print(str(nd))
        data[[gsub(f,pattern=".csv",replacement="")]] <- rbind(data[[gsub(f,pattern=".csv",replacement="")]],
                                                               nd,fill=T)
      }

    }

  }
  return(data)
}
