readin_datalist <- function(folder="processed",flist=NULL, data=list(),drp=F,token=NULL){
  #' reads in a set of data files and stores them in a list
  #' @param folder where to find the data, defaults to 'processed'
  #' @param flist what files do you want from folder? Defaults to all of them
  #' @param data already existing list to add data to
  #' @param drp is this to be downloaded from dropbox URL?
  #' @param token dropbox token
  #' @export

  for (fitem in folder){

    #  if (!file.exists(fitem)){
    #   cat("Show me a folder of processed data\n")
    #    fitem <- rstudioapi::selectDirectory()
    # }

    if(length(flist)==0){flist <- list.files(fitem)}
    #flist <- list.files(fitem)


    for (f in flist){

      if (drp){
        nd <- data.table::data.table(drop_read_csv(file = paste0(fitem,"/",f) ,
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
