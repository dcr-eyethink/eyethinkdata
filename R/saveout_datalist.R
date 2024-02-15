saveout_datalist <- function(data,folder="processed"){
  #' saves out data tables in a list to a folder containing .csv files
  #' @export
  if (!file.exists(folder)){dir.create(folder)}
  for (i in 1:length(data)){
    #write.csv(x = data[[i]],file = paste0(folder,"/",names(data)[i],".csv"))
    data.table::fwrite(x = data[[i]],file = paste0(folder,"/",names(data)[i],".csv"))
    cat(names(data)[i] , " saved\n")
  }
}
