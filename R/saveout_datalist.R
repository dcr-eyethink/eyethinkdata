saveout_datalist <- function(data,folder="processed",
                             filter_col=NULL,filter_value=NULL){
  #' saves out data tables in a list to a folder containing .csv files
  #' @param data list of data.tables
  #' @param folder to save into
  #' @param filter_col save out a subset of data, based on this column
  #' @param filter_value using this value as filter
  #' @export
  if (!file.exists(folder)){dir.create(folder)}
  for (i in 1:length(data)){


      if (!is.null(filter_col)  & !is.null(filter_value) ){
        if(  filter_col %in% colnames(data[[i]]) ){

        data[[i]] <- data[[i]][ data[[i]][[filter_col]] %in% filter_value]

      }}



    data.table::fwrite(x = data[[i]],file = paste0(folder,"/",names(data)[i],".csv"))
    cat(names(data)[i] , " saved\n")
  }
}
