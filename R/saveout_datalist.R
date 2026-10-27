saveout_datalist <- function(data,folder="processed",
                             filter_col=NULL,filter_value=NULL){
  #' Save a list of data tables to a folder of .csv files
  #'
  #' Saves each data.table in a list as a .csv file in a folder, named after the
  #' item in the list. Items that are not data frames, or have no columns, are
  #' skipped. It reverses [readin_datalist()].
  #'
  #' @param data A list of data.tables, with names.
  #' @param folder The folder to save into (created if needed). Default is
  #'   "processed".
  #' @param filter_col Optional column name. If you give this and `filter_value`,
  #'   only the rows with those values are saved (tables without that column are
  #'   saved in full).
  #' @param filter_value The value (or values) of `filter_col` to keep.
  #' @return Nothing is returned; the names of the items saved or skipped are
  #'   printed.
  #' @export
  #' @seealso [readin_datalist()], [filtered_datalist()]
  if (!file.exists(folder)){dir.create(folder)}
  for (i in 1:length(data)){

    if  ("data.frame" %in% class(data[[i]])) {

      if(  length(data[[i]])>0 ){

          if (!is.null(filter_col)  & !is.null(filter_value) ){
        if(  filter_col %in% colnames(data[[i]]) ){

          data[[i]] <- data[[i]][ data[[i]][[filter_col]] %in% filter_value]

        }}

      data.table::fwrite(x = data[[i]],file = paste0(folder,"/",names(data)[i],".csv"))
      cat(names(data)[i] , " saved\n")
      }
    }else{
      cat(names(data)[i] , "skipped\n")
    }

  }
}
