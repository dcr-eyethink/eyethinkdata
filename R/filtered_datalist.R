
filtered_datalist <- function(data,
                             filter_col=NULL,filter_value=NULL){
  #' filters whole data set to values of specified column
  #' @param data list of data.tables
  #' @param filter_col save out a subset of data, based on this column
  #' @param filter_value using this value as filter
  #' @export

for (i in 1:length(data)){


  if (!is.null(filter_col)  & !is.null(filter_value) ){
    if(  filter_col %in% colnames(data[[i]]) ){

      data[[i]] <- data[[i]][ data[[i]][[filter_col]] %in% filter_value]

    }}
}
  return(data)
}
