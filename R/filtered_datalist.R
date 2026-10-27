
filtered_datalist <- function(data,
                             filter_col=NULL,filter_value=NULL){
  #' Filter a whole list of data tables on one column
  #'
  #' Goes through a list of data.tables (such as that returned by
  #' [data_collator_gorilla()]) and keeps only the rows where the column
  #' `filter_col` has one of the values in `filter_value`. Tables in the list that
  #' do not have a column with that name are left as they are.
  #'
  #' If either `filter_col` or `filter_value` is missing, the list is returned
  #' unchanged.
  #'
  #' @param data A list of data.tables.
  #' @param filter_col Name of the column to filter on, eg "pid".
  #' @param filter_value The value (or vector of values) to keep.
  #' @return The list of data.tables, filtered.
  #' @export
  #' @examples
  #' \dontrun{
  #' # keep only the data for two participants, in every table that has a pid column
  #' two_people <- filtered_datalist(data, filter_col = "pid", filter_value = c(101, 102))
  #' }

for (i in 1:length(data)){


  if (!is.null(filter_col)  & !is.null(filter_value) ){
    if(  filter_col %in% colnames(data[[i]]) ){

      data[[i]] <- data[[i]][ data[[i]][[filter_col]] %in% filter_value]

    }}
}
  return(data)
}
