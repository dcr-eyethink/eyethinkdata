clip_c_elements <- function (x,containing=NULL){
  #' takes a vector of values and puts them in a list with quotes and commas to put in code
  #' @export

  if (!is.null(containing)){
    x <- grep(x = x,pattern = containing,value = T)
  }

  clipr::write_clip(  paste0('c("', paste(x,collapse = '", "'),'")'))

}
