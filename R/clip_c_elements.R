clip_c_elements <- function (x,containing=NULL){
  #' Copy a vector to the clipboard as R code
  #'
  #' Takes a vector of values and puts a string like `c("a", "b", "c")` on the
  #' clipboard, ready to paste into a script. Handy for turning column names or
  #' file names into a vector you can edit.
  #'
  #' @param x A vector of values (they are converted to text and quoted).
  #' @param containing Optional text pattern (a regular expression). If given, only
  #'   the elements of `x` that match it are copied.
  #' @return Nothing is returned; called for its side effect of writing to the
  #'   clipboard. Needs the clipr package to be installed.
  #' @export
  #' @examples
  #' \dontrun{
  #' clip_c_elements(colnames(mtcars), containing = "^c")
  #' }

  if (!is.null(containing)){
    x <- grep(x = x,pattern = containing,value = T)
  }

  clipr::write_clip(  paste0('c("', paste(x,collapse = '", "'),'")'))

}
