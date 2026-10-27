replacecondcols <- function(condlevels){
  #' Get colours for the levels of a condition
  #'
  #' Returns a set of default colours for a vector of condition names. If the names
  #' are themselves colours (eg "red", "blue" or "#FF0000"), those colours are used
  #' instead for those levels.
  #'
  #' @param condlevels The levels of a factor, eg `levels(data$condition)`.
  #' @return A vector of colours, in the same order as `condlevels`.
  #' @export
  #' @examples
  #' replacecondcols(c("red", "blue", "green"))
  #' replacecondcols(c("low", "high"))


  ctest <- sapply(condlevels, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })


  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  coltable <- data.table(colset=gg_color_hue(length(condlevels)),ctest,condlevels)

  coltable[!ctest,condlevels:=colset,by=condlevels]

  return(coltable$condlevels)}
