replacecondcols <- function(condlevels){
  #' returns either a set of default colours, or if condition names are also colours, returns those
  #' @param condlevels the levels of a factor in a condition column
  #' @export


  ctest <- sapply(condlevels, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })


  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  coltable <- data.table(colset=gg_color_hue(length(condlevels)),ctest,condlevels)

  coltable[!ctest,condlevels:=colset]

  return(coltable$condlevels)}
