resolve.args <- function(..., PRE.ARGS=list(), POST.ARGS=list()) {
  #' @export
  a <- list()
  l <- c(PRE.ARGS, list(...), POST.ARGS)
  for (name in unique(names(l))) {
    a[[name]] <- l[[name]] # First occurrence will be found.
  }
  return(a)
}
