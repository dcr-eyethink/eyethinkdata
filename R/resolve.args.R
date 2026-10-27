resolve.args <- function(..., PRE.ARGS=list(), POST.ARGS=list()) {
  #' Combine argument lists, with earlier ones taking priority
  #'
  #' A helper for passing arguments on to other functions, so you can set some
  #' defaults while letting users override them. Joins `PRE.ARGS`, then the
  #' arguments in `...`, then `POST.ARGS`. If a name appears more than once, the
  #' first one is used.
  #'
  #' @param ... Named arguments.
  #' @param PRE.ARGS A list of arguments that take priority over everything else.
  #' @param POST.ARGS A list of arguments that are used only if the name is not
  #'   given in `PRE.ARGS` or `...`.
  #' @return A list of arguments, with no repeated names.
  #' @export
  #' @examples
  #' resolve.args(a = 1, POST.ARGS = list(a = 2, b = 3))
  #' # a = 1, b = 3
  a <- list()
  l <- c(PRE.ARGS, list(...), POST.ARGS)
  for (name in unique(names(l))) {
    a[[name]] <- l[[name]] # First occurrence will be found.
  }
  return(a)
}
