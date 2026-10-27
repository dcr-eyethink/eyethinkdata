pid_merge <- function(...,link="pid"){
  #' Merge data tables together on participant id
  #'
  #' Takes two or more data tables and joins them up, matching rows by `pid`
  #' (participant id) or whichever other column(s) you name in `link`. For
  #' example, you can add trial data to participant data, or add questionnaire
  #' scores to a table of reaction times.
  #'
  #' @details
  #' All of the rows of the *first* table are kept. Rows in later tables that
  #' match no row in the first are dropped, and rows of the first table with no
  #' match get `NA`. Where there is more than one row in a later table for a
  #' participant, the first table's rows are repeated for each of them.
  #'
  #' If a later table has columns with the same name as those that already exist
  #' (other than the link columns), the later values replace the earlier ones, and
  #' the repeated names are printed.
  #'
  #' The link columns are converted to factors in the result (so that, for example,
  #' numeric and text versions of an id still match).
  #'
  #' **Note** that data frames you pass in are converted to data.tables in place
  #' (by reference), as with `data.table::setDT`.
  #'
  #' @param ... Two or more data.tables (or data frames) to join.
  #' @param link The name or names of the column(s) to match on. Default "pid".
  #' @return A data.table, with the columns of the first table followed by the new
  #'   columns of each later one.
  #' @export
  #' @seealso [data_collator_gorilla()]
  #' @examples
  #' a <- data.table::data.table(pid = 1:3, age = c(20, 25, 30))
  #' b <- data.table::data.table(pid = c(1, 2, 4), meanRT = c(500, 450, 600))
  #' pid_merge(a, b)
  data <- list(...)


  for (n in 1:length(data)){
    setDT(data[[n]])
    for (l in link){
      data[[n]][[l]] <- as.factor(data[[n]][[l]])
    }
  }


  md <- data[[1]]
  colorder <- colnames(md)
  for (n in 2:length(data)){

    repeatedcols <- setdiff(intersect(colnames(md),colnames(data[[n]])),link)
    md <- data[[n]][md,on=link]

    if (length(repeatedcols)>0){
      cat("These cols were repeated in datatable" , n , "and used to replace earlier ones\n")
      print(repeatedcols)
      md[,colnames(md)[ substr(colnames(md),start = 1,stop = 2)=="i."]:=NULL]
    }

    colorder <- c(setdiff(colorder,repeatedcols),setdiff(colnames(data[[n]]),link))
  }

  setcolorder(x=md,neworder=c(colorder))

  for (l in link){
    md[[l]] <- as.factor(md[[l]])
  }

  return(md)
}

exp_pid_merge <- function(...,link="pid"){
  #' Old name for [pid_merge()]
  #'
  #' Please use [pid_merge()] instead.
  #'
  #' @param ... Data tables to merge.
  #' @param link The column(s) to match on.
  #' @return See [pid_merge()].
  #' @export
  #' @keywords internal
  return( pid_merge(...,link=link))
}

mergey <- function(...,link="pid"){
  #' Old name for [pid_merge()]
  #'
  #' Please use [pid_merge()] instead.
  #'
  #' @param ... Data tables to merge.
  #' @param link The column(s) to match on.
  #' @return See [pid_merge()].
  #' @export
  #' @keywords internal
  return( pid_merge(...,link=link))
}

