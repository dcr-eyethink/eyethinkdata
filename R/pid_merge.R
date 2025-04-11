pid_merge <- function(...,link="pid"){
  #' takes all of the datatables passed to it, and merges on pid or whetever specified in link, getting rid of duplicates
  #' @param ... all the data tables
  #' @param link cols to link on
  #' @export
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
  #' please use pid_merge
  #' @export
  return( pid_merge(...,link))
}

mergey <- function(...,link="pid"){
  #' please use pid_merge
  #' @export
  return( pid_merge(...,link))
}

