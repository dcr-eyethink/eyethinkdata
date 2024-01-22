gorilla_q_checkbox <- function(data,checkkey="problem",newcolnames=NULL){
  #' Scores checkbox items
  #'
  #' Takes output from a checkbox questionniare (QB1) in gorilla and returns data in a wide format with one column per ranked item
  #' @param data data list or just the data_q
  #' @param checkkey what key was used in QB1 for this item
  #' @export


  if (inherits(data, "list")){
    qd <- data$data_q
  }else{qd <- data}

  qd <- qdata


  oldcolnames <- grep(colnames(qd),pattern = checkkey,value = T)

  oldcolnames <- oldcolnames[!grepl(oldcolnames,pattern = "_text")]


  for (colnum in 1:length(oldcolnames)){
    col <- oldcolnames[colnum]

    if (is.null(newcolnames)){val <- paste0(checkkey,"_",gsub(na.omit( unique(qd[[col]]))[1],
                                                              pattern = " ",replacement = "."))  }else{
                                                                val <- paste0(checkkey,"_",newcolnames[colnum]) }

    qd[!is.na(get(col)), (col):="1"]
    qd[is.na(get(col)), (col):="0"]
    qd[[col]] <- as.numeric(qd[[col]])

    setnames(qd,old = col,new=val)

  }

  return(qd)

}
