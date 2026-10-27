gorilla_q_checkbox <- function(data,checkkey="problem",newcolnames=NULL){
  #' Score checkbox items
  #'
  #' Takes the output from a checkbox question in a gorilla questionnaire (QB1,
  #' long format) and converts it so that there is one column for each checkbox
  #' option, scored 1 if it was ticked and 0 if it was not.
  #'
  #' @param data The list of data from a gorilla import, or just the questionnaire
  #'   data (`data_q`).
  #' @param checkkey The key that was used for this item in the gorilla
  #'   questionnaire. Columns whose names contain this text are used (but those
  #'   with "_text" in the name are ignored).
  #' @param newcolnames Optional vector of names for the new columns, one for each
  #'   checkbox option. If `NULL`, the names are made from the text of the option.
  #' @return A data.table with the checkbox columns recoded as 1 or 0, and
  #'   renamed starting with `checkkey`.
  #' @export


  if (inherits(data, "list")){
    qd <- data$data_q
  }else{qd <- data}



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
