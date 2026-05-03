builk_file_rename <- function(datafolder=NULL,p=" ",r="",suffix="",prefix="",pr=NULL){

#' Bulk search and replace in filenaames
#' Can do a single replacement if p and r given, or pr, a datatable of p and rs
#' Also can specify prefixes and suffixes to add
#' @export
#' @return Returns a list of data.tables for each type (task, questionnaire, continuous)
#' @param datafolder the folder with files to rename
#' @param p pattern to search
#' @param r replacement
#' @param pr a datatable of p and rs
#' @param prefix start of filename
#' @param suffix end of filename


if (is.null(datafolder)){
  rstudioapi::showDialog(message = "Select a folder of files to rename",title = "folder location?")
  datafolder <- rstudioapi::selectDirectory()}

setwd(datafolder)

if (is.null(pr)){
  pr <- data.table(p=p,r=r)
}

for (i in 1:nrow(sw)){
  for (f in dir(datafolder)){
    file.rename(from = f,to=paste0(suffix,
                                   gsub(x=f,pattern = sw[i]$p,replacement = sw[i]$r,fixed=T),
                                   prefix))
  }

}


}
