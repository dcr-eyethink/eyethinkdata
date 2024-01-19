lab_assignment_files <- function(assignmentfolder){
  #' Goes through assignment download from moodle, checks files and unzips
  #' @export

    library(fs)

  missing <- c()
  missws <- paste0(dirname(assignmentfolder),"/missing worksheet")
  try(dir.create(missws),silent = TRUE)

  datafolders <- list.files(assignmentfolder,
                            pattern="assignsubmission_file_",
                            full.names=TRUE, ignore.case=TRUE)

  #  df <- datafolders[1]
  for (df in datafolders){

    zf <- list.files(df,pattern=".zip",full.names = T)
    if (length(zf)>0){
      for (z in zf){
        unzip(z,exdir = df)
        file.remove(z)}
    }

    dirf <- list.dirs(df,full.names = T,recursive=F)
    if(length(dirf)>0){
      for (d in dirf){
        dircontents <- list.files(d,full.names = T)
        file.copy(from=dircontents,to=df)
        file.remove(dircontents)
        file.remove(d)}

    }

    worksheetfilename <-list.files(df,
                                   pattern=".xlsx", ignore.case=TRUE,full.names = TRUE)

    if(length(worksheetfilename)==0){
      missing <- c(missing,df)

      dir_copy(df,missws)
      system(paste0("rm -R '",df,"'"))

      tf <- gsub(x = df, pattern = "file_", replacement = "onlinetext_")
      if (file.exists(tf)){
        dir_copy(path = tf,new_path = missws)
        system(paste0("rm -R '",tf,"'"))}
    }
  }
  print(missing)
  ###

}
