lab_assignment_files <- function(assignmentfolder){
  #' Tidy up the files from a moodle assignment download
  #'
  #' Goes through a folder of student submissions downloaded from moodle. It unzips
  #' submissions, brings the files up out of any sub folders, and checks that each
  #' submission has an Excel (.xlsx) worksheet. Submissions without one are moved
  #' to a folder called "missing worksheet", next to `assignmentfolder`.
  #'
  #' @details
  #' **Take care:** this changes and deletes files. Zip files are deleted after they
  #' are unzipped, and submissions without a worksheet are moved with the system
  #' `rm -R` command, so it only works on Mac and Linux. Run it on a copy of the
  #' download. The fs package is needed.
  #'
  #' @param assignmentfolder The folder that was downloaded from moodle, containing
  #'   folders called `..._assignsubmission_file_`.
  #' @return Nothing is returned, but the folders of the submissions with a missing
  #'   worksheet are printed.
  #' @export

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

      fs::dir_copy(df,missws)
      unlink(df,recursive = TRUE)

      tf <- gsub(x = df, pattern = "file_", replacement = "onlinetext_")
      if (file.exists(tf)){
        fs::dir_copy(path = tf,new_path = missws)
        unlink(tf,recursive = TRUE)}
    }
  }
  print(missing)
  ###

}
