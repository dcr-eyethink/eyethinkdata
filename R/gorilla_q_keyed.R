gorilla_q_keyed <- function(data,keyout=FALSE,items=F,key_folder="/Users/dcr/Dropbox/Rwork/resources/"){
  #' Processes a gorilla QB1 questionnaire using a survey key
  #'
  #' The first time this is run, set keyout to TRUE to generate a key
  #' The key to the survey should either be in a central key_folder,
  #' or in the working directory in a folder called survey_key

  #' @param keyout generate a new key from the data
  #' @param data data list or just the data_q
  #' @param key_folder location of a central folder where you keep survey keys
  #' @export

  if (inherits(data, "list")){
    qdata <- data$data_q
  }else{qdata <- data}

  q_return <- data.table(pid=(unique(qdata[!is.na(pid)]$pid)))

  qdata <- qdata[!is.na(Task.Name)]

  for (tk in unique(qdata$Task.Name)){
    qkey <- NULL
    qt <- qdata[Task.Name==tk]
    tk <-  gsub(tk,pattern = "/",replacement = "_")

    tkf <- paste0("survey_key/",tk,".csv")

    if (file.exists(tkf)){
      qkey <- data.table(read.csv(tkf))
    }else{
      if (file.exists(paste0(key_folder,tkf))){
        # read from res folder
        qkey <- data.table(read.csv(paste0(key_folder,tkf)))
      }else{
        #  there is no key
        cat("There is no key for:",tk,"\n")
        #  make one?
        if(keyout){
          cat("... so I put a key for",tk,"in working directory for you to edit\n")

          suppressWarnings(dir.create("survey_key"))

          sname <- gsub("[^::A-Z::]","",tk)
          if(sname==""){sname <- tk}
          kw <-  data.table(Task.Name=tk,
                            Question.Key=unique(qt[!grep("quantised",Question.Key)]$Question.Key),
                            sum=1,rev=8,scaleName=sname,Subscore="",qual=0)



          kw <- kw[!Question.Key=="BEGIN QUESTIONNAIRE" & !Question.Key=="END QUESTIONNAIRE"]
          setkey(kw,Question.Key)
          write.csv(kw,tkf)
        }
      }
    }

    ## so do we have a key now?
    if (!is.null(qkey)){
      # process away

      qt <- q_duplicate(qt)
      qt <- qt[!is.na(Question.Key)]
      ## take out those that we just want returned

      qkey[scaleName=="" | is.na(scaleName),scaleName:=Question.Key]

      qt[,quant:="value"]
      qt[grep("quantised",Question.Key),quant:="quant"]
      qt[grep("quantised",Question.Key),
         Question.Key:=gsub(Question.Key,pattern = "_quantised",replacement = "")]

      qt <- qkey[qt, on="Question.Key"]

      if ("qual" %in% colnames(qt)){
        if(dim(qt[qual==1])[1]>0){
          q_return <- data.table(dcast(qt[qual==1],pid~scaleName+Question.Key,value.var = "Response"))[q_return,on="pid"]
          qt <- qt[qual==0 | is.na(qual)]}
      }


      q_quant <- qt[(sum==0 | is.na(sum)) & quant=="value"]
      if(dim(q_quant)[1]>0){
        q_return <- data.table(dcast(q_quant[!is.na(Response)],formula = pid~scaleName,
                                     value.var = "Response"))[q_return,on="pid"]


      }

      qt <- qt[!sum==0 & quant=="quant"]
      qt[,r:=as.numeric(as.character(Response))]
      qt[sum==-1,r:=rev-r]

      qcount <- qt[!is.na(Response),.(N=.N),by=pid]
      qcount <- qcount[N<dim(qkey[!sum==0])[1]]
      if (dim(qcount)[1]>0){
        cat("These people did not answer all items for",tk,"\n")
        print(qcount)
      }



      q_return <- data.table(dcast(qt[!is.na(r),sum(r),by=.(pid,scaleName)],formula = pid~scaleName,
                                   value.var = "V1"))[q_return,on="pid"]

      qt[,itemid:=paste0(scaleName,"_",Question.Key)]

      ## are there subscales?
      if ("Subscore" %in% colnames(qt)){
        qt[,itemid:=paste0(scaleName,"_",Subscore,"_",Question.Key)]

        qtsub <- qt[!(is.na(Subscore) | Subscore=="")]
        if(dim(qtsub)[1]>0){
          ## do subscores
          qtsub[,scalesub:=paste0(scaleName,"_",Subscore)]
          q_return <- data.table(dcast(qtsub[!is.na(r),sum(r),by=.(pid,scalesub)],formula = pid~scalesub,
                                       value.var = "V1"))[q_return,on="pid"]
        }}


      ## return items too and shuffle to end
      if (items){
        q_return <-  data.table(dcast(qt,formula = pid~itemid,value.var = "r"))[q_return,on="pid"]
        itemnames <- unique(qt$itemid)
        notitemnames <- colnames(q_return)[!colnames(q_return) %in% itemnames]
        setcolorder(q_return,notitemnames)
      }



    }

  }

  # now gone through all the tasks
  return(q_return)
}

q_duplicate <- function(qdata){
  if (!"pid" %in% colnames(qdata)){qdata[,pid:=factor(Participant.Private.ID)]}

  qdata <- qdata[!Question.Key %in% c("BEGIN QUESTIONNAIRE","ConsentFlag","END QUESTIONNAIRE")]

  duplicate_pids <- unique(qdata[!is.na(pid),
                                 .N,by=.(pid,Question.Key)][N>1]$pid)
  if(length(duplicate_pids)>0){
    cat("These people have duplicated answers, and we'll take the later ones\n")
    print(droplevels(duplicate_pids))
    cat("\n")

    qdata[,n:=1:.N,by=.(pid,Question.Key)]
    qdata[,maxn:=max(n),by=.(pid,Question.Key)]
    qdata <- qdata[maxn==n]
  }
  return(qdata)
}

