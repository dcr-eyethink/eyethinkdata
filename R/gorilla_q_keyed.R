gorilla_q_keyed <- function(data,keyout=FALSE,qlist,
                            items=F,key_folder=NULL){
  #' Processes a gorilla QB1 questionnaire using a survey key
  #'
  #' The first time this is run, set keyout to TRUE to generate a key
  #' The key to the survey should either be in a central key_folder,
  #' or in the working directory in a folder called survey_key
  #'
  #' When you have a new key, edit it in excel or text editor,
  #' then save it as .csv file. There is one survey key for each blob in gorilla.
  #' There is a row for every different survey item in that blob.
  #' They are identified by what you wrote in the 'key' box when making the survey in gorilla.
  #' You can change the numbers in the columns sum, ScaleNam, Subscore and qual, to score/summarize your survey.
  #' You will get an output that has one row per person, and one column that scores all the answers,
  #' and additionally other columns for subscales or text answers.
  #' Here's what the columns mean:
  #' sum - how this item contributes to scoring: set to 1 to add up, 0 to ignore and -1 for reverse score
  #' rev - if it is to be reversed scored, then subtract the answer from this number. eg assuming a 7 point scale, I've set this to 8.
  #' ScaleName - you will end up with one row per person and a variable with this name (eg IQ) summarizing all items. You can have one or many different scales in the same questionnaire and key
  #' Subscore  - you can break the scales down further into subscales. Name it here and it will also appear on output as a scored column (eg IQ_verbal)
  #' qual - If this item is a non numeric or qualitative response (eg a text box) then put a 1 here. It won't be summarized but till also be reported in output in a column


  #' @param keyout generate a new key from the data
  #' @param data data list or just the data_q
  #' @param key_folder location of a central folder where you keep survey keys
  #' @export

  if (inherits(data, "list")){
    qdata <- data$data_q
    if(data$data_qtype=="wide"){stop("Please output your questionnaire data in long format from gorilla and re-import")}
  }else{qdata <- data}


  qdata <- qdata[!is.na(Task.Name)]

  if (is.null(qlist)){
    qlist <- unique(qdata$Task.Name)
  }

  # all the people for whom we have some data
  q_full_return <- data.table(pid=(unique(qdata[!is.na(pid) & Task.Name %in% qlist]$pid)))


  for (tk in unique(qlist)){
    qkey <- NULL
    qt <- qdata[Task.Name==tk]

    q_return <- data.table(pid=(unique(qt[!is.na(pid)]$pid)))

    tk <-  gsub(tk,pattern = "/",replacement = "_")

    if (is.null(key_folder)){
      tkf <- paste0("survey_key/",tk,".csv")
    }else{
      tkf <- paste0(key_folder,"/",tk,".csv")
    }


    if (file.exists(tkf)){
      qkey <- data.table(read.csv(tkf))
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
          return(kw)
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


      qt <- pid_merge(qt,qkey,link=c("Task.Name","Question.Key"))

      qual_return <- data.table()

      if ("qual" %in% colnames(qt)){
        if(dim(qt[qual==1])[1]>0){
          qual_return <- data.table(dcast(qt[qual==1],pid~scaleName+Question.Key,value.var = "Response"))
          qt <- qt[qual==0 | is.na(qual)]}
      }

      quant_return <- data.table()
      q_quant <- qt[(sum==0 | is.na(sum)) & quant=="value"]
      if(dim(q_quant)[1]>0){
        quant_return <- data.table(dcast(q_quant[!is.na(Response)],formula = pid~scaleName,
                                     value.var = "Response"))

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

      q_scalescore <- data.table(dcast(qt[!is.na(r),sum(r),by=.(pid,scaleName)],
                       formula = pid~scaleName,value.var = "V1"))

      q_return <- pid_merge(q_return,q_scalescore)

      qt[,itemid:=paste0(scaleName,"_",Question.Key)]

      ## are there subscales?
      if ("Subscore" %in% colnames(qt)){
        qt[,itemid:=paste0(scaleName,"_",Subscore,"_",Question.Key)]

        qtsub <- qt[!(is.na(Subscore) | Subscore=="")]
        if(dim(qtsub)[1]>0){
          ## do subscores
          qtsub[,scalesub:=paste0(scaleName,"_",Subscore)]

          q_return <- pid_merge(q_return,data.table(dcast(qtsub[!is.na(r),sum(r),by=.(pid,scalesub)],
                                                 formula = pid~scalesub,value.var = "V1")))
        }}

      # return quant
      if(dim(quant_return)[1]>0){
        q_return <-  pid_merge(q_return,quant_return)
      }

      ## return qual items
      if(dim(qual_return)[1]>0){
        q_return <-  pid_merge(q_return,qual_return)
      }

      ## return full items to end if you want
      if (items){
        q_return <-  pid_merge(q_return,data.table(dcast(qt,formula = pid~itemid,value.var = "r")))
      }

    }

    # end of one task
    q_full_return <- merge(q_full_return,q_return,by.x="pid",by.y="pid",all.X=T,all.Y=T)
  }

  # now gone through all the tasks
  return(q_full_return)
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

