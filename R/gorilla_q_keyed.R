gorilla_q_keyed <- function(data,keyout=FALSE,qlist=NULL,
                            items=F,key_folder=NULL){
  #' Score a gorilla questionnaire using a survey key
  #'
  #' Processes questionnaires made with Questionnaire Builder 1 (long format data).
  #' A survey key is a .csv file that tells this function how to score each item,
  #' which items to reverse, and which items belong to which scale or subscale. The
  #' result has one row per participant.
  #'
  #' @details
  #' **How to use it**
  #'
  #' 1. The first time, run with `keyout = TRUE`. This writes a blank key for each
  #'    questionnaire in `qlist` to a folder called `survey_key` in your working
  #'    directory.
  #' 2. Edit the key in Excel or a text editor, and save it as a .csv file. There
  #'    is one key per questionnaire in gorilla, named after the questionnaire.
  #' 3. Run the function again (with `keyout = FALSE`). It looks for the key in
  #'    `key_folder`, or if that is not given in the folder `survey_key`.
  #'
  #' Each row of the key is one item, identified by the `Question.Key` (what you
  #' typed in the "key" box when you made the questionnaire in gorilla). The
  #' columns you can edit are:
  #'
  #' * `sum` - how the item is used in the score: 1 to add it, -1 to reverse score
  #'   it, and 0 to leave it out of the scores (it is returned by itself in its own
  #'   column).
  #' * `rev` - for reverse scored items, the answer is subtracted from this number.
  #'   eg for a 7 point scale, use 8 (the default).
  #' * `adj` - a number added to the item, if the scale needs adjusting (eg it
  #'   starts at 0 instead of 1). Optional: keys without this column are
  #'   treated as 0.
  #' * `scaleName` - the name of the scale that the item adds to. The output has a
  #'   column with this name. One questionnaire can have many scales.
  #' * `Subscore` - optional name of a subscale. It is returned as an extra column
  #'   `<scaleName>_<Subscore>`.
  #' * `qual` - put 1 here if the item is a text (qualitative) answer. It is not
  #'   scored but returned in its own column.
  #' * `ignore` - an answer that should be ignored when scoring, eg 8 for "not
  #'   applicable" on a scale of 1 to 7.
  #'
  #' Scales are the *sum* of the items (not the mean). People who did not answer all
  #' the items are listed.
  #'
  #' Questionnaires without a key are skipped, unless you make one with
  #' `keyout = TRUE`. If you only want to look at the data without scoring it, see
  #' [gorilla_q_parse()].
  #'
  #' @param data The list of data from a gorilla import, or just the questionnaire
  #'   data (`data_q`). It has to be in long format.
  #' @param keyout Set to `TRUE` to write out a new blank key for each
  #'   questionnaire, from the data. This replaces any existing key in
  #'   `survey_key`.
  #' @param qlist The names of the questionnaires (Task.Name in gorilla) to score.
  #'   If `NULL`, all of the questionnaires in the data.
  #' @param items If `TRUE`, also return the individual item scores (after
  #'   reversing etc.), one column for each item.
  #' @param key_folder Folder where the survey keys are kept. If `NULL`, the folder
  #'   `survey_key` in the working directory is used.
  #' @return A data.table with one row per participant (`pid`) and a column for each
  #'   scale (and subscale), and for any items that were left out of scoring. If
  #'   nothing could be scored, there is no return value.
  #' @export
  #' @seealso [gorilla_q_keyed_qb2()], [gorilla_q_parse()]
  #' @examples
  #' \dontrun{
  #' # make a blank key
  #' gorilla_q_keyed(data, qlist = "Interpersonal Reactivity Index (Davies, 1983)", keyout = TRUE)
  #' # ... edit the key in survey_key, then score:
  #' gorilla_q_keyed(data, qlist = "Interpersonal Reactivity Index (Davies, 1983)")
  #' }


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


  for (tk in qlist){
    qkey <- NULL
    qt <- qdata[Task.Name==tk]

    q_return <- data.table(pid=(unique(qt[!is.na(pid)]$pid)))

    tk <-  gsub(tk,pattern = "/",replacement = "_")

    if (is.null(key_folder)){
      tkf <- paste0("survey_key/",tk,".csv")
    }else{
      tkf <- paste0(key_folder,"/",tk,".csv")
    }


    if (file.exists(tkf) & !keyout){
      qkey <- data.table(read.csv(tkf))
      if (!"adj" %in% colnames(qkey)){qkey[,adj:=0]}   # older keys have no adj column
    }else{
        #  there is no key
        cat("There is no key for:",tk,"\n")
        #  make one?
        if(keyout){
          cat("... so I put a key for",tk,"in working directory for you to edit\n")

          suppressWarnings(dir.create("survey_key"))

          sname <- gsub("[^::A-Z::]","",tk)
          if(sname==""){sname <- tk}

          qt <- qt[!Question.Key=="BEGIN QUESTIONNAIRE" & !Question.Key=="END QUESTIONNAIRE" &
                     ! Question.Key  %like% "quantised" & !Question.Key=="Consent" ]

          if(dim(qt)[1]>0){
          qt[,order:=1:.N,by=.(pid)]

          qkitems <- qt[,.(Question.Key=unique(Question.Key)),by=order]

          # do we only have one order or lots
          if (!length(unique(qkitems$Question.Key) )==dim(qkitems)[1]){
          # we have lots so sort alphabetically
          #  qkitems <- data.table(order=NA,Question.Key=sort(unique(qkitems$Question.Key)))
            # we have lotss
            qkitems <- data.table(order=NA,
                                  Question.Key=qkitems$Question.Key[!duplicated(qkitems$Question.Key)])
            }

          kw <-  data.table(Task.Name=tk,qkitems,
                            sum=1,rev=8,adj=0,scaleName=sname,Subscore="",qual=0,ignore=NA)


          write.csv(kw,tkf)
          print(kw)
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


      qt <- pid_merge(qt,qkey,link=c("Task.Name","Question.Key"))

      qual_return <- data.table()

      ## all those flagged as 1 or 2 in qual taken out
      if ("qual" %in% colnames(qt)){
        if(dim((qt[(qual==1 | qual==2) & quant=="value"])  )[1]>0){
          qual_return <- dcast(qt[(qual==1 | qual==2) & quant=="value"],
                               pid~scaleName+Question.Key,value.var = "Response")
          # NOT IMPLEMENTED - if qual is 2 give both qual and quant value
          # qual_return <- dcast(qt[(qual==1 | qual==2) ],
          #                      pid~scaleName+Question.Key+quant,value.var = "Response")
          qt <- qt[!(qual==1 | qual==2)] }
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
      qt[!adj==0,r:=r+adj]

      ## if got ignore col, then delete responses that = this
      if ("ignore" %in% colnames(qt)){
        qt[ignore==r,r:=NA]
        }

      qcount <- qt[!is.na(Response),.(N=.N),by=pid]
      qcount <- qcount[N<dim(qkey[!sum==0])[1]]
      if (dim(qcount)[1]>0){
        cat("These people did not answer all items for",tk,"\n")
        print(qcount)
      }

      if(dim(qt)[1]>0){
        # q_scalescore <- data.table(dcast(qt[!is.na(r),sum(r),by=.(pid,scaleName)],
        #                formula = pid~scaleName,value.var = "V1"))
        q_scalescore <- dcast(qt[!is.na(r)],fun.aggregate = sum,
                                         formula = pid~scaleName,value.var = "r" )

      }else{
        q_scalescore <- data.table()
                       }

      q_return <- pid_merge(q_return,q_scalescore)

      qt[,itemid:=paste0(scaleName,"_",Question.Key)]

      ## are there subscales?
      if ("Subscore" %in% colnames(qt)){
        qt[,itemid:=paste0(scaleName,"_",Subscore,"_",Question.Key)]

        qtsub <- qt[!(is.na(Subscore) | Subscore=="")]
        if(dim(qtsub)[1]>0){
          ## do subscores
          qtsub[,scalesub:=paste0(scaleName,"_",Subscore)]

          # q_return <- pid_merge(q_return,data.table(dcast(qtsub[!is.na(r),sum(r),by=.(pid,scalesub)],
          #                                       formula = pid~scalesub,value.var = "V1")))

          q_return <- pid_merge(q_return,dcast(qtsub[!is.na(r)],
          formula = pid~scalesub,value.var = "r",fun.aggregate = sum))

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
   # q_full_return <- merge(q_full_return,q_return,by.x="pid",by.y="pid",all.X=T,all.Y=T)

    q_full_return <- pid_merge(q_full_return,q_return)

  }

  # now gone through all the tasks
  if (dim(q_full_return)[2]>1){
    return(q_full_return)
  }

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

