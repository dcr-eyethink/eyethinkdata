gorilla_q_parse_qb2 <- function(data,qlist=NULL,preface=NULL,strip=NULL,pd=NULL){
  #' Get unscored questionnaire answers from gorilla (Questionnaire Builder 2)
  #'
  #' Like [gorilla_q_parse()], but for questionnaires made with Questionnaire
  #' Builder 2. It returns the answers with one row per participant and one column
  #' per question.
  #'
  #' @details
  #' In this version, if a person gave the same answer to a question more than
  #' once, the *first* is kept (in [gorilla_q_parse()] it is the last), and the
  #' duplicates are printed. "other" response columns are dropped, and `_value` is
  #' removed from the column names.
  #'
  #' @param data The list of data from a gorilla import, or just the questionnaire
  #'   data (`data_q`). It has to be in long format.
  #' @param qlist The names of the questionnaires (Task.Name in gorilla) to use.
  #'   If `NULL`, all of them.
  #' @param preface Text to add to the start of all the question names. Use "task"
  #'   to add the name of the questionnaire.
  #' @param strip "quant" drops the numeric `-quantised` columns, "qual" drops the
  #'   text answers and keeps the numeric ones. Default `NULL` keeps both.
  #' @param pd Optional data.table of participant data (with a `pid` column) to
  #'   merge the answers into.
  #' @return A data.table with one row per participant (`pid`).
  #' @export
  #' @seealso [gorilla_q_parse()], [gorilla_q_keyed_qb2()]

  if (inherits(data, "list")){
    qdata <- data$data_q
    if(data$data_qtype=="wide"){stop("Please output your questionnaire data in long format from gorilla and re-import")}
  }else{qdata <- data}


  if (is.null(qlist)){
    qlist <- unique(qdata$Task.Name)
  }

  qdata <-  qdata[Task.Name %in% qlist]
  qdata <-  qdata[!Question.Key=="END QUESTIONNAIRE" &
                    !Question.Key=="BEGIN QUESTIONNAIRE" &
                    !Question.Key=="NA_NA"]

  q_return <- data.table(pid=unique(qdata$pid))



  if (!is.null(preface)){
    if(preface=="task"){
      qdata[,Question.Key:=paste(Task.Name,Question.Key,sep = "_")]
    }else{
      qdata[,Question.Key:=paste(preface,Question.Key,sep = "_")]
    }}

  for (tn in qlist){

    if (dim(qdata[!is.na(Response)& Task.Name==tn])[1]>0){

      qdata[Response.Type=="response",rn:=1:.N,by=.(pid,Question.Key,Key,Response)]


      if (dim(qdata[rn>1])[1]>0){
        cat("\nWe had these duplicate answers. I will take the first answers\n\n")
        print(qdata[pid %in% qdata[rn>1]$pid & Response.Type=="response",
              .(pid,Question.Key,Key,Response,rn)])
      }



      q <- dcast(qdata[Response.Type=="response" & rn==1],pid~Question.Key+Key,
                 value.var = "Response")

      # take out 'other' responese
      q[,eval(colnames(q)[grep(x=colnames(q),pattern = "___other")]):=NULL]


      ## if they are all numbers, turn to numbers
      cols=colnames(q)[-1]
      q[,(cols):= lapply(.SD,function(x){Hmisc::all.is.numeric(x,what="vector")}),.SDcols=cols]


      #q_return <-  q[q_return,on="pid"]
      q_return <-  pid_merge(q_return,q)
    }
  }

  if (!is.null(strip)){
    quantcols <-  c(grep(x=colnames(q_return),"_quantised",value = T),
                    grep(x=colnames(q_return),"-quantised",value = T))
   # qualcols <- grep(x=colnames(q_return),"_value",value = T)

    qualcols <- c(setdiff(colnames(q_return),c("pid",quantcols)))

    if(strip=="qual"){
      q_return <- q_return[,colnames(q_return)[!colnames(q_return) %in% qualcols],with=F]
      setnames(q_return,old=colnames(q_return),new=gsub(x=colnames(q_return),pattern = "_quantised",replacement = ""))
      setnames(q_return,old=colnames(q_return),new=gsub(x=colnames(q_return),pattern = "-quantised",replacement = ""))
    }

    if(strip=="quant"){
      q_return <- q_return[,colnames(q_return)[!colnames(q_return) %in% quantcols],with=F]
     # setnames(q_return,old=colnames(q_return),new=gsub(x=colnames(q_return),pattern = "_value",replacement = ""))
    }
  }

  setnames(q_return,old=colnames(q_return),new=gsub(x=colnames(q_return),pattern = "_value",replacement = ""))

  q_return_full <- copy(q_return)

  # get rid of other columns
#   ocol <- grep(x=colnames(q_return_full),pattern = "_other",value = T)
#   if (!length(ocol)==0){
#   vcol <- gsub(x=ocol,pattern="_other",replacement="")
#   q_return_full[!is.na(get(ocol)),c(vcol):=get(ocol)]
#   q_return_full[,c(ocol):=NULL]
# }

  if (!is.null(pd)){
    q_return_full <- pid_merge(pd,q_return_full)
  }



  return(q_return_full)
}
