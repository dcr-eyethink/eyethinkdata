gorilla_q_parse <- function(data,qlist=NULL,preface=NULL,strip=NULL,pd=NULL){
  #' Get unscored questionnaire answers from gorilla
  #'
  #' Takes the answers from gorilla questionnaires built with Questionnaire Builder
  #' 1 (long format data), and returns them with one row per participant and one
  #' column per question. Use this for answers such as age and gender that do not
  #' need to be scored. For scales that need scoring, see [gorilla_q_keyed()].
  #'
  #' @details
  #' Gorilla gives you each answer as the text the person chose (or typed), and
  #' for many questions also as a number (the "quantised" version, with
  #' `_quantised` on the end of the question name). Use `strip` to keep one or the
  #' other. If an answer is entirely numbers, it is returned as a number.
  #' If a person answered a question more than once, the later answer is used and
  #' the people affected are listed.
  #'
  #' @param data The list of data from a gorilla import, or just the questionnaire
  #'   data (`data_q`). It has to be in long format.
  #' @param qlist The names of the questionnaires (Task.Name in gorilla) to use.
  #'   If `NULL`, all of the questionnaires are processed.
  #' @param preface Text to add to the start of all the question names. Use "task"
  #'   to add the name of the questionnaire.
  #' @param strip Which version of the answers to drop. "quant" drops the numeric
  #'   `_quantised` columns, keeping the text answers. "qual" drops the text answers,
  #'   and keeps the numeric ones (renamed to the plain question name). Default
  #'   `NULL` keeps both.
  #' @param pd Optional data.table of participant data (with a `pid` column). If
  #'   given, the questionnaire answers are merged into it with [pid_merge()].
  #' @return A data.table with one row per participant (`pid`).
  #' @export
  #' @seealso [gorilla_q_keyed()], [gorilla_q_parse_qb2()]
  #' @examples
  #' \dontrun{
  #' gorilla_q_parse(data, qlist = "basic demographics", strip = "quant")
  #' }

  if (inherits(data, "list")){
    qdata <- data$data_q
    if(data$data_qtype=="wide"){stop("Please output your questionnaire data in long format from gorilla and re-import")}
  }else{qdata <- data}


  if (is.null(qlist)){
    qlist <- unique(qdata$Task.Name)
  }

  qdata <-  qdata[Task.Name %in% qlist]
  qdata <-  qdata[!Question.Key=="END QUESTIONNAIRE"]

  if ("Key" %in% colnames(qdata)){
    qdata[,Question.Key:=paste0(Question.Key,"_",Key)]
  }

  qdata <- q_duplicate(qdata)

  q_return <- data.table(pid=unique(qdata$pid))

  if (!is.null(preface)){
    if(preface=="task"){
      qdata[,Question.Key:=paste(Task.Name,Question.Key,sep = "_")]
    }else{
      qdata[,Question.Key:=paste(preface,Question.Key,sep = "_")]
    }}

  for (tn in qlist){

    if (dim(qdata[!is.na(Response)& Task.Name==tn])[1]>0){

      q <- data.table(dcast(qdata[!is.na(Response)& Task.Name==tn],pid  ~ Question.Key,
                            value.var= "Response"))

      ## if they are all numbers, turn to numbers
      cols=colnames(q)[-1]
      q[,(cols):= lapply(.SD,function(x){Hmisc::all.is.numeric(x,what="vector")}),.SDcols=cols]

      ## get rid of quantized if redundant
      qcols <- grep(cols,pattern = "_quantised",value = T)

      for (qc in qcols){
        qcq <- gsub(qc,pattern =  "_quantised",replacement = "")
        if (class(q[[qcq]])=="numeric"){
          q[[qc]] <- NULL
        }
      }

      q_return <-  q[q_return,on="pid"]
    }
  }

  if (!is.null(strip)){
    if (!strip %in% c("qual","quant")){warning("strip should be \"quant\" or \"qual\"; treating ",strip," as \"quant\"")}
    quantcols <-  grep(x=colnames(q_return),"_quantised",value = T)
    qualcols <- gsub(quantcols,pattern ="_quantised",replacement = "" )

    if(strip=="qual"){
      q_return <- q_return[,colnames(q_return)[!colnames(q_return) %in% qualcols],with=F]
      setnames(q_return,old=quantcols,new=qualcols)

    }else{
      q_return <- q_return[,colnames(q_return)[!colnames(q_return) %in% quantcols],with=F]
    }
  }


  if (!is.null(pd)){
    q_return <- pid_merge(pd,q_return)
  }



  return(q_return)
}
