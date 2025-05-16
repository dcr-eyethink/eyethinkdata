gorilla_q_parse <- function(data,qlist=NULL,preface=NULL,strip=NULL,pd=NULL){
  #' Get subject entered, unscored data like age and gender from gorilla. Will give you the qual or quantised versions
  #'
  #' @param data raw data packet from from gorilla import
  #' @param qlist list of Task.Name, ie questionnaire blob names, that you want to process
  #' @param strip get rid of either the quant or qual
  #' @param preface add some text to all variables
  #' @export

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

      oc <-

      for (qc in qcols){
        qcq <- gsub(qc,pattern =  "_quantised",replacement = "")
        if (class(q[[qcq]])=="numeric"){
          q[[qc]] <- NULL
        }
      }

    }
    q_return <-  q[q_return,on="pid"]
  }

  if (!is.null(strip)){
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
