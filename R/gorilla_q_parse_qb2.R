gorilla_q_parse_qb2 <- function(data,qlist=NULL,preface=NULL,strip=NULL,pd=NULL){
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

      q <- dcast(qdata[Response.Type=="response"],pid~Question.Key+Key,value.var = "Response")

      ## if they are all numbers, turn to numbers
      cols=colnames(q)[-1]
      q[,(cols):= lapply(.SD,function(x){Hmisc::all.is.numeric(x,what="vector")}),.SDcols=cols]


    }
    q_return <-  q[q_return,on="pid"]
  }
#
#   if (!is.null(strip)){
#     quantcols <-  grep(x=colnames(q_return),"_quantised",value = T)
#     qualcols <- grep(x=colnames(q_return),"_value",value = T)
#
#     if(strip=="qual"){
#       q_return <- q_return[,colnames(q_return)[!colnames(q_return) %in% qualcols],with=F]
#       setnames(q_return,old=colnames(q_return),new=gsub(x=colnames(q_return),pattern = "_quantised",replacement = ""))
#
#     }elseif(strip=="qual"){
#       q_return <- q_return[,colnames(q_return)[!colnames(q_return) %in% quantcols],with=F]
#       setnames(q_return,old=colnames(q_return),new=gsub(x=colnames(q_return),pattern = "_value",replacement = ""))
#     }
#   }

  setnames(q_return,old=colnames(q_return),new=gsub(x=colnames(q_return),pattern = "_value",replacement = ""))

  # get rid of other columns
  ocol <- grep(x=colnames(q_return),pattern = "_other",value = T)
  if (!length(ocol)==0){
  vcol <- gsub(x=ocol,pattern="_other",replacement="")
  q_return[!is.na(get(ocol)),c(vcol):=get(ocol)]
  q_return[,c(ocol):=NULL]
}
  if (!is.null(pd)){
    q_return <- pid_merge(pd,q_return)
  }



  return(q_return)
}
