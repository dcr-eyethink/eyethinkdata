
#' Busselle and Bilandzic, 2009. ‘Measuring Narrative Engagement’
#'  https://doi.org/10.1080/15213260903287259
#'
#'
#'
#'
#'
q_engagement <- function(qdata,full=FALSE){
  #' engagement
  #' @param qdata raw data from gorilla questionnaire

  #' @export

  q <- q_duplicate(qdata[Task.Name=="Engagement"])

  dvs <- c("attention","character","presence","narrative","readto")
  for (dv in dvs){
    q[grep(Question.Key,pattern=dv) ,key:=dv]
  }

  ## for book immersion
  qread <- q[key=="readto",.(pid,eng_readto=Response)]

  q <- q[grep(Question.Key,pattern="quantised") ]
  q[,r:= as.numeric(as.character(Response))]


  ## get the full answer set
  if (full){

    qfull <- q
    qk <- data.table(read.csv("/Users/dcr/Dropbox/Rwork/resources/survey_key/Engagement.csv"))
    qfull <- qk[qfull, on="Question.Key"]

    qfull[,qtext:=paste("eng_Q",qtext)]

    qfull <- dcast.data.table(qfull[],
                              formula = pid~qtext,value.var = "r")
  }

  q <- dcast.data.table(q[],
                        formula = pid~key,fun.aggregate = mean,value.var = "r")

  q[,eng_attention:=24-3*attention]
  q[,eng_character:=3*character]
  q[,eng_narrative:=24-3*narrative]
  q[,eng_presence:=24-3*presence]
  q[,eng_engagement:=mean(c(eng_attention,eng_character,eng_narrative,eng_presence),na.rm=T),by=pid]
  q[,c("attention","character","narrative","presence"):=NULL]

  if (dim(qread)[1]>0) {
    q <-   qread[q,on="pid"]
  }

  if (full){
    q <-   qfull[q, on="pid"]
  }

  return(q)
}
