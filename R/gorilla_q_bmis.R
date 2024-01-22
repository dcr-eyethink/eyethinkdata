gorilla_q_bmis <- function(data,full=F){
  #' Processes BMIS mood survey
  #'
  #' @param data raw data from gorilla questionnaire
  #' @param data data list or just the data_q
  #' @export

  # Pleasant-Unpleasant
  # Aroused-Calm
  # Positive-Tired
  # Negative-Relaxed

  if (inherits(data, "list")){
    qdata <- data$data_q
  }else{qdata <- data}

  qdata <- q_duplicate(qdata[grep(as.character(Task.Name),pattern="BMIS")])



  lk <- data.table(Response=c("XX","X","V","VV",
                              "definitely do not feel", "do not feel", "slightly feel", "definitely feel"),
                   r=c(1:4,1:4))
  setkey(lk,Response)
  setkey(qdata,Response)
  qdata <- lk[qdata,allow.cartesian=TRUE]

  qdata[,Question.Key := gsub(pattern = " ", replacement = "_",x= Question.Key)]

  # in older version this had an averaging function. Not sure why...?
  q <- data.table(dcast(qdata[!is.na(qdata$r)], fun.aggregate = mean,
                        pid  ~ Question.Key, value.var= "r" ))

  # q <- data.table(dcast(qdata[!is.na(qdata$r)],
  #                       pid  ~ Question.Key, value.var= "r" ))

  q[, BMIS_pu := active + calm + caring + content +
      happy +  lively + loving + peppy -
      (drowsy + fedup + gloomy + grouchy + jittery + nervous + Sad + tired)]
  q[, BMIS_ac := active +  caring + fedup + gloomy +
      jittery + lively +loving + nervous + peppy +  Sad -
      (calm + tired)]
  q[, BMIS_pt := active  + caring +  lively + loving + peppy -
      (drowsy + tired)]
  q[, BMIS_nr := fedup + gloomy + jittery + nervous + Sad -
      calm]

  if (full){
    setnames(q,old = colnames(q)[2:17],new =paste0("BMIS_Q_",colnames(q)[2:17]))
    return(q)
  }else{
    return(q[,.(pid,BMIS_pu,BMIS_ac,BMIS_pt,BMIS_nr)])
  }
}
