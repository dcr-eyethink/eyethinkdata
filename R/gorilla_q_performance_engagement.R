gorilla_q_performance_engagement <- function(data,full=FALSE,taskname="Engagement"){
  #' Processes narrative engagement questionnaire
  #'
  #' From https://app.gorilla.sc/admin/task/352459/
  #'  Brown & Novak (2007)
  #'  https://wolfbrown.com/wp-content/uploads/Report-Assessing-The-Intrinsic-Impacts-of-a-Live-Performance.pdf
  #' @param data data list or just the data_q
  #' @export
  q <- q_duplicate(qdata[grep(Task.Name,pattern = "PerformanceExperience")])

  key <- data.table(Question.Key=c("absorb","bond","connectedness","crossculture","emotion","empowered","heritage","inhabit","insight","spiritual","therapy","transcendent"),
                    dv=paste0("pe_",c("captive","emotion","social","social","emotion","spirit","social","captive","social","spirit","emotion","spirit")),
                    qid=paste0("pe_",c(1:12),"_",
                               c("absorb","bond","connectedness","crossculture","emotion","empowered","heritage","inhabit","insight","spiritual","therapy","transcendent")))


  q <- key[q,on="Question.Key"]

  q <- q[!is.na(qid)]
  q <- q[,r := as.numeric(as.character(Response))]


  pe <- dcast.data.table(data=q,formula = pid~dv,value.var = "r",
                         fun.aggregate = mean)[dcast.data.table(data=q,formula = pid~qid,value.var = "r")]

  return(pe)
}
