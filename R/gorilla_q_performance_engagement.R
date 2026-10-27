gorilla_q_performance_engagement <- function(data,full=FALSE,taskname="Engagement"){
  #' Score the performance engagement questionnaire
  #'
  #' Scores a questionnaire on engagement with a performance, based on Brown &
  #' Novak (2007), "Assessing the intrinsic impacts of a live performance".
  #' The questionnaire template is at <https://app.gorilla.sc/admin/task/352459/>.
  #'
  #' @details
  #' It looks for questionnaires with "PerformanceExperience" in their name. The 12
  #' items are averaged to make four scores: `pe_captive`, `pe_emotion`,
  #' `pe_social` and `pe_spirit`. The individual items are also returned, as
  #' `pe_1_absorb` etc.
  #'
  #' @param data The list of data from a gorilla import, or just the questionnaire
  #'   data (`data_q`), in long format.
  #' @param full Not currently used.
  #' @param taskname Not currently used.
  #' @return A data.table with one row per participant, the four scores and the
  #'   item scores.
  #' @export
  if (inherits(data, "list")){qdata <- data$data_q}else{qdata <- data}
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
