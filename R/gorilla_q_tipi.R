gorilla_q_tipi <- function(data, qname= c("Big 5 ten items","Big 5 Personality","TIPI")){
  #' Score the TIPI ten item personality scale
  #'
  #' Scores the Ten Item Personality Inventory (Gosling, Rentfrow & Swann, 2003)
  #' from long format gorilla questionnaire data.
  #'
  #' @details
  #' The questionnaire should have `*_score` questions (agreeableness_score,
  #' conscientiousness_score, emotional_stability_score, extroversion_score,
  #' openness_score), as in the gorilla template. People who answered the
  #' questionnaire more than once are listed and left out of the results.
  #'
  #' @param data The list of data from a gorilla import, or just the questionnaire
  #'   data (`data_q`).
  #' @param qname The questionnaire name or names (Task.Name in gorilla) to look
  #'   for. By default, "Big 5 ten items", "Big 5 Personality" and "TIPI".
  #' @return A data.table with one row per participant (`pid`) and the five scores:
  #'   `TIPI_O` (openness), `TIPI_C` (conscientiousness), `TIPI_E` (extroversion),
  #'   `TIPI_A` (agreeableness) and `TIPI_N` (emotional stability).
  #' @export
  #' @seealso [gorilla_q_keyed()]

  if (inherits(data, "list")){
    qdata <- data$data_q
  }else{qdata <- data}

  if (!"pid" %in% colnames(qdata)){qdata[,pid:=factor(Participant.Private.ID)]}

  qdata <-  qdata[Task.Name %in% qname  &
                    !Question.Key=="BEGIN QUESTIONNAIRE" & !Question.Key=="END QUESTIONNAIRE" ]

  ## check to see if duplicates
  qdata[,c := paste(pid,Question.Key)]
  qdata[,qcount := frank(UTC.Timestamp),by=.(c)]
  duplics <- unique(qdata[qcount>1]$pid)

  if(!length(duplics)==0){
    cat("These people answered more than once
        they have been left out of the results\n")
    print(duplics)
  }

  qdata <- qdata[!pid %in% duplics]

  q <- data.table(dcast(qdata,
                        pid  ~ Question.Key, value.var= "Response" ))

  if (!is.null(q$agreeableness_score)){q[,TIPI_A := as.numeric(as.character(agreeableness_score))]}
  if (!is.null(q$conscientiousness_score)){q[,TIPI_C := as.numeric(as.character(conscientiousness_score))]}
  if (!is.null(q$emotional_stability_score)){q[,TIPI_N := as.numeric(as.character(emotional_stability_score))]}
  if (!is.null(q$extroversion_score)){q[,TIPI_E := as.numeric(as.character(extroversion_score))]}
  if (!is.null(q$openness_score)){q[,TIPI_O := as.numeric(as.character(openness_score))]}

  return(q[,.(pid,TIPI_O,TIPI_C,TIPI_E,
              TIPI_A,TIPI_N)])
}
