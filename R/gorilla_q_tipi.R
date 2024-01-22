gorilla_q_tipi <- function(data, qname= c("Big 5 ten items","Big 5 Personality","TIPI")){
  #' TIPI ten item personality scale
  #'
  #' @param q_name Name of the questionnaire
  #' @param data data list or just the data_q
  #' @export

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
        only first answers taken\n")
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
