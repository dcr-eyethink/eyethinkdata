gorilla_eye_processing <-  function (data,td=NULL,
                                     conf_threshold=0.5){
  #' Process gorilla eye tracking data and label it with zone information
  #'
  #' For gorilla Task Builder 2 data with eye tracking. Takes the data from a
  #' gorilla import, picks out the gaze predictions and the zones that were defined
  #' on each trial, and works out for each trial how long, how much and how soon
  #' people looked at each zone.
  #'
  #' @details
  #' The function needs `data$data_continuous` (see [data_collator_gorilla()]) to
  #' contain the gorilla eye tracking columns. It adds three items to the list:
  #'
  #' * `ed` - the gaze samples (`pid`, `sid`, `tid`, time `t`, gaze position `x`
  #'   and `y` and the `duration` of each sample) with a 0/1 column for each zone
  #'   to say if the gaze was inside it.
  #' * `zd` - the coordinates of each zone on each trial.
  #' * `td` - one row per trial, with three columns for each zone: `<zone>_ftot`
  #'   (total time looking at it), `<zone>_ftot_prop` (that as a proportion of the
  #'   trial time) and `<zone>_fons` (time of the first look).
  #'
  #' @param data The list of data from a gorilla import.
  #' @param td Optional trial level data.table (with `pid`, `sid` and `tid`
  #'   columns). If given, the eye measures are merged into it.
  #' @param conf_threshold Not currently used.
  #' @return The list `data`, with `ed`, `zd` and `td` added.
  #' @export
  #' @seealso [gorilla_eye_plot()], [gorilla_eye_processing_old()] for the earlier
  #'   gorilla format

## ADD IN CALIBRATION INFO AND RETURN IN pd


  if (inherits(data, "list")){
    ed <- data$data_continuous
    if("td" %in% names(data)){
      td <- data$td
    }
  }else{ed <- data}

if ("time_elapsed" %in% colnames(ed)){
  ed[,t:=time_elapsed]}else{
if ("Elapsed" %in% colnames(ed)){
    ed[,t:=Elapsed]
  }else{
      # have no time sign
      stop("I don't have a time code")
    }}


  zd <- ed[Type=="zone"]

  ed <- copy( ed[Type=="prediction",.(pid,sid,tid=Trial.Number,t,x=Predicted.Gaze.X.Normalised,y= Predicted.Gaze.Y.Normalised)] )
  ed[,duration:=t-shift(t,type = "lag"),by=.(pid,tid)]


## figure out zone coordinates

 zd <-  zd[,.(pid,sid,tid=Trial.Number,zone=Zone.Name,
        zxmin=Zone.X.Normalised,zxmax=Zone.X.Normalised+Zone.W.Normalised,
        zymax=Zone.Y.Normalised,zymin=Zone.Y.Normalised-Zone.H.Normalised)
        ]

  zone_names <- unique(zd$zone)

   ## identify if eye is in each zone
  for (z in zone_names){
   ed <-  pid_merge(ed,zd[zone==z],link = c("pid","tid","sid"))
   ed[,zone_hit:=ifelse(x>=zxmin & x<= zxmax & y>=zymin & y<=zymax,1,0)]
   setnames(ed,"zone_hit",z)
   ed[,c( "zone","zxmin", "zxmax","zymax","zymin" ):=NULL]
  }


  teye <- ed[,.(tid=unique(tid)),.(pid,sid)]

  for (zn in zone_names){
    zn_ed <-   ed[get(zn)==1  ,
                  .(ftot=sum(duration,na.rm = T),
                    ftot_prop=sum(duration,na.rm = T)/max(t,na.rm = T),
                    fons=min(t,na.rm = T)),
                  by=.(pid,sid,tid)]
    zn_ed[is.na(ftot),ftot:=0]
    zn_ed[is.na(ftot_prop),ftot_prop:=0]
    setnames(zn_ed,old=c("ftot","ftot_prop","fons"),
             new=paste0(zn,c("_ftot","_ftot_prop","_fons")))
    teye <- zn_ed[teye,on=.(pid,sid,tid)]
  }

  if(!is.null(td)) {
    ed <- pid_merge(ed,td,link = c("pid","sid","tid"))
    td <- pid_merge(td,teye,link = c("pid","sid","tid"))
  }else{
    td <- teye
  }

  data$td <- td
  data$ed <- ed
  data$zd <- zd

  return(data)

}
