gorilla_eye_processing <-  function (data,td=NULL,
                                     conf_threshold=0.5){
  #' TASK BUILDER 2 version:  Takes all data from gorilla import,
  #' gets eye movement data and labels with zone info
  #' @param data   compiled  data from a gorilla import
  #' @param td if trail data already generated, will add eye info to this
  #' @return eye data with simplified data
  #' @export

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
