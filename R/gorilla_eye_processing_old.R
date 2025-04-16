gorilla_eye_processing <-  function (data,td=NULL,link=c("pid","sid"),
                                     zone="gorilla",conf_threshold=0.5){
  #' Takes all data from gorilla import,
  #' filters for named displays and returns simplified data,
  #' @param data   compiled  data from a gorilla import
  #' @param td if trail data already generated, will add eye info to this
  #' @return eye data with simplified data
  #' @export


  if (inherits(data, "list")){
    ed <- data$data_continuous
  }else{ed <- data}

if ("time_elapsed" %in% colnames(ed)){
  ed[,t:=time_elapsed]}else{
if ("Elapsed" %in% colnames(ed)){
    ed[,t:=Elapsed]}else{
      # have no time sign
      stop("I don't have a time code")
    }}



  for (fn in unique(ed$filename)){
    d <- ed[filename==fn]
    zones <-  d[type=="zone" & !zone_name %in% c("screen"),
                .(zone_name,zone_x_normalised,zone_y_normalised,
                  zone_width_normalised,zone_height_normalised)]

    for (zn in zones$zone_name){
      ed[filename==fn,paste0(zn,"_fix"):= 0]
      ed[filename==fn & x_pred_normalised <= (zones[zone_name==zn]$zone_x_normalised+zones[zone_name==zn]$zone_width_normalised) &
           x_pred_normalised >= (zones[zone_name==zn]$zone_x_normalised) &
           y_pred_normalised <= (zones[zone_name==zn]$zone_y_normalised+zones[zone_name==zn]$zone_height_normalised) &
           y_pred_normalised >= (zones[zone_name==zn]$zone_y_normalised),
         paste0(zn,"_fix"):= 1]
    }
  }

  zones <- ed[type=="zone",.(pid,sid,filename,zone_name,
                             left=zone_x_normalised,right=zone_x_normalised+zone_width_normalised,
                             bottom=zone_y_normalised,top=zone_y_normalised+zone_height_normalised )]

  zone_names <-  grep(colnames(ed),pattern = "_fix",value = T)

  datacols <- c("pid","sid","filename","t",
                "x_pred_normalised","y_pred_normalised",
                "convergence","face_conf",
                zone_names)

  ed <-  ed[type=="prediction",..datacols]


  setnames(ed,old=c("x_pred_normalised","y_pred_normalised"),new=c("x","y"))

  ed[,duration:=t-shift(t,type = "lag"),by=filename]

  ## summarize each pid x trial
  ## with total time, average confidence
  ## for each zone
  ##    total time
  ##    percent time
  ##    onset time

  ted <- ed[,.(converge_mean=mean(convergence,na.rm=T),
               faceconf_mean=mean(face_conf,na.rm=T)),.(pid,sid,filename)]

  for (zn in zone_names){
    zn_ed <-   ed[get(zn)==1 & face_conf>=conf_threshold ,
                  .(ftot=sum(duration,na.rm = T),
                    ftot_prop=sum(duration,na.rm = T)/max(t,na.rm = T),
                    fons=min(t,na.rm = T)),
                  by=.(pid,sid,filename)]
    zn_ed[is.na(ftot),ftot:=0]
    zn_ed[is.na(ftot_prop),ftot_prop:=0]
    setnames(zn_ed,old=c("ftot","ftot_prop","fons"),
             new=paste0(zn,c("_ftot","_ftot_prop","_fons")))
    ted <- zn_ed[ted,on=.(pid,sid,filename)]
  }

  if(!is.null(td)) {
    td <- ted[td,on=link]
    ed <-  td[ed,on=link]
  }else{
    td <- ted
  }

  return(list(trial_data=td,eye_data=ed,zone_data= zones))

}
