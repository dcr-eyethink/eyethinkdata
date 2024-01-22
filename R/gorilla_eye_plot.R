gorilla_eye_plot <-  function(data,background=NULL,zones=NULL,plot_types=c("heat","path"),
                              outp="analysis",title="plot",linecolour=NULL){
  #' Takes one trial (or trial type) of data and optional image file an makes heat map
  #'
  #' Generates plots from imported gorilla data that has been processed by gorilla_eye_processing
  #' @param data  gorilla ET data
  #' @param background for background
  #' @return image
  #' @export

  #  data <- eyedata$eye_data[pid==3998912 & sid==6.1 ]
  #  zones <- eyedata$zone_data[pid==3998912 & sid== 6.1]

  if (is.character(outp)){dir.create(file.path(outp),showWarnings = FALSE)}


  if(is.null(linecolour)){
    p <- ggplot2::ggplot(data=data,ggplot2::aes(x=x,y=y))
  }else{
    p <- ggplot2::ggplot(data=data,ggplot2::aes_string(x="x",y="y",colour=linecolour))
  }


  if (!is.null(background)){
    img <- png::readPNG(background)

    p <- p + ggplot2::annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")),
                               0, 1, 0, 1)

  }

  p <- p+  xlim(0, 1) + ylim(0, 1) + ggplot2::theme_bw()

  plots <- list()

  for (pt in plot_types){

    if(pt=="heat"){
      pout <- p + ggplot2::geom_bin2d()+
        ggplot2::scale_fill_gradient2(low="blue",mid="red", high="yellow")+
        ggplot2::theme(legend.position="none")}
    else{

      if(is.null(linecolour)){
        pout <- p +ggplot2::geom_path(alpha=0.5,size=1,colour="green")}else{
          p <- p +ggplot2::geom_path(alpha=0.5,size=1)
        }
    }


    ## add in roi
    if(!is.null(zones)){

      # ignore any ROIs called screen
      pout <- pout + ggplot2::geom_rect(data=zones[!zone_name=="screen"],size = 0.5,fill=alpha("grey",0),
                                        ggplot2::aes(xmin=left,xmax=right,ymin=bottom,ymax=top,colour=zone_name),
                               inherit.aes = FALSE)+
        ggplot2::geom_text(data=zones[!zone_name=="screen"],nudge_y = 0.02,
                           ggplot2::aes(x=(left+right)/2,y=top,label=zone_name,colour=zone_name),inherit.aes = FALSE )
    }

    plots[[pt]] <- pout

    if (!is.null(outp)){
      ggplot2::ggsave(filename = paste0(outp,"/",title,"_",pt,".pdf"))
    }

  }

  return(plots)
}
