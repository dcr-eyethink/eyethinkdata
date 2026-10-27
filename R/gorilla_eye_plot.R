gorilla_eye_plot <-  function(data,background=NULL,zones=NULL,plot_types=c("heat","path"),
                              outp="analysis",title="plot",contrast=NULL){
  #' Plot eye movements for one trial (or trial type)
  #'
  #' Takes eye tracking data that has been processed by [gorilla_eye_processing()]
  #' and makes a heat map and/or a path plot of where people looked. You can add a
  #' background image and mark the zones of interest. Gaze position is plotted in
  #' normalised screen coordinates, from 0 to 1 in both directions.
  #'
  #' @param data The list returned by [gorilla_eye_processing()]; the gaze samples
  #'   are taken from its `ed` item (columns `t`, `x` and `y`). You will usually
  #'   want to filter this to a single trial or condition first.
  #' @param background Optional file name of a .png image to draw behind the gaze
  #'   data, eg a screenshot of the stimulus.
  #' @param zones Optional data.table describing zones to draw as boxes, with
  #'   columns `zone_name`, `left`, `right`, `bottom` and `top`. A zone called
  #'   "screen" is ignored.
  #' @param plot_types Which plots to make: "heat" for a heat map, "path" for the
  #'   gaze path. Default is both.
  #' @param outp Name of a folder to save the plots in, as pdfs (it is created if
  #'   needed). Set to `NULL` or "" to not save.
  #' @param title Start of the file name for the saved plots; the plot type is
  #'   added to the end.
  #' @param contrast Optional name of a column in `data$ed` used to colour the
  #'   gaze path, eg to compare two conditions.
  #' @return A named list of ggplot objects, one for each of `plot_types`.
  #' @export
  #' @seealso [gorilla_eye_processing()]

  #  data <- eyedata$eye_data[pid==3998912 & sid==6.1 ]
  #  zones <- eyedata$zone_data[pid==3998912 & sid== 6.1]

  if (is.character(outp) && nzchar(outp)){dir.create(file.path(outp),showWarnings = FALSE)}


  if(is.null(contrast)){
    p <- ggplot2::ggplot(data=data$ed[,,.SDcols=c("t","x","y")],ggplot2::aes(x=x,y=y))
  }else{
    p <- ggplot2::ggplot(data=data$ed[,.(t,x,y,contrast=get(contrast))],ggplot2::aes(x=x,y=y,colour=contrast))
  }


  if (!is.null(background)){
    img <- png::readPNG(background)

    p <- p + ggplot2::annotation_custom(grid::rasterGrob(img, width=grid::unit(1,"npc"), height=grid::unit(1,"npc")),
                               0, 1, 0, 1)

  }

  p <- p+  xlim(0, 1) + ylim(0, 1) + ggplot2::theme_bw()

  plots <- list()

  for (pt in plot_types){

    if(pt=="heat"){
      pout <- p + ggplot2::geom_bin2d()+
        ggplot2::scale_fill_gradient2(low="blue",mid="red", high="yellow")+
        ggplot2::theme(legend.position="none")
      }
    else{

      if(is.null(contrast)){
        pout <- p +ggplot2::geom_path(alpha=0.5,size=1,colour="green")
        }else{
          pout <- p +ggplot2::geom_path(alpha=0.5,size=1)
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

    if (is.character(outp) && nzchar(outp)){
      ggplot2::ggsave(filename = paste0(outp,"/",title,"_",pt,".pdf"),plot = pout)
    }

  }

  return(plots)
}
