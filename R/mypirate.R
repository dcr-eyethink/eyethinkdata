pirateye <- function(data,colour_condition=NULL,x_condition="variable",
                     facet_condition=NULL,facet_scales="fixed",facet_row=NULL,
                     pid_average=F,
                     dv,reorder=F,pid="pid",dodgewidth=0.8,plot_condition=NULL,
                     cond=NULL,cond2=NULL,facetby=NULL,ylim=NULL,xlim=NULL,
                     w=NULL,h=6,title=NULL,outp="analysis",cols=NULL,
                     pred_line=F,error_bar_data=NULL,ypercent=F,
                     x_axis=NULL,y_axis=NULL,
                     error_dim=F,error_dim_value=0,
                     pred=NULL,pred_means=NULL,pred_bar=T,xlabs=NULL,xlabpos=0.7,
                     error_data=NULL,cflip=F,norm=F,bars=F,violin=T,dots=T,splitV=F,svw=1,
                     dot_h_jitter=0,line=F,error_bars=T,useall=F,legend=T,title_overide=F,
                     combine_plots=list(),combine_position="right",elementinc=NULL,type=NULL,
                     ...){
  #' Outputs a pirate plot (RDI)
  #'
  #' Defaults to violin plot with error bars and dots, but elements such as dots, bars, violin, error_bars can be turned on or off
  #'
  #' @param data data with one person per line, excluding rows with use=0
  #' @param colour_condition  colour split
  #' @param x_condition x axis split (if not, specified colour condition used for x axis too)
  #' @param facet_condition for faceting
  #' @param pid_average plot an average of each participant's dv over the named conditions
  #' @param plot_condition instead of passing individually, you can give a vector of up to 3
  #' @param dv name of single dv column, or multiple columns, in which case they will be split by x_condition unless colour or facet condition set to 'variable'
  #' @param pid whats the name of col that identifies individuals
  #' @param cols specify the colours to use, can be a set of colours, or of condition_level to colour
  #' @param error_data distribution for mean, eg from Bayes analysis, to replace SE
  #' @param x_axis titles
  #' @param y_axis titles
  #' @param error_bar_data
  #' @param xlab Do we have labels to go across x axis, such as post hoc pvalues or MPEs
  #' @param xlabpos How high vertically should they be, as proportion of plot height
  #' @param ypercent Convert y scale to %
  #' @param cflip flip to horizontal plot
  #' @param norm normalise / z-score values for comparison across scales
  #' @param useall ignore the use column and plot all rows
  #' @param type shortcuts: m=just error bars, b=just bars
  #' @importFrom ggplot2 ggplot aes

  #' @export mypirate pirateye
  #' @alias mypirate


  setDT(data)

  ## set types
if (!is.null(type)){
  if (type=="m"){
    violin=F
    dots=F
  }
  if (type=="b"){
    violin=F
    dots=F
    bars=T
  }
}


  if (!is.null(plot_condition)){
    colour_condition <- plot_condition[1]
    if (length(plot_condition)>1){x_condition <- plot_condition[2]}
    if (length(plot_condition)>2){facet_condition <- plot_condition[3]}
  }

  # for backcompatiblity
  if (!is.null(cond)){
    # this is an old call
    x_condition <- cond
    if (is.null(cond2)){colour_condition <- cond}else{  colour_condition <- cond2}
    facet_condition <- facetby
  }

  if  (!useall) { if ("use" %in% colnames(data)){data <- data[use==1]} }

  ## average over pids before plotting
  if (pid_average & length(dv)==1){
    data$v <- data[[dv]]
    data <- data[,.(mean(v,na.rm=T)),by=c("pid",x_condition,colour_condition,facet_condition)]
    data[[dv]] <- data$V1
  }

  ## lump together predicted data, means and labels if we have it
  ## so that it all gets reshaped together

  if(is.null(pred)){data$data_type <- "obs"}else{
    data <-  rbind(data.table(data_type="obs",data),
                   data.table(data_type="pred",pred),fill=T)
  }
  if(!is.null(pred_means)){
    data <-  rbind(data,data.table(data_type="pred_means",pred_means),fill=T)
  }
  if(!is.null(xlabs)){
    data <-  rbind(data,data.table(data_type="xlabs",xlabs),fill=T)
  }

  if(!is.null(error_bar_data)){
    data <-  rbind(data,data.table(data_type="error_bar_data",error_bar_data),fill=T)
  }


  if (is.character(outp)){dir.create(file.path(outp),showWarnings = FALSE)}



  if (length(dv)>1){
    ##  dv is a set of values, then we need to melt
    data <-   melt.data.table(data,measure.vars = dv)
    dv <- "value"

    if(!x_condition=="variable" ){
      if (is.null(facet_condition)){ colour_condition <- "variable"}else{
        if (!facet_condition=="variable" ){colour_condition <- "variable"}}
    }

    if (norm){
      data[,value_norm:=scale(value),by=variable]
      setnames(data,old=c("value","value_norm"),new = c("value_raw","value"))
    }}else{
      ## there's only one dv
      ## if x_condition was not specified, so default it to colour
      if (x_condition=="variable"){x_condition <- colour_condition}
    }


  try(setnames(data,"dv","dvcol"),silent = T)

  ##



  # data$condx <- as.factor(data[[x_condition]])
  data$condx <- data[[x_condition]]


  if (is.null(colour_condition)) {
    if (line | pred_line){
      data$condcol <- as.factor(x_condition)
    }else{
      ## there's no colour condition given, so take the x_condition
      data$condcol <- as.factor(data[[x_condition]])
    } }else{
      data$condcol <- as.factor(data[[colour_condition]])}

  if (!is.null(facet_condition)){
    data$condfacet <- as.factor(data[[facet_condition]])
  }else{
    data$condfacet <- 0
    }

  setDT(data)
  data$dv <- data[[dv]]


  if (reorder){
    data$condx <- reorder(data$condx,data$dv,mean,na.rm=T)
    data$condcol <- reorder(data$condcol,data$dv,mean,na.rm=T)
  }

  ######## start the plot!

  if (error_dim){

    data[,errordim:=ifelse(error_dim_value>mean(dv,na.rm=T)+sd(dv,na.rm=T)/sqrt(length(dv)) |
                           error_dim_value<mean(dv,na.rm=T)-sd(dv,na.rm=T)/sqrt(length(dv)),
                           FALSE,TRUE),
                           by=.(condcol,condx,condfacet)]
    p <-  ggplot2::ggplot(data =data[data_type=="obs"],
                          ggplot2::aes(y = dv, x = condx,colour=condcol,fill=condcol,
                                       alpha=errordim))+scale_alpha_discrete(range = c(.6, 0.05))

    #pargs <- as.list(match.call())
    pargs <- as.list(environment())
    pargs$error_dim <- F
    do.call(pirateye,args=pargs)



  }else{
    data[,errordim:=FALSE]

    p <-  ggplot2::ggplot(data =data[data_type=="obs"],
                          ggplot2::aes(y = dv, x = condx,colour=condcol,fill=condcol,alpha=.4))


  }







  if (!is.null(elementinc)){
    p <- p + elementinc
  }

  if (bars){
    p <- p+ ggplot2::stat_summary(fun = "mean",geom = "bar",ggplot2::aes(group=condcol),
                         width=.8,position=ggplot2::position_dodge(width=dodgewidth))
    # if (!is.null(cols)){
    #   p <- p + scale_fill_manual(values=cols) }
  }

  if (violin){
    if (splitV & length(levels(data$condcol))<3){
      p <- p+ geom_split_violin(alpha=.8,fill="white",width=svw)
    }else{
      p <- p +ggplot2::geom_violin(position = ggplot2::position_dodge(width=dodgewidth),alpha=.6,fill="white" )
    }}

  if (dots){
    p <- p +     ggplot2::geom_point(aes(alpha=errordim*0.3),
                            position=ggplot2::position_jitterdodge(dodge.width = dodgewidth,
                                                          jitter.height = dot_h_jitter))

  }

  if (line){
    p <- p +   ggplot2::stat_summary(fun = "mean",geom = "line",linewidth=2,
                                     ggplot2::aes(group=condcol,alpha=errordim*0.6),
                                     position=ggplot2::position_dodge(width=dodgewidth))
  }

  if (error_bars){

    if (is.null(error_bar_data)){


    p <- p +        ggplot2::stat_summary(fun.data = ggplot2::mean_se,  geom = "errorbar",
                                          alpha=.8,
                                          width=.4,position=ggplot2::position_dodge(width=dodgewidth))
    }else{
      ## use given data for error bars
      p <- p +        ggplot2::geom_errorbar( data=data[data_type=="error_bar_data"],
                                              ggplot2::aes(ymin=ymin,ymax=ymax),
                                              width=.4,
                                             position=ggplot2::position_dodge(width=dodgewidth))
    }
  }


  ## draw a little violin for error?
  if (!is.null(pred)){
    if (splitV & length(levels(data$condcol))<3){
      p <- p+geom_split_violin(data=data[data_type=="pred"],alpha=.4,width=svw)
    }else{
      p <- p+ggplot2::geom_violin(data=data[data_type=="pred"],alpha=.4,width=svw,
                         position=ggplot2::position_dodge(width=dodgewidth))
    }}


  ## draw  predicted means if we have them
  if (!is.null(pred_means)){
    if(pred_line){

      p <- p+ggplot2::geom_line(data=data[data_type=="pred_means"],linewidth=2)

    }else{

      if(pred_bar){
        p <- p+ggplot2::geom_crossbar(data=data[data_type=="pred_means"],
                             aes(ymin=dv,ymax=dv,y=dv),fatten=4,
                             position=ggplot2::position_dodge(width=1))}
    }
    # else calculate observed means
  }else{
    p <- p +  ggplot2::stat_summary(fun = "mean",geom = "crossbar",fun.min = "mean",fun.max = "mean",width=.6,alpha=.8,
                           position=ggplot2::position_dodge(width=dodgewidth))
  }



  ###### do we have labels (typically MPEs or pvalues)
 #  if (!is.null(xlabs) & !is.null(xlabs$dv[1]) ){

    if (!is.null(xlabs) ){
    if(pred_line){
      p <- p + ggrepel::geom_label_repel(data=data[data_type=="xlabs"],fill="white", show.legend = FALSE,
                                         ggplot2::aes(label=lab))
    }else{
      yl <- layer_scales(p)$y$get_limits()
      xlabposy <-diff(yl)*xlabpos+yl[1]
      if(x_condition ==colour_condition){
        # label need to go between the x axis categories
        p <- p + ggplot2::geom_label(data=data[data_type=="xlabs"],inherit.aes = F,fill="white",alpha=.7, label.size = NA,
                                     ggplot2::aes(x=1.5,label=lab,y=xlabposy))
       #                              ggplot2::aes_string(x=1.5,label="lab",y=xlabpos*mean(data[[dv]],na.rm = T)))
      }else{
        # they go above
        p <- p + ggplot2::geom_label(data=data[data_type=="xlabs"],inherit.aes = F,fill="white",alpha=.7, label.size = NA,
                                     ggplot2::aes(x=condx,label=lab,y=xlabposy))}
        #                             ggplot2::aes_string(x="condx",label="lab",y=xlabpos*mean(data[[dv]],na.rm = T)))}
    }}




  p <- p+ ggplot2::theme_minimal()+ggplot2::labs(y=dv,x=x_condition, colour=colour_condition,
                               fill=colour_condition)

  ## use the specified colours, or the color names in data, or default
  if (!is.null(cols)){
 #   p <- p + ggplot2::scale_colour_manual(values=cols) + ggplot2::scale_fill_manual(values=cols)
    p <- p + ggplot2::scale_colour_manual(values=cols, aesthetics = c("colour", "fill"))
  }else{
    condcols <- replacecondcols(condlevels = levels(data$condcol))
    if(!is.null(condcols)){
      p <- p + ggplot2::scale_colour_manual(values=condcols) + ggplot2::scale_fill_manual(values=condcols)}
  }

  p <- p+ggplot2::theme(legend.position = "top")

  # get rid of colour legend if its on the x-axis
  if (identical(as.character(data$condcol),as.character(data$condx))  | !legend){
    p <- p+ggplot2::theme(legend.position = "none")
  }

  if (!is.null(ylim)){
    p <- p + ggplot2::coord_cartesian(ylim = ylim)
  }

  if (ypercent){
    p <- p + scale_y_continuous(labels = scales::percent)
  }

  if (!is.null(xlim)){
    p <- p + ggplot2::coord_cartesian(xlim = xlim)
  }

  if (!is.null(x_axis)){
    p <- p + ggplot2::xlab(label = x_axis)
  }
  if (!is.null(y_axis)){
    p <- p + ggplot2::ylab(label = y_axis)
  }

  if (cflip){p <- p+ ggplot2::coord_flip()}

  if (is.null(w)){w <- 2+length(unique(data$condx))*.5}

  if(!is.null(facet_condition)){
    p <- p+ggplot2::facet_wrap(condfacet~.,scales = facet_scales,nrow = facet_row)
    w= w* max(ggplot2::ggplot_build(p)$layout$layout$COL)*.8
  }

  ## combine other plots?

  if (length(combine_plots)>0){

    if (combine_position=="under"){
      p <- cowplot::plot_grid(plotlist = append(list(mfx=p),combine_plots),ncol = 1,rel_heights = c(6,1),
                              align = "v",axis="b")
    }else{
      p <- cowplot::plot_grid(plotlist = append(list(mfx=p),combine_plots),
                              align = 'v',axis = "l")
    }}


  p <- p+ggplot2::labs(title=title)

  if (is.character(outp)){



    if(!title_overide){
      if(!is.null(colour_condition) & ! is.null(x_condition)){
      if (colour_condition==x_condition){
        title <-paste(c(title,dv,
                        paste0(c(colour_condition,facet_condition),collapse = "-")),collapse = " ")
      }else{
      title <-paste(c(title,dv,paste0(c(colour_condition,x_condition,facet_condition),collapse = "-")),collapse = " ") }
      }}

    if (error_dim){title <- paste(title," DIMMED")}

    ggplot2::ggsave(paste0(outp,"/",title,".pdf"),
           width = w, height = h, , limitsize = FALSE)
  }

  return(p)
}


GeomSplitViolin <- ggplot2::ggproto(
  "GeomSplitViolin", ggplot2::GeomViolin,
  draw_group = function(self, data, rel.scale, ..., draw_quantiles = NULL) {

    grp <- data[1, "group"]
    rel.scale <- rel.scale / max(rel.scale) # rescale to (0, 1] range
    rel.scale <- rel.scale[ifelse(grp %% 2 == 1, 1, 2)] # keep only first OR second part of relative scale

    data <- transform(data,
                      xminv = x - violinwidth * (x - xmin) * rel.scale,  # apply scale multiplier
                      xmaxv = x + violinwidth * (xmax - x) * rel.scale)

    newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv),
                             if (grp %% 2 == 1) y else -y)
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])

    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                1))
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    }
    else {
      ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, rel.scale = c(1, 1)) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm,
                      rel.scale = rel.scale, ...))
}
mypirate <- pirateye
