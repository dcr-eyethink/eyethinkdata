pirateye <- function(data,colour_condition=NULL,x_condition="variable",
                     facet_condition=NULL,facet_scales="fixed",facet_row=NULL,
                     pid_average=F,norm_pid="no",
                     dv,reorder="no",pid="pid",dodgewidth=0.8,plot_condition=NULL,
                     cond=NULL,cond2=NULL,facetby=NULL,ylim=NULL,xlim=NULL,
                     w=NULL,h=6,title=NULL,outp="analysis",cols=NULL,
                     pred_line=F,error_bar_data=NULL,ypercent=F,
                     x_axis=NULL,y_axis=NULL,
                     error_dim=F,error_dim_value=0,
                     pred=NULL,pred_means=NULL,pred_bar=T,
                     xlabs=NULL,xlabpos=0.7,col_pc_change=F,
                     error_data=NULL,cflip=F,
                     norm=F,norm_iq=F,norm_by="none",norm_scale="z",
                     bars=F,violin=T,dots=T,splitV=F,svw=1,
                     dot_h_jitter=0,line=F,error_bars=T,useall=F,legend=T,title_overide=F,
                     combine_plots=list(),combine_position="right",elementinc=NULL,type=NULL,
                     saveoutdata=F,display_values=F,
                     ...){
  #' Plot data as a pirate plot (RDI: raw data, descriptives, inferences)
  #'
  #' Makes a plot that shows the raw data points, a violin (the distribution), the
  #' mean, and error bars for the mean, for the conditions of an experiment. By
  #' default it draws all of those things, but you can turn on and off each element
  #' (`dots`, `violin`, `bars`, `line`, `error_bars`) and change the layout.
  #' Plots are saved as a pdf in `outp`, and returned as a ggplot that you can
  #' customise further. `mypirate` is an older name for the same function.
  #'
  #' @details
  #' **Specifying the conditions**
  #'
  #' * `x_condition` is the column shown along the x axis, `colour_condition` splits
  #'   by colour, and `facet_condition` makes a separate panel for each level.
  #'   `plot_condition` is a shortcut where you give a vector of up to three
  #'   names, in the order colour, x, facet.
  #' * If you give only `colour_condition` it is also used for the x axis.
  #' * `dv` is the column with the data to plot. If you give several columns, they
  #'   are stacked into a column called `variable`, which you can use as a
  #'   condition.
  #' * Data should be in long format, with a row for each observation (for example
  #'   each trial), and a column identifying participants (`pid`).
  #'   Set `pid_average = TRUE` to average each participant's data within
  #'   conditions before plotting.
  #' * If the data has a column called `use`, only rows where `use` is 1 are
  #'   plotted, unless you set `useall = TRUE`.
  #'
  #' **Normalising**: `norm_by = "dv"` converts values to z scores for each of the
  #' dvs, so that you can compare them on one axis. `norm_by = "pid"` does that for
  #' each participant. Use `norm_scale = "iq"` for a scale with mean 100 and
  #' standard deviation 15 instead.
  #'
  #' **Saving**: the file name is made from the title, the dv and the names of the
  #' conditions. Set `outp = NULL` to avoid saving a file.
  #'
  #' **Old arguments** `cond`, `cond2`, `facetby`, `norm`, `norm_iq`, `norm_pid` are
  #' kept so that old code still works. Use `x_condition`, `colour_condition`,
  #' `facet_condition`, `norm_by` and `norm_scale` in new code.
  #'
  #' @param data A data frame or data.table (converted to a data.table in place).
  #' @param colour_condition Name of the column to split by colour.
  #' @param x_condition Name of the column for the x axis. If not given, the colour
  #'   condition is used.
  #' @param facet_condition Name of the column that makes separate panels.
  #' @param facet_scales Scales for the panels: "fixed" (the same), "free" or
  #'   "free_y" etc. (see `ggplot2::facet_wrap`).
  #' @param facet_row Number of rows of panels.
  #' @param pid_average Average each participant's dv over the conditions, before
  #'   plotting?
  #' @param dv Name of the column to plot, or a vector of names of columns.
  #' @param pid Name of the column that identifies participants. Default "pid".
  #' @param plot_condition A vector of up to three condition names, used instead of
  #'   `colour_condition`, `x_condition` and `facet_condition` (in that order).
  #' @param reorder "increasing" or "decreasing" to put the conditions in order of
  #'   their means. Default "no".
  #' @param dodgewidth How far apart to put the colour conditions within each x
  #'   position.
  #' @param ylim Limits for the y axis, as a vector of two numbers.
  #' @param xlim Limits for the x axis.
  #' @param w Width of the saved plot in inches. By default this is chosen from the
  #'   number of conditions.
  #' @param h Height of the saved plot in inches.
  #' @param title Title of the plot.
  #' @param title_overide If `TRUE`, the file name is made from the title alone
  #'   rather than also adding the dv and conditions.
  #' @param outp Name of the folder for saving the plot as a pdf (it is created if
  #'   needed). Set to `NULL` or "" to not save.
  #' @param saveoutdata Also save the data used for the plot, as a .csv file.
  #' @param cols The colours to use: a vector of colours, or a named vector
  #'   matching condition levels to colours. If the condition levels are themselves
  #'   colour names, those are used.
  #' @param bars Show means as bars?
  #' @param violin Show violins of the distributions?
  #' @param dots Show a point for each data point?
  #' @param line Join the means with lines?
  #' @param error_bars Show error bars (standard errors)?
  #' @param splitV Draw a split violin (half for each colour) when there are two
  #'   colour conditions.
  #' @param svw Width of the split violins.
  #' @param dot_h_jitter Amount of vertical jitter for the dots.
  #' @param type Shortcut: "m" shows only the means and error bars, "b" shows just
  #'   bars and error bars.
  #' @param display_values Write the mean values on the plot?
  #' @param ypercent Show the y axis as a percentage?
  #' @param cflip Flip the plot so that the x axis is vertical?
  #' @param legend Show the legend? It is hidden when colour and x axis show the
  #'   same condition.
  #' @param x_axis Label for the x axis (default is the condition name).
  #' @param y_axis Label for the y axis (default is the dv name).
  #' @param col_pc_change Calculate the percentage change from the first to the
  #'   second colour condition and show it along the x axis.
  #' @param xlabs A table of labels to show above the x axis, such as post hoc
  #'   p-values. It needs a column called `lab`, and the same x condition column
  #'   as the plot.
  #' @param xlabpos How high up the labels in `xlabs` go, as a proportion of the
  #'   height of the plot.
  #' @param error_bar_data A table of ymin and ymax values to use for the error
  #'   bars, instead of calculating standard errors from the data, with the same
  #'   condition columns as the plot. Used by [plot_model()].
  #' @param error_dim Fade out the data for the conditions whose mean (with SE)
  #'   excludes `error_dim_value`.
  #' @param error_dim_value The value used by `error_dim`.
  #' @param pred Optional table of predicted data (eg samples from a model), shown
  #'   as violins.
  #' @param pred_means Optional table of predicted means, shown as bars or lines.
  #' @param pred_line Draw `pred_means` as a line rather than as bars?
  #' @param pred_bar Draw `pred_means` as bars? Default `TRUE`.
  #' @param combine_plots A list of other plots to put together with this one
  #'   (using cowplot).
  #' @param combine_position Where to put the combined plots: "right" or "under".
  #' @param elementinc An extra ggplot element (eg a theme) added to the plot early
  #'   on.
  #' @param cond,cond2,facetby Old names for `x_condition`, `colour_condition` and
  #'   `facet_condition`.
  #' @param norm_by Normalise by "pid" or by "dv", or "none" (the default).
  #' @param norm_scale "z" (the default) for z scores, or "iq" for a scale with a
  #'   mean of 100 and an SD of 15.
  #' @param norm,norm_pid,norm_iq Old arguments for normalising. Use `norm_by` and
  #'   `norm_scale` instead.
  #' @param useall Plot all rows, ignoring any `use` column.
  #' @param error_data Not currently used.
  #' @param ... Not currently used.
  #' @return A ggplot object (or a combined cowplot figure if `combine_plots` is
  #'   used).
  #' @importFrom ggplot2 ggplot aes
  #' @export mypirate pirateye
  #' @aliases mypirate
  #' @seealso [plot_model()]
  #' @examples
  #' \dontrun{
  #' pirateye(data = iat_data, colour_condition = "congruency",
  #'          x_condition = "firstblock", dv = "rt", outp = NULL)
  #' # average over participants, with bars rather than violins
  #' pirateye(iat_data, colour_condition = "congruency", x_condition = "firstblock",
  #'          dv = "rt", pid_average = TRUE, violin = FALSE, bars = TRUE, outp = NULL)
  #' }



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

  ##### for backcompatiblity
  if (!is.null(cond)){
    # this is an old call
    x_condition <- cond
    if (is.null(cond2)){colour_condition <- cond}else{  colour_condition <- cond2}
    facet_condition <- facetby}
  if (norm){norm_by <- "dv"}
  if (norm_iq){norm_scale <- "iq"}
  if (norm_pid=="z"){
    norm_by <- "pid"
    norm_scale <- "z"}
  if (norm_pid=="ip"){
    norm_by <- "pid"
    norm_scale <- "iq"}
  ##### for backcompatiblity end

  if  (!useall) { if ("use" %in% colnames(data)){data <- data[use==1]} }

  ## average over pids before plotting
  if (pid_average & length(dv)==1){
    data$v <- data[[dv]]
    data <- data[,.(mean(v,na.rm=T)),by=c(pid,x_condition,colour_condition,facet_condition)]
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

  if(!is.null(error_bar_data)){
    data <-  rbind(data,data.table(data_type="error_bar_data",error_bar_data),fill=T)
  }


  if (is.character(outp) && nzchar(outp)){dir.create(file.path(outp),showWarnings = FALSE)}



  if (length(dv)>1){
    ##  dv is a set of values, then we need to melt
    data <-   melt.data.table(data,measure.vars = dv)
    if (is.null(title)){title <- paste0(substr(dv,1,3),collapse = "_") }

    dv <- "value"

    if(!x_condition=="variable" ){
      if (is.null(facet_condition)){ colour_condition <- "variable"}else{
        if (!facet_condition=="variable" ){colour_condition <- "variable"}}

    }


    }else{
      ## there's only one dv
      ## if x_condition was not specified, so default it to colour
      if (x_condition=="variable"){x_condition <- colour_condition}
    }


  ################ scaling by dv

    if (norm_by=="dv"){
    data[,value_norm:=scale(value),by=variable]
    if (norm_scale=="iq"){data[,value_norm:=value_norm*15+100]
    }
    setnames(data,old=c("value","value_norm"),new = c("value_raw","value"))
  }

  try(setnames(data,"dv","dvcol"),silent = T)


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
  data$pid <- data[[pid]]

################ scaling by pid

  if (norm_by=="pid"){
    data[,dv_scaled:=scale(dv),by=pid]
    if (norm_scale=="iq"){data[,dv_scaled:=dv_scaled*15+100]
    }
    setnames(data,old = c("dv","dv_scaled"),new=c("dv_raw","dv"))
  }

  ############### reordering

  if (reorder=="increasing"){
    data$condx <- reorder(data$condx,data$dv,mean,na.rm=T)
    data$condcol <- reorder(data$condcol,data$dv,mean,na.rm=T)
  }
  if (reorder=="decreasing"){
    data$condx <- reorder(data$condx,-data$dv,mean,na.rm=T)
    data$condcol <- reorder(data$condcol,-data$dv,mean,na.rm=T)
  }

  ###############  % change calc, or use given xlabs

  if(col_pc_change){
   ccomp <-  sort(unique(data$condcol)[1:2])
   xlabs <- data[,.(lab=sprintf("%.1f%%", 100*(mean(.SD[condcol==ccomp[2]]$dv,na.rm=T)-
              mean(.SD[condcol==ccomp[1]]$dv,na.rm=T))/mean(.SD[condcol==ccomp[1]]$dv,na.rm=T))),by=condx]
  }
  if(!is.null(xlabs)){
    ## a character dv column in xlabs would turn the numeric dv column into text
    xlabs <- data.table::as.data.table(xlabs)
    if ("dv" %in% names(xlabs)){xlabs[,dv:=NULL]}
    data <-  rbind(data,data.table(data_type="xlabs",xlabs),fill=T)
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



  if (norm_pid=="iq" | norm_iq==T | norm_scale=="iq"){
    p <- p+geom_hline(yintercept = 100, size=2, alpha=.2)
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


  ### put the means onto plot as well
  if (display_values){
    p <- p +  ggplot2::stat_summary(  fun = mean,
                                      geom = "label",
                                      aes(label = after_stat(round(y, 2))),
                                      vjust = -0.5,
                                      fill = scales::alpha("white", 0.6),
                                      label.size = NA,
                                      position=ggplot2::position_dodge(width=dodgewidth),
                                      show.legend = FALSE)
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


  ## calc lims?
  if ( is.null(ylim) & (norm_pid=="iq" | norm_iq==T | norm_scale=="iq")) {
    # which layer is the error bars?
    if (error_bars){
      lim_layer <- grep(pattern=T,x= sapply(p$layers, function(layer) {
      inherits(layer$geom, "GeomErrorbar")
      }))
      ylim <-  c( 100- ( 100- min(ggplot2::ggplot_build(p)$data[[lim_layer]]$ymin) ) * 1.2,
                  (max(ggplot2::ggplot_build(p)$data[[lim_layer]]$ymax) - 100 ) * 1.2 + 100)
    }else{
        lim_layer <- 2
        ylim <-  c( 100- ( 100- min(ggplot2::ggplot_build(p)$data[[lim_layer]]$y) ) * 1.2,
                    (max(ggplot2::ggplot_build(p)$data[[lim_layer]]$y) - 100 ) * 1.2 + 100)
        } }

######## sort out ylims and flipping

  if(is.null(ylim)){
  if (cflip){p <- p+ ggplot2::coord_flip()}
  }else{
  if (cflip)
    {p <- p+ ggplot2::coord_flip(ylim = ylim)
    }else{
    p <- p + ggplot2::coord_cartesian(ylim = ylim)}
}




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

  ## this is a bit of a hack. Alpha started popping up in legend, so am just globally turning it off

  p <- p + guides(alpha = "none")

  if (is.character(outp) && nzchar(outp)){



    if(!title_overide){
      if(!is.null(colour_condition) & ! is.null(x_condition)){
      if (colour_condition==x_condition){
        title <-paste(c(title,dv,
                        paste0(c(colour_condition,facet_condition),collapse = "-")),collapse = " ")
      }else{
      title <-paste(c(title,dv,paste0(c(colour_condition,x_condition,facet_condition),collapse = "-")),collapse = " ") }
      }
      title <-paste(c(title,dv,paste0(c(colour_condition,x_condition,facet_condition),collapse = "-")),collapse = " ")
      }

    if (error_dim){title <- paste(title," DIMMED")}

    ggplot2::ggsave(paste0(outp,"/",title,".pdf"),
           width = w, height = h, , limitsize = FALSE)

    ## save out the data that was used to make the plot in standardized form

    if (saveoutdata){
      write.csv(file = paste0(outp,"/",title,"_data.csv"),
                x=p$data[,.SD,.SDcols = (intersect(colnames(p$data),
                                                   c("pid","data_type" , "variable" ,  "condx" ,
                                                     "condcol" ,"condfacet"  ,"value_raw",  "dv" )))])
    }

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
