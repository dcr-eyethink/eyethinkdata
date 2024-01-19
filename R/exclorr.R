exclorr  <- function(data,var1,var2,bayes_factor=F,
                     plim=0.05,cplot=F, cplot_title="Correlation Matrix", outp="analysis") {
  #' Explore correlations between large numbers of DVs
  #'
  #' Takes a data set and two lists of DVs
  #' looks at correlations between one set and another
  #' r values, p values and bayes factors
  #' @param data data
  #' @param var1 list of DV names that will be rows
  #' @param var2 list of DV names that will be cols
  #' @param bayes_factor do you want bayes analysis? banal
  #' @param plim cut off for p-value reporting
  #' @param outp name of folder to save plot in, set to blank if no save needed
  #' @param cplot generate a correlation matrix plot

  #' @export



  missing <- c(var1,var2)[!c(var1,var2) %in% colnames(data)]
  if (length(missing)>0){
    cat("You're missing named variables")
    return(missing)
  }

  data <- data.frame(data)
  var1 <- gsub(var1,pattern = " ",replacement = ".")
  var2 <- gsub(var2,pattern = " ",replacement = ".")
  corrdata=data[c(var1,var2)]

  corrs <- Hmisc::rcorr(as.matrix((corrdata)))
  corrP <- data.frame(corrs["P"])
  corrR <- data.frame(corrs["r"])

  resrows <- dim(corrP)[2]
  #get the subset of the p values and r values
  subcorrP <- corrP[1:(length(var1)),(resrows-length(var2)+1):(resrows)]
  subcorrR <- corrR[1:(length(var1)),(resrows-length(var2)+1):(resrows)]

  phack <- function(x) {
    r=0
    #  print(x)
    if(!is.nan(x) & !is.na(x)){ if (x<plim){r <- 1}}
    return(r)
  }

  ##find out which are significant
  sig <- matrix(mapply(as.matrix(subcorrP),FUN=phack),length(var1))

  corrsig  <- round(sig *  subcorrR,2)

  res <- list(corrsig=data.frame(corrsig), pvalues=data.frame(subcorrP),rvalues=data.frame(subcorrR))

  if (is.character(outp)){dir.create(file.path(outp),showWarnings = FALSE)}

  if (cplot){

    pdf(file=paste0(outp,"/",cplot_title,".pdf"))

    plot_r <-  subcorrR
    plot_p <-  subcorrP
    colnames(plot_r) <- gsub(x=colnames(plot_r),pattern="r.",replacement="",fixed = T)
    colnames(plot_p) <- gsub(x=colnames(plot_r),pattern="P.",replacement="",fixed = T)

    p <- plot_corr(r=plot_r,p=plot_p,title=cplot_title,plim=plim)

    p$plot()
    dev.off()

    res$plot <- p$plot()

  }

  ## run bayes analysis
  ## this should be done without loops...

  b <- matrix(nrow=length(var1),ncol=length(var2))
  # dimnames(b) = list(colnames(data)[var1],colnames(data)[var2])
  dimnames(b) = list(var1,var2)
  b <- data.frame(b)

  if (bayes_factor==TRUE){
    for (x in var1){
      for (y in var2){

        bf <- data.frame( BayesFactor::regressionBF(data= na.omit(data.frame(x=data[[x]],y=data[[y]])),
                                                    formula=x~y,progress=FALSE) )[1]
        b[x,y] <- floor(as.numeric(bf))
      }}
    res$bfs=b
  }

  return(res)
}


plot_corr <- function(r,p,title,plim){
  plot <- function(){
    corrplot::corrplot(as.matrix(r), order="original", diag = F,method="circle",
                       p.mat = as.matrix(p), sig.level = plim, insig = "blank",mar=c(0,0,2,0),title = title)
  }
  list(plot=plot,r=r,p=p,title=title,plim=plim)
}
