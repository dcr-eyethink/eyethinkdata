exclorr  <- function(data,var1,var2,bayes_factor=F,
                     plim=0.05,cplot=F, cplot_title="Correlation Matrix", outp="analysis") {
  #' Explore correlations between large numbers of variables
  #'
  #' Takes a data set and two lists of column names, and correlates every variable
  #' in the first list with every variable in the second. It returns the r values,
  #' the p values, and a table showing only the r values that are significant.
  #' It can also draw a correlation matrix plot, and calculate Bayes factors.
  #'
  #' @details
  #' Correlations are Pearson's r, calculated on all the pairs of values available
  #' (`Hmisc::rcorr`). Spaces and hyphens in the variable names are converted to
  #' full stops, as R does when it reads in column names.
  #' If any of the names you give are not columns in `data`, the function prints a
  #' message and returns the missing names instead of results.
  #'
  #' @param data A data frame or data.table with one row per participant.
  #' @param var1 Vector of column names. These will be the rows of the output.
  #' @param var2 Vector of column names. These will be the columns of the output.
  #' @param bayes_factor Do you want Bayes factors for each correlation? Needs the
  #'   BayesFactor package, and can be slow for large numbers of variables.
  #' @param plim Cut off for which p-values count as significant. Default 0.05.
  #' @param cplot Make a correlation matrix plot (circles for the significant
  #'   correlations)? If `TRUE` it is saved as a pdf in `outp`.
  #' @param cplot_title Title for the plot, which is also used as its file name.
  #' @param outp Name of a folder in which to save the plot (it will be created if
  #'   it does not exist). If `NULL` or "", the plot is drawn on the current device
  #'   rather than saved.
  #' @return A list with data frames `corrsig` (r values, rounded, with
  #'   non-significant ones set to 0), `pvalues` and `rvalues`. Also `plot` if
  #'   `cplot = TRUE`, and `bfs` (Bayes factors, rounded down to a whole number) if
  #'   `bayes_factor = TRUE`.
  #' @export
  #' @examples
  #' \dontrun{
  #' exclorr(mtcars, var1 = c("mpg", "disp"), var2 = c("hp", "wt", "qsec"))
  #' }




  missing <- c(var1,var2)[!c(var1,var2) %in% colnames(data)]
  if (length(missing)>0){
    cat("You're missing named variables")
    return(missing)
  }

  data <- data.frame(data)
  var1 <- gsub(var1,pattern = " ",replacement = ".")
  var2 <- gsub(var2,pattern = " ",replacement = ".")
  var1 <- gsub(var1,pattern = "-",replacement = ".")
  var2 <- gsub(var2,pattern = "-",replacement = ".")
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

  if (is.character(outp) && nzchar(outp)){dir.create(file.path(outp),showWarnings = FALSE)}

  if (cplot){

    saving <- is.character(outp) && nzchar(outp)
    if (saving){pdf(file=paste0(outp,"/",cplot_title,".pdf"))}

    plot_r <-  subcorrR
    plot_p <-  subcorrP
    colnames(plot_r) <- gsub(x=colnames(plot_r),pattern="r.",replacement="",fixed = T)
    colnames(plot_p) <- gsub(x=colnames(plot_r),pattern="P.",replacement="",fixed = T)

    p <- plot_corr(r=plot_r,p=plot_p,title=cplot_title,plim=plim)

    p$plot()
    if (saving){dev.off()}

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
