#'
#'@title Function to calculate ABC and OFL from an MCMC dataframe
#'
#'@description This function calculates ABC and OFL from TCSAM02 MCMC output.
#'
#' @param mcmc - mcmc object
#' @param dfr - dataframe with OFL as column (and possibly 'case')
#' @param pstar - pstar value for ABC calculation
#' @param buffer - buffer for ABC as fraction (i.e., 0.20 = 20 \% yields ABC = 0.8*OFL)
#' @param doPlot - flag (T/F) to create plot
#' @param xlims - range for x-axis in plot
#' @param title - title for plot
#' @param verbose flag to rpint diagnostic info
#'
#' @return list with elements
#' \itemize{
#'   \item{dfrABCs - dataframe with median results, ABC.pstar and ABC.buffer for each case}
#'   \item{dfrOFLs - dataframe with empirical cdf of OFL for each case}
#'   \item{plot - ggplot2 object}
#'}
#'
#' @details uses \code{plotABC} to produce ABC plots for each case
#'
#' @export
#'
mcmc.CalcABC<-function(mcmc=NULL,
                       dfr=NULL,
                       pstar=0.49,
                       buffer=0.2,
                       doPlot=TRUE,
                       xlims=NULL,
                       verbose=FALSE){
    if (verbose) cat("Starting mcmc.CalcABC\n")
    if (is.null(dfr)&&!is.data.frame(mcmc)){
        dfr<-mcmc.ExtractOFLResults(mcmc);
        dfr$case<-" ";
    } else if (is.data.frame(mcmc)){dfr<-mcmc;}

    if (!("case" %in% names(dfr))) dfr$case<-" ";
    cases<-unique(dfr$case);

    probs<-seq(from=0.01,to=0.99,by=0.01);
    dfrABCs<-NULL;
    dfrOFLs<-NULL;
    for (case in cases){
        if (verbose) cat("Processing case '",case,"'\n");
        dfrp<-dfr[dfr$case==case,];
        nms<-c("avgRec","B100","Fmsy","Bmsy","MSY","Fofl","OFL","prjB");
        for (nm in nms){
            str<-paste0("ecdf",nm,"<-ecdf(dfrp[['",nm,"']]); v",nm,"<-quantile(ecdf",nm,",probs=0.50);");
            if (verbose) cat(str,"\n");
            eval(parse(text=str));
        }
        valsOFL<-quantile(ecdfOFL,probs=probs);#values at empirical cdf probs
        dfrpOFLs<-data.frame(case=case,probs=probs,OFLs=valsOFL,stringsAsFactors=FALSE,row.names=NULL);
        dfrOFLs<-rbind(dfrOFLs,dfrpOFLs);
        vABC.pstar<-stats::quantile(ecdfOFL,probs=pstar);
        vABC.buff<-(1-buffer)*vOFL;
        #plotABC(xOFL,OFL,ABC.pstar,ABC.buff,buffer=buffer,title=case,xlims=xlims,save=FALSE);
        dfrpABCs<-data.frame(case=case,OFL=vOFL,ABC.pstar=vABC.pstar,ABC.buff=vABC.buff,
                             avgRec=vavgRec,Bmsy=vBmsy,B100=vB100,Fmsy=vFmsy,Fofl=vFofl,prjB=vprjB,
                             stringsAsFactors=FALSE,row.names=NULL);
        dfrABCs<-rbind(dfrABCs,dfrpABCs);
    }

    p<-NULL;
    if (doPlot) p<-mcmc.PlotABC(dfrABCs,dfrOFLs,xlims=xlims);

    if (verbose) cat("Finished mcmc.CalcABC\n")
    return(list(dfrABCs=dfrABCs,dfrOFLs=dfrOFLs,plot=p));
}
