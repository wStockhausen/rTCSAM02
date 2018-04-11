#'
#'@title Function to calculate ABC and OFL from an MCMC dataframe
#'
#'@description This function calculates ABC and OFL from TCSAM02 MCMC output.
#'
#' @param mcmc - mcmc object
#' @param dfr - dataframe with OFL as column (and possibly 'case')
#' @param pstar - pstar value for ABC calculation
#' @param buffer - buffer for ABC as fraction (i.e., 0.20 = 20 \% yields ABC = 0.8*OFL)
#' @param xlims - range for x-axis in plot
#' @param title - title for plot
#' @param verbose flag to rpint diagnostic info
#'
#' @return dataframe with median results, ABC.pstar and ABC.buffer for each case
#'
#' @details uses \code{plotABC} to produce ABC plots for each case
#'
#' @export
#'
mcmc.CalcABC<-function(mcmc=NULL,
                      dfr=NULL,
                      pstar=0.49,
                      buffer=0.2,
                      xlims=NULL,
                      title=NULL,
                      verbose=FALSE){
    if (verbose) cat("Starting mcmc.CalcABC\n")
    if (is.null(dfr)&&!is.data.frame(mcmc)){
        dfr<-mcmc.ExtractOFLResults(mcmc);
        dfr$case<-" ";
    } else if (is.data.frame(mcmc)){dfr<-mcmc;}

    if (!("case" %in% names(dfr))) dfr$case<-" ";
    cases<-unique(dfr$case);

    res<-NULL;
    for (case in cases){
        if (verbose) cat("Processing case '",case,"'\n");
        dfrp<-dfr[dfr$case==case,];
        nms<-c("avgRec","B100","Fmsy","Bmsy","MSY","Fofl","OFL","prjB");
        for (nm in nms){
            str<-paste0("x",nm,"<-ecdf(dfrp[['",nm,"']]); ",nm,"<-quantile(x",nm,",probs=0.50);");
            if (verbose) cat(str,"\n");
            eval(parse(text=str));
        }
        ABC.pstar<-quantile(xOFL,probs=pstar);
        b.y<-1-ABC.pstar/OFL;
        ABC.buff<-(1-buffer)*OFL;
        plotABC(xOFL,OFL,ABC.pstar,ABC.buff,buffer=buffer,title=case,xlims=xlims,save=FALSE);
        resp<-data.frame(case=case,OFL=OFL,ABC.pstar=ABC.pstar,ABC.buff=ABC.buff,
                         avgRec=avgRec,Bmsy=Bmsy,B100=B100,Fmsy=Fmsy,Fofl=Fofl,prjB=prjB);
        res<-rbind(res,resp);
    }
    if (verbose) cat("Finished mcmc.CalcABC\n")
    return(res);
}
