#'
#' @title Get objective function components for growth data from a tcsam02.resLst or tcsam02.rep object
#'
#' @description Function to get objective function components for growth data from a tcsam02.resLst or tcsam02.rep object.
#'
#' @param obj - a tcsam02.resLst or tcsam02.rep object
#' @param category - fleet category ("surveys" or "fisheries")
#' @param verbose - flag (T/F) to print diagnostic info
#'
#' @return a dataframe
#'
#' @details Returned dataframe has columns:
#' \itemize{
#'   \item{case - model case (blank, to be filled in by caller)}
#'   \item{category - fleet category}
#'   \item{fleet - fleet name}
#'   \item{catch.type - catch type}
#'   \item{data.type - data type}
#'   \item{fit.type - fit type}
#'   \item{nll.type - likelihood type}
#'   \item{y - year}
#'   \item{x - sex}
#'   \item{m - maturity state}
#'   \item{s  - shell condition}
#'   \item{ wgt - likelihood weight}
#'   \item{nll - (unweighted) negative log-likelihood}
#'   \item{objfun - objective function value}
#' }
#'
getMDFR.OFCs.FleetData<-function(obj,
                                 category=c("surveys","fisheries"),
                                 verbose=FALSE){
    category<-category[1];

    if (inherits(obj,"tcsam02.rep")){
        #do nothing, will fall out to code below
    } else if (inherits(obj,"tcsam02.resLst")){
        #pull out tcsam02.rep object and process
        mdfr<-getMDFR.OFCs.FleetData(obj$rep,category,verbose);
        return(mdfr);
    } else {
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        cat("Error in rTCSAM02::getMDFR.OFCs.FleetData().\n")
        cat("Input object not reducible to a tcsam02.rep object!\n")
        cat("Classes = ",class(obj),"\n");
        cat("Returning NULL.\n")
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        return(NULL);
    }

    #obj should now be a tcsam02.rep object
    lst<-obj$model.fits[[category]];

    dfr<-NULL;
    fltnms<-names(lst);
    fltnms<-fltnms[fltnms!=""];

    for (fltnm in fltnms){
        if (verbose) cat("Processing fleet",fltnm,"\n")
        flt<-lst[[fltnm]];
        ctynms<-names(flt);#catch types
        ctynms<-ctynms[ctynms!=''];
        for (ctynm in ctynms){
            if (verbose) cat("Processing catch type",ctynm,"\n")
            cty<-flt[[ctynm]];
            dcnms<-names(cty);#data components
            dcnms<-dcnms[dcnms!=''];
            for (dcnm in dcnms){
                if (verbose) cat("Processing data component",dcnm,"\n")
                if (dcnm=="n.at.z"){
                    dc<-cty[[dcnm]];
                    nfits<-length(dc);
                    for (ifit in 1:nfits){
                        fit<-dc[[ifit]];
                        if (!is.null(fit)){
                            if (verbose) cat("Processing fit",ifit,"\n");
                            rw<-data.frame(case="",
                                           category=paste(category,"data"),
                                           fleet=gsub("_"," ",fltnm,fixed=TRUE),
                                           catch.type=gsub("."," ",ctynm,fixed=TRUE),
                                           data.type=dcnm,
                                           fit.type=fit$fit.type,
                                           nll.type=fit$fit$nll.type,
                                           y=fit$y,
                                           x=fit$x,
                                           m=fit$m,
                                           s=fit$s,
                                           wgt=fit$fit$wgt,
                                           nll=fit$fit$nll,
                                           objfun=fit$fit$objfun);
                            dfr<-rbind(dfr,rw);
                        }
                    }#ifit
                } else {
                    dc<-cty[[dcnm]]$fits;
                    nfits<-length(dc);
                    for (ifit in 1:nfits){
                        fit<-dc[[ifit]];
                        if (!is.null(fit)){
                            if (verbose) cat("Processing fit",ifit,"\n")
                            rw<-data.frame(case="",
                                           category=paste(category,"data"),
                                           fleet=gsub("_"," ",fltnm,fixed=TRUE),
                                           catch.type=gsub("."," ",ctynm,fixed=TRUE),
                                           data.type=dcnm,
                                           fit.type=cty[[dcnm]]$fit.type,
                                           nll.type=fit$nll$nll.type,
                                           y='all',
                                           x=fit$x,
                                           m=fit$m,
                                           s=fit$s,
                                           wgt=fit$nll$wgt,
                                           nll=fit$nll$nll,
                                           objfun=fit$nll$objfun);
                            dfr<-rbind(dfr,rw);
                        }
                    }#ifit
                }
            }#dcnm
        }#ctynm
    }#fltnm

    return(dfr);
}
