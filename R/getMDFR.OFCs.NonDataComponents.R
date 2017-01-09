#'
#' @title Get non-data components from the objective function for a TCSAM02 model as a dataframe
#'
#' @description Function to get non-data components from the objective function for a TCSAM02 model as a dataframe.
#'
#' @param obj - a tcsam02.resLst or tcsam02.rep object
#' @param categories - non-data-related objective function components to get ("all","penalties","penFDevs","components","priors")
#' @param  verbose - flag (T/F) to print diagnostic info
#'
#' @return dataframe
#'
#' @details Returned dataframe has columns
#' \itemize{
#'   \item{case - model case [blank, to be filled in by calling function]}
#'   \item{category - objective function component category}
#'   \item{type - type name}
#'   \item{element - element name}
#'   \item{level - level name}
#'   \item{wgt - likelihood weight}
#'   \item{nll - negative log-likelihood (unweighted)}
#'   \item{objfun - objective function value}
#' }
#'
#' @export
#'
getMDFR.OFCs.NonDataComponents<-function(obj,
                                         categories=c("penalties","penFDevs","components","priors"),
                                         verbose=FALSE){
    if (inherits(obj,"tcsam02.rep")){
        #do nothing, will fall out to code below
    } else if (inherits(obj,"tcsam02.resLst")){
        #pull out tcsam02.rep object and process
        mdfr<-getMDFR.OFCs.NonDataComponents(obj$rep,categories,verbose);
        return(mdfr);
    } else {
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        cat("Error in rTCSAM02::getMDFR.OFCs.NonDataComponents().\n")
        cat("Input object not reducible to a tcsam02.rep object!\n")
        cat("Classes = ",class(obj),"\n");
        cat("Returning NULL.\n")
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        return(NULL);
    }

    #obj should now be a tcsam02.rep object
    if ("all" %in% categories) categories<-c("penalties","penFDevs","components","priors");

    mdfr<-NULL;
    for (category in categories){

        if (category=="penFDevs") {
            dfr<-getMDFR.OFCs.FDevPenalties(obj$model.fits,verbose);
            mdfr<-rbind(mdfr,dfr);
        }

        lst<-obj$model.fits[[category]];

        dfr<-NULL;
        types<-names(lst);
        types<-types[types!=""];

        for (type in types){
            lstt<-lst[[type]];

            elnms<-names(lstt);#type element names
            elnms<-elnms[elnms!=""];#remove empty elements
            for (elnm in elnms){
                if(verbose) cat("Processing objective function penalties for ",type,"/",elnm,'\n',sep='')
                el<-lstt[[elnm]]; #element
                levnms<-names(el);#names of element levels
                levnms<-levnms[levnms!=""];
                for (levnm in levnms){
                    if(verbose) cat("\tProcessing level",levnm,'\n')
                    lev<-el[[levnm]];
                    if (!is.null(lev)){
                        if (!is.null(lev$nll)) {nll<-lev$nll;} else
                        if (!is.null(lev$pen)) {nll<-lev$pen;}
                        rw<-data.frame(case="",
                                       category=category,
                                       type=type,
                                       element=elnm,
                                       level=levnm,
                                       wgt=lev$wgt,nll=nll,objfun=lev$objfun);
                        dfr<-rbind(dfr,rw);
                    }#lev!=NULL
                }#levnms
            }#elnm
        }#type
        mdfr<-rbind(mdfr,dfr);
    }#category

    return(mdfr);
}

#'
#' @title Get F-devs penalties from the objective function for a TCSAM02 model as a dataframe
#'
#' @description Function to get F-devs penalties  from the objective function for a TCSAM02 model as a dataframe.
#'
#' @param fits - model.fits list element from a tcsam02.rep object
#' @param  verbose - flag (T/F) to print diagnostic info
#'
#' @return dataframe
#'
#' @details Called by \code{getMDFR.OFCs.NonDataComponents()}. Returned dataframe has columns
#' \itemize{
#'   \item{case - model case [blank, to be filled in by calling function]}
#'   \item{category - objective function component category}
#'   \item{type - type name}
#'   \item{element - element name}
#'   \item{level - level name}
#'   \item{wgt - likelihood weight}
#'   \item{nll - negative log-likelihood (unweighted)}
#'   \item{objfun - objective function value}
#' }
#'
getMDFR.OFCs.FDevPenalties<-function(fits,
                                     verbose=FALSE){
    category<-"penFDevs";
    lst<-fits[[category]];

    dfr<-NULL;
    levnms<-names(lst);
    levnms<-levnms[levnms!=""];

    for (levnm in levnms){
        if(verbose) cat("Processing objective function F-dev penalties for fishery ",levnm,'\n',sep='')
        lev<-lst[[levnm]];
        if (!is.null(lev)){
            nll<-lev$pen;
            rw<-data.frame(case="",
                           category="penalties",
                           type="F-devs",
                           element="fisheries",
                           level=levnm,
                           wgt=lev$wgt,nll=nll,objfun=lev$objfun);
            dfr<-rbind(dfr,rw);
        }#lev!=NULL
    }#levnms
    return(dfr);
}
