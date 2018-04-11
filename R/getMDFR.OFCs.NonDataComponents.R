#'
#' @title Get non-data components from the objective function for a (list of) TCSAM02 model(s) as a dataframe
#'
#' @description Function to get non-data components from the objective function for a (list of) TCSAM02 model(s) as a dataframe.
#'
#' @param obj - a single tcsam02 rep object, a single tcsam02 resLst object, or a named list of such
#' @param categories - non-data-related objective function components to get ("all","penalties","penFDevs","components","priors")
#' @param  verbose - flag (T/F) to print diagnostic info
#'
#' @return dataframe
#'
#' @details Returned dataframe has columns
#' \itemize{
#'   \item{case - model case (blank, to be filled in by caller if obj is not a list of tcsam02.resLst objects)}
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
                                         categories=c("all","penalties","penFDevs","components","priors"),
                                         verbose=FALSE){
    if (verbose) cat("Starting rTCSAM02::getMDFR.OFCs.NonDataComponents().\n")
    options(stringsAsFactors=FALSE);
    mdfr<-NULL;
    if ((class(obj)[1]=="list")){
        if (verbose) cat("--Processing list\n",sep='')
        for (case in names(obj)){
            if (verbose) cat("\tProcessing list element '",case,"'\n",sep='')
            dfr<-getMDFR.OFCs.NonDataComponents(obj[[case]],categories,verbose);
            if (!is.null(dfr)){
                dfr$case<-case;
                mdfr<-rbind(mdfr,dfr);
            }
        }
        return(mdfr);
    } else  if (inherits(obj,"tcsam02.resLst")){
        #pull out tcsam02.rep object and process
        if (verbose) cat("--Processing tcsam02.resLst object\n",sep='')
        mdfr<-getMDFR.OFCs.NonDataComponents(obj$rep,categories,verbose);
        return(mdfr);
    } else if (!inherits(obj,"tcsam02.rep")){
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        cat("Error in rTCSAM02::getMDFR.OFCs.NonDataComponents().\n")
        cat("Input object not reducible to a tcsam02.rep object!\n")
        cat("Classes = ",class(obj),"\n");
        cat("Returning NULL.\n")
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        return(NULL);
    }

    #obj should now be a tcsam02.rep object
    if (verbose) cat("----Processing tcsam02.rep object\n",sep='');

    if ("all" %in% categories) categories<-c("penalties","penFDevs","components","priors");

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
