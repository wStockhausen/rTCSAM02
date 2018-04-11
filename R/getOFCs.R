#'
#'@title Get model components in the objective function as a tcsam02.ofc (melted dataframe) object
#'
#'@description Function to get model components in the objective function as a tcsam02.ofc (melted dataframe) object.
#'
#'@param obj - a single tcsam02 rep object, a single tcsam02 resLst object, or a named list of such
#'@param mdl - name to associate with model results object
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return a melted dataframe with class tcsam02.ofc
#'
#'@details If obj is a list of tcsam02 resLst (tcsam.resLst) or rep (tcsam02.rep) objects, then the function
#'is called recursively for each object, with the associated list component name used as
#'mdl. If obj is a tcsam02 resLst or rep object and mdl is NULL (the default), then
#'the configName is used as the model name.
#'
#'The returned dataframe has columns named
#'"model", "type", "category", "name", "level", "variable", and "value".
#'
#'The "variable" column indicates whether the "value" is a weight ('wgt'),
#'negative log-likelihood ('nll'), or objective function value ('objfun').
#'
#'@export
#'
getOFCs<-function(obj,
                  mdl=NULL,
                  verbose=FALSE){
    if (verbose) cat("Starting rTCSAM02::getOFCs().\n")
    options(stringsAsFactors=FALSE);

    if (class(obj)[1]=='list'){
        #obj is a list of tcsam02 resLst or rep objects
        mdls<-names(obj);
        mdfr<-NULL;
        for (mdl in mdls){
            mdfr<-rbind(mdfr,getOFCs(obj[[mdl]],mdl=mdl));
        }
        class(mdfr)<-c('tcsam02.ofc',class(mdfr));#re-apply class designation
        return(mdfr);
    } else if (inherits(obj,'tcsam02.resLst')){
        #obj is a tcsam02 resLst object
        mdfr<-getOFCs(obj$rep,mdl=mdl);
        return(mdfr);
    } else if (!inherits(obj,'tcsam02.rep')){
        cat("Error in getOFCs(obj).\n")
        cat("'obj' should be an object of class 'tcsam02.rep', 'tcsam02.resLst', or a list of such.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }

    #obj is a tcsam02 model report object
    if (is.null(mdl)) {
        mdl<-'tcsam02';
        if (!is.null(obj$mc)) mdl<-obj$mc$configName;
    }
    comps<-obj$model.fits$components;
    nmctgs<-names(comps);#names of model categories for components
    dfr<-NULL;
    for (nmctg in nmctgs){
        if(verbose) cat("Processing priors for category",nmctg,'\n')
        ctg<-comps[[nmctg]];#model category object
        nmps<-names(ctg);   #names of elements in category
        for (nmp in nmps){
            if(verbose) cat("Processing element",nmp,'\n')
            elem<-ctg[[nmp]]; #element object
            if (!is.null(elem)){
                nmlevs<-names(elem);#names of element levels
                for (nmlev in nmlevs){
                    if(verbose) cat("\tProcessing level",nmlev,'\n')
                    lev<-elem[[nmlev]];
                    if (!is.null(lev)){
                        rw<-data.frame(model=mdl,type='prior',category=nmctg,name=nmp,level=nmlev,wgt=lev$wgt,nll=lev$nll,objfun=lev$objfun);
                        dfr<-rbind(dfr,rw);
                    }#level!=NULL
                }#levels
            }#parameter!=NULL
        }#parameters
    }#categories
    mdfr<-reshape2::melt(dfr,id.vars=c('model','type','category','name','level'),measure.vars=c('wgt','nll','objfun'))

    class(mdfr)<-c('tcsam02.ofc',class(mdfr));
    if (verbose) cat("Finished rTCSAM02::getOFCs().\n")
    return(mdfr)
}
