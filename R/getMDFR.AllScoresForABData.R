#'
#'@title Get z-scores and other model fits info for abundance or biomass data from fits to data as dataframe
#'
#'@description Function to get z-scores and other model fits info for abundance, biomass data from fits to data as dataframe.
#'
#'@param afits -
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details The returned dataframe is **NOT** in canonical format and has columns
#'x,m,s,y,val,var,process,fleet,category,case. Column \code{var} gives the type
#'of value, which is one of "z-score","observed","predicted","sdobs","xcv","stdv", and "useFlg".
#'
#'@import dplyr
#'
#'@export
#'
getMDFR.AllScoresForABData<-function(afits,
                                   verbose=FALSE){
    if (verbose) message("---Running rTCSAM02::getMDFR.AllScoresForABData(...).\n");

    nf<-length(afits);
    if (verbose) message("----number of fits =",nf,"\n");

    mdfr<-NULL;
    ctr = 0;
    lst = list();
    for (n in 1:nf){
        afit<-afits[[n]];
        if (!is.null(afit)){
            nll<-afit$nll;
            pdfType<-nll$nll.type;
            if (tolower(pdfType)!='none'){
                cp = 0;
                lp = list();
                var="zscrs"; if (var %in% names(nll))
                    lp[[cp<-cp+1]]<-data.frame(x=afit$x,m=afit$m,s=afit$s,
                                         y=as.numeric(names(nll[[var]])),
                                         val=nll[[var]],var='z-score');
                var="obs"; if (var %in% names(nll))
                    lp[[cp<-cp+1]]<-data.frame(x=afit$x,m=afit$m,s=afit$s,
                                         y=as.numeric(names(nll[[var]])),
                                         val=nll[[var]],var='observed');
                var="mod"; if (var %in% names(nll))
                    lp[[cp<-cp+1]]<-data.frame(x=afit$x,m=afit$m,s=afit$s,
                                         y=as.numeric(names(nll[[var]])),
                                         val=nll[[var]],var='predicted');
                var="sdobs"; if (var %in% names(nll))
                    lp[[cp<-cp+1]]<-data.frame(x=afit$x,m=afit$m,s=afit$s,
                                         y=as.numeric(names(nll[[var]])),
                                         val=nll[[var]],var='sdobs');
                var="xcv"; if (var %in% names(nll))
                    lp[[cp<-cp+1]]<-data.frame(x=afit$x,m=afit$m,s=afit$s,
                                         y=as.numeric(names(nll[[var]])),
                                         val=nll[[var]],var='xcv');
                var="stdv"; if (var %in% names(nll))
                    lp[[cp<-cp+1]]<-data.frame(x=afit$x,m=afit$m,s=afit$s,
                                         y=as.numeric(names(nll[[var]])),
                                         val=nll[[var]],var='stdv');
                var="useFlgs"; if (var %in% names(nll))
                    lp[[cp<-cp+1]]<-data.frame(x=afit$x,m=afit$m,s=afit$s,
                                         y=as.numeric(names(nll[[var]])),
                                         val=nll[[var]],var='useFlg');
                if (length(lp)>0) lst[[ctr<-ctr+1]] = dplyr::bind_rows(lp);
                rm(cp,lp);
            }
        }
    }
    if (length(lst)>0) mdfr = dplyr::bind_rows(lst);
    rm(ctr,lst)

    mdfr$x<-gsub("_"," ",tolower(mdfr$x),fixed=TRUE);
    mdfr$m<-gsub("_"," ",tolower(mdfr$m),fixed=TRUE);
    mdfr$s<-gsub("_"," ",tolower(mdfr$s),fixed=TRUE);

    if (verbose) message("---Done running rTCSAM02::getMDFR.AllScoresForABData(...).\n");
    return(mdfr);
}
