#'
#' @title Get parameters at one of their bounds from a set of TCSAM02 model runs as a dataframe
#'
#' @description Function to get parameters at one of their bounds from a set of TCSAM02 model runs as a dataframe.
#'
#' @param tcsams - a tcsam02.resLst object or named list of such
#' @param delta - relative fraction of range which defines being at a boundary
#' @param verbose - flag to print debugging info
#'
#' @return - a dataframe
#'
#' @details Returns a dataframe with parameters at one of their bounds.
#'
#' @export
#'
getMDFR.ParametersAtBounds<-function(tcsams,delta=0.01,verbose=FALSE){
    if (verbose) cat("--starting rTCSAM02::ParametersAtBounds().\n");
    options(stringsAsFactors=FALSE);

    if (inherits(tcsams,'tcsam02.resLst')){
        #tcsams is a tcsam02 resLst object
        #do nothing and drop out of "if" statement
    } else if (inherits(tcsams,'list')){
        #tcsams should be a list of tcsam02.resLst objects
        mdfr<-NULL;
        nl<-length(tcsams);
        nms<-names(tcsams);
        for (l in 1:nl){
            if (verbose) cat("Processing",nms[l],"\n")
            tcsam1<-tcsams[[l]];
            mdfrp<-getMDFR.ParametersAtBounds(tcsams=tcsam1,verbose=verbose);
            if (!is.null(mdfrp)){
                if (!is.null(nms[l])) mdfrp$case<-nms[l];
                mdfr<-rbind(mdfr,mdfrp);
            }
        }
        if (verbose) cat("--finished rTCSAM02::ParametersAtBounds().\n");
        return(mdfr);
    } else {
        cat("Error in getMDFR.ParametersAtBounds(tcsams).\n")
        cat("'tcsams' should be a 'tcsam02.resLst' object or a list of such.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }

    #tcsams is a single tcsam02.resLst object
    if (verbose) cat("\nProcessing resLst object")
    dfr<-getMDFR.ParameterValues(tcsams,verbose);
    testLower<-(dfr$final_param_value-dfr$min_param)<delta*(dfr$max_param-dfr$min_param);
    testUpper<-(dfr$max_param-dfr$final_param_value)<delta*(dfr$max_param-dfr$min_param);
    dfr$test<-"ok";
    dfr$test[testLower]<-"at lower bound";
    dfr$test[testUpper]<-"at upper bound";

    cols<-c("category","process","name","type","index","parameter_scale",
            "min_arith","max_arith","final_arith_value",
            "min_param","max_param","final_param_value",
            "test","label")
    mdfrp<-dfr[testUpper|testLower,cols];
    mdfr<-cbind(case="tcsam",mdfrp)
    if (verbose) cat("--finished rTCSAM02::ParametersAtBounds().\n");
    return(mdfr);
}
