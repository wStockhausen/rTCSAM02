#'
#' @title Get sd_report info on derived quantities as a dataframe
#'
#' @description Function to get sd_report info on derived time series estimates as a dataframe.
#'
#' @param tcsams - a tcsam02.resLst object or named list of such
#' @param ci - confidence interval
#' @param verbose - flag to print debugging info
#'
#' @return a dataframe
#'
#' @details Derived variables are
#' "sdrAvgRec","sdrBmsy","sdrFmsy","sdrMSY","sdrCurB","sdrFofl","sdrOFL","sdrPrjB".
#' The column names in the output dataframe have the "sdr" prefix dropped.
#'
#' @import magrittr
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
#'
getMDFR.SdRep.DerivedVars<-function(tcsams,
                                    ci=0.95,
                                    verbose=FALSE){
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
          mdfrp<-getMDFR.SdRep.DerivedVars(tcsams=tcsam1,verbose=verbose);
          if (!is.null(mdfrp)){
              if (!is.null(nms[l])) mdfrp$case<-nms[l];
              mdfr<-rbind(mdfr,mdfrp);
          }
      }
      if (verbose) cat("--finished rTCSAM02::getMDFR.SdRep.DerivedVars().\n");
      return(mdfr);
  } else {
      cat("Error in getMDFR.SdRep.DerivedVars(tcsams).\n")
      cat("'tcsams' should be a 'tcsam02.resLst' object or a list of such.\n")
      cat("Returning NULL.\n")
      return(NULL);
  }

  #--extract variables info
  vars = c("sdrAvgRec","sdrBmsy","sdrFmsy","sdrMSY","sdrCurB","sdrFofl","sdrOFL","sdrPrjB");
  #----extract std info
  dfrStd = tcsams$std;
  dfr    = dfrStd %>% subset(name %in% vars) %>% dplyr::select(!one_of("row id")) %>%
                      dplyr::mutate(variable=stringr::str_sub(name,4,-1),
                                    y="all",x="all") %>%
                      dplyr::select(variable,y,x,name,est,`std.dev`) %>%
                      dplyr::mutate(lci=qnorm(  (1-ci)/2,mean=est,sd=`std.dev`),
                                    uci=qnorm(1-(1-ci)/2,mean=est,sd=`std.dev`));

 return(dfr);
}
