#'
#' @title Get sd_report info on recruitment and SSB time series estimates as a dataframe
#'
#' @description Function to get sd_report info on recruitment and SSB time series estimates as a dataframe.
#'
#' @param tcsams - a tcsam02.resLst object or named list of such
#' @param ci - confidence interval
#' @param verbose - flag to print debugging info
#'
#' @return a dataframe
#'
#' @details Recruitment values are on ln-scale.
#'
#' @import magrittr
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
#'
getMDFR.SdRep.RecAndSSB<-function(tcsams,
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
          mdfrp<-getMDFR.SdRep.RecAndSSB(tcsams=tcsam1,verbose=verbose);
          if (!is.null(mdfrp)){
              if (!is.null(nms[l])) mdfrp$case<-nms[l];
              mdfr<-rbind(mdfr,mdfrp);
          }
      }
      if (verbose) cat("--finished rTCSAM02::getMDFR.SdRep.RecAndSSB().\n");
      return(mdfr);
  } else {
      cat("Error in getMDFR.SdRep.RecAndSSB(tcsams).\n")
      cat("'tcsams' should be a 'tcsam02.resLst' object or a list of such.\n")
      cat("Returning NULL.\n")
      return(NULL);
  }

  #tcsams is a single tcsam02.resLst object
  #--extract model years
  y<-tcsams$rep$mc$dims$y;
  ny<-length(y$vls);
  #--extract std info
  dfrStd<-tcsams$std;
  #----extract recruitment
  dfrRec<-dfrStd %>% subset(name=="sdrLnR_y") %>% dplyr::select(!one_of("row id"));
  dfrRec<-cbind(data.frame(variable="lnRec",y=y$vls[1:(ny-1)],x="all",stringsAsFactors=FALSE),dfrRec);
  dfrRec<-dfrRec %>% dplyr::mutate(y=y+1);     #change rec year to year it enters population
  dfrRec %<>% dplyr::mutate(lci=qnorm(  (1-ci)/2,mean=est,sd=`std.dev`),
                            uci=qnorm(1-(1-ci)/2,mean=est,sd=`std.dev`));
  # ar.est<-exp(dfrRec$est);                     #convert ln-scale recruitment estimates to arithmetic-scale
  # ar.sdv<-sqrt(exp(dfrRec$std.dev^2)-1)*ar.est;#convert ln-scale recruitment sdvs to arithmetic-scale
  # dfrRec<-dfrRec %>% dplyr::mutate(est=ar.est,std.dev=ar.sdv);
  #----extract SSB
  yv<-c(y$vls[1:(ny-1)],y$vls[1:(ny-1)]);
  xv<-c(rep("male",times=ny-1),rep("female",times=ny-1));
  dfrSSB<-dfrStd %>% subset(name=="sdrSpB_xy") %>% dplyr::select(!one_of("row id"));
  dfrSSB<-cbind(data.frame(variable="SSB",y=yv,x=xv),dfrSSB);
  dfrSSB %<>% dplyr::mutate(lci=qnorm(  (1-ci)/2,mean=est,sd=`std.dev`),
                            uci=qnorm(1-(1-ci)/2,mean=est,sd=`std.dev`));

  dfr<-rbind(dfrRec,dfrSSB);
 return(dfr);
}
