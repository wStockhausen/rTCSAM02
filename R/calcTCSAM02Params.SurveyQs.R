#'
#' @title Calculate survey Q parameters for TCSAM02 based on values from a TCSAM2013 model.
#'
#' @description Function to calculate survey Q parameter values for TCSAM02 based on values from a TCSAM2013 model.
#'
#' @param obj - tcsam2013.resLst object or tcsam2103.prs object
#' @param tpl - character vector with file template
#' @param verbose - flag to print debugging info
#'
#' @return file template with survey Q parameter values filled in
#'
#' @details Calculates and returns a list of survey Q parameter values for a TCSAM02 model equivalent to those from a TCSAM2013 model run.
#'
#' @export
#'
calcTCSAM02Params.SurveyQs<-function(obj,tpl,verbose=FALSE){
  if (verbose) cat("starting calcTCSAM02Params.SurveyQs()\n")
  prs<-NULL;
  if (inherits(obj,"tcsam2013.prs"))    prs<-obj;
  if (inherits(obj,"tcsam2013.resLst")) prs<-obj$prs;

  #TCSAM2013 parameter values
  pSrv1_QM<-prs$value[prs$name==" pSrv1_QM"];
  pSrv2_QM<-prs$value[prs$name==" pSrv2_QM"];
  pSrv1_QF<-prs$value[prs$name==" pSrv1_QF"];
  pSrv2_QF<-prs$value[prs$name==" pSrv2_QF"];

  #TCSAM02 parameter values
  pLnQ.1 <- format(log(pSrv1_QM),digits=10);
  pLnQ.2 <- format(log(pSrv2_QM),digits=10);
  pLnQ.3 <- format(log(pSrv1_QF),digits=10);
  pLnQ.4 <- format(log(pSrv2_QF),digits=10);

  if (verbose){
    cat("pLnQ.1 = ",pLnQ.1,"\n")
    cat("pLnQ.2 = ",pLnQ.2,"\n")
    cat("pLnQ.3 = ",pLnQ.3,"\n")
    cat("pLnQ.4 = ",pLnQ.4,"\n")
  }

  tpl<-gsub("&&pLnQ.1",pLnQ.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnQ.2",pLnQ.2,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnQ.3",pLnQ.3,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnQ.4",pLnQ.4,tpl,fixed=TRUE);

  if (verbose) cat("finished calcTCSAM02Params.SurveyQs()\n")
  return(tpl)
}
