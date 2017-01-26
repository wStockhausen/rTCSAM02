#'
#' @title Calculate parameter values from a TCSAM2013 model for TCSAM02
#'
#' @description Function to calculate parameter values from a TCSAM2013 model for TCSAM02.
#'
#' @param obj - tcsam2013.resLst object or tcsam2103.prs object
#' @param template - file name of TCSAM02 parameters info template
#' @param out - file name for output file
#' @param verbose - flag to print debugging info
#'
#' @return character string written to a file
#'
#' @details Calculates and exports parameter values for a TCSAM02 model equivalent to those from a TCSAM2013 model run.
#'
#' @export
#'
calcTCSAM02Params<-function(obj,template,out="Model.ParametersInfo.dat",verbose=TRUE){
  prs<-NULL;
  if (inherits(obj,"tcsam2013.prs"))    prs<-obj;
  if (inherits(obj,"tcsam2013.resLst")) prs<-obj$prs;

  con<-file(template,open="r");
  tpl<-readLines(con=con);
  close(con);
  #-------------------------------
  # Recruitment parameters
  #-------------------------------
  pLnR.1<-format(prs$value[prs$name==" pMnLnRecInit"],digits=10)
  pLnR.2<-format(prs$value[prs$name==" pMnLnRec"    ],digits=10)
  pLnRCV<-format(log(0.5),digits=10);
  pLgtRX<-format(0,digits=10)
  pLnRa<-format(log(prs$value[prs$name==" pRecAlpha"]),digits=10)
  pLnRb<-format(log(prs$value[prs$name==" pRecBeta" ]),digits=10)
  pDevsLnR.1<-format(prs$value[prs$name==" pRecDevsHist"],digits=10)
  pDevsLnR.2<-format(prs$value[prs$name==" pRecDevs"    ],digits=10)
  #print(pLnR.1,pLnR.2,pLnRCV,pLgtRX,pLnRa,pLnRb,pDevsLnR.1,pDevsLnR.2);
  tpl<-gsub("&&pLnR.1",pLnR.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnR.2",pLnR.2,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnRCV",pLnRCV,tpl,fixed=TRUE);
  tpl<-gsub("&&pLgtRX",pLgtRX,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnRa",pLnRa,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnRb",pLnRb,tpl,fixed=TRUE);
  tpl<-gsub("&&pDevsLnR.1",paste(pDevsLnR.1,collapse=" "),tpl,fixed=TRUE);
  tpl<-gsub("&&pDevsLnR.2",paste(pDevsLnR.2,collapse=" "),tpl,fixed=TRUE);

  #-------------------------------
  # Natural mortality parameters
  #-------------------------------
  tpl<-calcTCSAM02Params.NaturalMortality(prs,tpl,verbose);
  #-------------------------------
  # Growth parameters
  #-------------------------------
  pLnGrA.1<-format(prs$value[prs$name==" pGrAM1"],digits=10)
  pLnGrA.2<-format(prs$value[prs$name==" pGrAF1"],digits=10)
  pLnGrB.1<-format(prs$value[prs$name==" pGrBM1"],digits=10)
  pLnGrB.2<-format(prs$value[prs$name==" pGrBF1"],digits=10)
  pLnGrBeta<-format(0.750005,digits=10)
  tpl<-gsub("&&pLnGrA.1",pLnGrA.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnGrA.2",pLnGrA.2,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnGrB.1",pLnGrB.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnGrB.2",pLnGrB.2,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnGrBeta",pLnGrBeta,tpl,fixed=TRUE);
  #-------------------------------
  # Maturity parameters
  #-------------------------------
  pvLgtPrM2M.1<-format(prs$value[prs$name==" pPrM2MM"],digits=10)
  pvLgtPrM2M.2<-format(prs$value[prs$name==" pPrM2MF"],digits=10)
  tpl<-gsub("&&pvLgtPrM2M.1",paste(pvLgtPrM2M.1,collapse=" "),tpl,fixed=TRUE);
  tpl<-gsub("&&pvLgtPrM2M.2",paste(pvLgtPrM2M.2,collapse=" "),tpl,fixed=TRUE);
 #-------------------------------
  # Selectivity parameters
  #-------------------------------
  tpl<-calcTCSAM02Params.SelFcns(obj,tpl,verbose);
  #-------------------------------
  # Fishery parameters
  #-------------------------------
  tpl<-calcTCSAM02Params.FisheryCs(obj,tpl,verbose);
  #-------------------------------
  # Survey parameters
  #-------------------------------
  tpl<-calcTCSAM02Params.SurveyQs(obj,tpl,verbose);

  if (verbose) cat(tpl,sep="\n")

  if (!is.null(out)) writeLines(tpl,con=out);

  return(tpl);
}

