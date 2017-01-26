#'
#' @title Calculate natural mortality parameters for TCSAM02 based on natural mortality rates from TCSAM2013.
#'
#' @description Function to calculate natural mortality parameter values from a TCSAM2013 model for TCSAM02.
#'
#' @param obj - tcsam2013.rep object or tcsam2103.prs object
#' @param tpl - character vector with file template
#' @param verbose - flag to print debugging info
#'
#' @return file template with natural mortality parameter values filled in
#'
#' @details Calculates and returns a list of natural mortality parameter values for a TCSAM02 model equivalent to those from a TCSAM2013 model run.
#'
calcTCSAM02Params.NaturalMortality<-function(obj,tpl,verbose=FALSE){
  if (verbose) cat("starting calcTCSAM02Params.NaturalMortality()\n")
  if (inherits(obj,"tcsam2013.rep")){
    if (verbose) cat("processing tcsam2013.resLst object\n")
    #get TCSAM2013 values
    mdfr<-rTCSAM2013::getMDFR.Pop.NaturalMortality(obj=obj,verbose=verbose);

    idyO<-(mdfr$y==2015)
    idyE<-(mdfr$y==1982)
    idimm<-(mdfr$m=="immature")
    idmat<-(mdfr$m=="mature")
    idM<-(mdfr$x=="male")
    idF<-(mdfr$x=="female")

    #rates
    nmImmAO<-mdfr$val[idyO&idimm&idM];
    nmMatMO<-mdfr$val[idyO&idmat&idM];
    nmMatFO<-mdfr$val[idyO&idmat&idF];
    nmMatME<-mdfr$val[idyE&idmat&idM];
    nmMatFE<-mdfr$val[idyE&idmat&idF];

    nmB<-0.23; #base NM

    #multipliers in TCSAM2013
    nmfImmAO<-nmImmAO/nmB;
    nmfMatMO<-nmMatMO/nmB;
    nmfMatFO<-nmMatFO/nmB;
    nmfMatME<-nmMatME/nmMatMO;
    nmfMatFE<-nmMatFE/nmMatFO;
  } else {
    if (verbose) cat("processing tcsam2013.prs object\n")
    prs<-NULL;
    if (inherits(obj,"tcsam2013.prs"))    prs<-obj;
    if (inherits(obj,"tcsam2013.resLst")) prs<-obj$prs;

    nmB<-0.23; #base NM

    #multipliers in TCSAM2013
    nmfImmAO<-prs$value[prs$name==" pMfac_Imm"];
    nmfMatMO<-prs$value[prs$name==" pMfac_MatM"];
    nmfMatFO<-prs$value[prs$name==" pMfac_MatF"];
    nmfMatMBig<-prs$value[prs$name==" pMfac_Big"][2];
    nmfMatFBig<-prs$value[prs$name==" pMfac_Big"][1];

    #values
    nmImmAO<-nmB*nmfImmAO;
    nmMatMO<-nmB*nmfMatMO;
    nmMatFO<-nmB*nmfMatFO;
    nmMatME<-nmB*nmfMatMO*nmfMatMBig;
    nmMatFE<-nmB*nmfMatFO*nmfMatFBig;
  }

  #calculate associated TCSAM02 values
  #lnM = pLnM + pLnDMT                      applies to immature males
  #lnM = pLnM + pLnDMT + pLnDMX             applies to immature females
  #lnM = pLnM + pLnDMT + pLnDMM             applies to mature crab
  #lnM = pLnM + pLnDMT + pLnDMM  + pLnDMXM  applies to mature female crab

  pLnM   <- log(nmB);                                     #applies to all crab
  pLnDMT <- log(nmImmAO) - pLnM;                          #applies to all crab
  pLnDMX <- 0;                                            #applies to all female crab
  pLnDMM.O <- log(nmMatMO) - (pLnM + pLnDMT);             #applies to all mature crab
  pLnDMXM.O <- log(nmMatFO) - (pLnM + pLnDMT + pLnDMM.O); #applies to mature female crab
  pLnDMM.E <- log(nmMatME) - (pLnM + pLnDMT) ;            #applies to all mature crab
  pLnDMXM.E <- log(nmMatFE) - (pLnM + pLnDMT + pLnDMM.E); #applies to mature female crab

  if (verbose){
    cat("pLnM     : ",pLnM,"\n");
    cat("pLnDMT   : ",pLnDMT,"\n");
    cat("pLnDMX   : ",pLnDMX,"\n");
    cat("pLnDMM.O : ",pLnDMM.O,"\n");
    cat("pLnDMM.E : ",pLnDMM.E,"\n");
    cat("pLnDMXM.O: ",pLnDMXM.O,"\n");
    cat("pLnDMXM.E: ",pLnDMXM.E,"\n");
  }

  pLnM.1<-format(pLnM,digits=10)
  pLnDMT.1<-format(pLnDMT,digits=10)
  pLnDMX.1<-format(0,digits=10)
  pLnDMM.1<-format(pLnDMM.O,digits=10)
  pLnDMM.2<-format(pLnDMM.E,digits=10)
  pLnDMXM.1<-format(pLnDMXM.O,digits=10)
  pLnDMXM.2<-format(pLnDMXM.E,digits=10)

  tpl<-gsub("&&pLnM.1",pLnM.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnDMT.1",pLnDMT.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnDMX.1",pLnDMX.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnDMM.1",pLnDMM.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnDMM.2",pLnDMM.2,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnDMXM.1",pLnDMXM.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnDMXM.2",pLnDMXM.2,tpl,fixed=TRUE);

  if (verbose) cat("finished calcTCSAM02Params.NaturalMortality()\n")
  return(tpl)
}
