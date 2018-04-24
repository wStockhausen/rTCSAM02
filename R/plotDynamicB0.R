#'
#' @title Plot dyanmic B0 results
#'
#' @description Fuction to plot dynamic B0 details.
#'
#' @param obj - tcsam.resLst, tcsam.rep, or list object
#' @param B100 - B100 from OFL calculation
#'
#' @return ggplot object
#'
#' @details Extracts the dynamic B0 results from a TCSAM02 model run and plots
#' time series of dynamic B0 and MMB for comparison.
#'
#' @export
#'
plotDynamicB0<-function(obj,B100=NULL){
  if (inherits(obj,what="tcsam02.resLst")){
    res<-obj$rep$res;
    if (!is.null(obj$rep$ptrOFLResults)) B100<-obj$rep$ptrOFLResults$B100;
  } else if (inherits(obj,what="tcsam02.rep")){
    res<-obj$res;
    if (!is.null(obj$ptrOFLResults)) B100<-obj$ptrOFLResults$B100;
  } else if (class(obj)=="list"){
    res<-obj;
  } else {
    str<-"\n--'obj' not recognized.\n";
    str<-paste0(str,"--obj is class ",class(obj),"\n")
    str<-paste0(str,"--but should be tcsam.resLst, tcsam.rep, or list object.\n")
    stop(str);
  }
  dfr0<-as.data.frame(res$MB_yx);
  dfr0$year<-row.names(dfr0);
  dfr0$type<-"MMB";
  dfr1<-as.data.frame(res$dB0_yx);
  dfr1$year<-row.names(dfr1);
  dfr1$type<-"dynamic B0";
  dfr<-rbind(dfr0,dfr1);
  require(ggplot2);
  row.names(dfr)<-NULL;
  dfr$year<-as.numeric(dfr$year);
  p <- ggplot(data=dfr,mapping=aes_string(y="male",x="year",colour="type"))+geom_line();
  if (!is.null(B100)) p <- p + geom_hline(yintercept=B100,linetype=2)
  p <- p + labs(y="MMB (thousands t)");

  return(p)
}
