#'
#' @title Invert an element of an mcmc list
#'
#' @description Function to invert an element of an mcmc list
#'
#' @param mcmc - mcmc list
#' @param element - name of element to extract
#' @param verbose - T/F to print diagnostic info
#'
#' @return a list
#'
#' @details None
#'
#' @export
#'
mcmc.InvertListElement<-function(mcmc,element="",verbose=FALSE){
  xlst<-list();
  n<-length(mcmc);
  cle<-class(mcmc[[1]][[element]]);
  if (cle=="numeric") {
    if (verbose) cat("\tinverting numeric element '",element,"'\n")
    x<-vector("numeric",n);
    x[1:n]<-NA;
    for (i in 1:n) {if (!is.null(mcmc[[i]][[element]])) x[i]<-mcmc[[i]][[element]];}
    xlst[["0"]]<-x;
  } else if (cle=='list'){
    cat("\tinverting list element '",element,"'\n")
    nms<-names(mcmc[[1]][[element]]);
    cle<-class(mcmc[[1]][[element]][[nms[1]]]);
    if (cle=="numeric"){
      for (nm in nms){
        if (verbose) cat("\t\tinverting numeric element '",element,"[",nm,"]'\n")
        x<-vector("numeric",n);
        x[1:n]<-NA;
        for (i in 1:n) {if (!is.null(mcmc[[i]][[element]][[nm]])) x[i]<-mcmc[[i]][[element]][[nm]];}
        xlst[[nm]]<-x;
      }
    }
  } else {xlst<-NULL;}
  return(xlst);
}

