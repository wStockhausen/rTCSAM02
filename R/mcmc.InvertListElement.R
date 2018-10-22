#'
#' @title Invert a list of mcmc results for a numeric element to a dataframe
#'
#' @description Function to invert a list of mcmc results for a numeric element to a dataframe.
#'
#' @param mcmc - list of mcmc results
#' @param el - name of an element that is a number to extract, as character string
#' @param verbose - flag (T/F) to print diagositc info
#'
#' @details Throws an error if "el" is not an element of the mcmc results list.
#'
#' @return a dataframe
#'
#' @export
#'
mcmc.InvertNumberElement<-function(mcmc,el,verbose=FALSE){
  tst<-mcmc[[1]][[el]];
  if ((!is.numeric(tst))|(length(tst)!=1)){
    str <- paste0("MCMC list element '",el,"' is mode '",mode(tst),"' of length ",length(tst),".\n",
                  "It should be a single numerical value.\n")
    stop(str);
  }
  n<-length(mcmc);
  x<-vector("numeric",n);
  x[1:n]<-NA;
  for (i in 1:n) {if (!is.null(mcmc[[i]][[el]])) x[i]<-mcmc[[i]][[el]];}
  xdfr <- cbind(mcmc=1:n,variable=el,reshape2::melt(x));
  return(xdfr);
}

#'
#' @title Invert a list of mcmc results for a numeric vector element to a dataframe
#'
#' @description Function to invert a list of mcmc results for a numeric vector element to a dataframe.
#'
#' @param mcmc - list of mcmc results
#' @param el - name of an element that is a numeric vector with length >1 to extract, as character string
#' @param verbose - flag (T/F) to print diagositc info
#'
#' @details Throws an error if "el" is not an element of the mcmc results list, if el is not numeric, and if
#' length(el) is not > 1.
#'
#' @return a dataframe
#'
#' @export
#'
mcmc.InvertVectorElement<-function(mcmc,el,verbose=FALSE){
  tst<-mcmc[[1]][[el]];
  if ((!is.vector(tst))|(!length(tst)>1)){
    str <- paste0("MCMC list element '",el,"' is mode '",mode(tst),"' of length ",length(tst),".\n",
                  "It should be a numerical vector with more than 1 value.\n")
    stop(str);
  }
  n<-length(mcmc);
  xdfr<-NULL;
  for (i in 1:n) {
    x<-mcmc[[i]][[el]];
    if (!is.null(x)) xdfr<-rbind(xdfr,cbind(mcmc=i,reshape2::melt(x)));
  }
  return(xdfr);
}

#'
#' @title Invert a list of mcmc results for a numeric array element to a dataframe
#'
#' @description Function to invert a list of mcmc results for a numeric array element to a dataframe.
#'
#' @param mcmc - list of mcmc results
#' @param el - name of an element that is a numeric array with length >1 to extract, as character string
#' @param verbose - flag (T/F) to print diagositc info
#'
#' @details Throws an error if "el" is not an element of the mcmc results list, if el is not an array, and if
#' length(el) is not > 1.
#'
#' @return a dataframe
#'
#' @export
#'
mcmc.InvertArrayElement<-function(mcmc,el,verbose=FALSE){
  tst<-mcmc[[1]][[el]];
  if ((!is.array(tst))|(!length(tst)>1)){
    str <- paste0("MCMC list element '",el,"' is mode '",mode(tst),"' of length ",length(tst),".\n",
                  "It should be a numerical array with more than 1 value.\n")
    stop(str);
  }
  n<-length(mcmc);
  xdfr<-NULL;
  for (i in 1:n) {
    x<-mcmc[[i]][[el]];
    if (!is.null(x)) xdfr<-rbind(xdfr,cbind(mcmc=i,reshape2::melt(x)));
  }
  return(xdfr);
}

#'
#' @title Invert an element of a sublist list
#'
#' @description Function to invert an element of an mcmc list
#'
#' @param mcmc - an mcmc results list from a TCSAM02 model run
#' @param element - name of a list element to extract
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
    xdfr<-mcmc.InvertNumberElement(mcmc,element,verbose=verbose);
    return(xdfr);
  } else if (cle=='list'){
    if (verbose) cat("\tinverting list element '",element,"'\n",sep="")
    nms<-names(mcmc[[1]][[element]]);
    cle<-class(mcmc[[1]][[element]][[nms[1]]]);
    if (cle=="numeric"){
      for (nm in nms){
        if (verbose) cat("\t\tinverting numeric element '",element,"[",nm,"]'\n",sep="")
        x<-vector("numeric",n);
        x[1:n]<-NA;
        for (i in 1:n) {if (!is.null(mcmc[[i]][[element]][[nm]])) x[i]<-mcmc[[i]][[element]][[nm]];}
        xlst[[nm]]<-x;
      }
    }
  } else {
    if (verbose) {
      if (element %in% names(mcmc[[1]])){
        cat("--ERROR: Class of element '",element,"' is '",cle,"' but should be 'numeric' or 'list'.\n",sep="")
      } else {
        cat("--ERROR: '",element,"' is not a member of the mcmc list.\n",sep="");
      }
    }
    xlst<-NULL;
  }
  return(xlst);
}

