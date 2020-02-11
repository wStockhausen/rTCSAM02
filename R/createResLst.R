#'
#' @title Create a tcsam02.rep object from components
#'
#' @description Function to create a tcsam02.rep object from components.
#'
#' @param rep - tcsam02 rep object
#' @param prs - tcsam02 prs object
#' @param std - tcsam02 std object
#' @param ofc - tcsam02 ofc object
#' @param verbose - flag (T/F) to print debugging info
#'
#' @return a tcsam02.resLst object
#'
#' @details None.
#'
#' @export
#'
createResLst<-function(rep=NULL,
                       prs=NULL,
                       std=NULL,
                       ofc=NULL,
                       verbose=FALSE){

    if (is.null(ofc)&!is.null(rep)) ofc<-getOFCs(rep,verbose=verbose);

    resLst<-list(rep=rep,prs=prs,std=std,ofc=ofc);
    class(resLst)<-c('tcsam02.resLst',class(resLst));

    return(resLst);
}
