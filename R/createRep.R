#'
#' @title Create a tcsam02.rep object from components
#'
#' @description Function to create a tcsam02.rep object from components.
#'
#' @param mc - model configuration list
#' @param mr - model results list
#' @param model.fits - model fits list
#'
#' @return a tcsam02.rep object
#'
#' @details None.
#'
#' @export
createRep<-function(mc=NULL,mr=NULL,model.fits=NULL){
    res<-list(mc=mc,mr=mr,model.fits=model.fits)
    class(res)<-c('tcsam02.rep',class(res));#set class attribute to 'tcsam02.rep' for identification
    return(res);
}
