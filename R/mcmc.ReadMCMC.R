#'
#' @title Parse TCSAM02 MCMC.R file
#'
#' @description Function to parse a TCSAM02 MCMC.R file and return a list of lists
#' of mcmc results.
#'
#' @param fn - the filename of te MCMC.R file to parse
#' @param verbose - flag to print extra information
#'
#' @details Concatenates text lines up to each ";", parses the resulting string and
#' evaluates the expression.
#'
#' @return list of individual mcmc results lists
#'
#' @export
#'
mcmc.ReadMCMC<-function(fn,
                        verbose=FALSE){
    f<-readLines(fn);
    ids<-grep(";",f);        #look for semicolon, which ends containing list
    nidsm1<-(length(ids)-1);
    if (verbose) cat("Processing",nidsm1,"MCMC list elements.\n")
    eval(parse(text=f[1]));  #evaluates "mcmc<-list();"
    for (i in 1:nidsm1){
      if (verbose) cat("evaluating ",i,"th list element (lines ",ids[i]+1,"to",ids[i+1],").\n")
      txt<-paste0(f[(ids[i]+1):ids[i+1]],collapse="");
      eval(parse(text=txt));
    }
    return(mcmc);
}
