#'
#' @title Parse a TCSAM02 calcProjections.mcmc.R file
#'
#' @description Function to parse a calcProjections.mcmc.R file and return a list of lists
#' of mcmc projection results.
#'
#' @param path - path to the calcProjections.mcmc.R file to parse
#' @param verbose - flag to print extra information
#'
#' @details Uses [rTCSAM02::mcmc.ReadMCMC()] to read the file
#'
#' @return list of individual mcmc projection results lists
#'
#' @export
#'
mcmc.ParseMCMC.Projections<-function(path,
                                    verbose=FALSE){
    fn = file.path(path,"calcProjections.mcmc.R");
    mcmc = rTCSAM02::mcmc.ReadMCMC(fn,verbose);
    return(mcmc);
}
