#'
#' @title Find substantially-correlated model parameters, based on the model "cor" file
#'
#' @description Function to find substantially-correlated model parameters, based on the model "cor" file.
#'
#' @param dfr - dataframe from a call to \link{\code{readCorFile}}, or NULL
#' @param folder - folder containing the model run (if dfr is NULL)
#' @param model - model name (default is "tcsam02")
#' @param minLevel - minimum correlation level to use to report parameters
#'
#' @return a dataframe
#'
#' @details This function extracts "substantially"-correlated parameters (ultimately) from the model
#' "cor" file, where "substantial" is based on the value of minLevel.
#' A dataframe with the parameter names and correlation values is returned.
#'
#' @export
#'
getCorr<-function(dfr=NULL,folder=NULL,model="tcsam02",minLevel=0.95){
  if (is.null(dfr)) dfr <- readCorFile(folder=folder,model=model);

  dfr.cor <- dfr[(dfr1$p_i!=dfr1$p_j)&(abs(dfr$val)>=minLevel),c("p_i","p_j","val")];

  return(dfr.cor);
}
