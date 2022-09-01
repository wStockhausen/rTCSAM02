#'
#'@title Function to calculate ABCs from sd_report output
#'
#'@description This function calculates ABCs from TCSAM02 sd_report output.
#'
#' @param tcsams - a named list of tcsam02.resLst objects
#' @param pstar - pstar value for ABC calculation
#' @param buffer - buffer for ABC as fraction (i.e., 0.20 = 20 \% yields ABC = 0.8*OFL)
#' @param doPlot - flag (T/F) to create plot
#' @param xlims - range for x-axis in plot
#' @param title - title for plot
#' @param verbose flag to print diagnostic info
#'
#' @return dfrABCs - dataframe with MLE management quantities, stdvOFL, ABC.pstar and ABC.buff for each case
#'
#' @details Use [rTCSAM02::sdRep.plotABC()] to produce ABC plots for each case
#'
#' @import magrittr
#' @import dplyr
#' @import tidyr
#'
#' @export
#' @md
#'
sdRep.CalcABC<-function(tcsams=NULL,
                        pstar=0.49,
                        buffer=0.2,
                        doPlot=TRUE,
                        xlims=NULL,
                        verbose=FALSE){
  if (verbose) cat("Starting sdRep.CalcABC\n");

  cases = names(tcsams);

  dfr  = rTCSAM02::getMDFR.SdRep.DerivedVars(tcsams) %>%
           dplyr::select(case,variable,est,stdv=`std.dev`)
  dfrEsts = dfr %>% dplyr::select(!stdv) %>%
                    tidyr::pivot_wider(names_from="variable",values_from="est");
  dfrSDs = dfr %>% dplyr::select(!est) %>%
                    tidyr::pivot_wider(names_from="variable",values_from="stdv");
  vOFLs = dfrOFLs$est;
  uOFLs = dfrOFLs$stdv;
  vABCs.pstar = stats::qnorm(pstar,mean=vOFLs,sd=uOFLs,lower.tail=TRUE);
  vABCs.buffr = (1-buffer)*vOFLs;

  dfrABCs = dplyr::bind_cols(dfrEsts,
                             ABC.pstar=vABCs.pstar,
                             ABC.buff=vABCs.buffr,
                             stdvOFL=uOFLs);

  if (verbose) cat("Finished sdRep.CalcABC\n")
  return(dfrABCs);
}
