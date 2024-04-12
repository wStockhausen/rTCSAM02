#' 
#' @title Find differences between 'optimal' fits to models
#' @description
#' Function to find differences between 'optimal' fits to models
#' 
#' @param o - category object from original model.fit object
#' @param n - category object from model.fit object for comparison
#' @param str - character string 
#' @param type - value for comparisons: 'objfun' or 'nll' 
#' @param print - flag to print 'critical' differences
#' 
#' @return data.frame with elements:
#' \itemize{
#'  \item{diff - difference in values} 
#'  \item{str - string indicating category, value, and difference}
#' }
#' 
#' @details
#' Example: \cr
#' o: model.fits list object element (e.g., an element from the list from source('ModelFits.init.R'))\cr
#' n: corresponding  model.fits list object element for comparison\cr
#' #--loop over model fit categories and compare differences in 'objfun'\cr
#' for (ctg in names(o)) \cr
#'  findOptDiffByCat(o[[ctg]],n[[ctg]],ctg,type="objfun",diffcrit=10);\cr
#' 
#' @export
#' 
findOptDiffByCat<-function(o,n,str='',type='objfun',diffcrit=10,print=FALSE){
  #--cat("diffcrit =",diffcrit,"\n")
  lstAll = list();
  nms = names(o);
  if (type %in% nms) {
    strp = paste(str,"\n");
    diff = o[[type]]-n[[type]];
    if (abs(diff)>diffcrit){
      strp = paste(strp,"\t",type,":",o[[type]],n[[type]],"| DIFF =",diff,"\n");
      if (print) cat(strp);
    }
    return(list(diff=diff,str=strp));
  } else {
    for (nm in nms){
      strp = paste0(str,"/",nm);
      rsp = findOptDiffByCat(o[[nm]],n[[nm]],strp,type,diffcrit);
      lstAll[[nm]] = rsp;
    }
  }
  dfrAll = dplyr::bind_rows(lstAll);
  return(invisible(dfrAll));
}
#' 
#' @title Find differences between 'optimal' fits to models across all categories
#' @description
#' Function to find differences between 'optimal' fits to models across all categories
#' 
#' @param o - tcsam02 model.fit list object
#' @param n - tcsam02 model.fit list object for comparison
#' @param type - value for comparisons: 'objfun' or 'nll' 
#' @param diffcrit - difference to consider critical
#' @param showTop - max number of top differences to print
#' 
#' @export
#' 
findOptDiffs<-function(o,n,type='objfun',diffcrit=10,showTop=10){
  lstAll = list();
  for (ctg in names(o))
    lstAll[[ctg]] = findOptDiffByCat(o[[ctg]],n[[ctg]],ctg,type=type,diffcrit=diffcrit);
  dfrAll = dplyr::bind_rows(lstAll) |> 
             dplyr::arrange(dplyr::desc(abs(diff)));
  nr = nrow(dfrAll);
  if (min(nr,showTop)>0){
    cat("top",min(nr,showTop),"differences:\n");
    for (r in 1:min(nr,showTop)) cat(dfrAll$str[r]);
  }
  return(invisible(dfrAll));
}







