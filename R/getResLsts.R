#'
#'@title Create a list of tcsam02.resLst objects, from a set of model runs
#'
#'@description Function to create a list of tcsam02.resLst objects from a set of model runs.
#'
#'@param inp.dirs - a dataframe with columns "case" and "path"
#'@param rep - filename to read for rep object
#'@param model - name of model executable
#'@param prsType -  type ('all' or 'active') of parameters to read
#'
#'@return a list of tcsam02.resLst objects.
#'
#'@details Uses \code{getResLst} to open a folders with model runs listed in inp.dirs.
#'
#'@export
#'
getResLsts<-function(inp.dirs=NULL,
                     rep='tcsam02.rep',
                     model='tcsam02',
                     prsType=c('all','active')){
    if (is.null(inp.dirs)){
        cat("Warning from getResLsts(...).\n");
        cat("--The dataframe inp.dirs cannot be NULL.\n",sep='');
        cat("--Returning NULL.\n")
        return(NULL);
    }

    cases<-inp.dirs$case;
    paths<-inp.dirs$path;
    nr<-nrow(inp.dirs);
    lst<-list();
    for (r in 1:nr){
        resLst<-getResLst(paths[r],rep,model,prsType[1]);
        if (!is.null(resLst)){
            lst[[cases[r]]]<-resLst;
            resLst<-NULL;
        }
    }

    return(lst);
}
