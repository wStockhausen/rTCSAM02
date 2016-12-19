#'
#'@title Create a tcsam02.resLst object from a model run
#'
#'@description Function to create a tcsam02.resLst object from a model run.
#'
#'@param inp.dir - a folder with model output
#'@param rep - filename to read for rep object
#'@param model - name of model executable
#'@param prsType -  type ('all' or 'active') of parameters to read
#'@param verbose - flag (T/F) to prnt diagnostic info
#'
#'@return a tcsam02.resLst object.
#'
#'@details Uses \code{tcltk::tk_chose.dir} to open a file dialog to select model directory
#'if inp.dir is NULL. A tcsam02.resLst object is a list with elements
#'\itemize{
#'  \item{rep - a tcsam02.rep object, or NULL}
#'  \item{prs - a tcsam02.prs object, or NULL}
#'  \item{std - a tcsam02.std object, or NULL}
#'  \item{ofc - a tcsam02.ofc object, or NULL}
#'}
#'
#'@export
#'
getResLst<-function(inp.dir=NULL,
                    rep='tcsam02.rep',
                    model='tcsam02',
                    prsType=c('all','active'),
                    verbose=FALSE){
    if (is.null(inp.dir)){
        inp.dir<-tcltk::tk_choose.dir(caption="Select folder with model run");
    }

    cat("rTCSAM02::getResLst(): Reading from folder:\n\t'",inp.dir,"'.\n",sep='');
    if (!dir.exists(inp.dir)) {
        cat("Warning from getResLst(...).\n");
        cat("--The following folder does not exist:\n\t'",inp.dir,"'\n",sep='');
        cat("--Returning NULL.\n")
        return(NULL);
    }

    prs<-getPrs(inp.dir=inp.dir,type=prsType[1],verbose=verbose);
    rep<-getRep(repFile=file.path(inp.dir,rep),verbose=verbose);
    std<-getStd(stdFile=file.path(inp.dir,paste0(model,".std")),verbose=verbose);
    ofc<-getOFCs(rep,verbose=verbose);

    resLst<-list(rep=rep,prs=prs,std=std,ofc=ofc);
    class(resLst)<-c('tcsam02.resLst',class(resLst));

    if (verbose) cat("rTCAM02:getResLst(): Done!\n")
    return(resLst);
}
