#'
#'@title Get a TCSAM02 report object (by reading a TCSAM02 model report [.rep] file)
#'
#'@description Function to get a TCSAM02 report object
#'
#'@param repFile - report file from a TCSAM02 model run to source. can be NULL.
#'
#'@return TCSAM02 report object (a list). The returned object will be a list of class 'tcsam02.rep'.
#'
#'@details If \code{repFile} is NULL, the user will be prompted to identify a 
#'TCSAM02 model report file from which to source the results object.
#'The returned object will be a list of class 'tcsam02.rep'.
#'
#'@export
#'
getRep<-function(repFile=NULL){
    rep<-NULL;
    if(is.null(repFile)){
        Filters<-wtsUtilities::addFilter("rep","report files (*.rep)","*.rep",Filters=NULL);
        Filters<-wtsUtilities::addFilter("R","R files (*.R)","*.R",Filters=Filters);
        Filters<-wtsUtilities::addFilter("r*","report files (*.r*)","*.r*",Filters=Filters);
        repFile<-tcltk::tk_choose.files(caption="Select TCSAM2015 R Model Report (.rep or .R) file",
                                 multi=FALSE,filters=Filters);
        if (length(repFile)==0) {
            cat("User canceled file selection!! Returning NULL as model results.\n")
            return(NULL);#user canceled file selection
        }
        strs<-strsplit(repFile,'.',fixed=TRUE);
        n<-length(strs[[1]]);
        if (tolower(strs[[1]][n])!="rep"){
            cat("The file '",repFile,"'\n",
                "\tdoes not appear to be a TCSAM02 model report file.\n",
                "\tTCSAM02 report files have extension '.rep'.\n",
                "\tReturning NULL.\n",sep="");
            return(NULL);
        }
    }
    if (file.exists(repFile)){
        cat("Reading model report from file:\n",repFile,"\n")
        source(repFile,local=TRUE);
        if(!any(names(rep)=='mc')){
                cat("The file '",repFile,"'\n",
                    "\tdoes not appear to be a TCSAM02 model report file.\n",
                    "\tTCSAM02 results files are R lists, with 'mc' as the first element.\n",
                    "\tReturning NULL.\n",sep="");
                return(NULL);
        }
        class(rep)<-c('tcsam02.rep',class(rep));#set class attribute to 'tcsam2015.rep' for identification
    } else {
        cat('\tFile "',repFile,'" does not exist.\n\tReturning NULL\n',sep='');
    }
    return(invisible(rep));
}
