#'
#'@title Create a TCSAM2015 par object from a par file.
#'
#'@description Function to create a TCSAM2015 par object from a par file.
#'
#'@param in.par = filename of par file
#'
#'@return a prs object (a dtaframe of class 'tcsam2015.par') corresponding to the par file (or NULL if file does not exist)
#'
#'@details Uses \code{wtsUtilities::selectFile} to open a file dialog if in.par is NULL.
#' 
#'@export
#' 
readParFile<-function(in.par=NULL){
    if (is.null(in.par)){
        in.par<-wtsUtilities::selectFile(ext='par',caption="Select par file");
    }
    if (!file.exists(in.par)) return(NULL);
    
    r1<-readLines(con=in.par);
    
    #parse first line
    str <- strsplit(r1[1],'[[:blank:]]');#split based on blanks
    num <- as.numeric(str[[1]]);         #convert to numeric, non-numerics will be NAs
    objfun <- num[!is.na(num)];          #extract numeric elements (num par, obj fun, max gradient)
    names(objfun)<-c('number of parameters','objective function','max gradient');
    dfr<-data.frame(name=names(objfun),value=objfun,stringsAsFactors=FALSE)
    
    #parse remaining lines
    nr<-length(r1);
    for (r in 1:((nr-1)/2)){
        nam  <- gsub('[^[:alnum:]_]','',r1[2*r],perl=TRUE);#get parameter name
        str  <- gsub('[^[:digit:][:blank:].]','',r1[2*r+1],perl=TRUE);#remove all characters except numbers and blanks
        str  <- str<-strsplit(str,'[[:blank:]]');#split str based on blanks
        str  <- str[[1]][str[[1]]!=''];#remove empty array elements
        valu <- as.numeric(str);
        nams <- nam;
        if (length(valu)>1) nams <- paste(nam,'[',1:length(valu),']',sep='');
        dfr <- rbind(dfr,data.frame(name=nams,value=valu,stringsAsFactors=FALSE));
    }
    
    class(dfr)<-c("tcasm2015.par",class(dfr));
    
    return(dfr)
}
