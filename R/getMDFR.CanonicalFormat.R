#'
#'@title Convert dataframe to canonical format
#'
#'@description Function to convert dataframe to canonical format.
#'
#'@param mdfr - dataframe to convert
#'
#'@return dataframe in canonical format, or NULL if input was not a dataframe
#'
#'@details returns a dataframe in canonical format
#'\itemize{
#'  \item{case - model case name (scenario)}
#'  \item{process - 'population','fisheries',or 'surveys'}
#'  \item{fleet - fishery or survey "fleet" name}
#'  \item{category - data category (e.g., "index", "captured", etc.)}
#'  \item{type - 'observed' or 'predicted',or ''}
#'  \item{pc - parameter combination index, or ''}
#'  \item{y - year or 'all'}
#'  \item{x - sex or 'all'}
#'  \item{m - maturity state or 'all'}
#'  \item{s - shell condition or 'all'}
#'  \item{z - size or 'all'}
#'  \item{zp - size, [only for growth increments and growth transition matrices]}
#'  \item{val - value}
#'  \item{lci - lower ci}
#'  \item{uci - upper ci}
#'}
#'
#'@export
#'
getMDFR.CanonicalFormat<-function(mdfr){
    options(stringsAsFactors=FALSE);
    if (!is.null(mdfr)&&inherits(mdfr,"data.frame")&&nrow(mdfr)>0){
        #check existing columns and add missing ones
        nms<-names(mdfr);
        if (!('case' %in% nms))     mdfr[['case']]    <-"";
        if (!('process' %in% nms))  mdfr[['process']] <-"";
        if (!('fleet' %in% nms))    mdfr[['fleet']]   <-"";
        if (('f' %in% nms)) {
            #convert fisheries to fleets
            mdfr$process<-"fisheries";
            mdfr$fleet<-mdfr$f;
        }
        if (('v' %in% nms)){
            #convert surveys to fleets
            mdfr$process<-"surveys";
            mdfr$fleet<-mdfr$v;
        }
        if (!('category' %in% nms)) mdfr[['category']]<-"";
        if (!('type' %in% nms))     mdfr[['type']]    <-"";
        if (!('pc' %in% nms))       mdfr[['pc']]      <-"";
        if (!('y' %in% nms)) mdfr[['y']]<-"all";
        if (!('x' %in% nms)) mdfr[['x']]<-"all";
        if (!('m' %in% nms)) mdfr[['m']]<-"all";
        if (!('s' %in% nms)) mdfr[['s']]<-"all";
        if (!('z' %in% nms)) mdfr[['z']]<-"all";
        if (!('val' %in% nms)) mdfr[['val']]<-NA;
        if ('.' %in% nms) mdfr$val<-mdfr[['.']];
        if (!('zp' %in% nms)) {
            if (!('lci' %in% nms)) mdfr[['lci']]<-NA;
            if (!('uci' %in% nms)) mdfr[['uci']]<-NA;
        }

        #check for fisheries and surveys info
        if (all(grepl("[[:blank:]]",mdfr$process))) {
            mdfr$process<-'population';
            mdfr$fleet<-'';
        }

        #re-order to canconical format
        if ('zp' %in% nms){
            mdfr<-mdfr[,c('case','process','fleet','category','type','pc','y','x','m','s','z','zp','val')]
        } else {
            mdfr<-mdfr[,c('case','process','fleet','category','type','pc','y','x','m','s','z','val','lci','uci')]
        }
    } else {
        mdfr<-NULL;
    }
    return(mdfr);
}
