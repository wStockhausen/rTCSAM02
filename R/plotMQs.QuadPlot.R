#'
#'@title Make a quad plot of F vs. B over time
#'
#'@description Function to make a quad plot of F vs. B over time.
#'
#'@param tcsams - a named list of tcsam02.resLst objects
#'@param dfrMQs - dataframe with Fmsy, Bmsy, Fofl and OFL by case
#'@param aYr - assessment year (default: 2022)
#'@param min_year - minimum year to include in plot (default: 1980)
#'
#'@return cowplot grid graphics object
#'
#'@details The standard quad plot is faceted by model case. One possibility for
#'obtaining a suitable dfrMQs is output from [rTCSAM02::sdRep.CalcABC()].
#'
#'@import magrittr
#'@import dplyr
#'@import ggplot2
#'@import cowplot
#'@import wtsPlots
#'
#'@export
#'@md
#'
plotMQs.QuadPlot<-function(tcsams,
                           dfrMQs,
                           aYr=2022,
                           min_year=1980){
  cases = names(tcsams);

  lst1 = list();
  for (case_ in cases){
    #--testing: case_ = cases[1];
    tcsam = tcsams[case_];
    dirFsh = tcsam[[1]]$rep$mc$dims$f$nms[1];#--name of directed fishery
    dfrFs = rTCSAM02::getMDFR.Fisheries.Catchability(tcsam,cast="x") %>%
              dplyr::filter(fleet==dirFsh,x=="male",y>=min_year);
    dfrBs = rTCSAM02::getMDFR.Pop.Quantities(tcsam,"MB_yx") %>%
              dplyr::filter(x=="male",y>=min_year);
    tmp = tibble::tibble(case=case_,y=dfrFs$y,F=dfrFs$val,y1=dfrBs$y,B=dfrBs$val);
    lst1[[case_]] = tmp;
  }
  dfrFBs = dplyr::bind_rows(lst1);
  dfrFBs %<>% dplyr::mutate(period=ifelse(y<2005,paste0(10*floor(as.numeric(y)/10),"'s"),"PR (2005)"));

  mxF = max(dfrFBs$F);
  mxB = max(dfrFBs$B);
  alpha<-0.1
  beta<-0.25
  r1 = c(0,beta);
  r2 = c(beta,1);
  lst2 = list();
  for (case_ in cases){
    #--testing: case_ = cases[1];
    rwMQs = dfrMQs %>% dplyr::filter(case==case_);
    Fmsy  = rwMQs$Fmsy;
    Bmsy  = rwMQs$Bmsy;
    F1 = 0*r1;                      B1 = Bmsy*r1;         #--lower flat
    F2 = Fmsy*(r2-alpha)/(1-alpha); B2 = Bmsy*r2;         #--slope
    F3 = Fmsy*c(1,1);               B3 = c(Bmsy,1.1*mxB); #--upper flat
    dfrCR = tibble::tibble(case=case_,
                           F=c(F1,F2,F3),
                           B=c(B1,B2,B3));
    lst2[[case_]] = dfrCR;
  }
  dfrCR = dplyr::bind_rows(lst2);

  p =  ggplot(dfrFBs) +
          geom_point(mapping=aes(x=B,y=F,colour=period));
  lg = cowplot::get_legend(p);#--get the legend from simple plot
  p =  ggplot(dfrFBs) +
          geom_hline(data=dfrMQs,mapping=aes(yintercept=Fmsy),linetype=2,colour="light blue") +
          geom_vline(data=dfrMQs,mapping=aes(xintercept=Bmsy),linetype=2,colour="light blue") +
          geom_hline(yintercept=0,linetype=1,colour="black") +
          geom_vline(data=dfrMQs,mapping=aes(xintercept=0.5*Bmsy),linetype=2,colour="orange") +
          geom_vline(data=dfrMQs,mapping=aes(xintercept=beta*Bmsy),linetype=2,colour="red") +
          geom_ribbon(data=dfrCR,mapping=aes(x=B,ymax=F),ymin=0,fill="grey",colour="black") +
          geom_point(mapping=aes(x=B,y=F,colour=period)) +
          geom_text(mapping=aes(x=B,y=F,label=y,colour=period),hjust="left",guide="none") +
          geom_point(data=dfrMQs,mapping=aes(x=PrjB,y=Fmsy),size=3) +
          geom_text(data=dfrMQs,mapping=aes(x=PrjB,y=Fmsy,label=aYr)) + #NOTE: should be Fofl!
          scale_x_continuous(limits=c(0,1.05*mxB),oob=scales::squish,expand=c(0,0)) +
          scale_y_continuous(limits=c(0,1.05*mxF),oob=scales::squish,expand=c(0,0)) +
          labs(x="MMB (1,000's t)",y="Fully-selected fishing mortality rate (per year)") +
          facet_grid(case~.) +
          wtsPlots::getStdTheme() +
          theme(legend.position="none");
  pg = cowplot::plot_grid(p,lg,nrow=1,rel_widths=c(6,1));

  return(pg);
}
