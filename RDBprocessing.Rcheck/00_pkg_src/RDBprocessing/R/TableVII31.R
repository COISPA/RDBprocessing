#' TableVII.3.1 - GFCM DCRF datacall
#'
#' @param datacs CS table in RCG format
#' @param verbose boolean. If TRUE a message is printed.
#' @return TableVII31 (Biological information: Size at first maturity)
#' @export
#'
#' @examples TableVII31(RDBprocessing::data_ex)
#'
TableVII31<-function(datacs,verbose=F){

if(FALSE){

    datacs=RDBprocessing::data_ex

    }

    datacs=check_cs_header(datacs)

ML_tab=ML_MEDBS(datacs,verbose=F)
species=unique(ML_tab$SPECIES)

ML_tab=ML_tab[as.character(ML_tab$SPECIES) %in% as.character(species),]
ML_tab_50=ML_tab[ML_tab$PRM<0.60 & ML_tab$PRM>0.4,]

L50=aggregate(ML_tab_50$LENGTHCLASS,
              by=list(ML_tab_50$COUNTRY,ML_tab_50$START_YEAR,ML_tab_50$END_YEAR,
                      ML_tab_50$SPECIES,ML_tab_50$SEX,ML_tab_50$AREA),FUN="mean")

#L50=L50[L50$Group.2<=YEAR & L50$Group.3>=YEAR,]    # selection on the year

#L50$AREA=ML_tab_50$AREA
#L50$YEAR=L50$START_YEAR
L50$Reference = "MEDBS"

#Final=L50[,c(1,8,7,4,5,6,9)]

Final=L50[,c(1,2,6,4,5,7,8)]

colnames(Final)=c("Country","Reference_year","GSA","Species","Sex","L50","Reference")

return(Final)

}
