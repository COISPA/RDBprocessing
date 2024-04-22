
#' Table II.2  - GFCM DCRF data call
#'
#' @param CatchFDI Catch table FDI
#' @param verbose boolean. If TRUE a message is printed.
#' @return Table II.2 GFCM DCRF table
#' @export
#'
#' @examples TabII2_GFCM(RDBprocessing::CATCH_FDIex)
#'@import reshape
#'@import reshape2
#'@import dplyr


TabII2_GFCM <- function(ACatch,verbose=FALSE) {

    if(FALSE){
    #ACatch =CATCH_MEDBS(RDBprocessing::data_ex,RDBprocessing::data_exampleCL,RDBprocessing::ce_example,verbose=F)
    ACatch=RDBprocessing::CATCH_FDIex

    }

    species=unique(ACatch$SPECIES)

#ACatch=ACatch[as.character(ACatch$SPECIES) %in% as.character(species[,1]),]

    #ACatch=ACatch[,c(2:9,11:13)]

ACatch_L=aggregate(ACatch$TOTWGHTLANDG,by=list(ACatch$COUNTRY, ACatch$YEAR,
                                           ACatch$VESSEL_LENGTH, ACatch$GEAR_TYPE,
                                           ACatch$TARGET_ASSEMBLAGE, ACatch$SUB_REGION, ACatch$SPECIES),FUN="sum")

class(ACatch$DISCARDS)="numeric"
#ACatch$DISCARDS[ACatch$DISCARDS=="NK"]=0

ACatch_D=aggregate(ACatch$DISCARDS,by=list(ACatch$COUNTRY, ACatch$YEAR,
                                           ACatch$VESSEL_LENGTH, ACatch$GEAR_TYPE,
                                           ACatch$TARGET_ASSEMBLAGE, ACatch$SUB_REGION, ACatch$SPECIES),FUN="sum",na.rm=F)


Merge=merge(ACatch_L,ACatch_D,by=c("Group.1","Group.2","Group.3","Group.4","Group.5",
                                   "Group.6","Group.7"))

colnames(Merge) =c("COUNTRY", "YEAR", "VESSEL_LENGTH","GEAR", "FISHERY",
                   "AREA", "SPECIES","LANDINGS","DISCARDS")
Merge$DISCARDS[Merge$DISCARDS<0]=NA
Merge$GFCM_fleetsegment=as.character(Merge$GEAR)
Merge$GFCM_fleetsegment=""

CT=RDBprocessing::CT

for (i in 1:nrow(Merge)){
    if (nrow(CT[as.character(CT$LOA)== as.character(Merge$VESSEL_LENGTH[i]) &
                as.character(CT$GEAR_ACatch)== as.character(Merge$GEAR[i]) &
                as.character(CT$FISHERY_ACatch)== as.character(Merge$FISHERY[i]),])>0) {

        Merge$GFCM_fleetsegment[i]= as.character(CT[as.character(CT$LOA)==
                                                        as.character(Merge$VESSEL_LENGTH[i]) &
                                                        as.character(CT$GEAR_ACatch)== as.character(Merge$GEAR[i]) &
                                                        as.character(CT$FISHERY_ACatch)==
                                                        as.character(Merge$FISHERY[i]) ,]$Fleet_segment)
    } else {
        Merge$GFCM_fleetsegment[i]=""}
}

Merge_noempty=Merge[Merge$GFCM_fleetsegment!="",]

Merge_noempty_L=aggregate(Merge_noempty$LANDINGS,by=list(Merge_noempty$COUNTRY,
                                                         Merge_noempty$YEAR, Merge_noempty$GFCM_fleetsegment,
                                                         Merge_noempty$SPECIES),FUN="sum")
Merge_noempty_D=aggregate(Merge_noempty$DISCARDS,by=list(Merge_noempty$COUNTRY,
                                                         Merge_noempty$YEAR, Merge_noempty$GFCM_fleetsegment,
                                                         Merge_noempty$SPECIES),FUN="sum")

Merge=merge(Merge_noempty_L,Merge_noempty_D,by=c("Group.1","Group.2","Group.3",
                                                 "Group.4"))
Merge$GSA=ACatch$SUB_REGION[1]

Merge=Merge[,c(1,2,7,3,4,5,6)]

Merge$Catch= rowSums(data.frame(col1=Merge[,6],col2=Merge[,7]),na.rm=T)
colnames(Merge) =c("Country","Reference_year","GSA","Fleet_segment","Species",
                   "Total_landing_per_species_(tons)","Total_discards_per_species_(tons)",
                   "Total_catch_per_species")

return(Merge)

}
