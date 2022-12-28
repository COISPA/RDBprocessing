#' Sex ratio at length (SRL) table - MED & BS data call
#' @param data Detailed data in RCG CS format
#' @return SRL table
#' @export
#' @examples SRL_MEDBS(RDBprocessing::data_ex)
#' @importFrom stats aggregate
SRL_MEDBS<-function(data) {

    header=c("COUNTRY","AREA","START_YEAR","END_YEAR","SPECIES","LENGTHCLASS","UNIT","SEX_RATIO","COMMENTS")

    data=data[-which(data$Sex %in%c(NA,-1,0,"","U","C","N","I")),]

    data$fem=ifelse(data$Sex== "F",1,0)

    data2=merge(data,RDBprocessing::Annex17,by.x="Species",by.y="Scientific_name") #

    data=data2 #[-which(as.character(data2$Species.y)==""),]

    matrix=aggregate(data$Number_at_length,by=list(data$Flag_country, data$Year,data$Area,data$Species.y,data$Length_class,data$fem,data$Length_code),FUN="sum")

    colnames(matrix)=c("COUNTRY", "START_YEAR","AREA","SPECIES","LENGTHCLASS","fem","UNIT","SAMPLE_SIZE")
    fem=matrix[matrix$fem==1,]
    mal=matrix[matrix$fem==0,]

    tab1=merge(fem,mal,by=c("COUNTRY", "START_YEAR","AREA","SPECIES","LENGTHCLASS","UNIT") )
    tab1=tab1[,c(1:6,8,10)]
    colnames(tab1)[c(7,8)]=c("fem","mal")

    tab1=merge(tab1,RDBprocessing::Annex17,by.x="SPECIES",by.y="Species") #


    tab1$LENGTHCLASS=ifelse(tab1[,10]=="cm",tab1$LENGTHCLASS/10,tab1$LENGTHCLASS)
    tab1$LENGTHCLASS=trunc(tab1$LENGTHCLASS)
    colnames(tab1)[10]="UNIT"

    tab1=aggregate(c(tab1[,c(7,8)]),by=list(tab1$SPECIES,tab1$COUNTRY, tab1$START_YEAR, tab1$AREA, tab1$LENGTHCLASS,tab1$UNIT),FUN="sum")
    colnames(tab1)=c("SPECIES","COUNTRY","START_YEAR","AREA","LENGTHCLASS","UNIT","fem","mal")
    tab1$SEX_RATIO=tab1$fem/(tab1$fem+tab1$mal)
    #tab1$SAMPLE_SIZE=(tab1$imm+tab1$mat)

    SRL=tab1[,c(2,4,3,3,1,5,6,9)]

    SRL$COMMENTS="Observed F/(F+M)"
    colnames(SRL)[4]="END_YEAR"

    return(SRL)

}
