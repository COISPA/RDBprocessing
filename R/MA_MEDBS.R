#' Maturity at age (MA) table - MED & BS data call
#' @param data Detailed data in RCG CS format
#' @param imm maturity stages to be considered as immature
#' @param verbose boolean. If TRUE a message is printed.
#' @return MA table
#' @export
#' @examples MA_MEDBS(RDBprocessing::data_ex)
#' @importFrom stats aggregate

MA_MEDBS<-function(data,imm=c("1","2","2a"), verbose = FALSE) {

    data=check_cs_header(data)
    header=c("COUNTRY","AREA","START_YEAR","END_YEAR","SPECIES","SEX","AGECLASS","SAMPLE_SIZE","PRM","METHOD_USED")

    data=data[-which(data$Maturity_Stage %in% c(NA,-1,0,"") | data$Age %in% c(NA,-1,"")),]

    data$mat=ifelse(data$Maturity_Stage %in% imm,0,1)

    data2=merge(data,RDBprocessing::Annex17,by.x="Species",by.y="Scientific_name")

    data=data2 #[-which(as.character(data2$Species.y)==""),]

    matrix=aggregate(data$Number_at_length,by=list(data$Flag_country, data$Year,data$Area,data$Species.y,data$Sex,data$Age,data$mat),FUN="sum")
    colnames(matrix)=c("COUNTRY", "START_YEAR","AREA","SPECIES","SEX","AGECLASS","mat","SAMPLE_SIZE")
    immat=matrix[matrix$mat==0,]
    mat=matrix[matrix$mat==1,]

    tab1=merge(immat,mat,by=c("COUNTRY", "START_YEAR","AREA","SPECIES","SEX","AGECLASS") )
    tab1=tab1[,c(1:6,8,10)]
    colnames(tab1)[c(7,8)]=c("imm","mat")

    tab1=merge(tab1,RDBprocessing::Annex17,by.x="SPECIES",by.y="Species")


   # tab1$LENGTHCLASS=ifelse(tab1[,11]=="cm",tab1$LENGTHCLASS/10,tab1$LENGTHCLASS)
    tab1$AGECLASS=trunc(tab1$AGECLASS)
    #colnames(tab1)[11]="UNIT"

    tab1=aggregate(c(tab1[,c(7,8)]),by=list(tab1$SPECIES,tab1$COUNTRY, tab1$START_YEAR, tab1$AREA, tab1$SEX, tab1$AGECLASS),FUN="sum")

    colnames(tab1)=c("SPECIES","COUNTRY","START_YEAR","AREA","SEX","AGECLASS","imm","mat")
    tab1$PRM=tab1$mat/(tab1$imm+tab1$mat)
    tab1$SAMPLE_SIZE=(tab1$imm+tab1$mat)

    MA=tab1[,c(2,4,3,3,1,5,6,10,9)]

    MA$METHOD_USED="Observed proportions (immatures 1,2,2a)"
    colnames(MA)[4]="END_YEAR"

    return(MA)

}
