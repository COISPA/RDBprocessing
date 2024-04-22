

#' TableVII2 - GFCM DCRF datacall
#'
#' @param datacs Detailed data in RCG CS format
#' @param datacl Landings aggregated data in RCG CL format
#' @param verbose boolean. If TRUE a message is printed.
#'
#' @return TableVII2
#' @export
#'
#' @examples library(COSTcore)
#' TableVII2(RDBprocessing::data_ex,RDBprocessing::data_exampleCL)
#' @imports COSTcore
#' @imports reshape
TableVII2 <- function(datacs,datacl,verbose=F) {

    if (FALSE){
datacs=RDBprocessing::data_ex
datacl=RDBprocessing::data_exampleCL


    }
#datacs=check_cs_header(data)
    library(COSTcore)
Bland=LAND_MEDBS(datacs,datacl)

Cost_obs=RCGtoCOST_CS(datacs)
CA=Cost_obs@ca
TR=Cost_obs@tr



#CA=read.table("CA_example.csv",sep=";",header=T)
#TR=read.table("TR_example.csv",sep=";",header=T)
#CT=read.table("Communication_table.csv",sep=";",header=T)
#alpha=read.table("Scientific name to FAO 3alphacode.csv",sep=",",header=T)
#species=read.table("Species TableVII.2.csv",sep=";",header=F)

AREA=Bland$AREA[1]
species=unique(Bland$SPECIES)

#Bland=Bland[as.character(Bland$SPECIES) %in% as.character(species),]

# Transformation of BLanding

B_melt <- melt(Bland, id=c( "ID" , "COUNTRY", "YEAR" , "QUARTER" ,
                            "VESSEL_LENGTH" ,"GEAR"  ,"MESH_SIZE_RANGE"  ,"FISHERY" ,
                            "AREA"  , "SPECON" , "SPECIES" , "LANDINGS" , "UNIT" ))

B_melt$variable <-   apply(B_melt, 1, function(x) substring(x[14], 12, nchar(x[14])) )
B_melt <- B_melt[B_melt$variable != "", ]
B_melt$variable[B_melt$variable == "100_PLUS"] <- 100
B_melt$variable <- as.numeric(as.character(B_melt$variable))
B_melt$value <- as.numeric(as.character(B_melt$value))

#Association of GFCM fleet segment to B Landings

B_melt$GFCM_fleetsegment=as.character(B_melt$GEAR)
B_melt$GFCM_fleetsegment=""

for (i in 1:nrow(B_melt)){
    if (nrow(CT[as.character(CT$LOA)== as.character(B_melt$VESSEL_LENGTH[i]) &
                as.character(CT$GEAR_ACatch)== as.character(B_melt$GEAR[i]) &
                as.character(CT$MESH_SIZE_RANGE_ACatch)== as.character(B_melt$MESH_SIZE_RANGE[i]) &
                as.character(CT$FISHERY_ACatch)== as.character(B_melt$FISHERY[i]),])>0) {

        B_melt$GFCM_fleetsegment[i]=
            as.character(CT[as.character(CT$LOA)== as.character(B_melt$VESSEL_LENGTH[i]) &
                                as.character(CT$GEAR_ACatch)== as.character(B_melt$GEAR[i]) &
                                as.character(CT$MESH_SIZE_RANGE_ACatch)== as.character(B_melt$MESH_SIZE_RANGE[i]) &
                                as.character(CT$FISHERY_ACatch)== as.character(B_melt$FISHERY[i]) ,]$Fleet_segment)

    } else {
        B_melt$GFCM_fleetsegment[i]=""}
}

B_melt=B_melt[as.character(B_melt$GFCM_fleetsegment)!="",]

B_land1=aggregate(B_melt$value,by=list(B_melt$COUNTRY,
                                       B_melt$YEAR,B_melt$AREA,B_melt$GFCM_fleetsegment,
                                       B_melt$SPECIES,B_melt$UNIT,B_melt$variable),FUN="sum")
B_land1[,3]=AREA
B_land1$Source_of_data="BS"
B_land1$Name_of_the_scientific_survey =""
B_land1=B_land1[,c(1,2,3,9,10,4,5,6,7,8)]
colnames(B_land1)=c("Country","YEAR","GSA","Source_of_data",
                    "Name_of_the_scientific_survey",
                    "GFCM_fleetsegment","SPECIES","Length_unit","Length",
                    "Number_of_individuals_expanded_per_length_classes")

# only landing should be taken into account, according to the specifications

CA=CA[CA$catchCat=="LAN",]

# Association of 3alpha code to primary data

CA2 = merge(CA,alpha,by.x="spp",by.y="Scientific")
colnames(CA2)[ncol(CA2)]="spp2"
CA2$spp= CA2$spp2
CA <- CA2[,c(2:15, 1, 16:32)]

length_unit <- unique(data.frame(species = Bland$SPECIES, unit = Bland$UNIT) )

for (nr in 1:nrow(length_unit) ) {
    if (nrow(CA[as.character(CA$spp) == as.character(length_unit$species[nr]), ]) >0) {
        if (length_unit$unit[nr] == "cm") {
            CA$lenCls[as.character(CA$spp) == as.character(length_unit$species[nr])] <- CA$lenCls[as.character(CA$spp) == as.character(length_unit$species[nr])]/10
        }
    }
}

# association of GFCM fleet segment to primary data

TR_CA=merge(TR,CA,by=c("year","trpCode", "sampType", "landCtry",
                       "vslFlgCtry", "proj"),all=F)
TR_CA$GFCM_fleetsegment=TR_CA$landCtry
TR_CA$GFCM_fleetsegment=""

TR_CA=TR_CA[,c(1,15,7,25,27,32,33,41,44,45)]

TR_CA$lenCls=round(TR_CA$lenCls,0)


for (i in 1:nrow(TR_CA)){
    if (nrow(CT[as.character(CT$LOA)== as.character(TR_CA$vslLen[i]) &
                as.character(CT$METIER)== as.character(TR_CA$foCatEu6[i]) ,])>0) {
        TR_CA$GFCM_fleetsegment[i]=
            as.character(CT[as.character(CT$LOA)== as.character(TR_CA$vslLen[i]) &
                                as.character(CT$METIER)== as.character(TR_CA$foCatEu6[i]) ,]$Fleet_segment)
    } else {
        TR_CA$GFCM_fleetsegment[i]=""
    }
}

TR_CA=TR_CA[as.character(TR_CA$GFCM_fleetsegment)!="",]

TR_CA <- TR_CA[as.character(TR_CA$spp) %in% as.character(length_unit$species),]

write.table(TR_CA,"TR_CA.csv",sep=";",row.names=F)

agg=aggregate(TR_CA$spp,by=list(TR_CA$year,TR_CA$spp,
                                TR_CA$GFCM_fleetsegment,TR_CA$lenCls),FUN="length")
colnames(agg)=c("YEAR","SPECIES","GFCM_fleetsegment","Length",
                "Number_of_individuals_sampled")

agg_w=aggregate(TR_CA$indWt,by=list(TR_CA$year,TR_CA$spp,
                                    TR_CA$GFCM_fleetsegment,TR_CA$lenCls),FUN="mean",na.rm=T)
colnames(agg_w)=c("YEAR","SPECIES","GFCM_fleetsegment","Length",
                  "Weight_of_individuals_sampled")

Merg=merge(B_land1,agg,by=c("YEAR","SPECIES","GFCM_fleetsegment","Length"),all.x=T)

Merg2=merge(Merg,agg_w,by=c("YEAR","SPECIES","GFCM_fleetsegment","Length"),all.x=T)

# conversion in kg

Merg2$Weight_of_individuals_sampled=Merg2$Weight_of_individuals_sampled/1000

Final=merge(B_land1,Merg2,by=c("YEAR","SPECIES","GFCM_fleetsegment","Length"))
Final=Final[,c(5,1,6,7,8,3,2,9,4,17,18,16)]

colnames(Final)=c("Country","Reference_year","GSA","Source_of_data",
                  "Name_of_the_scientific_survey","Fleet_segment","Species",
                  "Length_unit","Length",
                  "Number_of_individuals_sampled_per_length_classes",
                  "Weight_of_individuals_sampled_per_length_classes",
                  "Number_of_individuals_expanded_per_length_classes")

# Final[Final[, 10] == 0, 10] = NA
Final[,ncol(Final)]=round(Final[,ncol(Final)]*1000,0)

Final2 <- Final[with(Final, order(Species,	Reference_year, Fleet_segment, Length) ), ]
Final2 <- Final2[Final2$Number_of_individuals_expanded_per_length_classes != 0, ]

return(Final2)
}
