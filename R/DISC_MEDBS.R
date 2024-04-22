#' Discard by length (DISCARDS) table - MED & BS data call
#'
#' @param datacs Detailed data in RCG CS format
#' @param datacl Landings aggregated data in RCG CL format
#' @param datace Effort aggregated data in RDB CE format (https://www.ices.dk/data/Documents/RDB/RDB%20Exchange%20Format.pdf)
#' @param verbose boolean. If TRUE a message is printed.
#' @return DISCARDS table
#' @export
#' @examples DISC_MEDBS(RDBprocessing::data_ex,RDBprocessing::data_exampleCL,RDBprocessing::ce_example)
#' @importFrom stats complete.cases
#' @import COSTeda
#' @importFrom COSTdbe dbeObject RaiseLgth
#' @importFrom COSTcore subsetSpp
#' @importFrom dplyr rename left_join bind_rows ungroup
#' @importFrom plyr round_any
#' @importFrom data.table as.data.table set setDF setDT
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @import reshape2
DISC_MEDBS<-function(datacs,datacl, datace, verbose=FALSE){

    # datacs=data_ex
    # datacl=data_exampleCL
    # datace=ce_example

    datacs=check_cs_header(datacs)

    FISHERY<- GEAR<- ID<- LENGTHCLASS100_PLUS<- LENGTHCLASS99<- MESH_SIZE_RANGE<-QUARTER<- SPECIES<- VESSEL_LENGTH<- VL<- Year<- fishery<- gear<- id <- space<- stock<- technical<- value<-.<-NULL

    fri_cs1<-RCGtoCOST_CS(datacs)
    fri_cl1<-RCGtoCOST_CL(datacl)
    fri_ce1 = ceData(ce=datace)

    COUNTRY<-unique(fri_cs1@tr$landCtry)
    YEAR=unique(fri_cs1@tr$year)

    fri_strD1 <- strIni(timeStrata="quarter", techStrata = "foCatEu6",
                        spaceStrata = "area")

    fri_csv <- suppressWarnings(csDataVal(fri_cs1))
    fri_clv <- suppressWarnings(clDataVal(fri_cl1))
    fri_cev <- suppressWarnings(ceDataVal(fri_ce1))


    fri_csc <- suppressWarnings(csDataCons(fri_csv, fri_strD1))
    fri_clc <- suppressWarnings(clDataCons(fri_clv, fri_strD1))
    fri_cec <- suppressWarnings(ceDataCons(fri_cev, fri_strD1))


    header=c("ID","COUNTRY","YEAR","QUARTER","VESSEL_LENGTH","GEAR","MESH_SIZE_RANGE","FISHERY","AREA","SPECON", "SPECIES","DISCARDS","UNIT",paste("LENGTHCLASS",seq(0,99),sep=""),"LENGTHCLASS100_PLUS")


    mat=  aggregate(datacs$Length_class,by=list(datacs$Flag_country,datacs$Year,datacs$Area,datacs$Species),FUN="length")
    colnames(mat)=c("COUNTRY","YEAR","AREA","SPECIES","NB")
    tab1=merge(mat,RDBprocessing::Annex17,by.x="SPECIES",by.y="Scientific_name")


    colnames(tab1)[6]="SPE"
    sel_spe<-tab1
    sel_spe$type<-"trip"

    colnames(sel_spe)[4]="GSA"
    sel_spe$FISHERY<-"-1"
    sel_spe$LC_RANGE=""
    sel_spe$landSpp=""

    if(nrow(sel_spe[sel_spe$UNIT=="cm",])>0) {    sel_spe[sel_spe$UNIT=="cm",]$LC_RANGE=10} else if (nrow(sel_spe[sel_spe$UNIT=="mm",])>0){
        sel_spe[sel_spe$UNIT=="mm",]$LC_RANGE=1
    }

    i=1
        for (i in 1:dim(sel_spe)[1]) {

            STK<- sel_spe$SPECIES[i]

            AREA <- sel_spe$GSA[i]

            fri_csc1<- subset(fri_csc, space==sel_spe$GSA[i],table="ca",link=T)
            fri_clc1<- subset(fri_clc, space==sel_spe$GSA[i],table="cl")

            fri_cec1<- subset(fri_cec, space==sel_spe$GSA[i],table="ce")

            # The first step is to create the empty object, that will be given
            # the appropriate values for the descritor  fields.

            DIS_dbe <- dbeObject(desc= paste(STK, AREA,"Discards", sep="_"),
                                 species=STK,
                                 catchCat="DIS",
                                 strataDesc=fri_strD1,
                                 methodDesc="analytical"
            )

            if (sel_spe$type[i]=="landings" ) {

                DIS_dbe <- totVolume(DIS_dbe,fri_csc1,fri_cec1, fri_clc1,
                                     type=sel_spe$type[i],val="nAtLength",landSpp=sel_spe$landSpp[i])
            } else {
                DIS_dbe <- totVolume(DIS_dbe,fri_csc1,fri_cec1, type=sel_spe$type[i],
                                     val="nAtLength")
            }


            # totalW\$estim : total weight,
            aa <-DIS_dbe@totalW$estim

            aa$value<- aa$value/1000 # tons

            aa<- rename(aa, "totalW"=value)
            # lenStruc\$estim : numbers-at-length estimates,

            bb<- DIS_dbe@lenStruc$estim

            bb$length=as.numeric(bb$length)

            # define LCs and UNIT len
            UNIT <- as.character( unique(fri_csc@ca$lenCode[fri_csc@ca$spp==STK]) )

            if (UNIT %in% c("mm", "MM")& sel_spe$LC_RANGE[i]==10) {
                bb$length<-as.numeric(bb$length)/10
                UNIT1<-"CM"
            }

            if (UNIT %in% c("mm", "MM") & sel_spe$LC_RANGE[i]==1) {
                bb$length<-as.numeric(bb$length)
                UNIT1<- "MM"
            }

            if (UNIT %in% c("mm", "MM") & sel_spe$LC_RANGE[i]==5) {
                bb$length<-as.numeric(bb$length)/10
                UNIT1<-"CM"
            }

            if (UNIT %in% c("cm", "CM") ) {
                bb$length<-as.numeric(bb$length)
                UNIT1<- "CM"
            }

            if (UNIT %in% c("scm", "SCM") & sel_spe$LC_RANGE[i]==5) {
                bb$length<-as.numeric(bb$length)/10
                UNIT1<-"CM"
            }

            if (UNIT %in% c("scm", "SCM") & sel_spe$LC_RANGE[i]==10) {
                bb$length<-as.numeric(bb$length)/10
                UNIT1<-"CM"
            }

            bb$length<- plyr::round_any( bb$length,1,floor)

            bb$value<- bb$value/1000 # '000 ind

            bb<-aggregate(bb$value,by=list(bb$time,bb$space,bb$technical,bb$length),FUN="sum")
            colnames(bb)=c("time","space","technical","length","value")

            ab= suppressMessages(left_join(bb,aa ,by = c("time", "space", "technical")))

            ab<- suppressWarnings(ab %>% separate(technical, c("gear","FISHERY", "MESH_SIZE_RANGE"),
                                 sep = "_",remove=T))


            ab<-cbind(ab[,c(1:5)],rep(NA,nrow(ab)),ab[,c(6:8)])
            #, "VL"
            colnames(ab)[6]="VL"

            ab$length<- as.numeric(as.character(ab$length))

            ab<- ab%>% group_by(time,   space , gear  ,FISHERY, VL,MESH_SIZE_RANGE  ) %>%
                mutate(minlc=min(length,na.rm=T),maxlc=max(length,na.rm=T))

            # matrix with all combinations of "time"   "space"  "gear"   "VL"
            # "length" ,"MESH_SIZE_RANGE"

            dt <- as.data.table(ab)

            dt[,c(1:7)][is.na(dt[,c(1:7)])]<- -1

            seq_l <-  seq(0, 99, by = 1) #

            dt$id<- paste(dt$time,dt$space,dt$gear,dt$FISHERY,dt$VL,
                          dt$MESH_SIZE_RANGE,sep=":")

            dt1<- dt[, list(length = seq_l), by = id]

            dt1<- suppressMessages(dt1 %>% separate(id, c("time", "space", "gear", "FISHERY","VL",
                                         "MESH_SIZE_RANGE"), sep = ":"))

            ab[is.na(ab)]<- -1
            class(dt1$VL)<-"numeric"

            # ab[,`1:6]: NA-->-1
            ab<- suppressWarnings(ab %>%ungroup()%>% mutate_at(vars(c(time:MESH_SIZE_RANGE) ),
                                              funs( ifelse( is.na(.), -1, .)) ) )


            dt2<- suppressWarnings(dplyr::left_join(dt1,ab))
            dt2$stock<- STK

            ##

            dt3 <- suppressWarnings(reshape2::dcast(dt2,as.formula(paste(paste(names(dt2)[! names(dt2) %in%
                                                                               c("length","value")], collapse='+'), "length", sep="~")),
                                     value.var = "value"))

            dt3=dt3[complete.cases(dt3[,c(7:9)]), ]

            dt3 <- suppressWarnings(dt3 %>% separate(time, c("Year","Quarter")," - "))

            dt3$MESH_SIZE_RANGE<-as.character(dt3$MESH_SIZE_RANGE)


            # numbers at LC : NA-->0
            dt3<- suppressWarnings(dt3 %>% mutate_at(vars( -(Year:stock) ),
                                    funs( if_else( is.na(.), 0, .) ) ))

            DISCARDS <- data.frame(

                ID = NA ,
                COUNTRY = COUNTRY ,
                YEAR = YEAR ,
                QUARTER =dt3$Quarter,
                VESSEL_LENGTH = dt3$VL,
                GEAR = dt3$gear,
                MESH_SIZE_RANGE = dt3$MESH_SIZE_RANGE,
                FISHERY= dt3$FISHERY  ,
                AREA = sel_spe$GSA[i],
                SPECON = "",
                SPECIES = STK ,
                DISCARDS = dt3$totalW ,
                UNIT = UNIT1
            )

            DISCARDS<- suppressMessages(left_join(DISCARDS,dt3[,-c(1,3,8:11)],by=c( "QUARTER"  ="Quarter" ,
                                                                  "GEAR"="gear" ,  "VESSEL_LENGTH" = "VL"  ,"MESH_SIZE_RANGE","FISHERY" )))

            # take care of number of Length classes (max is 100 acc. to JRC template)
            zz<-dim(DISCARDS[-c(1:13)])[2]
            names(DISCARDS)[-c(1:13)]<- paste("LENGTHCLASS",seq(0,zz-1,1),sep="")

            if(zz>=100){
                DISCARDS$LENGTHCLASS100_PLUS<- rowSums(DISCARDS[,!1:113],na.rm = T)
                DISCARDS<- suppressWarnings(DISCARDS %>% select(ID:LENGTHCLASS99,LENGTHCLASS100_PLUS))
            }

            # FISHERY to DG MARE Med&BS specification
            DISCARDS$FISHERY <- RDBprocessing::fishery$SDEF_codification[match(DISCARDS$FISHERY ,
                                                                               RDBprocessing::fishery$DGMARE_Med_BS_codification)]
            DISCARDS$SPECIES<-RDBprocessing::Annex17$Species[match(DISCARDS$SPECIES ,
                                                                   RDBprocessing::Annex17$Scientific_name)]
            DISCARDS$VESSEL_LENGTH<-RDBprocessing::msr$SDEF_codification_MSR[match(DISCARDS$VESSEL_LENGTH ,
                                                                                   RDBprocessing::msr$DGMARE_Med_BS_codification_MSR)]

            # species to FAO three a?pha code and set ID (COUNTRY, AREA, GEAR,
            # VESSEL_LENGTH,  MESH_SIZE_RANGE,QUARTER, SPECIES)

            dis.tab <-DISCARDS %>% mutate(ID = paste(COUNTRY, AREA, GEAR,FISHERY, VESSEL_LENGTH,
                                                     MESH_SIZE_RANGE,YEAR, QUARTER, SPECIES, sep = "_"))

            dis.temp2<-data.frame(matrix(nrow=0,ncol=length(header)))
            colnames(dis.temp2)=as.vector(header)

           # lan.temp2<-rbind(lan.temp2,land.tab)

            dis.temp2<-rbind(dis.temp2,dis.tab)

            # col after 13: set -1 or NA to 0
            dis.temp2[,-c(1:13)][is.na(dis.temp2[,-c(1:13)])] <- 0

            dis.temp2<-setDT(dis.temp2)
            for (jj in c(14:114)) data.table::set(dis.temp2, i = which(dis.temp2[[jj]]==-1), j = jj, v = 0)

            dis.temp2<-setDF(dis.temp2)

        }
    dis.temp2[is.na(dis.temp2$VESSEL_LENGTH),]$VESSEL_LENGTH="NA"


    return(dis.temp2)

}


