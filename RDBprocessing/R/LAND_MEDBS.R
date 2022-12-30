
LAND_MEDBS<-function(datacs,datacl){
    #datacs$Date=format(as.POSIXct(datacs$Date, format = "%d/%m/%Y"), format="%Y-%m-%d")

    fri_cs1<-RCGtoCOST_CS(datacs)
    merge=merge(fri_cs1@ca,fri_cs1@hh,by="trpCode")
    merge$quarter=quarter(merge$date)

    fri_cs1@ca$quarter=merge$quarter
    fri_cl1<-RCGtoCOST_CL(datacl)


    fri_strD1 <- strIni(timeStrata="quarter", techStrata = "foCatEu6",
                        spaceStrata = "area")

    fri_csv <- csDataVal(fri_cs1)
     fri_csc <- csDataCons(fri_csv, fri_strD1)

     fri_clv <- clDataVal(fri_cl1)
     fri_clc <- clDataCons(fri_clv, fri_strD1)

    # extract COUNTRY  and YEAR
    COUNTRY<-unique(fri_cl1@cl$landCtry)
    YEAR=unique(fri_cl1@cl$year)

    header=c(c("COUNTRY","AREA","YEAR","QUARTER","VESSEL_LENGTH","GEAR","MESH_SIZE_RANGE","FISHERY","AREA","SPECON", "SPECIES","LANDINGS","UNIT",paste("LENGTHCLASS",seq(0,99),sep=""),"LENGTHCLASS100_PLUS")
)

    mat=  aggregate(datacs$Length_class,by=list(datacs$Flag_country,datacs$Year,data$Area,data$Species),FUN="length")
    colnames(mat)=c("COUNTRY","YEAR","AREA","SPECIES","NB")
    tab1=merge(mat,RDBprocessing::Annex17,by.x="SPECIES",by.y="Scientific_name")


    colnames(tab1)[6]="SPE"
    sel_spe<-tab1
    sel_spe$lanEstim_methodDesc<-"analytical"

    colnames(sel_spe)[4]="GSA"
    sel_spe$SPECON<-""
    sel_spe$LC_RANGE=""
    if(nrow(sel_spe[sel_spe$UNIT=="cm",])>0) {    sel_spe[sel_spe$UNIT=="cm",]$LC_RANGE=10} else if (nrow(sel_spe[sel_spe$UNIT=="mm",])>0){
    sel_spe[sel_spe$UNIT=="mm",]$LC_RANGE=1
}


    for (i in 1:dim(sel_spe)[1]) {

        STK<- sel_spe$SPECIES[i]

        AREA <- sel_spe$GSA[i]

        fri_csc1<- subset(fri_csc, space==sel_spe$GSA[i],table="ca",link=T)
        fri_clc1<- subset(fri_clc, space==sel_spe$GSA[i],table="cl")

        # The first step is to create the empty object, that will be given
        # the appropriate values for the descritor  fields.

        lanEstim <-
            dbeObject(
                desc = paste(STK, AREA,"Landings", sep="_"),
                species = STK,
                catchCat = "LAN",
                strataDesc = fri_strD1,
                methodDesc = sel_spe$lanEstim_methodDesc[i]
            )

        # the only arguments to pass to the function are the dbe object,
        # the consolidated cs and cl datasets.

        if ( sel_spe$lanEstim_methodDesc[i]=="analytical"){
            lanEstim <- RaiseLgth(lanEstim, fri_csc1, fri_clc1,incl.precision =F)
        } else {
            lanEstim <- RaiseLgthBoot(lanEstim, fri_csc1, fri_clc1,
                                      incl.precision =F,B=15)
        }


        # totalW\$estim : total weight,
        aa <-lanEstim@totalW$estim

        aa$value<- aa$value/1000 # tons

        aa<- rename(aa, "totalW"=value)

        # lenStruc\$estim : numbers-at-length estimates

        bb<- lanEstim@lenStruc$estim

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

        bb$length<- plyr::round_any( bb$length,1,floor)

        bb$value<- bb$value/1000 # '000 ind

        ab=left_join(bb,aa ,by = c("time", "space", "technical"))

        ab<- ab %>% separate(technical, c("gear","FISHERY", "VL","MESH_SIZE_RANGE"),
                             sep = "_",remove=T)

        ab$length<- as.numeric(as.character(ab$length))

        ab<- ab%>% group_by(time,   space , gear  ,FISHERY, VL,MESH_SIZE_RANGE  ) %>%
            mutate(minlc=min(length,na.rm=T),maxlc=max(length,na.rm=T))

        # matrix with all combinations of "time"   "space"  "gear"   "VL"
        # "length" ,"MESH_SIZE_RANGE"

        dt <- as.data.table(ab)

        dt[,c(1:7)][is.na(dt[,c(1:7)])]<- -1

        seq_l <-  seq(0, max(dt$length,na.rm = T), by = 1) #

        dt$id<- paste(dt$time,dt$space,dt$gear,dt$FISHERY,dt$VL,
                      dt$MESH_SIZE_RANGE,sep=":")

        dt1<- dt[, list(length = seq_l), by = id]

        dt1<- dt1 %>% separate(id, c("time", "space", "gear", "FISHERY","VL",
                                     "MESH_SIZE_RANGE"), sep = ":")


        ab[is.na(ab)]<- -1

        dt2<- left_join(dt1,ab)
        dt2$stock<- STK

        ##

        dt3 <- data.table::dcast(dt2,as.formula(paste(paste(names(dt2)[! names(dt2) %in%
                                                                           c("length","value")], collapse='+'), "length", sep="~")),
                                 value.var = "value")


        dt3=dt3[complete.cases(dt3[,c(7:9)]), ]

        dt3 <- dt3 %>% separate(time, c("Year","Quarter")," - ")

        dt3$MESH_SIZE_RANGE<-as.character(dt3$MESH_SIZE_RANGE)


        # numbers at LC : NA-->0
        dt3<- dt3 %>% mutate_at(vars( -(Year:stock) ),
                                funs( if_else( is.na(.), 0, .) ) )


        LANDINGS <- data.frame(

            ID = NA ,
            COUNTRY = COUNTRY ,
            YEAR = YEAR ,
            QUARTER =dt3$Quarter,
            VESSEL_LENGTH = dt3$VL,
            GEAR = dt3$gear,
            MESH_SIZE_RANGE = dt3$MESH_SIZE_RANGE,
            FISHERY =  dt3$FISHERY ,
            AREA = sel_spe$GSA[i],
            SPECON = -1 ,
            SPECIES = STK ,
            LANDINGS = dt3$totalW ,
            UNIT = UNIT1
        )




        LANDINGS<-left_join(LANDINGS,dt3[,-c(1,3,8:11)],by=c( "QUARTER"  ="Quarter" ,
                                                              "GEAR"="gear" ,  "VESSEL_LENGTH" = "VL"  ,
                                                              "MESH_SIZE_RANGE","FISHERY" ))

        # take care of number of Length classes (max is 100 acc. to DG MARE Med&BS template)
        zz<-dim(LANDINGS[-c(1:13)])[2]
        names(LANDINGS)[-c(1:13)]<- paste("LENGTHCLASS",seq(0,zz-1,1),sep="")


        if(zz>=100){

            LANDINGS$LENGTHCLASS100_PLUS<- rowSums(LANDINGS[,!1:113])

            LANDINGS<-LANDINGS %>% select(ID:LENGTHCLASS99,LENGTHCLASS100_PLUS)
        }



        # FISHERY to DG MARE Med&BS codification
        LANDINGS$FISHERY <- fishery$SDEF_codification[match(LANDINGS$FISHERY ,
                                                            fishery$DGMARE_Med_BS_codification)]

        # species to FAO three alpha code and set ID (COUNTRY, AREA, GEAR, VESSEL_LENGTH,
        # MESH_SIZE_RANGE,QUARTER, SPECIES)

        land.tab <-LANDINGS %>% mutate(SPECIES=sel_spe$SPE[match(SPECIES,sel_spe$SPECIES)],
                                       ID = paste(COUNTRY, AREA, GEAR,FISHERY, VESSEL_LENGTH,
                                                  MESH_SIZE_RANGE,YEAR, QUARTER, SPECIES, sep = "_"))

        lan.temp2<-bind_rows(lan.temp2,land.tab)


        lan.temp2[,-c(1:13)][is.na(lan.temp2[,-c(1:13)])] <- 0

    }


    lan.temp2 <- lan.temp2[, 2:ncol(lan.temp2)]

    return(lan.temp2)


    }
