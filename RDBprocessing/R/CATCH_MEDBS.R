#' Catch at age (CATCH) table - MED & BS data call
#'
#' @param datacs Detailed data in RCG CS format
#' @param datacl Landings aggregated data in RCG CL format
#' @param datace Effort aggregated data in RDB CE format (https://www.ices.dk/data/Documents/RDB/RDB%20Exchange%20Format.pdf)
#' @param verbose boolean. If TRUE a message is printed.
#' @return CATCH table
#' @export
#' @examples CATCH_MEDBS(RDBprocessing::data_ex,RDBprocessing::data_exampleCL,RDBprocessing::ce_example)
#' @importFrom stats complete.cases
#' @import COSTeda
#' @importFrom COSTdbe dbeObject RaiseLgth
#' @importFrom COSTcore subsetSpp
#' @importFrom dplyr rename left_join bind_rows ungroup distinct everything n_distinct
#' @importFrom plyr round_any
#' @importFrom data.table as.data.table set setDF setDT
#' @importFrom tidyr separate spread
#' @importFrom magrittr %>%
#' @import reshape2

CATCH_MEDBS<-function(datacs,datacl, datace, verbose=FALSE){

if (FALSE){
    CS=read.table("C:\\RDBprocessing\\da Ioannis\\CS.csv",sep=",",header=T)

    CS$date=format(as.Date(CS$date, format ="%Y-%m-%d" ),"%d/%m/%Y" )
    CL=read.table("C:\\RDBprocessing\\da Ioannis\\CL.csv",sep=",",header=T)
    CL$species=CS$species[1]
    CE=read.table("C:\\RDBprocessing\\da Ioannis\\COST.csv",sep=",",header=T)

    datacs=CS # RDBprocessing::data_ex #check_cs_header(CS) #CS #
     datacl=CL #RDBprocessing::data_exampleCL
datace=CE #RDBprocessing::ce_example
datacs$Aggregation_level="T"
}

    datacs=check_cs_header(datacs)
    . <-.id <- id<- Dis<- FISHERY<- GEAR<- ID<- LANDINGS<- Lan<- MESH_SIZE_RANGE<- NO_AGE_MEASUREMENTS_DISCARDS<- NO_AGE_MEASUREMENTS_LANDINGS <-   NO_LENGTH_MEASUREMENTS_DISCARDS <- NO_LENGTH_MEASUREMENTS_LANDINGS<-   NO_SAMPLES_DISCARDS <- NO_SAMPLES_LANDINGS<- QUARTER<- SPECIES<- VESSEL_LENGTH<- VL<- Year<- age<- area<- catchCat<- foCatEu6<- funs<- gear<- id<-   lenCls<- meanW.at.age<- n <-n.at.age<-  space<- spp<- stock<- technical<-    trpCode<- value<- vars<- year <- NULL

    if (nrow(datacs[which(as.numeric(datacs$Age)>=20),])>0)   datacs[datacs$Age>=20,]$Age=20


    datacs$Age=round(as.numeric(datacs$Age),0)


    #if(any(is.na(datacs[datacs$Age=="",]$Age))){
    datacs[is.na(datacs)]<-""
    #}

    datacs$Individual_weight=""
    datacs$fish_ID=""
    datacs$Maturity_Stage=""

    datacs2=aggregate(datacs$Number_at_length, by=list(datacs$Sampling_type,datacs$Flag_country,datacs$Year,datacs$Trip_code,datacs$Harbour,datacs$Number_of_sets_hauls_on_trip,datacs$Days_at_sea,datacs$Sampling_method,datacs$Aggregation_level,datacs$Station_number,datacs$Duration_of_fishing_operation,datacs$Initial_latitude,datacs$Initial_longitude,datacs$Final_latitude,datacs$Final_longitude,datacs$Depth_of_fishing_operation,datacs$Water_depth,datacs$Catch_registration,datacs$Species_registration,datacs$Date,datacs$Area,datacs$Fishing_activity_category_National,datacs$Fishing_activity_category_European_lvl_6,datacs$Species,datacs$Catch_category,datacs$Weight,datacs$Subsample_weight,datacs$Sex,datacs$Maturity_method,datacs$Maturity_scale,datacs$Maturity_Stage,datacs$Ageing.method,datacs$Age,datacs$Length_code,datacs$Length_class,datacs$Commercial_size_category_scale,datacs$Commercial_size_category,datacs$fish_ID,datacs$Individual_weight), FUN="sum")


    datacs=datacs2[,c(1:35,40,36:39)]
    colnames(datacs)=colnames(RDBprocessing::data_ex)
    datacs=check_cs_header(datacs)


    if(any(is.na(datacs[datacs$Age=="",]$Age))){
    datacs[datacs$Age=="",]$Age<-NA
    }



    header=c("ID","COUNTRY","YEAR","QUARTER","VESSEL_LENGTH","GEAR","MESH_SIZE_RANGE","FISHERY","AREA","SPECON", "SPECIES","LANDINGS", "DISCARDS", "NO_SAMPLES_LANDINGS","NO_LENGTH_MEASUREMENTS_LANDINGS", "NO_AGE_MEASUREMENTS_LANDINGS", "NO_SAMPLES_DISCARDS","NO_LENGTH_MEASUREMENTS_DISCARDS", "NO_AGE_MEASUREMENTS_DISCARDS", "NO_SAMPLES_CATCH", "NO_LENGTH_MEASUREMENTS_CATCH", "NO_AGE_MEASUREMENTS_CATCH", "MIN_AGE", "MAX_AGE" ,
"AGE_0", "AGE_0_NO_LANDED", "AGE_0_MEAN_WEIGHT_LANDED", "AGE_0_MEAN_LENGTH_LANDED", "AGE_0_NO_DISCARD", "AGE_0_MEAN_WEIGHT_DISCARD", "AGE_0_MEAN_LENGTH_DISCARD",
"AGE_1", "AGE_1_NO_LANDED", "AGE_1_MEAN_WEIGHT_LANDED", "AGE_1_MEAN_LENGTH_LANDED", "AGE_1_NO_DISCARD", "AGE_1_MEAN_WEIGHT_DISCARD", "AGE_1_MEAN_LENGTH_DISCARD",
"AGE_2", "AGE_2_NO_LANDED", "AGE_2_MEAN_WEIGHT_LANDED", "AGE_2_MEAN_LENGTH_LANDED", "AGE_2_NO_DISCARD", "AGE_2_MEAN_WEIGHT_DISCARD", "AGE_2_MEAN_LENGTH_DISCARD",
"AGE_3", "AGE_3_NO_LANDED", "AGE_3_MEAN_WEIGHT_LANDED", "AGE_3_MEAN_LENGTH_LANDED", "AGE_3_NO_DISCARD", "AGE_3_MEAN_WEIGHT_DISCARD", "AGE_3_MEAN_LENGTH_DISCARD",
"AGE_4", "AGE_4_NO_LANDED", "AGE_4_MEAN_WEIGHT_LANDED", "AGE_4_MEAN_LENGTH_LANDED", "AGE_4_NO_DISCARD", "AGE_4_MEAN_WEIGHT_DISCARD", "AGE_4_MEAN_LENGTH_DISCARD",
"AGE_5", "AGE_5_NO_LANDED", "AGE_5_MEAN_WEIGHT_LANDED", "AGE_5_MEAN_LENGTH_LANDED", "AGE_5_NO_DISCARD", "AGE_5_MEAN_WEIGHT_DISCARD", "AGE_5_MEAN_LENGTH_DISCARD",
"AGE_6", "AGE_6_NO_LANDED", "AGE_6_MEAN_WEIGHT_LANDED", "AGE_6_MEAN_LENGTH_LANDED", "AGE_6_NO_DISCARD", "AGE_6_MEAN_WEIGHT_DISCARD", "AGE_6_MEAN_LENGTH_DISCARD",
"AGE_7", "AGE_7_NO_LANDED", "AGE_7_MEAN_WEIGHT_LANDED", "AGE_7_MEAN_LENGTH_LANDED", "AGE_7_NO_DISCARD", "AGE_7_MEAN_WEIGHT_DISCARD", "AGE_7_MEAN_LENGTH_DISCARD",
"AGE_8", "AGE_8_NO_LANDED", "AGE_8_MEAN_WEIGHT_LANDED", "AGE_8_MEAN_LENGTH_LANDED", "AGE_8_NO_DISCARD", "AGE_8_MEAN_WEIGHT_DISCARD", "AGE_8_MEAN_LENGTH_DISCARD",
"AGE_9", "AGE_9_NO_LANDED", "AGE_9_MEAN_WEIGHT_LANDED", "AGE_9_MEAN_LENGTH_LANDED", "AGE_9_NO_DISCARD", "AGE_9_MEAN_WEIGHT_DISCARD", "AGE_9_MEAN_LENGTH_DISCARD",
"AGE_10", "AGE_10_NO_LANDED", "AGE_10_MEAN_WEIGHT_LANDED", "AGE_10_MEAN_LENGTH_LANDED", "AGE_10_NO_DISCARD", "AGE_10_MEAN_WEIGHT_DISCARD", "AGE_10_MEAN_LENGTH_DISCARD",
"AGE_11", "AGE_11_NO_LANDED", "AGE_11_MEAN_WEIGHT_LANDED", "AGE_11_MEAN_LENGTH_LANDED", "AGE_11_NO_DISCARD", "AGE_11_MEAN_WEIGHT_DISCARD", "AGE_11_MEAN_LENGTH_DISCARD",
"AGE_12", "AGE_12_NO_LANDED", "AGE_12_MEAN_WEIGHT_LANDED", "AGE_12_MEAN_LENGTH_LANDED", "AGE_12_NO_DISCARD", "AGE_12_MEAN_WEIGHT_DISCARD", "AGE_12_MEAN_LENGTH_DISCARD",
"AGE_13", "AGE_13_NO_LANDED", "AGE_13_MEAN_WEIGHT_LANDED", "AGE_13_MEAN_LENGTH_LANDED", "AGE_13_NO_DISCARD", "AGE_13_MEAN_WEIGHT_DISCARD", "AGE_13_MEAN_LENGTH_DISCARD",
"AGE_14", "AGE_14_NO_LANDED", "AGE_14_MEAN_WEIGHT_LANDED", "AGE_14_MEAN_LENGTH_LANDED", "AGE_14_NO_DISCARD", "AGE_14_MEAN_WEIGHT_DISCARD", "AGE_14_MEAN_LENGTH_DISCARD",
"AGE_15", "AGE_15_NO_LANDED", "AGE_15_MEAN_WEIGHT_LANDED", "AGE_15_MEAN_LENGTH_LANDED", "AGE_15_NO_DISCARD", "AGE_15_MEAN_WEIGHT_DISCARD", "AGE_15_MEAN_LENGTH_DISCARD",
"AGE_16", "AGE_16_NO_LANDED", "AGE_16_MEAN_WEIGHT_LANDED", "AGE_16_MEAN_LENGTH_LANDED", "AGE_16_NO_DISCARD", "AGE_16_MEAN_WEIGHT_DISCARD", "AGE_16_MEAN_LENGTH_DISCARD",
"AGE_17", "AGE_17_NO_LANDED", "AGE_17_MEAN_WEIGHT_LANDED", "AGE_17_MEAN_LENGTH_LANDED", "AGE_17_NO_DISCARD", "AGE_17_MEAN_WEIGHT_DISCARD", "AGE_17_MEAN_LENGTH_DISCARD",
"AGE_18", "AGE_18_NO_LANDED", "AGE_18_MEAN_WEIGHT_LANDED", "AGE_18_MEAN_LENGTH_LANDED", "AGE_18_NO_DISCARD", "AGE_18_MEAN_WEIGHT_DISCARD", "AGE_18_MEAN_LENGTH_DISCARD",
"AGE_19", "AGE_19_NO_LANDED", "AGE_19_MEAN_WEIGHT_LANDED", "AGE_19_MEAN_LENGTH_LANDED", "AGE_19_NO_DISCARD", "AGE_19_MEAN_WEIGHT_DISCARD", "AGE_19_MEAN_LENGTH_DISCARD",
"AGE_20_PLUS", "AGE_20_PLUS_NO_LANDED", "AGE_20_PLUS_MEAN_WEIGHT_LANDED", "AGE_20_PLUS_MEAN_LENGTH_LANDED", "AGE_20_PLUS_NO_DISCARD", "AGE_20_PLUS_MEAN_WEIGHT_DISCARD", "AGE_20_PLUS_MEAN_LENGTH_DISCARD")

    header2=c("ID","COUNTRY","YEAR","QUARTER","VESSEL_LENGTH","GEAR","MESH_SIZE_RANGE","FISHERY","AREA","SPECON", "SPECIES","LANDINGS", "DISCARDS", "NO_SAMPLES_LANDINGS","NO_LENGTH_MEASUREMENTS_LANDINGS", "NO_AGE_MEASUREMENTS_LANDINGS", "NO_SAMPLES_DISCARDS","NO_LENGTH_MEASUREMENTS_DISCARDS", "NO_AGE_MEASUREMENTS_DISCARDS", "NO_SAMPLES_CATCH", "NO_LENGTH_MEASUREMENTS_CATCH", "NO_AGE_MEASUREMENTS_CATCH", "MIN_AGE", "MAX_AGE" ,
             "AGE_0", "AGE_0_NO_LANDED", "AGE_0_MEAN_WEIGHT_LANDED", "AGE_0_MEAN_LENGTH_LANDED", "AGE_0_NO_DISCARD", "AGE_0_MEAN_WEIGHT_DISCARD", "AGE_0_MEAN_LENGTH_DISCARD",
             "AGE_1", "AGE_1_NO_LANDED", "AGE_1_MEAN_WEIGHT_LANDED", "AGE_1_MEAN_LENGTH_LANDED", "AGE_1_NO_DISCARD", "AGE_1_MEAN_WEIGHT_DISCARD", "AGE_1_MEAN_LENGTH_DISCARD",
             "AGE_2", "AGE_2_NO_LANDED", "AGE_2_MEAN_WEIGHT_LANDED", "AGE_2_MEAN_LENGTH_LANDED", "AGE_2_NO_DISCARD", "AGE_2_MEAN_WEIGHT_DISCARD", "AGE_2_MEAN_LENGTH_DISCARD",
             "AGE_3", "AGE_3_NO_LANDED", "AGE_3_MEAN_WEIGHT_LANDED", "AGE_3_MEAN_LENGTH_LANDED", "AGE_3_NO_DISCARD", "AGE_3_MEAN_WEIGHT_DISCARD", "AGE_3_MEAN_LENGTH_DISCARD",
             "AGE_4", "AGE_4_NO_LANDED", "AGE_4_MEAN_WEIGHT_LANDED", "AGE_4_MEAN_LENGTH_LANDED", "AGE_4_NO_DISCARD", "AGE_4_MEAN_WEIGHT_DISCARD", "AGE_4_MEAN_LENGTH_DISCARD",
             "AGE_5", "AGE_5_NO_LANDED", "AGE_5_MEAN_WEIGHT_LANDED", "AGE_5_MEAN_LENGTH_LANDED", "AGE_5_NO_DISCARD", "AGE_5_MEAN_WEIGHT_DISCARD", "AGE_5_MEAN_LENGTH_DISCARD",
             "AGE_6", "AGE_6_NO_LANDED", "AGE_6_MEAN_WEIGHT_LANDED", "AGE_6_MEAN_LENGTH_LANDED", "AGE_6_NO_DISCARD", "AGE_6_MEAN_WEIGHT_DISCARD", "AGE_6_MEAN_LENGTH_DISCARD",
             "AGE_7", "AGE_7_NO_LANDED", "AGE_7_MEAN_WEIGHT_LANDED", "AGE_7_MEAN_LENGTH_LANDED", "AGE_7_NO_DISCARD", "AGE_7_MEAN_WEIGHT_DISCARD", "AGE_7_MEAN_LENGTH_DISCARD",
             "AGE_8", "AGE_8_NO_LANDED", "AGE_8_MEAN_WEIGHT_LANDED", "AGE_8_MEAN_LENGTH_LANDED", "AGE_8_NO_DISCARD", "AGE_8_MEAN_WEIGHT_DISCARD", "AGE_8_MEAN_LENGTH_DISCARD",
             "AGE_9", "AGE_9_NO_LANDED", "AGE_9_MEAN_WEIGHT_LANDED", "AGE_9_MEAN_LENGTH_LANDED", "AGE_9_NO_DISCARD", "AGE_9_MEAN_WEIGHT_DISCARD", "AGE_9_MEAN_LENGTH_DISCARD",
             "AGE_10", "AGE_10_NO_LANDED", "AGE_10_MEAN_WEIGHT_LANDED", "AGE_10_MEAN_LENGTH_LANDED", "AGE_10_NO_DISCARD", "AGE_10_MEAN_WEIGHT_DISCARD", "AGE_10_MEAN_LENGTH_DISCARD",
             "AGE_11", "AGE_11_NO_LANDED", "AGE_11_MEAN_WEIGHT_LANDED", "AGE_11_MEAN_LENGTH_LANDED", "AGE_11_NO_DISCARD", "AGE_11_MEAN_WEIGHT_DISCARD", "AGE_11_MEAN_LENGTH_DISCARD",
             "AGE_12", "AGE_12_NO_LANDED", "AGE_12_MEAN_WEIGHT_LANDED", "AGE_12_MEAN_LENGTH_LANDED", "AGE_12_NO_DISCARD", "AGE_12_MEAN_WEIGHT_DISCARD", "AGE_12_MEAN_LENGTH_DISCARD",
             "AGE_13", "AGE_13_NO_LANDED", "AGE_13_MEAN_WEIGHT_LANDED", "AGE_13_MEAN_LENGTH_LANDED", "AGE_13_NO_DISCARD", "AGE_13_MEAN_WEIGHT_DISCARD", "AGE_13_MEAN_LENGTH_DISCARD",
             "AGE_14", "AGE_14_NO_LANDED", "AGE_14_MEAN_WEIGHT_LANDED", "AGE_14_MEAN_LENGTH_LANDED", "AGE_14_NO_DISCARD", "AGE_14_MEAN_WEIGHT_DISCARD", "AGE_14_MEAN_LENGTH_DISCARD",
             "AGE_15", "AGE_15_NO_LANDED", "AGE_15_MEAN_WEIGHT_LANDED", "AGE_15_MEAN_LENGTH_LANDED", "AGE_15_NO_DISCARD", "AGE_15_MEAN_WEIGHT_DISCARD", "AGE_15_MEAN_LENGTH_DISCARD",
             "AGE_16", "AGE_16_NO_LANDED", "AGE_16_MEAN_WEIGHT_LANDED", "AGE_16_MEAN_LENGTH_LANDED", "AGE_16_NO_DISCARD", "AGE_16_MEAN_WEIGHT_DISCARD", "AGE_16_MEAN_LENGTH_DISCARD",
             "AGE_17", "AGE_17_NO_LANDED", "AGE_17_MEAN_WEIGHT_LANDED", "AGE_17_MEAN_LENGTH_LANDED", "AGE_17_NO_DISCARD", "AGE_17_MEAN_WEIGHT_DISCARD", "AGE_17_MEAN_LENGTH_DISCARD",
             "AGE_18", "AGE_18_NO_LANDED", "AGE_18_MEAN_WEIGHT_LANDED", "AGE_18_MEAN_LENGTH_LANDED", "AGE_18_NO_DISCARD", "AGE_18_MEAN_WEIGHT_DISCARD", "AGE_18_MEAN_LENGTH_DISCARD",
             "AGE_19", "AGE_19_NO_LANDED", "AGE_19_MEAN_WEIGHT_LANDED", "AGE_19_MEAN_LENGTH_LANDED", "AGE_19_NO_DISCARD", "AGE_19_MEAN_WEIGHT_DISCARD", "AGE_19_MEAN_LENGTH_DISCARD",
             "AGE_20", "AGE_20_NO_LANDED", "AGE_20_MEAN_WEIGHT_LANDED", "AGE_20_MEAN_LENGTH_LANDED", "AGE_20_NO_DISCARD", "AGE_20_MEAN_WEIGHT_DISCARD", "AGE_20_MEAN_LENGTH_DISCARD")


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
    sel_spe$typeALK="fixed"
    sel_spe$type="trip"
    sel_spe$lanEstim_methodDesc="analytical"
    sel_spe$methodDesc_LAN.age.wght="analytical"
    sel_spe$methodDesc_LAN.len.age="analytical"
    sel_spe$methodDesc_LAN.age.wght="analytical"
    sel_spe$methodDesc_DIS.age.wght="analytical"
    sel_spe$methodDesc_DIS.len.wght="analytical"
    sel_spe$methodDesc_DIS.len.age<-"analytical"
    sel_spe$mcrs="."
    sel_spe$specon_catch=""
    sel_spe$landSpp=""

  sel_spe$adjust_L.w.a <-sel_spe$adjust_D.w.a<-sel_spe$adjust_L.len.a<-sel_spe$adjust_D.len.a<-TRUE

  # get col w. all=NA by spp
  #aa=fri_csc@ca %>% group_by(spp) %>% summarize_all(~all(is.na(.)))

  # to make it run for len if age=NA, impute age =="-1"
  #fri_csc@ca$age[fri_csc@ca$spp %in% aa$spp[aa$age==TRUE]]=-1


  ### error if sp-quarter-gear-VL not found in both CS and CL

  i=1
  for (i in 1:dim(sel_spe)[1]) {


      STK<- sel_spe$SPECIES[i]

      AREA <- sel_spe$GSA[i]

      datacs_temp=datacs[datacs$Area==AREA & datacs$Species==STK,]
      datacl_temp=datacl[datacl$area==AREA & datacl$species==STK,]
      datace_temp=datace[datace$area==AREA,]


      fri_cs1<-RCGtoCOST_CS(datacs_temp)
      fri_cl1<-RCGtoCOST_CL(datacl_temp)
      fri_ce1 = ceData(ce=datace_temp)


      COUNTRY<-unique(fri_cs1@tr$landCtry)
      YEAR=unique(fri_cs1@tr$year)

      fri_strD1 <- strIni(timeStrata="quarter", techStrata = "foCatEu6",
                          spaceStrata = "area")

      fri_csv <- csDataVal(fri_cs1)
      fri_clv <- clDataVal(fri_cl1)
      fri_cev <- ceDataVal(fri_ce1)

      fri_csc1 <- suppressWarnings( csDataCons(fri_csv, fri_strD1) )
      fri_clc1 <- suppressWarnings( clDataCons(fri_clv, fri_strD1) )
      fri_cec1 <- suppressWarnings( ceDataCons(fri_cev, fri_strD1) )

      fri_csc1@ca$age[fri_csc1@ca$age==-1]=NA
# da sostituire
      #fri_csc1<- subset(fri_csc, space==sel_spe$GSA[i],table="ca",link=T)
      #fri_clc1<- subset(fri_clc, space==sel_spe$GSA[i],table="cl")
      #fri_cec1<- subset(fri_cec, space==sel_spe$GSA[i],table="ce")


      str_ca=unique(fri_csc1@ca$time)
      str_cl=unique(fri_clc1@cl$time)
      str_ce=unique(fri_cec1@ce$time)

      intersection=intersect(intersect(str_ca,str_cl),str_ce)
      years=substr(intersection,1,4)

if (as.character(sel_spe$YEAR[i]) %in% years){

      #fri_csc1<- subset(fri_csc, space==sel_spe$GSA[i], time==intersection,table="ca",link=T)
      #fri_clc1<- subset(fri_clc, space==sel_spe$GSA[i],time== intersection,table="cl")

      #fri_cec1<- subset(fri_cec, space==sel_spe$GSA[i],time== intersection,table="ce")

      # Estimating age structure LAN -  -----------------------------------


      lanEstim <-
          dbeObject(
              desc = paste(STK, "Landings.str", sep="_"),
              species = STK,
              catchCat = "LAN",
              strataDesc = fri_strD1,
              methodDesc = sel_spe$lanEstim_methodDesc[i]
          )

      # length str LAN

      if ( sel_spe$lanEstim_methodDesc[i]=="analytical"){
          lanEstim <- RaiseLgth(lanEstim, fri_csc1, fri_clc1,incl.precision =F)
      } else {
          lanEstim <- RaiseLgthBoot(lanEstim, fri_csc1, fri_clc1,incl.precision =F,B=15)
      }

      # subset lenstruc for MCRS

      if (!is.na(sel_spe$mcrs[i]) & sel_spe$mcrs[i]!=".") {
          lanEstim@lenStruc$estim =
              lanEstim@lenStruc$estim[as.numeric(lanEstim@lenStruc$estim$length) <= sel_spe$mcrs[i],]
          lanEstim@lenVar = lanEstim@lenVar[as.numeric(lanEstim@lenVar$length) <= sel_spe$mcrs[i],]
      }

      #########>>>>>>>>>>>>> Estimation of total numbers-at-age from market sampling

      lanEstim <- RaiseAge(lanEstim, csObject=fri_csc1, fri_clc1, type = sel_spe$typeALK[i],
                           strataDesc = fri_strD1)

      # Estimating age structure DIS - ----------------------------------------------

      DIS_dbe <- dbeObject(desc= paste(STK, AREA,"Discards", sep="_"),
                           species=STK,
                           catchCat="DIS",
                           strataDesc=fri_strD1,
                           methodDesc="analytical")

      # discards raising

      if (sel_spe$type[i]=="landings" ) {
          DIS_dbe <- totVolume(DIS_dbe,fri_csc1,fri_cec1, fri_clc1,
                               type=sel_spe$type[i],landSpp=sel_spe$landSpp[i])
      } else {
          DIS_dbe <- totVolume(DIS_dbe,fri_csc1,fri_cec1, type=sel_spe$type[i])
      }

      DIS_dbe <- suppressWarnings(RaiseAge(DIS_dbe, fri_csc1, fri_clc1,type = sel_spe$typeALK[i],
                          strataDesc = fri_strD1))

      # CATCH1 :  cols ID: MAX_AGE --------------------------------------------------------

      # NO SAMPLES ----------------------------------------------------------------------

      ### No Samples == No trips (see ANNEX2- DG MARE Med&BS data call spec.).
      # Note: dbe estimates n.samples as trips*fo , thus the estimation must be based on HL

       newhl<-mergecsData(fri_cs1)@hl %>%
           #rename("space"=area, "technical"=foCatEu6) %>%
           mutate(time=paste(year, quarter, sep=" - ")) %>% filter( spp==STK)
      colnames(newhl)[18]="space"
      colnames(newhl)[22]="technical"
      # newhl<-mergecsData(fri_cs1)@tr %>%
      #     rename("space"=area, "technical"=foCatEu6) %>%
      #     mutate(time=paste(year, quarter, sep=" - ")) #%>% filter( spp==STK)
      #

      if (!is.na(sel_spe$mcrs[i]) & sel_spe$mcrs[i]!=".") { # MCRS

          no.samples<- suppressWarnings( data.frame(newhl) %>% filter(lenCls<=sel_spe$mcrs[i]) %>%
              dplyr::group_by(time,space,technical,catchCat)%>%
              summarize(value=n_distinct(trpCode)))

          no.samples<- no.samples%>% tidyr::spread(catchCat,value)

          L.no.samples<- suppressWarnings(no.samples%>% select(-Dis) ) #%>% rename("value"=Lan))
          D.no.samples<- suppressWarnings(no.samples%>% select(-Lan)%>% rename("value"=Dis))

      } else {

           no.samples<- suppressMessages( data.frame(newhl) %>%
               dplyr::group_by(time,space,technical,catchCat)%>%
                   dplyr::summarize(value=n_distinct(trpCode)) )


          no.samples<- no.samples%>% tidyr::spread(catchCat,value)

          L.no.samples<- suppressWarnings(no.samples%>% dplyr::select(-Dis) )
          colnames(L.no.samples)[4]="value"
                                          #%>% rename("value"=Lan))
          D.no.samples<- suppressWarnings(no.samples%>% select(-Lan))
colnames(D.no.samples)[4]="value"
#rename("value"=Dis))
      }

      # MCRS : no age and len measur. for LAN < MCRS----------------------

      if (!is.na(sel_spe$mcrs[i]) & sel_spe$mcrs[i]!="."){

          # No age measurements LAN

          newca <- mergecsData(fri_cs1)@ca %>%
              rename("space"=area, "technical"=foCatEu6) %>%
              mutate(time=paste(year, quarter, sep=" - ")) %>% filter( spp==STK)

          no.age.meas.lan.mcrs<- suppressWarnings(  data.frame(newca) %>%
              filter(lenCls<=sel_spe$mcrs[i]&!is.na(age)&catchCat=="LAN" )%>%
              dplyr::group_by(time,space,technical)%>% summarize(value=n()) )

          # remove technical if all ==NA
          no.age.meas.lan.mcrs<- no.age.meas.lan.mcrs[,!apply(is.na(no.age.meas.lan.mcrs), 2, all)]

          # No len measurements LAN

          no.len.meas.lan.mcrs<- suppressWarnings(  data.frame(newca) %>%
              filter(lenCls<=sel_spe$mcrs[i]&!is.na(lenCls)&catchCat=="LAN")%>%
              dplyr::group_by(time,space,technical)%>% summarize(value=n()) )

      }

      ### end

      # separate merge for age samples: may not have technical strata (use : space, time)

      if (!is.na(sel_spe$mcrs[i]) & sel_spe$mcrs[i]!=".") {
          # if mcrs use no.age.meas.lan.mcrs
          list.age.smp<-list(no.age.meas.lan.mcrs,DIS_dbe@nMeas$age)
      } else{
          list.age.smp<-list(lanEstim@nMeas$age,DIS_dbe@nMeas$age)
      }

      names(list.age.smp)<-c("L.nmeas.age", "D.nmeas.age")
      list.age.smp.merge = data.table::rbindlist(list.age.smp,id=T)
      age.smp= tidyr::spread(list.age.smp.merge,key=.id,value=value)

      # list all remaining output tables (excl. age meas & no samples)

      if (!is.na(sel_spe$mcrs[i]) & sel_spe$mcrs[i]!=".") { # if mcrs use no.len.meas.lan.mcrs

          list2<- list(no.len.meas.lan.mcrs,DIS_dbe@nMeas$len,lanEstim@totalN$estim,
                       lanEstim@totalW$estim ,DIS_dbe@totalW$estim ,DIS_dbe@totalN$estim,
                       L.no.samples,D.no.samples)

      } else {

          list2<- list(lanEstim@nMeas$len,DIS_dbe@nMeas$len,
                       lanEstim@totalN$estim,lanEstim@totalW$estim,
                       DIS_dbe@totalW$estim ,DIS_dbe@totalN$estim,
                       L.no.samples,D.no.samples)
      }

      names(list2)<-c("LnMeas.len","DnMeas.len","LtotalN","LtotalW","totalWDIS",
                      "totalNDIS","L.no.samples","D.no.samples")

      list3=append(list.age.smp,list2)
      list..merge = data.table::rbindlist(list3,id=T,fill=T)

      all.merge= tidyr::spread(list..merge,key=.id,value=value)

      all.merge <- suppressWarnings(all.merge%>% mutate( "stock"=STK) %>% select(stock,everything()))

      # delete rows (age...) with no "technical": a number should be given only
      # if it relates to this fishery only
      # (see DG MARE Med&BS Data Call specificatin - Annex 2 - Catch)

      all.merge<-  all.merge[complete.cases(all.merge$technical), ]

      aa.len <- all.merge
      aa.len$totalN=all.merge$LtotalN/1000 # '000 ind
      aa.len$totalW=all.merge$LtotalW/1000 # tons

      aa.len$totalNDIS=all.merge$totalNDIS/1000 # '000 ind
      aa.len$totalWDIS=all.merge$totalWDIS/1000 # tons

      # AgeStruc : n.at.age LAN / DIS -----------------------------------------

      # landings
      bb<-lanEstim@ageStruc$estim

      bb$value=bb$value/1000 # '000 ind
      colnames(bb)[5]="n.at.age"
      #bb<-rename(bb, "n.at.age"=value)

      # discards
      bbd<-DIS_dbe@ageStruc$estim

      bbd$value=bbd$value/1000 # '000 ind
colnames(bbd)[5]="DIS.n.at.age"
      #bbd<-rename(bbd, "DIS.n.at.age"=value)

      ab<- suppressMessages(suppressMessages(left_join(bb,bbd) %>% left_join(aa.len)))


      ab<- suppressWarnings(ab %>% tidyr::separate(technical, c("gear","FISHERY","MESH_SIZE_RANGE"),
                           sep = "_"))

      # min age/ max age----------------------------------------------------------
      ab<-cbind(ab[,c(1:5)],rep(NA,nrow(ab)),ab[,c(6:21)])
      #, "VL"
      colnames(ab)[6]="VL"

      ab$age <- as.numeric(as.character(ab$age))

      ab$age<- as.numeric(as.character(ab$age))
      ab <- ab%>% group_by(time,   space , gear,FISHERY  , VL ,MESH_SIZE_RANGE ) %>%
          mutate(minage=min(age,na.rm=T),maxage=max(age,na.rm=T))


      ab <- ab %>% tidyr::separate(time, c("Year","Quarter")," - ",remove=F)

      # ### >>>>>>>>>. catch1: info by row --------------------------------------


      catch1= data.frame(
          ID = NA ,
          COUNTRY =COUNTRY ,
          YEAR = sel_spe$YEAR[i] ,
          QUARTER = ab$Quarter ,
          VESSEL_LENGTH = ab$VL ,
          GEAR = ab$gear ,
          MESH_SIZE_RANGE = ab$MESH_SIZE_RANGE ,
          FISHERY = ab$FISHERY[i] ,
          AREA = AREA ,
          SPECON = sel_spe$specon_catch[i] ,
          SPECIES = STK ,
          LANDINGS = ab$totalW , # MCRS: meanW.at.age * n.at.age
          DISCARDS = ab$totalWDIS,
          NO_SAMPLES_LANDINGS =ab$L.no.samples, # = TRIPS
          NO_LENGTH_MEASUREMENTS_LANDINGS = ab$LnMeas.len ,
          NO_AGE_MEASUREMENTS_LANDINGS = ab$L.nmeas.age ,
          NO_SAMPLES_DISCARDS = ab$D.no.samples , # = TRIPS
          NO_LENGTH_MEASUREMENTS_DISCARDS = ab$DnMeas.len ,
          NO_AGE_MEASUREMENTS_DISCARDS = ab$D.nmeas.age,
          NO_SAMPLES_CATCH = 0,
          NO_LENGTH_MEASUREMENTS_CATCH = 0,
          NO_AGE_MEASUREMENTS_CATCH = 0 ,
          MIN_AGE = ab$minage ,
          MAX_AGE = ab$maxage
      )

      # if mcrs delete LANDINGS: estimated below
      if (!is.na(sel_spe$mcrs[i]) & sel_spe$mcrs[i]!="."){
          catch1<- suppressWarnings(catch1 %>% select(-LANDINGS))
      }

      # ### >>>>>>>>>. catch1: no samples & no meas.  ---------------------------

      # NO_SAMPLES_CATCH - NO_LENGTH_MEASUREMENTS_CATCH -NO_AGE_MEASUREMENTS_CATCH

      catch1 <- catch1 %>% mutate(
          NO_SAMPLES_CATCH = rowSums( cbind (NO_SAMPLES_LANDINGS , NO_SAMPLES_DISCARDS),
                                      na.rm=TRUE),
          NO_LENGTH_MEASUREMENTS_CATCH = rowSums( cbind (NO_LENGTH_MEASUREMENTS_DISCARDS ,
                                                         NO_LENGTH_MEASUREMENTS_LANDINGS),
                                                  na.rm=TRUE),

          NO_AGE_MEASUREMENTS_CATCH=rowSums( cbind (NO_AGE_MEASUREMENTS_DISCARDS,
                                                    NO_AGE_MEASUREMENTS_LANDINGS),
                                             na.rm=TRUE)) %>% distinct()

      # ############ >>>>>> LAN- DIS n.at.age ------------------------------------


      # matrix with all combinations of "time" "space"  "gear" "VL"
      # "MESH_SIZE_RANGE" "length"

      ab[,c(1:8)][is.na(ab[,c(1:8)])]<--1

      dt <- data.table::as.data.table(ab )

      seq_l <-  try(seq(0, 20, by = 1),silent=T)
      if(class(seq_l)=="try-error"){seq_l=-1}

      dt$id<- paste(dt$time,dt$space,dt$gear,dt$FISHERY,dt$VL,dt$MESH_SIZE_RANGE,
                    sep=":")

      dt1<- dt[, list(age = seq_l), by = id]
      dt1<- dt1 %>% tidyr::separate(id, c("time", "space", "gear", "FISHERY",
                                   "VL","MESH_SIZE_RANGE"), sep = ":")
      class(dt1$VL)<-"numeric"
      #dt1[is.na(dt1$VL),]$VL=-1
      dt2=suppressMessages(suppressWarnings(left_join(dt1,ab %>% select("time" , "space", "gear" , "FISHERY",
                                      "VL" , "MESH_SIZE_RANGE","age","n.at.age",
                                      "DIS.n.at.age","stock" ))))

      dt2$stock=STK

      #  MEAN LENGTH LAND, MEAN WEIGHT LAND -------------------------------------

      #####>>>>>>>>>>> LAN weight at age!!

      wtEstim_An <-
          dbeObject(
              desc = "Weights at age",
              species = STK,
              catchCat = "LAN",
              param = "weight",
              strataDesc = fri_strD1, # strBP,
              methodDesc = sel_spe$methodDesc_LAN.age.wght[i]
          )



      if(sel_spe$methodDesc_LAN.age.wght[i]=="analytical") {
          wtEstim_An <- bpEstim(wtEstim_An, fri_csc1,adjust = sel_spe$adjust_L.w.a[i])
      } else{
          wtEstim_An <- bpBoot(wtEstim_An, fri_csc1, adjust = sel_spe$adjust_L.w.a[i])
      }

      # # LAN mean weight at age -------------------------------------------------

      cc=wtEstim_An@ageStruc$estim

      cc$value<- cc$value/1000 # g ->kg
colnames(cc)[5]="meanW.at.age"
      cc$age=as.numeric(as.character(cc$age))

      #cc=rename(cc, "meanW.at.age"=value)
      cc= suppressWarnings( cc %>% tidyr::separate(technical, c("gear", "FISHERY","MESH_SIZE_RANGE"),
                         sep = "_"))
      cc<-cbind(cc[,c(1:5)],rep(NA,nrow(cc)),cc[,c(6:7)])
      #, "VL"
      colnames(cc)[6]="VL"

      # LAN length at age --------------------------------------------------------

      LEstim_An <-
          dbeObject(
              desc = "Length at age",
              species = STK,
              catchCat = "LAN",
              param = "length",
              strataDesc = fri_strD1, # strBP,
              methodDesc = sel_spe$methodDesc_LAN.len.age[i]
          )


      if(sel_spe$methodDesc_LAN.len.age[i]=="analytical") {
          LEstim_An <-  bpEstim(LEstim_An, fri_csc1, adjust = sel_spe$adjust_L.len.a[i])

      } else{
          LEstim_An <-  bpBoot(LEstim_An, fri_csc1, adjust = sel_spe$adjust_L.len.a[i])
      }

      # LAN mean length at age -----------------------------------------------------

      ff=LEstim_An@ageStruc$estim

      UNIT <- unique(fri_csc1@ca$lenCode[fri_csc1@ca$spp==STK])
      if (UNIT=="mm" | UNIT=="MM"){
          ff$value <- ff$value/10  # mm-> cm
      }


      ff$age=as.numeric(as.character(ff$age))
      colnames(ff)[5]="meanL.at.age"
      #ff=rename(ff, "meanL.at.age"=value)
      ff= suppressWarnings( ff %>% tidyr::separate(technical, c("gear","FISHERY","MESH_SIZE_RANGE"),
                         sep = "_"))
      ff<-cbind(ff[,c(1:5)],rep(NA,nrow(ff)),ff[,c(6:7)])
      #, "VL"
      colnames(ff)[6]="VL"

      # MEAN LENGTH DISCARD, MEAN WEIGHT DISCARD --------------------------------

      # DIS weight at age -------------------------------------------------------


      DwtEstim_An <-
          dbeObject(
              desc = "Weights at age",
              species = STK,
              catchCat = "DIS",
              param = "weight",
              strataDesc = fri_strD1, # strBP,
              methodDesc = sel_spe$methodDesc_DIS.age.wght[i]
          )


      if(sel_spe$methodDesc_DIS.age.wght[i]=="analytical") {
          DwtEstim_An <- bpEstim(DwtEstim_An, fri_csc1, adjust = sel_spe$adjust_D.w.a[i])
      } else{
          DwtEstim_An <- bpBoot(DwtEstim_An, fri_csc1, adjust = sel_spe$adjust_D.w.a[i])
      }

      # DIS mean weight at age
      ccD=DwtEstim_An@ageStruc$estim

      ccD$value <- ccD$value/1000 # g -> kg

      ccD$age=as.numeric(as.character(ccD$age))
      colnames(ccD)[5]="DmeanW.at.age"
      #ccD=rename(ccD, "DmeanW.at.age"=value)
      ccD=suppressWarnings(  ccD %>% tidyr::separate(technical, c("gear", "FISHERY","MESH_SIZE_RANGE"),
                           sep = "_") )

      ccD<-cbind(ccD[,c(1:5)],rep(NA,nrow(ccD)),ccD[,c(6:7)])
      #, "VL"
      colnames(ccD)[6]="VL"
      # DIS length at age -----------------------------------------------------------

      DLEstim_An <-
          dbeObject(
              desc = " Length at age",
              species = STK,
              catchCat = "DIS",
              param = "length",
              strataDesc = fri_strD1, # strBP,
              methodDesc = sel_spe$methodDesc_DIS.len.age[i]
          )

      if(sel_spe$methodDesc_DIS.len.age[i]=="analytical") {
          DLEstim_An <- bpEstim(DLEstim_An, fri_csc1, adjust = sel_spe$adjust_D.len.a[i])
      } else{
          DLEstim_An <- bpBoot(DLEstim_An, fri_csc1, adjust = sel_spe$adjust_D.len.a[i])
      }

      ####### >> DIS mean length at age
      ffD=DLEstim_An@ageStruc$estim

      UNIT <- unique(fri_csc1@ca$lenCode[fri_csc1@ca$spp==STK])
      if (UNIT=="mm" |UNIT== "MM"){
          ffD$value <- ffD$value/10  # mm-> cm
      }

      ffD$age=as.numeric(as.character(ffD$age))
      colnames(ffD)[5]="DmeanL.at.age"
      #ffD=rename(ffD, "DmeanL.at.age"=value)
      ffD=suppressWarnings(  ffD %>% tidyr::separate(technical, c("gear", "FISHERY","MESH_SIZE_RANGE"),
                           sep = "_") )

      ffD<-cbind(ffD[,c(1:5)],rep(NA,nrow(ffD)),ffD[,c(6:7)])
      #, "VL"
      colnames(ffD)[6]="VL"

      # ## combine: w.age, l.age, no.age (LAN- DIS)... to get CATCH cols --------

      dt2[dt2$VL==-1,]$VL=NA


      dt2[which(is.na(dt2[,c("n.at.age")])),c("n.at.age")]<--1
      dt2[which(is.na(dt2[,c("DIS.n.at.age")])),c("DIS.n.at.age")]<--1

      cc[which(is.na(cc[,c("meanW.at.age")])),c("meanW.at.age")]<--1

      ff[which(is.na(ff[,c("meanL.at.age")])),c("meanL.at.age")]<--1

      ccD[which(is.na(ccD[,c("DmeanW.at.age")])),c("DmeanW.at.age")]<--1

      ffD[which(is.na(ffD[,c("DmeanL.at.age")])),c("DmeanL.at.age")]<--1

      l3=list(dt2,cc,ff,ccD,ffD)



      l3=lapply(l3, function(x){ x[,c(1:8)][is.na(x[,c(1:8)])]<- -1;return(x)})


      cfdt2=Reduce(function(x, y) merge(x, y, by = c("time", "space",  "gear" ,
                                                     "FISHERY","VL","MESH_SIZE_RANGE" ,"age"  ),all.x=T), l3)

# c'era un try
      dt3 = try(data.table::dcast(data.table::setDT(distinct(cfdt2)),
                                  time + space + gear +FISHERY+ VL+MESH_SIZE_RANGE ~ age,
                                  value.var = c("n.at.age","DIS.n.at.age","meanW.at.age",
                                                "meanL.at.age","DmeanW.at.age","DmeanL.at.age",
                                                "age" )),silent=T)

      # if mcrs
      # Landings= meanW.at.age*n.at.age
      if (!is.na(sel_spe$mcrs[i]) & sel_spe$mcrs[i]!="."){

          # tonnes
          landings <- suppressWarnings(cfdt2 %>% mutate(LANDINGS= meanW.at.age*n.at.age) %>%
              select(time, space, gear, FISHERY, VL, MESH_SIZE_RANGE,LANDINGS))

          landings <- landings[!is.na(landings$LANDINGS ), ]

          dt3<- suppressMessages(left_join(dt3,landings))

      }

      ## rename col to match CATCH

      names(dt3)[grep("DIS.n.at.age", names(dt3)) ]<-paste("AGE",
                                                           0:(length(grep("DIS.n.at.age", names(dt3)))-1),"NO_DISCARD",sep="_")

      names(dt3)[grep("n.at.age", names(dt3)) ]<-paste("AGE",
                                                       0:(length(grep("n.at.age", names(dt3)))-1),"NO_LANDED",sep="_")

      names(dt3)[grep("DmeanW.at.age", names(dt3)) ]<-paste("AGE",
                                                            0:(length(grep("DmeanW.at.age", names(dt3)))-1),"MEAN_WEIGHT_DISCARD",
                                                            sep="_")

      names(dt3)[grep("DmeanL.at.age", names(dt3)) ]<-paste("AGE",
                                                            0:(length(grep("DmeanL.at.age", names(dt3)))-1),"MEAN_LENGTH_DISCARD",
                                                            sep="_")

      names(dt3)[grep("meanW.at.age", names(dt3)) ]<-paste("AGE",
                                                           0:(length(grep("meanW.at.age", names(dt3)))-1),"MEAN_WEIGHT_LANDED",
                                                           sep="_")

      names(dt3)[grep("meanL.at.age", names(dt3)) ]<-paste("AGE",
                                                           0:(length(grep("meanL.at.age", names(dt3)))-1),"MEAN_LENGTH_LANDED",
                                                           sep="_")

      names(dt3)[grep("age.1", names(dt3))]<-paste("AGE",
                                                   0:(length(grep("age.1", names(dt3)))-1),sep="_")

      dt3 <- dt3 %>% tidyr::separate(time, c("Year","Quarter")," - ",remove=T)

      dt3<-data.frame(dt3)

      dt3[is.na(dt3)]<--1

      ###########

      # FINAL CATCH TAB ---------------------------------------------------------

      catch1<- catch1 %>% mutate_at(vars( c(ID:SPECIES) ),
                                    list(~ ifelse( is.na(.), -1, .) ) )


      dt3<- dt3 %>% mutate_at(vars( c(Year:MESH_SIZE_RANGE) ),
                              list(~ ifelse( is.na(.), -1, .) ) )

      #which(is.na(dt3))<--1


      catch1$YEAR=as.character(catch1$YEAR)
      catch1$FISHERY=as.character(catch1$FISHERY)

colnames(dt3)[c(1,2,3,4,6)]=c("YEAR","QUARTER","AREA","GEAR","VESSEL_LENGTH")

      catch.tab <- suppressMessages( left_join(catch1,dt3,by=c( "QUARTER" ,  "YEAR",  "AREA", "GEAR" ,  "VESSEL_LENGTH", "FISHERY", "MESH_SIZE_RANGE")) )

      # FISHERY to DG MARE Med&BS codification
      catch.tab$FISHERY <- RDBprocessing::fishery$SDEF_codification[match(catch.tab$FISHERY ,
                                               RDBprocessing::fishery$DGMARE_Med_BS_codification)]

      # species to FAO three alpha code and set ID (COUNTRY, AREA, GEAR, VESSEL_LENGTH,
      # MESH_SIZE_RANGE,QUARTER, SPECIES)
      catch.tab <- catch.tab %>%
          mutate(SPECIES=sel_spe$SPE[match(SPECIES,sel_spe$SPECIES)],
                 ID = paste(COUNTRY, AREA, GEAR,FISHERY, VESSEL_LENGTH, MESH_SIZE_RANGE,
                            YEAR, QUARTER, SPECIES, sep = "_"))

      catch.tab$YEAR=as.numeric(catch.tab$YEAR)

      catch.temp2<-data.frame(matrix(nrow=0,ncol=length(header)))
      colnames(catch.temp2)=as.vector(header)

      # lan.temp2<-rbind(lan.temp2,land.tab)

      catch.temp2<-rbind(catch.temp2,catch.tab)

      #catch.temp2<-bind_rows(catch.temp2,catch.tab)
}
  }



  #catch.temp2[,-c(1:11)][is.na(catch.temp2[,-c(1:11)])] <- 0

  catch.temp2<-data.table::setDT(catch.temp2)


  for (jj in c(12:171)) set(catch.temp2, i = which(catch.temp2[[jj]]==-1),
                            j = jj, v = 0)

  catch.temp2<-setDF(catch.temp2)



  catch.temp2<-  suppressWarnings(catch.temp2 %>% select(all_of(header2)))

  colnames(catch.temp2)=header


return(catch.temp2)
    }
