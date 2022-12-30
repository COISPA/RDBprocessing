#' Age Length Key (ALK) table - MED & BS data call
#'
#' @param data Detailed data in RCG CS format
#' @param verbose boolean. If TRUE a message is printed.
#' @return ALK table
#' @export
#' @examples ALK_MEDBS(RDBprocessing::data_ex)
#' @importFrom data.table last first between
#' @importFrom stats as.formula
#' @import COSTcore COSTeda COSTdbe
#' @importFrom dplyr group_by filter select

ALK_MEDBS<-function(data, verbose = FALSE) {
    #data=data[!is.na(data$Age),]
    fri_cs<-RCGtoCOST_CS(data)
    LC<- age<- area<- logMsg<- n.at.len<- sex<- spp<- value<-NULL

    header<-c("COUNTRY","AREA","START_YEAR","END_YEAR","SPECON","SPECIES","SEX","APPLY_TO_CATCHES_FILE","TOTAL_NUMBER_OF_HARD_STRUCTURE_READ_BY_AGE","CV","UNIT","AGE" ,paste("LENGTHCLASS",seq(0,99),sep=""),"LENGTHCLASS100_PLUS","COMMENTS")


    fri_strD <- strIni(spaceStrata="area")
    COUNTRY<-unique(fri_cs@ca$landCtry)

  mat=  aggregate(data$Age,by=list(data$Flag_country,data$Year,data$Sex,data$Area,data$Species),FUN="length")
  colnames(mat)=c("COUNTRY","START_YEAR","SEX","AREA","SPECIES","NB")
  tab1=merge(mat,RDBprocessing::Annex17,by.x="SPECIES",by.y="Scientific_name")
  tab1=tab1[tab1$SEX=="F" | tab1$SEX=="M",]
  tab2=tab1[tab1$SEX=="F",]
  tab2$SEX="C"
  tab1=rbind(tab1,tab2)

  colnames(tab1)[7]="SPE"


  sel_spe<-tab1
  sel_spe$END_YEAR<-sel_spe$START_YEAR
  sel_spe$methodDesc_LAN.len.age<-"analytical"
  sel_spe$adjust<-"FALSE"
  colnames(sel_spe)[5]="GSA"

  sel_spe$typeALK<- "stepIncr"
  sel_spe$valueALK<-10
  sel_spe$APPLY_TO_CATCHES_FILE <- "Y"
  sel_spe$COMMENTS <-""
  sel_spe$LC_RANGE <-10

  for (i in 1:dim(sel_spe)[1]) {

      STK<- sel_spe$SPECIES[i]

      fri_cs1<- subset(fri_cs, year%in% seq(sel_spe$START_YEAR[i],
                                            sel_spe$END_YEAR[i],by=1),table="ca",link=T)

      #  estimate sample size (number of otoliths per stock, sex and age)
      if (sel_spe$SEX[i]=="C"){

          fri_csv <- csDataVal(fri_cs1)

          nml<- data.frame(fri_cs1@ca) %>% filter(!is.na(age))%>%
              dplyr::group_by(area,spp,age)%>%
              dplyr::summarize(TOTAL_NUMBER_OF_HARD_STRUCTURE_READ_BY_AGE=n())

      } else { # ALK for selected sex

          fri_cs1=subset(fri_cs1,sex==sel_spe$SEX[i],table="ca",link=T)
          fri_csv <- csDataVal(fri_cs1)

          # get sample size: number of otoliths
          nml<- data.frame(fri_cs1@ca) %>% filter(!is.na(age))%>%
              dplyr::group_by(area,spp,age,sex)%>%
              summarize(TOTAL_NUMBER_OF_HARD_STRUCTURE_READ_BY_AGE=n())

      }


      fri_csv1<- subSetSpp(fri_csv, STK)
      fri_csv1<- subset(fri_csv1, area%in% sel_spe$GSA[i],table="ca",link=T)
      fri_csc1 <- csDataCons(fri_csv1, fri_strD)

       ## ### CV from individual length-at-age
      LEstim_An <-
          dbeObject(
              desc = "Length at age",
              species = STK,
              catchCat = "LAN",
              param = "length",
              strataDesc = fri_strD, # ,
              methodDesc = "analytical"   #sel_spe$methodDesc_LAN.len.age[i]
          )


      if(sel_spe$methodDesc_LAN.len.age[i]=="analytical") {
          LEstim_An <-  bpEstim(LEstim_An, fri_csc1, adjust = sel_spe$adjust[i])

      } else{
          LEstim_An <-  bpBoot(LEstim_An, fri_csc1, adjust = sel_spe$adjust[i])

      }


      ## ALK

      res1 <- alkLgthRec(fri_csc1,type=sel_spe$typeALK[i],value=sel_spe$valueALK[i],
                         update=F, preview=F,postview = F)

      if (sel_spe$typeALK[i]=="fillALKmult"){
          fri_csc2 <- fillALKmult(fri_csc1,STK,p=10,trace=T)
          res1 <- alkLgthRec(fri_csc2,update=F, preview=F,postview = F,
                             value=sel_spe$valueALK[i])
      }

      dfALK <-
          data.frame(
              COUNTRY =COUNTRY ,
              AREA =sel_spe$GSA[i],
              START_YEAR = unique(sel_spe$START_YEAR[i]) ,
              END_YEAR = unique( sel_spe$END_YEAR[i]) ,
              SPECIES = STK ,
              SEX = sel_spe$SEX[i],
              UNIT = unique(fri_cs1@ca$lenCode[fri_cs1@ca$spp==STK]) ,
              SPECON=  -1,
              APPLY_TO_CATCHES_FILE= unique(sel_spe$APPLY_TO_CATCHES_FILE[i]),
              CV=NA,
              AGE = as.numeric(colnames(res1$alk)),
              COMMENTS= sel_spe$COMMENTS[i],
              LENGTHCLASS100_PLUS=0
          )

      # get sample size

      if (sel_spe$SEX[i]=="C"){

          dfALK <-  dfALK %>%
              left_join(nml, by = c("AREA" = 'area', 'SPECIES' = 'spp',"AGE"="age")) %>%
              dplyr::mutate(SPECIES =  sel_spe$SPE[i])
          # FAO Three alpha code
      } else{

          dfALK <-  dfALK %>%
              left_join(nml, by = c("AREA" = 'area', 'SPECIES' = 'spp',"AGE"="age",
                                    "SEX"="sex")) %>%
              dplyr::mutate(SPECIES =  sel_spe$SPE[i])
          # FAO Three alpha code

      }


      aa=data.frame(res1$alk)
      names(aa)=colnames(res1$alk)

      aa=aa%>% dplyr::mutate(LC=rownames(res1$alk))

      ## fix LC
      UNIT <- as.character( unique(fri_csc1@ca$lenCode[fri_csc1@ca$spp==STK]) )

      if (UNIT %in% c("mm", "MM")& sel_spe$LC_RANGE[i]==10) {
          aa$LC<-as.numeric(aa$LC)/10
          UNIT1<-"cm"
      }

      if (UNIT %in% c("mm", "MM") & sel_spe$LC_RANGE[i]==1) {
          aa$LC<-as.numeric(aa$LC)
          UNIT1<- "mm"
      }

      if (UNIT %in% c("mm", "MM")& sel_spe$LC_RANGE[i]==5) {
          aa$LC<-as.numeric(aa$LC)/10
          UNIT1<-"cm"
      }

      if (UNIT %in% c("cm", "CM") ) {
          aa$LC<-as.numeric(aa$LC)
          UNIT1<- "cm"
      }

      if (UNIT %in% c("scm", "SCM")& sel_spe$LC_RANGE[i]==10) {
          aa$LC<-as.numeric(aa$LC)/10
          UNIT1<-"cm"
      }
        round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

      aa$LC<- round_any( aa$LC,1,floor)


      ###

      dfALK$UNIT<-UNIT1

      aa1=aa %>%  gather(age, n.at.len, -LC)

      aa1 <- as.data.table(aa1)
#aa$LC
      seq_l <-  seq(0, 99, by = 1) #
      dt1<- aa1[, list(LC = seq_l), by = age]


      dt2<- left_join(dt1,aa1)

      dt3 <- data.table::dcast(dt2,as.formula(paste(paste(names(dt2)[! names(dt2)
                                                                     %in% c("LC","n.at.len")], collapse='+'), "LC", sep="~")),
                               value.var = "n.at.len")

      dt3$age=as.numeric(dt3$age)


      dt3<- dt3 %>% mutate_at(vars( -(age) ),
                              funs( if_else( is.na(.), 0, .) ) )

      dfALK<-left_join(dfALK,dt3,by=c("AGE"="age"))

      ## CV
      # LEstim_An @ageNum$ cv

      LEstim_An@ageNum[["cv"]]$age=as.numeric(LEstim_An@ageNum[["cv"]]$age)

      dfALK<- dfALK%>% left_join(LEstim_An@ageNum[["cv"]]%>% select(age,value),
                                 by=c( "AGE"="age"))%>% dplyr::mutate(CV=value)%>% select(-c(value))

      # take care of number of Length classes (max is 100 acc. to DG MARE Med&BS template)
      zz<-dim(dfALK[-c(1:14)])[2]
      names(dfALK)[-c(1:14)]<- paste("LENGTHCLASS",seq(0,zz-1,1),sep="")


      if(zz>=100){
          dfALK$LENGTHCLASS100_PLUS<- rowSums(dfALK[,-c(1:114)])
      }


      alk.temp2<-data.frame(matrix(nrow=0,ncol=114))
colnames(alk.temp2)=as.vector(header)

      dfALK<-dfALK%>% dplyr::select(one_of(as.vector(names(alk.temp2))))


     # dfALK


      alk.temp2<- rbind(alk.temp2,(dfALK))
      #alk.temp2<-dfALK
      #alk.temp2[,-c(1:13,114)][is.na(alk.temp2[,-c(1:13,114)])] <- 0


      # export updated CsDataCons

      if (sel_spe$APPLY_TO_CATCHES_FILE[i] == "Y") {
          if (sel_spe$typeALK[i] == "fillALKmult") {
              fri_csc2 <- fillALKmult(fri_csc1, STK, p = 10, trace = T)

              save(fri_csc2,
                   file = paste("upd", STK, sel_spe$SEX[i], sel_spe$GSA[i], ".Rdata", sep ="_"))

          } else{
              res1 <-
                  alkLgthRec(
                      fri_csc1,
                      type = sel_spe$typeALK[i],
                      value = sel_spe$valueALK[i],
                      update = T,
                      preview = F,
                      postview = F
                  )

              #save(res1,
                  # file = paste("upd", STK, sel_spe$SEX[i], sel_spe$GSA[i], ".Rdata", sep =
                                  #  "_"))
          }

      }

  }
  alk.temp2=alk.temp2[alk.temp2$AGE!=-1,]
return(alk.temp2)

}
