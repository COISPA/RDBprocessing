#' Function converting RCG CS in COST CS object
#' @param data Detailed data in RCG CS format
#' @param verbose boolean. If TRUE a message is printed.
#' @return COST CS object
#' @export
#' @examples RCGtoCOST_CS(RDBprocessing::data_ex)
#' @importFrom methods new
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom COSTcore csData

RCGtoCOST_CS<-function(data, verbose = FALSE){

#path.data=getwd()

CS=data
error <- FALSE
dataset_proj <- ""
bad.rm <- FALSE
log_var  <- TRUE
lenNum <- log_varMsg<- subSampWt<- vslFlgCtry<- NULL




    names(CS)[which(tolower(names(CS)) == "sampling.type")] <- "sampType"
    names(CS)[which(tolower(names(CS)) == "sampling_type")] <- "sampType"
    names(CS)[which(tolower(names(CS)) == "samplingtype")] <- "sampType"
    names(CS)[which(tolower(names(CS)) == "flag.country")] <- "vslFlgCtry"
    names(CS)[which(tolower(names(CS)) == "flag_country")] <- "vslFlgCtry"
    names(CS)[which(tolower(names(CS)) == "flagcountry")] <- "vslFlgCtry"
    names(CS)[which(tolower(names(CS)) == "year")] <- "year"
    names(CS)[which(tolower(names(CS)) == "trip.code")] <- "trpCode"
    names(CS)[which(tolower(names(CS)) == "trip_code")] <- "trpCode"
    names(CS)[which(tolower(names(CS)) == "tripcode")] <- "trpCode"
    names(CS)[which(tolower(names(CS)) == "number.of.sets...hauls.on.trip")] <- "foNum"
    names(CS)[which(tolower(names(CS)) == "number_of_sets_hauls_on_trip")] <- "foNum"
    names(CS)[which(tolower(names(CS)) == "number_of_setshauls")] <- "foNum"
    names(CS)[which(tolower(names(CS)) == "nsets")] <- "foNum"
    names(CS)[which(tolower(names(CS)) == "days.at.sea")] <- "daysAtSea"
    names(CS)[which(tolower(names(CS)) == "days_at_sea")] <- "daysAtSea"
    names(CS)[which(tolower(names(CS)) == "daysatsea")] <- "daysAtSea"
    names(CS)[which(tolower(names(CS)) == "sampling.method")] <- "sampMeth"
    names(CS)[which(tolower(names(CS)) == "sampling_method")] <- "sampMeth"
    names(CS)[which(tolower(names(CS)) == "aggregation.level")] <- "aggLev"
    names(CS)[which(tolower(names(CS)) == "aggregation_level")] <- "aggLev"
    names(CS)[which(tolower(names(CS)) == "aggregationlevel")] <- "aggLev"
    names(CS)[which(tolower(names(CS)) == "station.number")] <- "staNum"
    names(CS)[which(tolower(names(CS)) == "station_number")] <- "staNum"
    names(CS)[which(tolower(names(CS)) == "stationnumber")] <- "staNum"
    names(CS)[which(tolower(names(CS)) == "catch.registration")] <- "catReg"
    names(CS)[which(tolower(names(CS)) == "catch_registration")] <- "catReg"
    names(CS)[which(tolower(names(CS)) == "catchregistration")] <- "catReg"
    names(CS)[which(tolower(names(CS)) == "species.registration")] <- "sppReg"
    names(CS)[which(tolower(names(CS)) == "species_registration")] <- "sppReg"
    names(CS)[which(tolower(names(CS)) == "speciesregistration")] <- "sppReg"
    names(CS)[which(tolower(names(CS)) == "date")] <- "date"
    names(CS)[which(tolower(names(CS)) == "area")] <- "area"
    names(CS)[which(tolower(names(CS)) ==
                        "fishing.activity.category.national")] <- "foCatNat"
    names(CS)[which(tolower(names(CS)) ==
                        "fishing_activity_category_european_lvl_6")] <- "foCatEu6"

    names(CS)[which(tolower(names(CS)) ==
                        "fishing_activity_category_national")] <- "foCatNat"
    names(CS)[which(tolower(names(CS)) ==
                        "fishingactivitycategorynational")] <- "foCatNat"
    names(CS)[which(tolower(names(CS)) == "fac_national")] <- "foCatNat"
    names(CS)[which(tolower(names(CS)) ==
                        "fishing.activity.category.european.lvl.6")] <- "foCatEu6"
    names(CS)[which(tolower(names(CS)) == "metier")] <- "foCatEu6"
    names(CS)[which(tolower(names(CS)) == "fac_ec_lvl6")] <- "foCatEu6"
    names(CS)[which(tolower(names(CS)) == "species")] <- "spp"
    names(CS)[which(tolower(names(CS)) == "catch.category")] <- "catchCat"
    names(CS)[which(tolower(names(CS)) == "catch_category")] <- "catchCat"
    names(CS)[which(tolower(names(CS)) == "catchcategory")] <- "catchCat"
    names(CS)[which(tolower(names(CS)) == "commercial.size.category")] <- "commCat"
    names(CS)[which(tolower(names(CS)) == "commercial_size_category")] <- "commCat"
    names(CS)[which(tolower(names(CS)) == "weight")] <- "wt"
    names(CS)[which(tolower(names(CS)) == "subsample.weight")] <- "subSampWt"
    names(CS)[which(tolower(names(CS)) == "subsample_weight")] <- "subSampWt"
    names(CS)[which(tolower(names(CS)) == "subsampleweight")] <- "subSampWt"
    names(CS)[which(tolower(names(CS)) == "length.code")] <- "lenCode"
    names(CS)[which(tolower(names(CS)) == "length_code")] <- "lenCode"
    names(CS)[which(tolower(names(CS)) == "lengthcode")] <- "lenCode"
    names(CS)[which(tolower(names(CS)) == "length.class")] <- "lenCls"
    names(CS)[which(tolower(names(CS)) == "length_class")] <- "lenCls"
    names(CS)[which(tolower(names(CS)) == "lengthclass")] <- "lenCls"
    names(CS)[which(tolower(names(CS)) == "number.at.length")] <- "lenNum"
    names(CS)[which(tolower(names(CS)) == "number_at_length")] <- "lenNum"
    names(CS)[which(tolower(names(CS)) == "numberatlength")] <- "lenNum"


    # new Fields RCG 2018 -----------------------------------------------------

    names(CS)[which(tolower(names(CS)) == "commercial.size.category.scale")] <- "commCatScl"
    names(CS)[which(tolower(names(CS)) == "commercial_size_category_scale")] <- "commCatScl"
    names(CS)[which(tolower(names(CS)) == "commercialsizecategoryscale")] <- "commCatScl"

    names(CS)[which(tolower(names(CS)) == "fish.id")] <- "fishId"
    names(CS)[which(tolower(names(CS)) == "fish_id")] <- "fishId"
    names(CS)[which(tolower(names(CS)) == "fishid")] <- "fishId"

    names(CS)[which(tolower(names(CS)) == "individual.weight")] <- "indWt"
    names(CS)[which(tolower(names(CS)) == "individual_weight")] <- "indWt"
    names(CS)[which(tolower(names(CS)) == "individualweight")] <- "indWt"

    names(CS)[which(tolower(names(CS)) == "harbour")] <- "harbour"

    names(CS)[which(tolower(names(CS)) == "duration.of.fishing.operation")] <- "foDur"
    names(CS)[which(tolower(names(CS)) == "duration_of_fishing_operation")] <- "foDur"
    names(CS)[which(tolower(names(CS)) == "durationoffishing_operation")] <- "foDur"

    names(CS)[which(tolower(names(CS)) == "initial.latitude")] <- "latIni"
    names(CS)[which(tolower(names(CS)) == "initial_latitude")] <- "latIni"
    names(CS)[which(tolower(names(CS)) == "initiallatitude")] <- "latIni"

    names(CS)[which(tolower(names(CS)) == "initial.longitude")] <- "lonIni"
    names(CS)[which(tolower(names(CS)) == "initial_longitude")] <- "lonIni"

    names(CS)[which(tolower(names(CS)) == "final.latitude")] <- "latFin"
    names(CS)[which(tolower(names(CS)) == "final_latitude")] <- "latFin"
    names(CS)[which(tolower(names(CS)) == "finallatitude")] <- "latFin"

    names(CS)[which(tolower(names(CS)) == "final.longitude")] <- "lonFin"
    names(CS)[which(tolower(names(CS)) == "final_longitude")] <- "lonFin"
    names(CS)[which(tolower(names(CS)) == "finallongitude")] <- "lonFin"

    names(CS)[which(tolower(names(CS)) == "depth.of.fishing.operation")] <- "foDep"
    names(CS)[which(tolower(names(CS)) == "depth_of_fishing_operation")] <- "foDep"
    names(CS)[which(tolower(names(CS)) == "depthoffishingoperation")] <- "foDep"

    names(CS)[which(tolower(names(CS)) == "water.depth")] <- "waterDep"
    names(CS)[which(tolower(names(CS)) == "water_depth")] <- "waterDep"
    names(CS)[which(tolower(names(CS)) == "waterdepth")] <- "waterDep"

    names(CS)[which(tolower(names(CS)) == "sex")] <- "sex"

    names(CS)[which(tolower(names(CS)) == "maturity.method")] <- "matMeth"
    names(CS)[which(tolower(names(CS)) == "maturity_method")] <- "matMeth"
    names(CS)[which(tolower(names(CS)) == "maturitymethod")] <- "matMeth"

    names(CS)[which(tolower(names(CS)) == "maturity.scale")] <- "matScale"
    names(CS)[which(tolower(names(CS)) == "maturity_scale")] <- "matScale"
    names(CS)[which(tolower(names(CS)) == "maturityscale")] <- "matScale"


    names(CS)[which(tolower(names(CS)) == "maturity.stage")] <- "matStage"
    names(CS)[which(tolower(names(CS)) == "maturity_stage")] <- "matStage"
    names(CS)[which(tolower(names(CS)) == "maturitystage")] <- "matStage"

    names(CS)[which(tolower(names(CS)) == "ageing.method")] <- "ageMeth"
    names(CS)[which(tolower(names(CS)) == "ageing_method")] <- "ageMeth"
    names(CS)[which(tolower(names(CS)) == "ageingmethod")] <- "ageMeth"

    names(CS)[which(tolower(names(CS)) == "age")] <- "age"


    CS[is.na(CS[,])] <- -1


    CS$aggLev <- as.character(CS$aggLev)
    CS$aggLev[ CS$aggLev %in% c("t","TRUE",TRUE)] <- "T" ##!!

    CS$spp <- unlist(lapply(CS$spp, function(x) paste(toupper(substring(x,1, 1)),
                                                      tolower(substring(x, 2)), sep = "")))

    CS <-CS %>% mutate(proj=dataset_proj,landCtry=vslFlgCtry) ##!!


    trPk <- c("sampType", "vslFlgCtry", "year", "trpCode","proj","landCtry") ##!!
    trOther <- c("foNum", "daysAtSea", "sampMeth",
                 "harbour" ) # new : harbour

    hhPk <- c(trPk, "staNum")
    hhOther <- c("aggLev", "catReg", "sppReg", "date", "area",
                 "foCatNat", "foCatEu6",
                 "foDur", "latIni", "lonIni", "latFin", "lonFin", "foDep", "waterDep") # new

    # modified respect to the previous version
    slPk <- c(hhPk, "spp", "catchCat", "commCat", "commCatScl","sex") # new
    slOther <- c("wt", "subSampWt", "lenCode")

    #slPk <- c(hhPk, "spp", "catchCat", "commCat", "commCatScl","sex", "wt", "subSampWt") # new
    #slOther <- c( "lenCode")

    hlPk <- c(slPk, "lenCls")
    hlOther <- c("lenNum")

    # modified respect to the previous version
    #caPk <- c(hlPk, "age","area","fishId")
    #caOther <- c( "matMeth",  "matScale" ,"matStage", "ageMeth" , "indWt", "lenCode")

    caPk <- c(hlPk, "age","area","fishId", "matStage")
    caOther <- c( "matMeth",  "matScale" , "ageMeth" , "indWt", "lenCode")

    allFields <- c(caPk, trOther, hhOther, slOther, hlOther,caOther)
    missingFields <- allFields[!allFields %in% names(CS)]

    # check if all fields are used

    # names(CS)[!names(CS) %in% allFields]

    if (length(missingFields) > 0) {
        stop("Missing fields : ", paste(missingFields, collapse = ", ",
                                        sep = ""))
    }

    csTr <- unique(CS[, c(trPk, trOther)])

    trPkV <- fpKey(csTr, trPk)

    trPkVDup <- trPkV %in% trPkV[duplicated(trPkV)]
    if (any(trPkVDup)) {
        print(trPkVDup)
        if (log_var) {
            log_varCs$duplicated_TR <- fpKey(CS, trPk) %in% trPkV[trPkVDup]
        }
        if (bad.rm) {
            #message("Integrity problem for CS/TR, ", sum(trPkVDup),
                  #  " row(s) removed.", log_varMsg)
            print("Removed following row(s):", quote = F)
            print(csTr[trPkVDup, ])
            CS <- merge(CS, csTr[!trPkVDup, ])
        }    else {
            error <- TRUE
            message("Integrity problem for CS/TR, ", sum(trPkVDup),
                    " row(s) concerned.", log_varMsg)
        }
    }
    print(paste("No rows TR =",  nrow(csTr)), quote=F)


    csHh <- unique(CS[, c(hhPk, hhOther)])
    hhPkV <- fpKey(csHh, hhPk)
    hhPkVDup <- hhPkV %in% hhPkV[duplicated(hhPkV)]
    if (any(hhPkVDup)) {
        if (log_var) {
            log_varCs$duplicated_HH <- fpKey(CS, hhPk) %in% hhPkV[hhPkVDup]
        }
        if (bad.rm) {
            message("Integrity problem for CS/HH, ", sum(hhPkVDup),
                    " row(s) removed.", log_varMsg)
            print("Removed following row(s):", quote = F)
            print(csHh[hhPkVDup, ])
            CS <- merge(CS, csHh[!hhPkVDup, ])
        }
        else {
            error <- TRUE
            message("Integrity problem for CS/HH, ", sum(hhPkVDup),
                    " row(s) concerned.", log_varMsg)
        }
    }
    print(paste("No rows HH =",  nrow(csHh)), quote=F)


    csSl <- unique(CS[, c(slPk, slOther)])
    slPkV <- fpKey(csSl, slPk)
    slPkVDup <- slPkV %in% slPkV[duplicated(slPkV)]
    if (any(slPkVDup)) {
        if (log_var) {
            log_varCs$duplicated_SL <- fpKey(CS, slPk) %in% slPkV[slPkVDup]
        }
        if (bad.rm) {
            message("Integrity problem for CS/SL, ", sum(slPkVDup),
                    " row(s) removed.", log_varMsg)
            print("Removed following row(s):", quote = F)
            print(csSl[slPkVDup, ])
            CS <- merge(CS, csSl[!slPkVDup, ])
        }
        else {
            error <- TRUE
            message("Integrity problem for CS/SL, ", sum(slPkVDup),
                    " row(s) concerned.", log_varMsg)
        }
    }
    print(paste("No rows SL =",  nrow(csSl)), quote=F)


    CS_aggregated_by_length <-  aggregate(CS$lenNum, by=list(CS$sampType , CS$vslFlgCtry ,
                                                             CS$year,  CS$trpCode, CS$proj, CS$landCtry, CS$harbour,
                                                             CS$foNum, CS$daysAtSea ,  CS$sampMeth, CS$aggLev, CS$staNum,
                                                             CS$foDur, CS$foDep, CS$catReg, CS$sppReg, CS$date, CS$area,
                                                             CS$foCatNat, CS$foCatEu6, CS$spp,  CS$catchCat, CS$wt,
                                                             CS$subSampWt, CS$sex, CS$lenCode, CS$lenCls,
                                                             CS$commCatScl, CS$commCat), FUN="sum")

    colnames(CS_aggregated_by_length) <- c("sampType" , "vslFlgCtry" , "year" , "trpCode" ,
                                           "proj" , "landCtry","harbour" , "foNum" , "daysAtSea" ,
                                           "sampMeth" ,  "aggLev" , "staNum" , "foDur" , "foDep" ,
                                           "catReg", "sppReg" , "date" , "area" , "foCatNat" ,
                                           "foCatEu6",  "spp" , "catchCat" , "wt" , "subSampWt" ,
                                           "sex" , "lenCode" , "lenCls" , "commCatScl" , "commCat",
                                           "lenNum")

    # sum(CS$lenNum)
    # sum(CS_aggregated_by_length$lenNum)

    csHl <- unique(CS_aggregated_by_length[, c(hlPk, hlOther)])
    hlPkV <- fpKey(csHl, hlPk)
    hlPkVDup <- hlPkV %in% hlPkV[duplicated(hlPkV)]
    if (any(hlPkVDup)) {
        if (log_var) {
            log_varCs$duplicated_HL <- fpKey(CS, hlPk) %in% hlPkV[hlPkVDup]
        }
        else {
            error <- TRUE
            message("Integrity problem for CS/HL, ", sum(hlPkVDup),
                    " row(s) concerned.", log_varMsg)
            print("Check the following row(s):", quote = F)
            csHl[hlPkVDup, ]
        }
    }
    print(paste("No rows HL =",  nrow(csHl)), quote=F)


    ##

    #   # check CA ------------------------------------------------------------

    csCa <- unique(CS[, c(caPk, caOther)])
    caPkV <- fpKey(csCa, caPk)
    caPkVDup <- caPkV %in% caPkV[duplicated(caPkV)]
    if (any(caPkVDup)) {
        if (log_var) {
            log_varCs$duplicated_CA <- fpKey(CS, caPk) %in% caPkV[caPkVDup]
        }
        if (bad.rm) {
            message("Integrity problem for CS/CA, ", sum(caPkVDup),
                    " row(s) removed.", log_varMsg)
            print("Removed following row(s):", quote = F)
            print(csCa[caPkVDup, ])
            CS <- merge(CS, csCa[!caPkVDup, ])
        } else {
            error <- TRUE
            message("Integrity problem for CS/CA, ", sum(caPkVDup),
                    " row(s) concerned.", log_varMsg)
            print("Check the following row(s):", quote = F)
            csCa[caPkVDup, ]
        }
    }

    ##
#
#     if (log_var) {
#         if (missing(log_varFilePath)) {
#             log_varFilePath <- tempfile(fileext = ".csv")
#         }
#         write.table(log_varCs, file = log_varFilePath, row.names = FALSE,
#                     sep = ";")
#     }
#     if (error) {
#         print("See errors on log_var file!", quote = F)
#         message("log_var file: ", log_varFilePath)
#     }



    # All CS tables names -----------------------------------------------------

    ##!! Simpler way to define names for the CS tables
    obj <- new("csData")
    tr0 <- obj@tr
    hh0 <- obj@hh
    sl0 <- obj@sl
    hl0 <- obj@hl
    ca0 <- obj@ca

    TR.col <- names(tr0)
    HH.col <- names(hh0)
    SL.col <- names(sl0)
    HL.col <- names(hl0)
    CA.col <- names(ca0)

    ## end names ##!!

    if (!error) {

        missTR<-TR.col[!TR.col %in% names(csTr)]
        costCS.TR <- csTr
        costCS.TR[ ,missTR] <- NA
        costCS.TR<-  costCS.TR[,TR.col]

        #write.table(costCS.TR, file.path(path.data, "SDEF CS-TR data.csv"),
                   # sep = ";", row.names = FALSE)

        missHH<-HH.col[!HH.col %in% names(csHh)]
        costCS.HH <- csHh
        costCS.HH[ , missHH] <- NA

        costCS.HH<-  costCS.HH[,HH.col]

        #write.table(costCS.HH, file.path(path.data, "SDEF CS-HH data.csv"),
                   # sep = ";", row.names = FALSE)


        missSL<-SL.col[!SL.col %in% names(csSl)]
        costCS.SL <- csSl
        costCS.SL[ , missSL] <- NA

        costCS.SL<-  costCS.SL[,SL.col]

        #write.table(costCS.SL, file.path(path.data, "SDEF CS-SL data.csv"),
                    #sep = ";", row.names = FALSE)


        missHL<-HL.col[!HL.col %in% names(csHl)]
        costCS.HL <- csHl
        costCS.HL[ , missHL] <- NA

        costCS.HL <-  costCS.HL[,HL.col]

        #write.table(costCS.HL, file.path(path.data, "SDEF CS-HL data.csv"),
                    #sep = ";", row.names = FALSE)

        if (all(CS$fishId == -1)) {

            ## add subSampWt (SL) and lenNum (HL) to CA

            ca.sl.hl.COL<-c(caPk, caOther,"subSampWt","lenNum")
            ca.sl.hl <- unique(CS[, ca.sl.hl.COL])

            # indWt
            ca.sl.hl<-ca.sl.hl %>% mutate(indWt=subSampWt/lenNum)
            ca.sl.hl1 <- ca.sl.hl[rep(row.names(ca.sl.hl), ca.sl.hl$lenNum), 1:ncol(ca.sl.hl)]
            ca.sl.hl1$fishId <- 1:nrow(ca.sl.hl1)
            costCS.CA <- ca.sl.hl1[, names(  ca.sl.hl1) %in% CA.col]

            missCA<-CA.col[!CA.col %in% names(costCS.CA)]
            costCS.CA[ , missCA] <- NA
            costCS.CA<-  costCS.CA[,CA.col]
            costCS.CA$indWt <- -1

        } else {

            missCA <- CA.col[!CA.col %in% names(csCa)]
            costCS.CA <- csCa
            costCS.CA[ , missCA] <- NA

            costCS.CA$proj<- dataset_proj
            costCS.CA$landCtry<-costCS.CA$vslFlgCtry
            costCS.CA<-  costCS.CA[,CA.col]

            costCS.CA$fishId=seq(1,dim(costCS.CA)[1],by=1)

        }

        print(paste("No rows CA =",  nrow(costCS.CA)), quote=F)

       # write.table(costCS.CA, file.path(path.data, "SDEF CS-CA data.csv"),
                    #sep = ";", row.names = FALSE)

        costCS = csData(tr =costCS.TR, hh = costCS.HH, sl = costCS.SL,
                        hl = costCS.HL, ca=costCS.CA)

        #saveRDS(costCS, "costCS.rds")

    } else {

        print("An error occurred in the trasformation.
        Impossible to create the CS COST object!")
    }
    return(costCS)
}
