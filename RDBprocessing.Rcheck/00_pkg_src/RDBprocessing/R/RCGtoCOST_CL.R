#' Function converting RCG CL in COST CL object
#' @param data Landing data in RCG CL format
#' @param verbose boolean. If TRUE a message is printed.
#' @return COST CL object
#' @export
#' @examples RCGtoCOST_CL(RDBprocessing::data_exampleCL)
#' @importFrom methods new
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @import COSTcore

RCGtoCOST_CL<-function(data, verbose = FALSE){


    CL=data
    error <- FALSE
    dataset_proj <- ""
    bad.rm <- FALSE
    log_var  <- TRUE
    #lenNum <- log_varMsg<- vslFlgCtry
    logMsg<- NULL

    names(CL)[which(tolower(names(CL)) == "flag.country")] <- "vslFlgCtry"
    names(CL)[which(tolower(names(CL)) == "flag_country")] <- "vslFlgCtry"

    names(CL)[which(tolower(names(CL)) == "year")] <- "year"
    names(CL)[which(tolower(names(CL)) == "quarter")] <- "quarter"
    names(CL)[which(tolower(names(CL)) == "month")] <- "month"
    names(CL)[which(tolower(names(CL)) == "area")] <- "area"
    names(CL)[which(tolower(names(CL)) == "species")] <- "taxon"

    names(CL)[which(tolower(names(CL)) ==
                        "fishing.activity.category.national")] <- "foCatNat"
    names(CL)[which(tolower(names(CL)) == "fac_national")] <- "foCatNat"

    names(CL)[which(tolower(names(CL)) ==
                        "fishing.activity.category.european.lvl.6")] <- "foCatEu6"
    names(CL)[which(tolower(names(CL)) == "fac_ec_lvl6")] <- "foCatEu6"

    names(CL)[which(tolower(names(CL)) == "harbour")] <- "harbour"

    names(CL)[which(tolower(names(CL)) == "official.landings.weight")] <- "landWt"
    names(CL)[which(tolower(names(CL)) == "official_landings_weight")] <- "landWt"

    names(CL)[which(tolower(names(CL)) == "official.landings.value")] <- "landValue"
    names(CL)[which(tolower(names(CL)) == "official_landings_value")] <- "landValue"


    ## primary keys & fields

    clPk <- c("vslFlgCtry", "year", "quarter", "month", "area", "taxon",
              "foCatNat", "foCatEu6")
    clOther <- c("landWt", "landValue")

    # check fields
    allFields <- c(clPk, clOther)
    missingFields <- allFields[! allFields %in% names(CL)]
    if (length(missingFields) > 0) {
        stop("Missing fields : ", paste(missingFields, collapse = ", ", sep=""))
    }

    clPkV <- fpKey(CL, clPk)
    clPkVDup <- clPkV %in% clPkV[duplicated(clPkV)]

    # test integrity
    if (any(clPkVDup)) {
        if (log) {
            logCl$duplicated <-  fpKey(CL, clPk)
        }


        if (bad.rm) {
            message("Integrity problem for CL, ", sum(clPkVDup),
                    " row(s) removed.", logMsg)
            CL <- CL[! clPkVDup,]
        } else {
            error <- TRUE
            message("Integrity problem for CL, ", sum(clPkVDup),
                    " row(s) concerned.", logMsg)
        }
    }



    # if (log) {
    #     if (missing(logFilePath)) {
    #         logFilePath <- tempfile(fileext = ".csv")
    #     }
    #    # write.table(logCl, file=logFilePath, row.names = FALSE, sep=";")
    #     message("Log file: ", logFilePath)
    # }

    if (error) {
        stop("Stop on reported errors.")
    }


    # formating

    CL$taxon <- unlist(lapply(CL$taxon, function(x)
        paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)), sep="")))


    ## df
    clDf <- data.frame(
        landCtry=NA,
        vslFlgCtry=CL$vslFlgCtry,
        year=CL$year,
        quarter=CL$quarter,
        month=CL$month,
        area=CL$area,
        rect=NA,
        subRect=NA,
        taxon=CL$taxon,
        landCat=NA,
        commCatScl=NA,
        commCat=NA,
        foCatNat=CL$foCatNat,
        foCatEu5=NA,
        foCatEu6=CL$foCatEu6,
        harbour=CL$harbour,
        vslLenCat=NA,
        unallocCatchWt=NA,
        misRepCatchWt=NA,
        landWt=CL$landWt,
        landMult=NA,
        landValue=CL$landValue,
        stringsAsFactors=FALSE)

    #write.table(clDf, file.path(path.data,  "SDEF CL data.csv"), sep=";", row.names = FALSE)

    costCL = clData(cl=clDf)
    #saveRDS(costCL, "costCL.rds")

return(costCL)
    }
