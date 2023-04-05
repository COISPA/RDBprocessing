#' CATCH table  - FDI data call
#'
#' @param datacl Landing data in RCG CL format
#' @param verbose boolean. If TRUE a message is printed.
#'
#' @return CATCH FDI table
#' @export
#'
#' @examples CATCH_FDI(RDBprocessing::data_exampleCL)
#'@import tidyr

CATCH_FDI<-function(datacl,verbose=FALSE){

header=c("COUNTRY",
"YEAR",
"QUARTER",
"VESSEL_LENGTH",
"FISHING_TECH",
"GEAR_TYPE",
"TARGET_ASSEMBLAGE",
"MESH_SIZE_RANGE",
"METIER",
"METIER_7",
"DOMAIN_DISCARDS",
"DOMAIN_LANDINGS",
"SUPRA_REGION",
"SUB_REGION",
"EEZ_INDICATOR",
"GEO_INDICATOR",
"NEP_SUB_REGION",
"SPECON_TECH",
"DEEP",
"SPECIES",
"TOTWGHTLANDG",
"TOTVALLANDG",
"DISCARDS",
"CONFIDENTIAL")

#datacl=RDBprocessing::data_exampleCL

DF=data.frame(COUNTRY=datacl$flag_country,
              YEAR=datacl$year,
              QUARTER=datacl$quarter,
              VESSEL_LENGTH="NK",
              FISHING_TECH="", # da riempire
              GEAR_TYPE="", # da riempire
              TARGET_ASSEMBLAGE="", # da riempire
              MESH_SIZE_RANGE="NK",
              METIER=datacl$fishing_activity_category_eu_l6,
              METIER_7="NA",
              DOMAIN_DISCARDS="NK",
              DOMAIN_LANDINGS="NK",
              SUPRA_REGION="MBS",
              SUB_REGION=datacl$area,
              EEZ_INDICATOR="NA",
              GEO_INDICATOR="NK",
              NEP_SUB_REGION="NA",
              SPECON_TECH="NK",
              DEEP="NA",
              SPECIES=datacl$species, #da cambiare il 3alpha code,
              TOTWGHTLANDG=round(datacl$official_landings_weight,3)/1000, # wiehgts to be reported in tons
              TOTVALLANDG=round(datacl$official_landings_value,3),
              DISCARDS="NK",
              CONFIDENTIAL="N")

# update FISHING_TECH and GEAR

# extraction of the gear from the metier
DF= DF %>% tidyr::separate(col="METIER", into=c("GEAR_TYPE",NA,NA,NA,NA),sep="_")

DF$FISHING_TECH <- RDBprocessing::FT_GEAR$FISHING_TECH[match(DF$GEAR_TYPE ,
                                                             RDBprocessing::FT_GEAR$GEAR)]

DF$SPECIES<-RDBprocessing::Annex17$Species[match(DF$SPECIES ,
                                                       RDBprocessing::Annex17$Scientific_name)]
return(DF)

    }
