# Tested on R version 4.1.1
# packages to be loaded

library(RDBprocessing)
library(COSTcore)
library(COSTdbe)
library(COSTeda)

# RCG_CS the RCG CS table in the database
# RCG_CL the RCG CL table in the database
# COST_ce COST ce (effort) table

# The outcome of each function is the corresponindg table in MED & BS data call

# Landing-discard tables by length and age
LAND_MEDBS(RCG_CS,RCG_CL)
DISC_MEDBS(RCG_CS,RCG_CL,COST_ce)
CATCH_MEDBS(RCG_CS,RCG_CL,COST_ce)

# Biological tables
ALK_MEDBS(RCG_CS)
SRL(RCGtoCOST_CS)
ML_MEDBS(RCGtoCOST_CS)
SRA(RCGtoCOST_CS)
MA_MEDBS(RCGtoCOST_CS)
