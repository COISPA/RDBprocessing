pkgname <- "RDBprocessing"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "RDBprocessing-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('RDBprocessing')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ALK_MEDBS")
### * ALK_MEDBS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ALK_MEDBS
### Title: Age Length Key (ALK) table - MED & BS data call
### Aliases: ALK_MEDBS

### ** Examples

library(COSTcore)
ALK_MEDBS(RDBprocessing::data_ex)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ALK_MEDBS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CATCH_FDI")
### * CATCH_FDI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CATCH_FDI
### Title: CATCH table - FDI data call
### Aliases: CATCH_FDI

### ** Examples

CATCH_FDI(RDBprocessing::data_exampleCL)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CATCH_FDI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CATCH_MEDBS")
### * CATCH_MEDBS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CATCH_MEDBS
### Title: Catch at age (CATCH) table - MED & BS data call
### Aliases: CATCH_MEDBS

### ** Examples

CATCH_MEDBS(RDBprocessing::data_ex,RDBprocessing::data_exampleCL,RDBprocessing::ce_example)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CATCH_MEDBS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DISC_MEDBS")
### * DISC_MEDBS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DISC_MEDBS
### Title: Discard by length (DISCARDS) table - MED & BS data call
### Aliases: DISC_MEDBS

### ** Examples

DISC_MEDBS(RDBprocessing::data_ex,RDBprocessing::data_exampleCL,RDBprocessing::ce_example)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DISC_MEDBS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("LAND_MEDBS")
### * LAND_MEDBS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: LAND_MEDBS
### Title: Landing by length (LANDINGS) table - MED & BS data call
### Aliases: LAND_MEDBS

### ** Examples

LAND_MEDBS(RDBprocessing::data_ex,RDBprocessing::data_exampleCL)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("LAND_MEDBS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("MA_MEDBS")
### * MA_MEDBS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MA_MEDBS
### Title: Maturity at age (MA) table - MED & BS data call
### Aliases: MA_MEDBS

### ** Examples

MA_MEDBS(RDBprocessing::data_ex)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MA_MEDBS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ML_MEDBS")
### * ML_MEDBS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ML_MEDBS
### Title: Maturity at length (ML) table - MED & BS data call
### Aliases: ML_MEDBS

### ** Examples

ML_MEDBS(RDBprocessing::data_ex)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ML_MEDBS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RCGtoCOST_CL")
### * RCGtoCOST_CL

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RCGtoCOST_CL
### Title: Function converting RCG CL in COST CL object
### Aliases: RCGtoCOST_CL

### ** Examples

RCGtoCOST_CL(RDBprocessing::data_exampleCL)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RCGtoCOST_CL", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RCGtoCOST_CS")
### * RCGtoCOST_CS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RCGtoCOST_CS
### Title: Function converting RCG CS in COST CS object
### Aliases: RCGtoCOST_CS

### ** Examples

RCGtoCOST_CS(RDBprocessing::data_ex)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RCGtoCOST_CS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SRA_MEDBS")
### * SRA_MEDBS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SRA_MEDBS
### Title: Sex ratio at age (SRA) table - MED & BS data call
### Aliases: SRA_MEDBS

### ** Examples

SRA_MEDBS(RDBprocessing::data_ex)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SRA_MEDBS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SRL_MEDBS")
### * SRL_MEDBS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SRL_MEDBS
### Title: Sex ratio at length (SRL) table - MED & BS data call
### Aliases: SRL_MEDBS

### ** Examples

SRL_MEDBS(RDBprocessing::data_ex)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SRL_MEDBS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TabII2_GFCM")
### * TabII2_GFCM

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TabII2_GFCM
### Title: Table II.2 - GFCM DCRF data call
### Aliases: TabII2_GFCM

### ** Examples

TabII2_GFCM(RDBprocessing::CATCH_FDIex)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TabII2_GFCM", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TableVII2")
### * TableVII2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TableVII2
### Title: TableVII2 - GFCM DCRF datacall
### Aliases: TableVII2

### ** Examples

library(COSTcore)
TableVII2(RDBprocessing::data_ex,RDBprocessing::data_exampleCL)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TableVII2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TableVII31")
### * TableVII31

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TableVII31
### Title: TableVII.3.1 - GFCM DCRF datacall
### Aliases: TableVII31

### ** Examples

TableVII31(RDBprocessing::data_ex)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TableVII31", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_cs_header")
### * check_cs_header

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_cs_header
### Title: Headers check for CS table
### Aliases: check_cs_header

### ** Examples

check_cs_header(data_ex)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_cs_header", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fpKey")
### * fpKey

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fpKey
### Title: Function to create a key from the fields of a table
### Aliases: fpKey

### ** Examples

fpKey(data_ex, colnames(data_ex)[1:5])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fpKey", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
