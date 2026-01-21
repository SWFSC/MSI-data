# MSI_data_check provides error checking for "HCMSI_Records_SWFSC_Main.xlsx"
# This code was cut and adapted from SeriousInjuryExtract.r

# Load required packages
library(dplyr)
library(magrittr)
library(readxl)

# import data
data <- data.og <- read_excel("../data/HCMSI_Records_SWFSC_Main.xlsx")
spgroups <- read.csv("../data/lt_SpeciesGroups.csv")
hcmsi.sources <- read.csv("../data/lt_IntrxnTypes.csv")

# check for missing data from critical fields (*** MAKE THIS MORE THOROUGH AND MORE INFORMATIVE ***)
missing.data <- function(df) {
  # find missing data by column
  a <- which(is.na(df$Initial.Injury.Assessment==TRUE))
  b <- which(is.na(df$Final.Injury.Assessment==TRUE))
  c <- which(is.na(df$MSI.Value==TRUE))
  d <- which(is.na(df$COUNT.AGAINST.LOF==TRUE))
  e <- which(is.na(df$COUNT.AGAINST.PBR==TRUE))
  f <- which(is.na(df$Year==TRUE))
  # list all rows with missing data
  missing.list <- rbind.data.frame(a,b,c,d,e,f)
  if(nrow(missing.list)>=1) {
    stop("missing data in lines: ", missing.list)
  } else { 
    message("no missing data") 
  }
}
missing.data(data)
invisible(Sys.sleep(1))
rm(missing.data)

# any species not captured in Species_Groups.csv?
unlisted.spp <- setdiff(unique(data$Species), spgroups$Species)
if (length(unlisted.spp)>0) 
  stop("The following species are not included in a species group:\n",
       paste(unlisted.spp, collapse=","))
rm(unlisted.spp)

# subset species list for each species group
pinn.spp <- spgroups %>% filter(SpeciesGroup=="pinniped") %>% select(Species) %>% unlist()
sm.cet.spp <- spgroups %>% filter(SpeciesGroup=="small cetacean") %>% select(Species) %>% unlist()
lg.whale.spp <- spgroups %>% filter(SpeciesGroup=="large whale") %>% select(Species) %>% unlist()
rm(spgroups)

###*** CHECK THAT ALL CAUSES ARE CLASSIFIED AS COMMERCIAL OR OTHER
unlisted.intrxns <- setdiff(unique(data$Interaction.Type), hcmsi.sources$Interaction.Type)
if (length(unlisted.intrxns)>0) 
  stop("The following interaction types are not classified as commercial or other:\n",
       paste(unlisted.intrxns, collapse=","))
rm(unlisted.intrxns)

# subset lists of 'commercial' and 'other' interaction types
commercial <- hcmsi.sources %>% filter(SARTable=="commercial") %>% select(Interaction.Type) %>% unlist()
other <- hcmsi.sources %>% filter(SARTable=="other") %>% select(Interaction.Type) %>% unlist()
rm(hcmsi.sources)


# edits to data

## correct MSI.Value field for mis-typed entries
data$MSI.Value[which(data$Final.Injury.Assessment=="NSI")] = 0
data$MSI.Value[which(data$Final.Injury.Assessment %in% c("DEAD","CAPTIVITY"))] = 1
### assign small cetacean and pinniped records with final SI designation as MSI.Value=1
### (not applicable to large whales, which may have a prorated value)
pinn.smcet.SI.final = which(data$Final.Injury.Assessment=="SI" & 
                              data$Species %in% c(pinn.spp, sm.cet.spp))
data$MSI.Value[pinn.smcet.SI.final] = 1
rm(pinn.smcet.SI.final)

###*** ADD CHECKS FOR WHALE VALUES

## List of Fisheries (LOF) codes 
data$COUNT.AGAINST.LOF[which(data$Interaction.Type %in% commercial)] = "Y"
data$COUNT.AGAINST.LOF[which(data$Interaction.Type %in% other)]="N"
### assign initial NSI designations as not counting against LOF
### (even if an interaction occurred with a commercial fishery, the interaction 
###  needs to have an initial designation of SI for it to 'count' against LOF)
data$COUNT.AGAINST.LOF[which(data$Initial.Injury.Assessment=="NSI")] = "N"

## PBR codes
PBR.yes = grep("CAPTIVITY|DEAD|SI|SI (PRORATE)", data$Final.Injury.Assessment)
PBR.no = grep("NSI", data$Final.Injury.Assessment)
data$COUNT.AGAINST.PBR[PBR.yes] = "Y"
data$COUNT.AGAINST.PBR[PBR.no] = "N"
rm(PBR.yes, PBR.no)

# summarize data changes suggested 
all.equal(data, data.og)


# review initial versus final determinations for most recent six years 
# (to account for addition of records for some species in advance of typical schedule)
# summarized initial condition NSI/SI records by interaction.type
yy <- (max(data$Year)-5):max(data$Year)
data.last6 <- data %>% filter(Year %in% yy)
mortality = which(data.last6$Final.Injury.Assessment=="DEAD")   ### SHOULDN'T THIS BE INITIAL? SAME IN SeriousInjuryExtract.R ###
alive = data.last6[-mortality,]
dead = data.last6[mortality,]
rm(mortality, yy)
# SI determinations
PINNIPEDS.DETERMINED = alive %>% filter(Species %in% pinn.spp)
SMALL.CET.DETERMINED = alive %>% filter(Species %in% sm.cet.spp)
WHALES.DETERMINED = alive %>% filter(Species %in% lg.whale.spp)
## check initial vs final assessments (*** MOVE TO A SCRIPT FOR CHECKING MOST RECENT YEAR OR FIVE YEARS?***)
table(PINNIPEDS.DETERMINED$Initial.Injury.Assessment, PINNIPEDS.DETERMINED$Final.Injury.Assessment)
table(SMALL.CET.DETERMINED$Initial.Injury.Assessment, SMALL.CET.DETERMINED$Final.Injury.Assessment)
table(WHALES.DETERMINED$Initial.Injury.Assessment, WHALES.DETERMINED$Final.Injury.Assessment)


# # write changes to xlsx (!!first copy original xlsx to R folder as working copy so can check changes!!)
# library(openxlsx)   # GHCopilot suggests this is the best package for editing individual cell values in excel
# wb <- loadWorkbook("HCMSI_Records_SWFSC_Main.xlsx")
# 
# ## write changes to MSI.Value
# ic.msi <- which(names(data) == "MSI.Value")
# ir.msi <- which(data.og$MSI.Value != data$MSI.Value)
# for (i in 1:length(ir.msi)) {
#   writeData(wb, sheet = "Anthropogenic_Mortality_Serious", x = unlist(data[ir.msi[i], ic.msi]),
#             startCol = ic.msi, startRow = ir.msi[i] + 1)
# }
# 
# ## write changes to COUNT.AGAINST.PBR
# ic.pbr <- which(names(data) == "COUNT.AGAINST.PBR")
# ir.pbr <- which(data.og$COUNT.AGAINST.PBR != data$COUNT.AGAINST.PBR)
# for (i in 1:length(ir.pbr)) {
#   writeData(wb, sheet = "Anthropogenic_Mortality_Serious", x = unlist(data[ir.pbr[i], ic.pbr]),
#             startCol = ic.pbr, startRow = ir.pbr[i] + 1)
# }
# 
# ## write changes to COUNT.AGAINST.LOF
# ic.lof <- which(names(data) == "COUNT.AGAINST.LOF")
# ir.lof <- which(data.og$COUNT.AGAINST.LOF != data$COUNT.AGAINST.LOF)
# for (i in 1:length(ir.lof)) {
#   writeData(wb, sheet = "Anthropogenic_Mortality_Serious", x = unlist(data[ir.lof[i], ic.lof]),
#             startCol = ic.lof, startRow = ir.lof[i] + 1)
# }
# 
# # Save workbook
# saveWorkbook(wb, "HCMSI_Records_SWFSC_Main.xlsx", overwrite = TRUE)
# 
# # check work
# data.ed = read_excel("HCMSI_Records_SWFSC_Main.xlsx")
# all.equal(data.ed, data)  # should return TRUE
