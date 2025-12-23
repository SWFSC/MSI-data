# MSI_data_check provides error checking for "HCMSI_Records_SWFSC_Main.xlsx"
# This code was cut and adapted from SeriousInjuryExtract.r

library(readxl)
library(dplyr)
library(magrittr)

# set local path to main data file
path.dat <- "C://Users/alex.curtis/Data/Github/MSI-data/data/"

data = read_excel(paste0(path.dat, "HCMSI_Records_SWFSC_Main.xlsx"))
spgroups <- read.csv(paste0(path.dat, "lt_SpeciesGroups.csv"))
hcmsi.sources <- read.csv(paste0(path.dat, "lt_IntrxnTypes.csv"))

# check for missing data from critical fields
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
invisible(Sys.sleep(10))
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


####**** edits to data ****#### (set up as stops with record numbers to fix)

# correct MSI.Value field for mis-typed entries
data$MSI.Value[which(data$Final.Injury.Assessment=="NSI")] = 0
data$MSI.Value[which(data$Final.Injury.Assessment %in% c("DEAD","CAPTIVITY"))] = 1

# assign small cetacean and pinniped records with final SI designation as MSI.Value=1
# (not applicable to large whales, which may have a prorated value)
pinn.smcet.SI.final = which(data$Final.Injury.Assessment=="SI" & 
                              data$Species %in% c(pinn.spp, sm.cet.spp))
data$MSI.Value[pinn.smcet.SI.final] = 1
rm(pinn.smcet.SI.final)

###*** add checks for whale values

## List of Fisheries (LOF) codes 
data[which(data$Interaction.Type %in% commercial), "COUNT.AGAINST.LOF"] = "Y"
data[which(data$Interaction.Type %in% other), "COUNT.AGAINST.LOF"]="N"
# assign initial NSI designations as not counting against LOF
# (even if an interaction occurred with a commercial fishery, the interaction 
#  needs to have an initial designation of SI for it to 'count' against LOF)
data[which(data$Initial.Injury.Assessment=="NSI"), "COUNT.AGAINST.LOF"] = "N"

## PBR codes
PBR.yes = grep("CAPTIVITY|DEAD|SI|SI (PRORATE)", data$Final.Injury.Assessment)
PBR.no = grep("NSI", data$Final.Injury.Assessment)
data[PBR.yes, "COUNT.AGAINST.PBR"] = "Y"
data[PBR.no, "COUNT.AGAINST.PBR"] = "N"
rm(PBR.yes, PBR.no)

