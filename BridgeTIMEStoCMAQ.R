#Create a CMAQ Emission Control File based on TIMES output
#Using EPAUS9rT database. Contact Lenox.Carol@epa.gov for the database. 

#To find the *.VD file (first run TIMES), go to Veda/Veda_FE/GAMS_WrkTIMES/
#Select the folder for the desired run, file will be <casename>_<day><month>.vd

#To run CMAQ, name the output file "EmissCtrl_*.nml"
# and update line 70 in the run_cctm....csh for SCENARIO to = * in nml

## User Choices; First two changed for each run, second chunk may only change for initial setup
TRunFile <-   "ISUS_2704.vd"  #Base/BAU, MUDL, CONS, ISUS, GOOW, for 2016 just delete below "TIMES scaling"
OutFileName <- "EmissCtrl_MUDL.nml" 

FutureYear <- 2045
BaseYear <- 2015  #Closest TIMES output year to your NEI year
#Location and name of TIMES *.VD files
FindFile<-("C:/Veda/Veda_FE/GAMS_WrkTIMES/")
OutLoc <- ("CMAQ_Server")

library(stringr)
library(dplyr)

## Import TIMES *.VD file
headers<-c("Attribute","Commodity","Process","Period","Region","Vintage",
           "TimeSlice","UserConstraint","Value")
Imported<- read.delim(file=paste(FindFile, TRunFile,sep=""), 
                      header=FALSE, sep=",",quote="\"", skip=12)
colnames(Imported)<-headers
message(TRunFile, " has been imported") #just so you know it's working, can be a bit slow

#Cuts time by shrinking dataframe. All emissions will be VAR_FOut.
Mapped <- filter(Imported, Attribute == "VAR_FOut") 

## Replace process names with a representation of the emissions sector they map to
#Transportation
Mapped$Process <- str_replace_all(Mapped$Process, 
                                        "^TL.*CONV.*|^TL.*GSL", "_LDV_gas")
Mapped$Process <- str_replace_all(Mapped$Process, 
                                       "^TL.*DSL", "_LDV_diesel")
#LD other in the CMAQ emissions files is basically just E85 (based on MOVES)
Mapped$Process <- str_replace_all(Mapped$Process, 
                                        "^TL.*CNG|^TL.*LPG|^TL.*ELC|^TL.*ETH.*|^TL.*E85.*","_LDV_other")
Mapped$Process <- str_replace_all(Mapped$Process, 
                                        "^TBGSL.*|^TCGSL.*|^TH.GSL.*|^TM.*GSL.*","_HDV_gas")
Mapped$Process <- str_replace_all(Mapped$Process, 
                                        "^TBDSL.*|^TCDSL.*|^TH.B20.*|^TH.DSL.*|^TMB20.*|^TMDSL.*","_HDV_diesel")
#HD other is basically just CNG for buses (based on MOVES)
Mapped$Process <- str_replace_all(Mapped$Process, 
                                        "^TBCNG.*|^TCCNG.*|^TH.CNG.*|^TH.LPG.*|^TMCNG.*|^TMLPG.*","_HDV_other")
Mapped$Process <- str_replace_all(Mapped$Process, "^TO.*","_nonroad")
Mapped$Process <- str_replace_all(Mapped$Process, "^TP.*|^TR.*","_rail")
Mapped$Process <- str_replace_all(Mapped$Process, "^TA.*","_airport")
Mapped$Process <- str_replace_all(Mapped$Process, "^TS.*","_shipping")

#Refineries don't need matching, can be done solely on commodity
refineries <- which(grepl("^REF.*",Mapped$Commodity))
Mapped$Process[refineries] <- "_refinery"

#EGU
#Effectively EGU is Coal and not-coal due to base year issues & ambiguous definitions
#Left detailed mapping in case it's useful for your specific use case, but these get summed here
Mapped$Process <- str_replace_all(Mapped$Process,"SEELCBIGCC","EBIOEMIS") #separate so that separate biomass doesn't get trapped in coal and can instead contribute to other
Mapped$Process <- str_replace_all(Mapped$Process,"^ECOAL.*|^ECHPCOAL.*|^ECSTMR.*|^SEC(IGCC|ST).*|^SEELCB.*","_EGU_coal") #includes biomass cofiring b/c co-located
Mapped$Process <- str_replace_all(Mapped$Process,"^ENGA+CT.*|^EDSL.*|^SEELCNGT.*|^SEELCDSL.*","_EGU_peak") #Peaking hard to match, just done using KEB's discression
Mapped$Process <- str_replace_all(Mapped$Process, "^ENGA+C.*|^ENGASTM.*|ECHPNGA.*|^SEELCNGC.*|^SEELCNGS.*|^ECHPOIL.*|^ERFL.*|^SEELCRF.*|^SENGAC.*","_EGU_gasoil")
Mapped$Process <- str_replace_all(Mapped$Process,"^EXP.*","TRADE") #get these out of the way for EGU_other
Mapped$Process <- str_replace_all(Mapped$Process,"^E.*|^SELF.*|^SEBSTM.*|^SEELCWTE.*","_EGU_other")#by using gsub, EGU other should be any EGU left after the first three redefined
Mapped$Process <- str_replace_all(Mapped$Process, "_EGU_peak|_EGU_other", "_EGU_gasoil")

#Industrial
#Like export in electricity, imports will accidentally mix in here, so move
Mapped$Process <- str_replace_all(Mapped$Process,"^IMP.*","TRADE")
Mapped$Process <- str_replace_all(Mapped$Process,"^INC.*","_I_cement")
Mapped$Process <- str_replace_all(Mapped$Process,"^IC.*","_I_chem")
Mapped$Process <- str_replace_all(Mapped$Process,"^IF.*","_I_food")
Mapped$Process <- str_replace_all(Mapped$Process,"^IM.*","_I_metal")
Mapped$Process <- str_replace_all(Mapped$Process,"^IN.*","_I_mineral")
Mapped$Process <- str_replace_all(Mapped$Process,"^IP.*","_I_paper")
Mapped$Process <- str_replace_all(Mapped$Process,"^IO.*","_I_other")

#residential wood burning
Mapped$Process <- str_replace_all(Mapped$Process,"^RSHWD.*","_reswood")

#oil&gas extraction
wells <- which(grepl("^RSS.*",Mapped$Commodity))
Mapped$Process[wells] <- "_oilgas"

#move because could cause problems (ends like OC or CO)
Mapped$Commodity <- str_replace_all(Mapped$Commodity, "ELCH2OC", "Water")
Mapped$Commodity <- str_replace_all(Mapped$Commodity, "CSTM.*CO", "coal")
Mapped$Commodity <- str_replace_all(Mapped$Commodity, "BSYOC", "soybeanoil")
Mapped$Commodity <- str_replace_all(Mapped$Commodity, "INDELCCO", "ELCtoOrgChem" )
Mapped$Commodity <- str_replace_all(Mapped$Commodity, "ICO", "orgChemDMD")
Mapped$Commodity <- str_replace_all(Mapped$Commodity, ".*CO2$","GHG")

#Simplify emissions names from source specific name to pollutant/species name
#This is required because the source specific names don't match up exactly to the streams in CMAQ
#Emis <- "BC$|CO$|NH3$|NOX$|OC$|PM10$|PM25$|SO2$|VOC$"
Mapped$Commodity <- str_replace_all(Mapped$Commodity, ".*BC$","BC")
Mapped$Commodity <- str_replace_all(Mapped$Commodity, ".*CO$","CO")
Mapped$Commodity <- str_replace_all(Mapped$Commodity, ".*NH3$","NH3")
Mapped$Commodity <- str_replace_all(Mapped$Commodity, ".*NOX$","NOX")
Mapped$Commodity <- str_replace_all(Mapped$Commodity, "(?!.*VOC$).*OC$","OC")
Mapped$Commodity <- str_replace_all(Mapped$Commodity, ".*PM10$","PM10")
Mapped$Commodity <- str_replace_all(Mapped$Commodity, ".*PM25$","PM25")
Mapped$Commodity <- str_replace_all(Mapped$Commodity, ".*SO2$","SO2")
Mapped$Commodity <- str_replace_all(Mapped$Commodity, ".*VOC$","VOC")

## Sum the value for all matching instances of Process, Commodity, Region, year
#And keep only relevant data points
#Emis <- "BC|CO|NH3|NOX|OC|PM10|PM25|SO2|VOC"
Emis <- "^BC$|^CO$|^NH3$|^NOX$|^OC$|^PM10$|^PM25$|^SO2$|^VOC$"
SumFuture2 <- Mapped %>%
  filter(Period %in% c(BaseYear, FutureYear)) %>%
  filter(str_detect(Process,"^_")) %>% #Process starts with "_" (meaning it was defined above)
  filter(str_detect(Commodity,Emis)) %>%
  group_by(Commodity, Process, Region, Period)  %>%
  summarize("Tot"=sum(Value)) %>%
  arrange(Commodity, Process, Region, Period)


## Calculate Scaling Factors
#define a function that will be needed in a second
divideif2 <- function(A, N) {
   if (N == 2){
    ans <- A[2]/A[1] #arrange above should keep this working right (fut/base)
    ans <- round(ans, digits = 3)
  } else {
    ans <- 0 #this captures both where there is none in the future (keep 0)
    #and where user insight might be needed
  }
  return(ans)
}

ScalingFactors <- SumFuture2 %>%
  group_by(Commodity, Process, Region) %>%
  summarize(
    "N"=n(), 
    "Early"=min(as.numeric(as.character(Period))),  #factors are annoying
    "Val"=sum(Tot),                                #For use with Decisions
    "Factor"=divideif2(Tot,N))


#In some cases there will be future emissions with no base emissions to scale
#This requires you to recategorize above and rerun
problems <- which(ScalingFactors$Early == FutureYear)
Decisions <- ScalingFactors[problems,]
#notify user to make a decision about these specific cases. 
message("There are future emissions that don't map to existing sources. User decision required.")

#After running once, check WhatToFix and see if this needs modification
WhatToFix <- unique(paste(Decisions$Process, Decisions$Region, sep = " "))
message("current issues: ", paste(WhatToFix,separator=" & "), "that's it")

# #############################################################################
# ## Uncomment to reassign unmapped future emissions
# 
# Redo <- SumFuture2  #Before you had calculated the scaling factors
# 
# Issues <- which(Redo$Process == "From WhatToFix" & Redo$Region == "From WhatToFix")
# Redo$Process[Issues] <- "Reassign this to one of the existing Processes, e.g. _LDV_gas"  
# 
# #After reassigning each new emission to an existing source, we need to resum and redivide
# ScalingFactors2 <- Redo %>%
#   group_by(Commodity, Process, Region, Period)  %>%  
#   summarize("Total"=sum(Tot)) %>%                      #Have to redefine Tot b/c changed processes
#   group_by(Commodity, Process, Region) %>%
#   summarize(
#     "N"=n(), 
#     "Early"=min(as.numeric(as.character(Period))), 
#     "Val"=sum(Total),                                
#     "Factor"=divideif2(Total,N))
# #################################################################################
## Comment next line if uncomment bit above
ScalingFactors2 <- ScalingFactors
###################################################################################

## Create Control File For CMAQ
#most of this file shouldn't change each time. Only the scaling factor values

#Create a dataframe with exactly the information needed for the control file
SF <- ScalingFactors2[c(3,2,1,7,6)]
newHeads <- c("Region","Stream","spc","factor","baseOp")
colnames(SF) <- newHeads

#Reformat the information in that data to output what's needed for the control file
SF$Region <- str_replace_all(SF$Region, "R1", "'Rone',    '")
SF$Region <- str_replace_all(SF$Region, "R2", "'Rtwo',    '")
SF$Region <- str_replace_all(SF$Region, "R3", "'Rthree',  '")
SF$Region <- str_replace_all(SF$Region, "R4", "'Rfour',   '")
SF$Region <- str_replace_all(SF$Region, "R5", "'Rfive',   '")
SF$Region <- str_replace_all(SF$Region, "R6", "'Rsix',    '")
SF$Region <- str_replace_all(SF$Region, "R7", "'Rseven',  '")
SF$Region <- str_replace_all(SF$Region, "R8", "'Reight',  '")
SF$Region <- str_replace_all(SF$Region, "R9", "'Rnine',   '")

SF$Stream <- str_replace_all(SF$Stream, "_", "")
#Figure out what names are needed and replace each individually as above and below

#Rearrange so that rules for aerosols are in the right order
#The modified rules for PM can then go in this order:
#	Rules for ‘FINE’ PM (use ‘m’)
#	Rules for ‘COARSE’ PM(use ‘m’)
#	Rules for EC (use ‘o’)
#	Rules for OC (use ‘o’)

fine <- which(SF$spc == "PM25")
coarse <- which(SF$spc == "PM10")
Black <- which(SF$spc == "BC")
Org <- which(SF$spc == "OC")
overwrite <- c(Black, Org)
obs <- c(1:length(SF$spc))
aerosols <- c(overwrite, fine, coarse)
gases <- obs[!(obs %in% aerosols)] 

SF$baseOp <- ", 'UNIT', 'm',"
SF$baseOp[overwrite] <- ", 'UNIT', 'o',"     #not double multiply
#Need to use overwrite before reordering

ordered <- rbind(SF[fine,], SF[coarse,], SF[Black,], SF[Org,], SF[gases,])
SF <- ordered

# ', surrogate, species, mode ,
#CO in particular was picking up tons of extra stuff, so ^$ makes it match exactly
SF$spc <- str_replace_all(SF$spc, "^BC$", "', 'PEC', 'AEC', 'AERO', ")
SF$spc <- str_replace_all(SF$spc, "^PM25$", "', 'ALL', 'ALL', 'FINE', ")
SF$spc <- str_replace_all(SF$spc, "^NOX$", "', 'NOX', 'NOX', 'GAS', ")
SF$spc <- str_replace_all(SF$spc, "^CO$", "', 'CO', 'CO', 'GAS', " )
SF$spc <- str_replace_all(SF$spc, "^NH3$", "', 'ALL', 'NH3', 'GAS', ")
SF$spc <- str_replace_all(SF$spc, "(?<!V)OC$", "',  'OC', 'ALL', 'AERO',  ")        
#SF$spc <- str_replace_all(SF$spc, "^OC$", "',  'OC', 'ALL', 'AERO',  ")
SF$spc <- str_replace_all(SF$spc, "^PM10$", "','ALL','ALL','COARSE',")
SF$spc <- str_replace_all(SF$spc, "^SO2$", "', 'SO2', 'SO2', 'GAS', ")
#SF$spc <- str_replace_all(SF$spc, "^VOC$", "', 'VOC', 'ALL', 'GAS', ")
SF$spc <- str_replace_all(SF$spc, "^VOC$", "', 'ALL', 'VOC','GAS', " )

#Output the following:
OutName <- paste(OutLoc, OutFileName)

#The part in "Unchanged" is required setup for all the runs
Unchanged <- c(
"!------------------------------------------------------------------------------!",
"! EMISSION CONTROL INPUT FILE FOR THE                                          !",
"! COMMUNITY MULTISCALE AIR QUALITY (CMAQ) MODEL                                !",
"! BASED ON OUTPUT FROM THE TIMES ENERGY SYSTEM MODEL                           !",
"!------------------------------------------------------------------------------!",
"\n",
"&RegionsRegistry",
  "RGN_NML  =",   
"!  | Region Label   | File_Label  | Variable on File",
"    'ALL'         ,'STATES_FILE' ,  'ALL',",
"\n",
"&RegionFamilies",
  "NRegionFamilies          = 9",
  "RegionFamilyName(1)      = 'Rone'",
  "RegionFamilyNum(1)       = 6",
  "RegionFamilyMembers(1,:) = 'ME','NH','MA','VT','CT','RI'",
  "RegionFamilyName(2)      = 'Rtwo'",
  "RegionFamilyNum(2)       = 3",
  "RegionFamilyMembers(2,:) = 'NY','NJ','PA'",
  "RegionFamilyName(3)      = 'Rthree'",
  "RegionFamilyNum(3)       = 5",
  "RegionFamilyMembers(3,:) = 'MI','OH','IN','IL','WI'",
  "RegionFamilyName(4)      = 'Rfour'",
  "RegionFamilyNum(4)       = 7",
  "RegionFamilyMembers(4,:) = 'MN','IA','MO','KS','NE','SD','ND'",
  "RegionFamilyName(5)      = 'Rfive'",
  "RegionFamilyNum(5)       = 8",
  "RegionFamilyMembers(5,:) = 'DE','MD','WV','VA','NC','SC','GA','FL'",
  "RegionFamilyName(6)      = 'Rsix'",
  "RegionFamilyNum(6)       = 4",
  "RegionFamilyMembers(6,:) = 'KY','TN','AL','MS'",
  "RegionFamilyName(7)      = 'Rseven'",
  "RegionFamilyNum(7)       = 4",
  "RegionFamilyMembers(7,:) = 'TX','LA','AR','OK'",
  "RegionFamilyName(8)      = 'Reight'",
  "RegionFamilyNum(8)       = 8",
  "RegionFamilyMembers(8,:) = 'MT','ID','WY','CO','UT','NM','AZ','NV'",
  "RegionFamilyName(9)      = 'Rnine'",
  "RegionFamilyNum(9)       = 3",
  "RegionFamilyMembers(9,:) = 'CA','OR','WA'",
"/",
"\n",

"&StreamFamilies",
"NstreamFamilies            = 4",
"StreamFamilyName(1)        = 'RAIL'",
"StreamFamilyNum(1)         =  2",
"StreamFamilyMembers(1,:)   = 'RAIL','PTNONIPM_RAIL'",
"StreamFamilyName(2)        = 'OILGAS'",
"StreamFamilyNum(2)         =  2",
"StreamFamilyMembers(2,:)   = 'NP_OILGAS','PT_OILGAS'",
"StreamFamilyName(3)        = 'SHIPPING'",
"StreamFamilyNum(3)         =  2",
"StreamFamilyMembers(3,:)   = 'CMV_C1C2','CMV_C3'",
"StreamFamilyName(4)        = 'EGUgasoil'",
"StreamFamilyNum(4)         = 2",
"StreamFamilyMembers(4,:)   = 'EGUgas','EGUpeak'", 
"/",
"\n",  

"&ChemicalFamilies",
  "NChemFamilies           = 3  ",          #maybe more
  "ChemFamilyName(1)       = 'NOX'",
  "ChemFamilyNum(1)        = 2",
  "ChemFamilyMembers(1,:)  = 'NO','NO2'",
  "ChemFamilyName(2)       = 'VOC'",
  "ChemFamilyNum(2)        = 20",       

#  "ChemFamilyMembers(2,:)  = ",
#"'TOL','XYLMN','BENZENE','NAPH','PAR','PRPA','MEOH','ETH','ETOH','OLE','ACET','FORM','GLY','KET','ETHY','ALD2','ETHA','IOLE','ALDX','ISOP','TERP'",
  "ChemFamilyMembers(2,:)  = ",
"'TOL','XYLMN','BENZENE','NAPH','PAR','PRPA','MEOH','ETH','ETOH','OLE','ACET','FORM','KET','ETHY','ALD2','ETHA','IOLE','ALDX','ISOP','TERP'",
  "ChemFamilyName(3)       = 'OC'",
  "ChemFamilyNum(3)        = 2",
  "ChemFamilyMembers(3,:)  = 'POC','PNCOM'",
" /",
 "\n",

"&SizeDistributions",
"SD_NML    = ",
"!         | Stream Label   | Surr. Mode   | Ref. Mode ",
"!<Default>  'ALL'          ,'FINE'        ,'FINE_REF',",
"!<Default>  'ALL'          ,'COARSE'      ,'COARSE_REF',",
"!            'WBDUST'       ,'FINE'        ,'FINE_WBDUST',",
"!            'WBDUST'       ,'COARSE'      ,'COARSE_WBDUST',",
"            'SEASPRAY'     ,'FINE'        ,'FINE_SEASPRAY',",
"            'SEASPRAY'     ,'COARSE'      ,'COARSE_SEASPRAY',",
#"!<Example>  'AIRCRAFT'     ,'FINE'        ,'AIR_FINE',   !To use these examples, you ",
#"!<Example>  'AIRCRAFT'     ,'COARSE'      ,'AIR_COARSE', ! must add entries for AIR_FINE",
#"                                                         ! and AIR_COARSE to the data structure",
#"                                                         ! em_aero_ref in AERO_DATA.",
"/",
"\n",
"&EmissionScalingRules",
"EM_NML=",
"! Region |Stream | Emission | CMAQ- |Phase/ |Scale |Basis |Op",
"! Label  |Label  | Surrogate|Species|Mode   |Factor|      |",
#Default mapping first, scaling factors will build on this, but this introduces them
"! Default Gases    ",
"'EVERYWHERE', 'ALL'    ,'NO2'    ,'NO2'         ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'NO'     ,'NO'          ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'HONO'   ,'HONO'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'SO2'    ,'SO2'         ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'NH3'    ,'NH3'         ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'AACD'   ,'AACD'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'ALD2'   ,'ALD2'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'FORM'   ,'FORM'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'MEOH'   ,'MEOH'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'FACD'   ,'FACD'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'CO'     ,'CO'          ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'ALDX'   ,'ALDX'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'ETHA'   ,'ETHA'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'ETOH'   ,'ETOH'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'KET'    ,'KET'         ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PAR'    ,'PAR'         ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'ACET'   ,'ACET'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PRPA'   ,'PRPA'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'ETHY'   ,'ETHY'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'ETH'    ,'ETH'         ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'OLE'    ,'OLE'         ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'IOLE'   ,'IOLE'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'ISOP'   ,'ISOP'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'APIN'   ,'APIN'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'TERP'   ,'TERP'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'BENZ'   ,'BENZENE'     ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'TOL'    ,'TOL'         ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'XYLMN'  ,'XYLMN'       ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'NAPH'   ,'NAPH'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'CH4'    ,'ECH4'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'CL2'    ,'CL2'         ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'HCL'    ,'HCL'         ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'SESQ'   ,'SESQ'        ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'SOAALK' ,'SOAALK'      ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'ACROLEIN','ACROLEIN'    ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'ALD2_PRIMARY','ALD2_PRIMARY','GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'FORM_PRIMARY','FORM_PRIMARY','GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'ACROLEIN','ACRO_PRIMARY','GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'BUTADIENE13','BUTADIENE13' ,'GAS'  ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'SULF'   ,'SULF'        ,'GAS'  ,0.  ,'UNIT','a', ",
"\n",
"! Default Aerosols",
"'EVERYWHERE', 'ALL'    ,'SULF'   ,'ASO4'        ,'FINE' ,1.  ,'MASS','a', ",
"'EVERYWHERE', 'ALL'    ,'PSO4'   ,'ASO4'        ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PNH4'   ,'ANH4'        ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PNO3'   ,'ANO3'        ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PCL'    ,'ACL'         ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PNA'    ,'ANA'         ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PEC'    ,'AEC'         ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMOTHR' ,'AOTHR'       ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PFE'    ,'AFE'         ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PAL'    ,'AAL'         ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PSI'    ,'ASI'         ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PTI'    ,'ATI'         ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PCA'    ,'ACA'         ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMG'    ,'AMG'         ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PK'     ,'AK'          ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMN'    ,'AMN'         ,'FINE' ,1.  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PH2O'   ,'AH2O'        ,'FINE' ,1.  ,'UNIT','a', ",
"\n",
"! Coarse-Mode Inorganic Ions Scaling                 ",
"'EVERYWHERE', 'ALL'    ,'PMC'    ,'ACORS'   ,'COARSE',0.99675,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMC'    ,'ASO4'    ,'COARSE',0.001  ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMC'    ,'ANO3'    ,'COARSE',0.00048,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMC'    ,'ACL'     ,'COARSE',0.00145,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMC'    ,'AH2O'    ,'COARSE',0.00032,'UNIT','a', ",
"\n",
"! Fine-Mode Primary Organic Aerosol Scaling",
"! --> Nonvolatile POA",
"'EVERYWHERE', 'ALL'    ,'POC'    ,'APOC'       ,'FINE',1.   ,'MASS','a', ",
"'EVERYWHERE', 'ALL'    ,'PNCOM'  ,'APNCOM'     ,'FINE',1.   ,'MASS','a', ",
"\n",
"! Wind-Blown Dust and Sea Spray Scaling",
"! Fine Components",
"'EVERYWHERE', 'ALL'    ,'PMFINE_SO4'  ,'ASO4'        ,'FINE',1.   ,'UNIT','a', ",
"!'EVERYWHERE', 'ALL'    ,'PMFINE_NO3'  ,'ANO3'        ,'FINE',1.   ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMFINE_CL'   ,'ACL'         ,'FINE',1.   ,'UNIT','a', ",
"!'EVERYWHERE', 'ALL'    ,'PMFINE_NH4'  ,'ANH4'        ,'FINE',1.   ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMFINE_NA'   ,'ANA'         ,'FINE',1.   ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMFINE_CA'   ,'ACA'         ,'FINE',1.   ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMFINE_MG'   ,'AMG'         ,'FINE',1.   ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMFINE_K'    ,'AK'          ,'FINE',1.   ,'UNIT','a', ",
"!'EVERYWHERE', 'ALL'    ,'PMFINE_FE'   ,'AFE'         ,'FINE',1.   ,'UNIT','a', ",
"!'EVERYWHERE', 'ALL'    ,'PMFINE_AL'   ,'AAL'         ,'FINE',1.   ,'UNIT','a', ",
"!'EVERYWHERE', 'ALL'    ,'PMFINE_SI'   ,'ASI'         ,'FINE',1.   ,'UNIT','a', ",
"!'EVERYWHERE', 'ALL'    ,'PMFINE_TI'   ,'ATI'         ,'FINE',1.   ,'UNIT','a', ",
"!'EVERYWHERE', 'ALL'    ,'PMFINE_MN'   ,'AMN'         ,'FINE',1.   ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMFINE_H2O'  ,'AH2O'        ,'FINE',1.   ,'UNIT','a', ",
"!'EVERYWHERE', 'ALL'    ,'PMFINE_OTHR' ,'AOTHR'       ,'FINE',1.   ,'UNIT','a', ",
"\n",
"! :Scaling of Fine-Mode POA from Wind-Blown Dust or Sea Spray. Either the ",
"! :Nonvolatile POA should be propagated to the transport model, or the Low ",
"! :Volatility POA should be propagated, not both.",
"! :  --> Nonvolatile POA",
"!'EVERYWHERE', 'ALL'    ,'PMFINE_POC' ,'APOC'         ,'FINE',1.   ,'UNIT','a', ",
"!'EVERYWHERE', 'ALL'   ,'PMFINE_PNCOM','APNCOM'       ,'FINE',1.   ,'UNIT','a', ",
"\n",
"! Wind-Blown Dust and Sea Spray Scaling",
"! Coarse Components                 ",
"'EVERYWHERE', 'ALL'    ,'PMCOARSE_SO4','ASO4'      ,'COARSE',1.   ,'UNIT','a', ",
"!'EVERYWHERE', 'ALL'    ,'PMCOARSE_NO3','ANO3'      ,'COARSE',1.   ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMCOARSE_CL' ,'ACL'       ,'COARSE',1.   ,'UNIT','a', ",
"'EVERYWHERE', 'ALL'    ,'PMCOARSE_H2O','AH2O'      ,'COARSE',1.   ,'UNIT','a', ",
"!'EVERYWHERE', 'ALL'   ,'PMCOARSE_SOIL','ASOIL'     ,'COARSE',1.   ,'UNIT','a', ",
"'EVERYWHERE', 'ALL' ,'PMCOARSE_SEACAT','ASEACAT'   ,'COARSE',1.   ,'UNIT','a', ",
"\n",
"!TIMES scaling factors")

#If you wanted to scale another stream, e.g. all ag emissions for future population growth,
#you could add lines for that in the above definition of "Unchanged" or add another bit here


#add the scaling factors to the control file
#and actually print/save the control file
eachline <- paste0(SF$Region, SF$Stream, SF$spc, SF$factor, SF$baseOp)
sink(OutName)
writeLines(Unchanged)
writeLines(eachline)
sink()
#remove very last comma in file, add / at end
message("remove very last comma, add / at end, change extension")

###################################################################
## possibly helpful troubleshooting expressions
#SF[which(SF$factor > 10),]  #or == 0, or < 0.1
#Above not necessarily an error, but may be places to look for issues

#View(Decisions)  #Always want to check this or WhatToFix
#SumFuture2[which(SumFuture2$Process == "_EGU_other"),]
#ScalingFactors2[which(ScalingFactors2$Process == "_EGU_coal" & ScalingFactors2$Region == "R9"),]

#OG <- Mapped %>%
#  filter(Period %in% c(BaseYear, FutureYear)) %>%
#  filter(Process == "_oilgas") %>%
#  filter(str_detect(Commodity, Emis))

#Check <- arrange(SF, Stream, Region) 
    