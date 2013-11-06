setwd("D:\\Dropbox_27Sep2013\\Hummers\\field_data\\P4_ExtremeEvents_Susan")
load("BTLH_Data_from_raw_L2.rdata")
site.dat$LOC <- site.dat$Location

save(site.dat,file="D:\\Hummer_code_transfer\\site.dat.rdata")


load("D:\\Dropbox_27Sep2013\\Hummers\\Hummer_code\\P4_Extreme_events_Broad_tailed\\Results\\yearly.climate.rdata")
save(yearly.climate,file="D:\\Hummer_code_transfer\\yearly.climate.rdata")


setwd('C:\\Data\\Dropbox\\Hummers\\Hummer_code\\P4_Extreme_events_Broad_tailed\\')
setwd('D:\\Dropbox_27Sep2013\\Hummers\\Hummer_code\\P4_Extreme_events_Broad_tailed\\')
source('Model_evaluation_v4.r')
save(mod.evaluation,file="D:\\Hummer_code_transfer\\Model_evaluation_v4.r")

