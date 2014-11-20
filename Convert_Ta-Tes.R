# convert Tmin to Te

#don't need this summary - too short
#dat = read.csv("/Users/sarah/Desktop/Dropbox/Hummingbirds/hb_extremelimits/summarized_climate_1999-2011_BT.csv")

load("/Users/sarah/Desktop/Dropbox/Hummingbirds/hb_extremelimits/Mexico_SOT_level3.rdata") # this is the met data extracted during the calculation of Stand. Op. Temp
load("/Users/sarah/Desktop/Dropbox/Hummingbirds/hb_extremelimits/Mexico_CFSR_level2.rdata") #this is the output of NCEP_CFSR_data_extract.R
load("/Users/sarah/Documents/GitHub/extreme_limits/data/annual.anom.df.rdata") #this df is needed for, and then updated in, Compare_years_extracted_CFSR_data.r??

# Step 0: Need to apply the Operative temperature calculations to the 6 hourly data summaries (allmet in Mexico_SOT_level3.rdata??)
# Step 1: Convert Ta to Te, using the Tes.calc.compl.incl.rad, which will require data for 
  #Ta, u, Li, Rsurface, R_extra_terr, solarzen
# Step 2: Run NCEP_CFSR_data_extract_v5.R, using the newly annotated data in place of the original allmet dataframe, save and regenerate Mexcio_CFSR_level2.rdata from this
# Step 3: Figure out how to recreate annual.anom.df.radata?
# Step 4: Take the new Mexico_CFSR_level2.rdata and annual.anom.df.rdata, and run Compare_years_extracted_CFSR_data.r
# Step 5: Select data from just the winter months (unless this occurs sooner??)
# Step 6: Take the new annual.anom.df.rdata and run with make_yearly_envir_dat.r to recreate yearly.climate.rdata
# Step 7: Use yearly.climate.rdata as the climate variables for the *_model.Rmd files
# Step 8: Save figures and results as output. Plug any changes into the extreme_limits manuscript.


#----- Try to piece together PB's code and data
load("/Users/sarah/Desktop/Dropbox/Hummingbirds/hb_extremelimits/Mexico_SOT_level3.rdata")

sub = allmet[1,]
subTe = Tes.calc.compl.incl.rad(273.15+sub$tmin,sub$wnd,sub$dlwrf, sub$dswrf, sub$R_extra, sub$solarzen)
subTe = subTe-273.15

Tes.calc.compl.incl.rad <- function(Ta, u, Li, Rsurface, R_extra_terr, solarzen)
