# convert Tmin to Te

dat = read.csv("/Users/sarah/Desktop/Dropbox/Hummingbirds/hb_extremelimits/summarized_climate_1999-2011_BT.csv")

load("/Users/sarah/Documents/GitHub/extreme_limits/data/annual.anom.df.rdata")

# Step 1: Read in the winter climate data
# Step 2: Convert Ta to Te, using the Tes.calc.compl.incl.rad, which will require data for 
  #Ta, u, Li, Rsurface, R_extra_terr, solarzen
# Step 3: Take the new climate data, that has Te calculated, and make the annual.anom.data. using Compare...
# Step 4:
