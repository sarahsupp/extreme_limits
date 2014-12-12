extreme_limits
==============

**authors**: S.R. Supp, T.A. Cormier, and C.H. Graham, modified from P.A. Beck


R Code to run the analyses for the hummingbird extreme limits paper. 
The code creates the models that investigate how Molt stage (`Molt_model.Rmd`), Weight (`Weight_model.Rmd`) and 
arrival in Arizona (`Arrival_model.Rmd`) are affected by resource availability (NDVI) and physiological demand 
(operative temperature).

Files
-------
  * .Rmd (Rmarkdown) files describe each of the analyses and .html files generated from them. 
  * The data sets (.rdata files) are kept on a personal computer, for questions, contact C.H. Graham. 
  * Functions (.r files) called by the code described in the different .Rmd files.

Requirements
-------------
 * `aplpack`
 * `bbmle`
 * `ggplot2`
 * `knitr`
 * `lme4`
 * `lattice`
 * `latticeExtra`
 * `maptools`
 * `ncdf`
 * `RNetCDF`
 * `raster`
 * `sirad`
 * `timeDate`
 * `zoo`

Data
-----
 * `Mexico_CFSR_level1.rdata` -
 * `Mexico_CFSR_level2.rdata` - 
 * `Mexico_SOT_level1.rdata` - 
 * `Mexico_SOT_level2.rdata` -
 * `Mexico_SOT_level3.rdata` -
 * `NDVI_df.rdata` - 
 * `Torpor_df.rdata` -
 * `annual.anom.df.rdata` - Winter range (Nov-Mar) climate data from the hummingbird site points
 * `site.dat.rdata` - Summarized site data
 * `yearly.catch.stat.rdata` - Summarized hummingbird catch data from Arizona, arrival dates as quantiles
 * `yearly.climate.rdata` - Summarized climate data, as quantiles, for precip, NDVI, and operative temperature

R Scripts (functions and data preparation)
------------
 * `Calculate_SOT_from_Met_Code_Copy.r` - contains standard operative temperature calculations (problem with code?)
 * `calculate_diffuse_fraction_code_copy.r` - contains diffuse fraction of radiation calculations (problem with code?)
 * `Calculate_SOT_from_CFSR_Met.r` - makes the dataframe allmet, makes Mexico_SOT_level3.rdata, uses functions from the above two files (Calculate_SOT_from_Met_Code_Copy.r, and calculate_diffuse_fraction_code_copy.r) to calculate Te and Tes, makes some plots of the met and temperature data.
 * `NCEP_CFSR_data_extract_v5.r` - a function to summarize extracted data (netcdf files) per day, sum/mean/max/min
 * `compare_years_extracted_CFSR_data.r` - uses Mexico CFSR_level2.rdata and makes annual.anom.df.rdata (which is later used to make yearly.climate.rdata)
 * `Make_yearly_enviro_dat.r` - code to make yearly.climate.rdata file from the summarized raw data (annual.anom.df.rdata), to be used in the models for the manuscript results
 * `Plot_conditional_effects_interaction_models.r` - Used for the models in the Rmd files: code for plotting conditional effects of the interaction models, to plot the effect of x1 on y, in  a 2-level regeression model that includes an interaction between x1 and x2, and random varying intercept
 * `plot_yearly_NDVI_cumprecip_series.r` - code to plot NDVI and cumulative precipitation over the time series. Figure 3: NDVI, cumulative precipitation, and ENSO
 * `Model_evaluation_v4.r` - code for evaluating mixed models and quantile regresstion for the models in the .Rmd files.
 * __defunct code__
 * `data_prep_for_transfer_PSAB_only.r` - IGNORE
 * `wrapper.r` - IGNORE
 *  * `Diffuse_fraction_of_solar_radiation.R` - IGNORE - old (incorrect) functions to calculate what fraction of incoming solar radiation reaches the surface as diffuse radiation
 * `Standard_Operative_Temparture_from_meteorology.R` - IGNORE - old (incorrect) Calculate Standard Operative Temperature (Tes) for hummingbirds using meteorological input data, based on Don Powers's email "Estimating Operative Temperature" and data from published literature


Rmd Scripts (generate the main results)
------------
 * `Arrival_model.Rmd` - model of Arrival/catch observed in Arizona and how it covaries with energetic demand and resource availability in Mexican wintering grounds
 * `Molt_model.Rmd` - model of Molt observed in Arizona and how it covaries with energetic demand and resource availability in Mexican wintering grounds
 * `Weight_model.Rmd` - model of Weight observed in Arizona and how it covaries with energetic demand and resource availability in Mexican wintering grounds

Markdown files
--------------
 * `Diffuse_fraction_of_solar_radiation.md` - Example for working with the data
 * `Standard_Operative_Temparture_from_meteorology.md` - Example for working with the data
 

Replicating the results from the raw data
---------------------------
To run, check the pathnames in each of the files, make sure you have the required libraries installed, and a copy of the .Rdata files.
 1. Convert temperature variable in the raw data (`Mexico_CFSR_level1.rdata` and `Mexico_CFSR_level2.rdata`) into Operative Temperature (`Calculate_SOT_from_CFSR_Met.r`). This refers to the scripts `Calculate_SOT_from_Met_Code_Copy.R` and `calculate_diffuse_fraction_code_copy.R`. Save as allmet within `Mexico_SOT_level3.rdata`.
 2. Input `Mexico_CFSR_level2.rdata` into `compare_years_extracted_CFSR_data.r` to create `annual.anom.df.rdata`
 3. Input `annual.anom.df.rdata` into `Make_yearly_enviro_dat.r`. This will create `yearly.climate.rdata`, which is summarized from Te.
 4. Run `Arrival_model.Rmd`, `Molt_model.Rmd`, `Weight_model.Rmd`, and `plot_yearly_NDVI_cumprecip_series.r` on the main datafiles to regenerate the main figures, tables, and results for the manuscript.

Contact T.A.C., S.G., or P.S.A.B. for questions related to the code that summarizes the remote sensing data.

*contact* S.R. Supp

*email*: sarah@weecology.org
