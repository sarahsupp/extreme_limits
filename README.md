extreme_limits
==============

**authors**: S.R. Supp, T.A. Cormier, and C.H. Graham, modified from P.A. Beck


R Code to run the analyses for the hummingbird extreme limits paper. 
The code creates the models that investigate how Molt stage (Molt_model.Rmd), Weight (Weight_model.Rmd) and 
arrival in Arizona (Arrival_model.Rmd) are affected by resource availability (NDVI) and physiological demand 
(operative temperature).

Files
-------
  * .Rmd (Rmarkdown) files describe each of the analyses and .html files generated from them. 
  * The data sets (.rdata files) are kept on a personal computer, for questions, contact C.H. Graham. 
  * Functions (.r files) called by the code described in the different .Rmd files.

Requirements
-------------
 * `bbmle`
 * `ggplot2`
 * `knitr`
 * `lme4`
 * `lattice`
 * `latticeExtra`

Code that generates the standard operative temperature estimates and NDVI summaries is not included here. 
Instead, the different .Rmd files load the yearly summaries of those variables. Contact P.A.B. for questions related to the code that summarizes the remote sensing data.

To run the code (the .Rmd should guide you through), the only thing to do is change the path and 
make sure you have the required libraries installed, and a copy of the Rdata files. 

*contact* S.R. Supp

*email*: sarah@weecology.org
