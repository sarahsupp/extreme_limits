extreme_limits
==============

**authors**: S.R. Supp, T.A. Cormier, C.H. Graham
modified from P.A. Beck


R Code to run the analyses for the hummingbird extreme limits paper. 
The code creates the models that investigate how Molt stage (Molt_model.Rmd), Weight (Weight_model.Rmd) and 
arrival in Arizona (Arrival_model.Rmd) are affected by resource availability (NDVI) and physiological demand 
(operative temperature).

Files
-------
  * .Rmd (Rmarkdown) files describe each of the analyses and .html files generated from them. 
  * The data sets (.rdata files) are kept on a personal computer (for now). 
  * Functions (.r files) called by the code described in the different .Rmd files.


Code that generates the standard operative temperature estimates and NDVI summaries is not included here. 
Instead, the different .Rmd files load the yearly summaries of those variables. Contact P.A.B. for that code.

To run the code (the .Rmd should guide you through), the only thing to do is change the path and 
make sure you have the required libraries installed. 

*contact* S.R. Supp
*email*: sarah@weecology.org
