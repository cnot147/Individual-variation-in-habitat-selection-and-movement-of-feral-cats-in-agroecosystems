# Individual-variation-in-habitat-selection-and-movement-of-feral-cats-in-agroecosystems

Data files
Cat_data_NZTM.csv
  GPS data for each of the Cats (labeled by id) 
Catdata.csv
  Home range sizes for each cat, Cat = cat id, sex is male or female, weight is in kg, 95KDE is the 95% kernal density estimate, 50KDE is the 50% kernel density estimate, rabbit level was low, medium, high based on Modified Mclean's scale
run1.land.csv
  Landscape level characteristics calculated by fragstats
run2.class.csv
  Class level characteristics calculated by fragstats

Code - all code is for R

Home range.R
  Code calculates home range and compares 95% KDE for males and females and 50% KDE for males and females
  Code used to calculate home range requires file CleanGPSdata
  Code used to compare home range size between males and females uses data Catdata.csv

Factors that affect habitat selection.R
  Requires data from Simple_landuse file and Cat_data_NZTM.csv for habitat selection
  Calulates habitat selection for all cats and for each individual cat
  Following calculating habitat selection the coeffiecients are used from the individual models with Catdata.csv, run1.land.csv and run2.class.csv to examine factors that affect habitat selection

All cats model validation.R
  Provides code for model validation of habitat selection models for all cats

Individual cat model validation.R
  Provides code for model validation of habitat selection models for each of the individual cats

Tortuosity veg vs pasture.R
  Provides code in caluculating tortuosities for paths in pasture and woody vegetation and code to determine if there was a difference in these

Step lengths veg vs pasture.R
  Provides code to determine if there was a difference between pasture and woody vegetation
  


  

  
