# *Hirundinidae* Example (Swallows)

## Data

The file *Hirundinidae_Sample_Data.csv*  includes 3 identifier columns:Species, Year, and Region	
It also includes 7 data columns:
* Index	is the mean abundance incex (used for metric calculations)
* remaining columns are uncertainty bounds (e.g. Index_q_0.05 is the 5th percentile)

This is actual data extracted from a large data set provided by Marcel Gahbauer (Environment and Climate Change Canada).
As with the fisheries examples, actual DU names are replaced with generic identifiers, in this case the family.



##  Script

The file *Birds_WorkedExample.R* has all the code required to read in the data, run the metric calculations,
and generate summary output.

## Outputs

The output folder contains 4 examples of output: 

* png files with time series plots by species and area
* a pdf file with diagnostic plots
* a csv file with detailed outputs
* a csv file with summary of results
