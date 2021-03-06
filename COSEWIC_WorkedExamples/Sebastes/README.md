# *Sebastes* Example

## Data

The file *Sebastes_SampleData.csv*  includes 4 columns:
Species, Year, Abd, and DU. Abundance is **XYZ*. DU is the label for the *designatable unit*.

The file *Sebastes_TimeWindows.csv* specifies the time window (number of years, typically 3 generations)
to be used for the metric calculation. It has 1 row for each DU in *Sebastes_SampleData.csv*.


##  Script

The file *Sebastes_Script.R* has all the code required to read in the data, run the metric calculations,
and generated summary output.

## Outputs

The output folder contains 3 examples of output: 

* a pdf file with diagnostic plots
* a csv file with detailed outputs
* a csv file with summary of results
