# Towards a statistical measure for durable solutions to internal displacement: a simulation to assess indicator and metric choice

This project simulates how different indicators and metric options to implement a measure of IDPs overcoming displacement related vulnerabilities (*IRIS solutions measure*) perform comparatively. To do so, two empirical datasets are used. The project identifies all suitable possible indicators for measuring one of the 10 sub-criteria in the IRIS solutions measure in the datasets. Then, the project iterates through all possible combinations of indicators and implements different metric options to combine them and compare them to the host community as comparator population. This generates a measure of how many IDPs exit the stock depending on different indicators and metric options. This information can be used to inform policy decisions to finalise the IRIS solutions measure and fully implement it. 

*PLEASE NOTE THAT THIS PROJECT IS AT A VERY EARLY STAGE AND THAT THE CODE REQUIRES MULTIPLE REVISIONS BEFORE WE CAN BE FULLY CERTAIN OF THE RESULTS!* If you have feedback or would like to contribute to the project, please contact Sigrid Weber (sweber1@worldbank.org / s.weber.17@ucl.ac.uk). 

## Structure of this repository


## To Dos and next steps
+ Write related note as an Rmarkdown to facilitate updating of numbers
+ Rewrite functions for increased efficiency (in particular for option 2!)
+ Implement a tracking of WHICH IDPs are classified as overcoming vulnerabilities (Do the different metric options identify the same IDPs as no longer vulnerable?)
+ Simulate synthetic data to test the metrics options and functions without empirical data (i.e. create enough indicators of varying "difficulty" to fully implement all options)
