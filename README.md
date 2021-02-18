# Towards a statistical measure for durable solutions to internal displacement: a simulation to assess indicator and metric choice

This project simulates how different indicators and metric options to implement a measure of IDPs overcoming displacement related vulnerabilities (*IRIS solutions measure*) perform comparatively. To do so, two empirical datasets are used. The project identifies all suitable possible indicators for measuring one of the 10 sub-criteria in the IRIS solutions measure in the datasets. Then, the project iterates through all possible combinations of indicators and implements different metric options to combine them and compare them to the host community as comparator population. This generates a measure of how many IDPs exit the stock depending on different indicators and metric options. This information can be used to inform policy decisions to finalise the IRIS solutions measure and fully implement it. 

**PLEASE NOTE THAT THIS PROJECT IS AT A VERY EARLY STAGE AND THAT THE CODE REQUIRES MULTIPLE REVISIONS BEFORE WE CAN BE FULLY CERTAIN OF THE RESULTS!** If you have feedback or would like to contribute to the project, please contact Sigrid Weber (sweber1@worldbank.org / s.weber.17@ucl.ac.uk). 

## Structure of this repository

The repository will soon be fully restructured to have a more collaborative structure and to extend the analysis more easily. For the moment, all data that is used is stored in the *Data* folder, including documentation to better understand the datasets. The R code that generates the preliminary findings is based on the scripts *Analysis_Hargeisa.R* and *Analysis_Nigeria.R*. The scripts are currently fully independent of each other (but should later be merged into one master script). Both R scripts depend on the file *functions.R*. *functions.R* saves functions to implement all of the different metric options. 

In principle, if you would want to use the functions on a new dataset that includes IDPs and host, you would have to follow these steps:

1. Create an ID column that takes the value 1 for IDP households and 0 for host community households. 
2. Name all indicators as according to the following principle: If an indicator could be used to measure subcriterion 1, it should be coded as "I1_ANYOTHERLETTERS". If an indicator could be used to measure subcriterion 10, it should be coded as "I10_ANYOTHERLETTERS". And so on. 
3. Turn all indicators into binary indicators that take the value 1 for overcoming a vulnerability and 0 for not overcoming it (e.g. school attandance should be a 1 but experiencing a security incident should be a 0).
4. Separate the dataset into two datasets: one for the IDPs, one for host households (call this dataset "benchmarks).
5. Define the list of indicators and the list of possible combinations (by expanding to a matrix)
6. Apply the function to the dataset by iterating through all possible combinations and comparing to the benchmarks. 
7. Write separate paragraphs on relevance of "rights-based indicators" in the statistical assessment

The script *dry_data_run.R* can be ignored for now as this is work in progress. 

## To Dos and next steps

This is a list of to-do steps that I would like to implement before we can be certain of the results. 

+ Write related note as an Rmarkdown to facilitate updating of numbers
+ Rewrite functions for increased efficiency (in particular for option 2!)
+ Double-check functions as some results are still quirky
+ Implement a tracking of WHICH IDPs are classified as overcoming vulnerabilities (Do the different metric options identify the same IDPs as no longer vulnerable?)
+ Simulate synthetic data to test the metrics options and functions without empirical data (i.e. create enough indicators of varying "difficulty" to fully implement all options) - this is started in the script *dry_data_run.R* but is not finalised yet
+ Construction of homogenous cells: further explorations needed, could benefit from implementing a clustering algorithm to increase similiarity within IDP groups and dissimilarity betwen IDP groups
+ Regression-based approach: further explorations needed, could use a lasso regularization, would need to test different cut-off points

## Possible next datasets to extend the analysis:
+ Darfur, Somalia, South Sudan
+ Survey of Syrian Refugees and Host Community Members in Kurdistan
+ JIPS IDP profiling in Honduras (Spanish only), with similar datasets from Belize and El Salvador 
+ Colombian government's LSMS survey 

