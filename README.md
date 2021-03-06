# Towards a statistical measure for durable solutions to internal displacement: a simulation to assess indicator and metric choice

This project simulates how different indicators and metric options to implement a measure of IDPs overcoming displacement related vulnerabilities (*IRIS solutions measure*) perform comparatively. To do so, two empirical datasets are used. The project identifies all suitable possible indicators for measuring one of the 10 sub-criteria in the IRIS solutions measure in the datasets. Then, the project iterates through all possible combinations of indicators and implements different metric options to combine them and compare them to the host community as comparator population. This generates a measure of how many IDPs exit the stock depending on different indicators and metric options. This information can be used to inform policy decisions to finalise the IRIS solutions measure and fully implement it. 

**PLEASE NOTE THAT THIS PROJECT IS AT A VERY EARLY STAGE AND THAT THE CODE REQUIRES MULTIPLE REVISIONS BEFORE WE CAN BE FULLY CERTAIN OF THE RESULTS!** If you have feedback or would like to contribute to the project, please contact Sigrid Weber (sweber1@worldbank.org / s.weber.17@ucl.ac.uk). 

## Structure of this repository

The repository will soon be fully restructured to have a more collaborative structure and to extend the analysis more easily. For the moment, all data that is used is stored in the *Data* folder, along with the data prepration scripts and documentation to better understand the datasets. The R code that generates the preliminary findings is based on the scripts *hargeisa.R* and *nigeria.R*. Both R scripts depend on the file *functions.R*. *functions.R* saves functions to implement all of the different metric options.

In principle, if you would want to use the functions on a new dataset that includes IDPs and host, you would have to write a script that preparses the data for simulations which:
1. Creates an ID column that takes the value `1` for IDP households and `0` for host community households. 
2. Names all variables as according to the following principle:
a. If the variable is an indicator that could be used to measure subcriterion 1, it should be coded as `I1_ANYOTHERLETTERS`. If an indicator could be used to measure subcriterion 10, it should be coded as `I10_ANYOTHERLETTERS`. And so on.
b. If the variable contains information on household characteristics that could be used to group households for the construction of homogenous cells under option #4, then it should be prefixed with `HH_`, example: `HH_Location`.
3. Turns all indicators into binary indicators that take the value `1` for overcoming a vulnerability and `0` for not overcoming it (e.g. school attandance should be a `1` but experiencing a security incident should be a `0`).
4. Outputs the final dataset to the console before exiting.

Once that's done, create a new worksheet in `Data/dict.xlsx` documenting the mapping of variables from your dataset to the solutions indicators following the structure in the other worksheets. This basically serves as a mapping of terse variable names to human-readable variable labels to be used in plotting so do make sure to use labels that are clear and concise.

To run the analysis, run `targets::tar_make()` from the console. `targets` will check the code for changes since the last run and only run the parts of the simulations affected by that change. So if you've added a new dataset, it will run the simulations for that dataset alone. And if you've changed the calculation of any indicator it will run the simulations for that indicator only across all datasets. Everything else is served from cache.

Once ready, the results of the simulations can be accessed from the console/scripts/Rmds using `targets::tar_load()` which loads everything into the current workspace/environment. For example: `targets::tar_load(contains("hargeisa"))` will load the results for the Hargeisa simulations into your workspace. These come as five data-frames named `DS_OptionN_hargeisa` where N is the number of the metric option plus another data-frame `DS_Original_hargeisa` holding the data for the IRIS pass/fail metric. If you'd rather exercise more control over where the data ends up, you can use `mydata <- targets::tar_read("DS_Option1_hargeisa")`, for example, to read that single dataset into the variable named `mydata`.

## To Dos and next steps

This is a list of to-do steps that I would like to implement before we can be certain of the results. 

+ Weighted calculations
+ Implement a tracking of WHICH IDPs are classified as overcoming vulnerabilities (Do the different metric options identify the same IDPs as no longer vulnerable?)
+ Construction of homogenous cells: further explorations needed (!!!), could benefit from implementing a clustering algorithm to increase similiarity within IDP groups and dissimilarity betwen IDP groups
+ Regression-based approach: further explorations needed, could use a lasso regularization, would need to test different cut-off points (!!!)
+ Create synergies with Kari-Anne's work on HLP indicators
+ Write a paragraph on taking out the HLP indicators
+ More information on relation to progress measure and which indicators to use
+ Add communication that we should not choose a too lenient or too strict indicator set/metric
+ Think harder about case selection!
+ Enhance focus on NSOs and simplicity for them to implement
+ Ask Felix about a workshop with NSOs on what would be possible

## Possible next datasets to extend the analysis:
+ Darfur, Somalia, South Sudan
+ Survey of Syrian Refugees and Host Community Members in Kurdistan, KRI
+ JIPS IDP profiling in Honduras (Spanish only), with similar datasets from Belize and El Salvador 
+ [Colombian government's LSMS survey](http://microdatos.dane.gov.co/index.php/catalog/678/study-description) (work-in-progress)
+ Bosnia or Kosovo
+ Ukraine (work-in-progress)
+ Sudan 
+ Natural disasters: two IOM/Brookings studies in [Haiti](https://www.brookings.edu/research/supporting-durable-solutions-to-urban-post-disaster-displacement-challenges-and-opportunities-in-haiti/) and [Philippine](https://www.brookings.edu/research/resolving-post-disaster-displacement-insights-from-the-philippines-after-typhoon-haiyan-yolanda/)

