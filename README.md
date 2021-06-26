# Towards a statistical measure for durable solutions to internal displacement: a simulation to assess indicator and metric choice

This project simulates how different indicators and metric options to implement a measure of IDPs overcoming displacement related vulnerabilities (*IRIS solutions measure*) perform comparatively. To do so, two empirical datasets are used. The project identifies all suitable possible indicators for measuring one of the 10 sub-criteria in the IRIS solutions measure in the datasets. Then, the project iterates through all possible combinations of indicators and implements different metric options to combine them and compare them to the host community as comparator population. This generates a measure of how many IDPs exit the stock depending on different indicators and metric options. This information can be used to inform policy decisions to finalise the IRIS solutions measure and fully implement it. 

If you have feedback or would like to contribute to the project, please contact Sigrid Weber (s.weber.17@ucl.ac.uk). 

## Structure of this repository

All data that is used is stored in the *Data* folder, along with the data prepration scripts and documentation to better understand the datasets. The simulations itself are based on the *functions.R* script, which intoduces the different metric options, the *simulations.R* script, which applies the functions to the data and runs the simulations, and the *_targets.R* script, which sets the pipeline to iterate through all data in our folder and execute the different simulations. 

In principle, if you would want to use the functions on a new dataset that includes IDPs and host, you would have to write a script that prepares the data for simulations which:
1. Creates an ID column that takes the value `1` for IDP households and `0` for host community households. 
2. Names all variables as according to the following principle:
a. If the variable is an indicator that could be used to measure subcriterion 1, it should be coded as `I1_ANYOTHERLETTERS`. If an indicator could be used to measure subcriterion 10, it should be coded as `I10_ANYOTHERLETTERS`. And so on.
b. If the variable contains information on household characteristics that could be used to group households for the construction of homogenous cells under option #4, then it should be prefixed with `HH_`, example: `HH_Location`.
3. Turns all indicators into binary indicators that take the value `1` for overcoming a vulnerability and `0` for not overcoming it (e.g. school attandance should be a `1` but experiencing a security incident should be a `0`).
4. Adds a column called `HHID` with a unique case identifier. This can be a simple call to `row_number()` or (preferably) a case identifier from the original dataset so that the simulation results could be merged with the rest of the data for further analysis.
5. Stores the case weights in `WT`. If the original dataset doesn't include weights, then `WT` should be set to `1`.
6. If applicable, assign per-capita income/expenditure measures to `PERCAPITA`. The variables should be set to `NA_real_` otherwise. 
7. Outputs the final dataset to the console before exiting.

Once that's done, create a new worksheet in `Data/dict.xlsx` documenting the mapping of variables from your dataset to the solutions indicators following the structure in the other worksheets. This basically serves as a mapping of terse variable names to human-readable variable labels to be used in plotting so do make sure to use labels that are clear and concise.

To run the analysis, run `targets::tar_make()` from the console. `targets` will check the code for changes since the last run and only run the parts of the simulations affected by that change. So if you've added a new dataset, it will run the simulations for that dataset alone. And if you've changed the calculation of any indicator it will run the simulations for that indicator only across all datasets. Everything else is served from cache.

Once ready, the results of the simulations can be accessed from the console/scripts/Rmds using `targets::tar_load()` which loads everything into the current workspace/environment. For example: `targets::tar_load(contains("hargeisa"))` will load the results for the Hargeisa simulations into your workspace. These come as five data-frames named `DS_OptionN_hargeisa` where N is the number of the metric option plus another data-frame `DS_Original_hargeisa` holding the data for the IRIS pass/fail metric. If you'd rather exercise more control over where the data ends up, you can use `mydata <- targets::tar_read("DS_Option1_hargeisa")`, for example, to read that single dataset into the variable named `mydata`.
