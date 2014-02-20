co-occurrence
=============

R scripts for performing co-occurrence analyses

|pulling_data_from_MGRAST_with_matR.R|
--------------------------------------

This script uses matR to pull data from MGRAST for use in the co-occurrence analyses.  The three datasets can be pulled seperately or together in a single dataframe (I recommned pulling all together to avoid issues with differences in richness within each dataset).

|pairwise_co-occurrence.R|
--------------------------

This script performs the correlations between all pairs of microbes in the dataset.  Attention needs to be paid to where the loop starts (it should start at the first column containing abundance data, not any metadata related to samples).

|co-occurrence_permanova_sim.R|
-------------------------------

This script contains the code for running the PERMANOVA that tests for differences in community co-occurrence between ecosystems.  At the beginnning of the script there is a simulation that shows how different pairs of correlated variables (i.e. co-occurring taxa) between two datasets (i.e. ecosystems) can affect the results of the test. 

|permutation_test.R|
--------------------

This is a script that contains a permutation test based on the F-statistic.  This is a function that can be sourced into other scripts for use.  Original credit goes to Dr. Dean Adams who wrote the code for the test, while I wrapped it in a function.