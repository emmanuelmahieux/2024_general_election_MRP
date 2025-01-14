# 2024_general_election_MRP

This repository contains two scripts to retrospectively estimate constituency-level party vote shares at the 2024 general election using multilevel regression with poststratification (MRP). 

mrp_data_processing : this script processes the census data obtained with the ONS’ dataset customisation tool (https://www.ons.gov.uk/datasets/create) so that it can be used as the poststratification frame. The census variables it uses to poststratify are education level and age group. 

mrp_script: this script tailors Kastellec et al.’s (2016, [link]([url](https://jkastellec.scholar.princeton.edu/sites/g/files/toruqf3871/files/jkastellec/files/mrp_primer.pdf))) MRP script to generate constituency-level predictions.
