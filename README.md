<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview
```
It is often said that data manipulation alone takes 50-70% time of a data science project. 
The duration of this never-ending activity can be attributed to our opaqueness with datasets provided.
Functions in this package enable familiarity with the data.frame to further reduce coding errors and re-work.
```

## Installation
``` r
# The easiest way to get dplyr is to install from GitHub:
install.packages("dataframeexplorer")

# Alternatively, you can install development version:
install.packages("devtools")
devtools::install_github("ashrithssreddy/dataframeexplorer")
```

## Next Steps
Functions:
[x] Percentiles
[x] Level of dataset
Univariate Analysis
Bivariate Analysis
Show progress bar for level_of_dataset
Run the level_of_dataset code in parallel for performance

Changes:
~~Return value for all functions to be included into documentation~~
~~Message not printed in all codes~~
~~Default filename not consistent~~
Outputs not refined
Pep 8 formatting
examples not consistent
Comments not consistent across all codes
sink() to be run in glimpse_to_file upon an error
Arguement format to be used: dataset = dataset, output_filename = "dataset_glimpse.txt"
Throw a warning when duplicate column names are found
~~Level: Unsink when interrupted~~
	~~Add instructions to interpretation of output~~

## Usage
```
1. glimpse_to_file
	glimpse_to_file(mtcars, "mtcars_glimpse.txt") or glimpse_to_file(mtcars, "C://Users/Desktop/mtcars_glimpse.txt")
	![Output](/man/figures/.png)

```

## Contact
Mail ashrithssreddy@gmail.com for suggestions with "dataframeexplorer" in subject line.
