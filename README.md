
# dataframeexplorer

[![CRAN](https://img.shields.io/cran/v/dataframeexplorer?color=blue)](https://cran.r-project.org/package=dataframeexplorer) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/dataframeexplorer)](https://cranlogs.r-pkg.org/badges/grand-total/dataframeexplorer)

A toolkit to simplify data exploration and preparation in R, enabling quicker insights and reducing time spent on tedious data manipulations.

## Features
- Identify constant columns to prevent redundant computations.
- Detect duplicate columns to streamline datasets.
- Generate frequency tables for each column, with outputs in Excel format.
- Create a "glimpse" of your dataset for quick overview.
- Determine the primary key of your dataset for efficient data manipulation.
- Calculate percentiles for all numeric columns to understand data distribution.

## Installation
To install the package from CRAN, use:
```R
install.packages("dataframeexplorer")
```

## Usage
Here are some common functions you can use with `dataframeexplorer`:

- **Detect constant columns**:
  ```R
  detect_const_cols(dataset)
  ```
- **Find duplicate columns**:
  ```R
  detect_dupl_cols(dataset)
  ```
- **Generate a frequency table**:
  ```R
  frequency_table(dataset, output_filename = "frequency_table.xlsx")
  ```
- **Create a dataset glimpse**:
  ```R
  glimpse_to_file(dataset, output_filename = "glimpse.txt")
  ```
- **Determine primary key**:
  ```R
  level_of_data(dataset, output_filename = "level.txt")
  ```
- **Calculate percentiles**:
  ```R
  percentiles_table(dataset, output_filename = "percentiles.xlsx")
  ```

## Dependencies
- R (>= 3.3.0)
- Imports: `tibble`, `data.table`, `magrittr`, `openxlsx`, `dplyr`, `plyr`, `tidyr`, `stringr`

## License
This package is licensed under the MIT License. See the [LICENSE](./LICENSE) file for details.

## Author
Developed and maintained by Ashrith Reddy.

## Documentation
For full documentation and examples, please visit the [CRAN page](https://cran.r-project.org/package=dataframeexplorer).
