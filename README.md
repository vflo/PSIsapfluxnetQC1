# psiQC

`psiQC` is an R package designed to facilitate quality control in leaf water potential measurements, integrated with the SAPFLUXNET database. This package provides tools and scripts to ensure data integrity and reliability.

## Installation

Currently, `psiQC` is available as a development version on GitHub. You can install it using `devtools`:

```r
# Install devtools if you haven't already
if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
}

# Install psiQC from GitHub
devtools::install_github("vflo/psiQC")
```

## Usage

To utilize `psiQC`, follow these steps:

1. Create an R project.

2.  **Server Preparation**: First, execute the `server_preparation_script.R` located in the `inst/run_scripts` folder. This script prepares your environment and data for analysis.

3. Server Preparation will create the folders structure. Data should be stored in your project root within a folder called `received_data`. It should be something such as `./received_data/ARG_MAZ.xlsx`.

4. **Quality Control**: Next, run the `main_script.R` to perform the quality control (QC) checks on your data. This script applies a series of QC measures to ensure data reliability. Follow the instructions in the `main_script.R` file.


## License

`psiQC` is licensed under the MIT License Creative Commons Attribution 4.0 International (CC BY 4.0) licensing guidelines.
