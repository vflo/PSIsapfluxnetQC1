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

To effectively use `psiQC` follow these steps:

### 1. **Set Up Your R Project**

Start by creating a new R project. This will be your working environment where all scripts and data will be organized.

### 2. **Prepare Your Server and Environment**

- **Server Preparation Script**: Begin by running the `server_preparation_script.R` script found in the `inst/run_scripts` directory of the `psiQC` package. This initial step sets up your working environment and prepares it for the quality control of your data.

### 3. **Organize Your Data**

- **Data Storage**: Place your raw data files in the `received_data` directory located at the root of your project. The path will typically look like `./received_data/ARG_MAZ.xlsx`. Ensure your data files, such as Excel spreadsheets, are correctly named and placed in this directory for processing.

### 4. **Execute Quality Control**

- **Running QC Script**: With your environment and data prepared, proceed to run the `main_script.R` found in your R project root. This script conducts a comprehensive series of quality control checks on your data. Make sure to follow any additional instructions specified within the `main_script.R`.

### 5. **Review QC Reports**

- **QC Report Generation**: For each site data processed, `psiQC` generates a detailed QC report. These reports highlight any identified issues with the data.


## License

`psiQC` is licensed under the MIT License Creative Commons Attribution 4.0 International (CC BY 4.0) licensing guidelines.
