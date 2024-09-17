# Introduction

The purpose of this project is to analyze the extent to which demographic changes in the U.S. population can account for changes in average American household size since 2000. The methodology, based upon [Galster (2024)](#galster2024), is to construct a multidimensional matrix, with each cell representing a unique combination of demographic (age, sex, race, etc) and geographic (PUMA) characteristics. The value in each cell represents the average household size among persons with that characteristic in the year 2000. The methodology is then applied to data from the year 2020 to determine which demographic groups have the largest changes.

The project uses American Community Survey (ACS) IPUMS microdata, which are freely available to the public after registering for an API key.

# File tree
## Project Structure

The following is an overview of the project's file structure:

```
household-size/
├── README.md                  # The document you're currently reading
├── household-size.Rproj       # RStudio project file
├── renv/                      # R environment directory
│   ├── activate.R             
│   ├── settings.dcf           
│   └── library/               
├── src/                       # Source code directory
│   ├── utils/                 # Utility scripts
│   │   ├── bucketing-tools.R  # Functions for bucketing data
│   │   ├── create-summary-array.R  # May be deprecated soon
│   │   └── create-synthetic-data.R # For testing? Maybe move to tests/
│   └── sample_code_for_sarah.R # Demonstrates SQL requests using lookup tables using DuckDB
│   
└── tests/                     # Testing directory
    ├── testthat/              # Unit tests directory
    │   └── test-bucketing.R   # Test script for bucketing functions
    └── test-data/             # Mock data for testing
        ├── mock_data.csv
        ├── mock_age_lookup.csv
        ├── mock_income_lookup.csv
        ├── expected_age_output.csv
        └── expected_income_output.csv

```

# Project Setup

The project is set up as an R project with `renv` to manage dependencies. When you open the project in RStudio, `renv` will automatically load the required environment, and you will have access to the correct versions of all the necessary packages.

## If you're running the code for the first time...

1. Sign up for an IPUMS account and obtain an API key.

2. Clone the repository:

    ```sh
    git clone https://github.com/lorae/household-size.git
    ```

3. Open the R project file (`household-size.Rproj`) in RStudio. 

4. Verify that [RTools](https://cran.r-project.org/bin/windows/Rtools/) is already installed on your computer. RTools is required for compiling some of the packages in this project's library.

    a. Load the `pkgbuild` package, installing if necessary:

        ```r
        if (!requireNamespace("pkgbuild", quietly = TRUE)) {
          install.packages("pkgbuild")
        }
        library("pkgbuild")
        ```

    b. Use the `find_rtools()` function from the `pkgbuild` package to verify that RTools is installed and configured.

        ```r
        pkgbuild::find_rtools()
        ```

        If the output is `[1] TRUE`, RTools is installed and ready to use. Proceed to step 5.
        
        If the output is `[2] FALSE`:
        
          i. Verify your current version of R by running 
          
              ```r 
              R.version.string
              ```
          ii. Navigate to https://cran.r-project.org/bin/windows/Rtools/. Download the version of RTools consistent with your version of R: For example, install RTools44 for version 4.4.0 and above, and RTools43 for version 4.3.0 and above.
          
          iii. Close any instances of R and RStudio and open the executable to follow installation instructions. Once downloaded, reopen RStudio rerun the above steps to verify proper RTools installation. 

5. Install the `renv` package by running the following command in R:

    ```r
    install.packages("renv")
    ```

6. Run `renv::restore()` to install all the required packages as specified in the `renv.lock` file:

    ```r
    renv::restore()
    ```

    This will ensure that your environment is set up with the same package versions used in the project.
  
7. Run the main script to perform the analysis. 

   [TODO: instructions here]

8. View the results as needed.

## If you're running the code any subsequent time...

1. Open the R project file (`household-size.Rproj`) in RStudio.

2. Run `renv::restore()` to install all the required packages as specified in the `renv.lock` file:

    ```r
    renv::restore()
    ```

    This will ensure that your environment is set up with the same package versions used in the project.
  
3. Run the main script to perform the analysis. 

   [TODO: instructions here]

4. View the results as needed.

# FAQ

1. How granular is this analysis, geographically?

    There are roughly 2,000 Census PUMAs in the United States. However, their size varies widely. In Minnesota, the smallest PUMA encompasses [what city?] and the largest PUMA encompasses [the rest of the state?]. 

    [map of Ohio PUMAs in 2020]

    Here's a map of the PUMAs in the United States.

    [map of all PUMAs]
    
    Unfortunately, PUMAs do not perfectly correspond with each other between the 2000 and 2020 Census.
    
    [map with altered PUMA boundaries]
    
2. How do you get around the issue of mismatched PUMAs?

  [TODO: Explain several robusteness tests here]

3. How granular is this analysis, demographically?

    The variables we use to bin individuals into groups are the following. See our codebook [link to codebook] for more information.

    **Age**: 5-year bins:
    - 0-4 years
    - 5-9 years
    - 10-14 years
    - 15-19 years
    - 20-24 years
    - 25-29 years
    - 30-24 years
    - ...
    - 75-79 years
    - 80-84 years
    - 85+

    [perhaps a table of how many individuals fall in each age group?]

    **Race**: Add info here

    **Household income**: Add information here on how it's defined, whether it's pre-tax/transfer, whether it's only earned or earned and unearned, how it's deflated to compare over time, etc.

# References

- <a name="galster2024" id="galster2024"></a>**George C. Galster (2024)**. *Is There Enough Housing Production? It Matters Which Indicators Are Used to Answer*, Housing Policy Debate. DOI: 10.1080/10511482.2024.2334018

- <a name="mcclure2024" id="mcclure2024"></a>**McClure, K., & Schwartz, A. (2024)**. *Where Is the Housing Shortage?* Housing Policy Debate, 1–15. [https://doi.org/10.1080/10511482.2024.2334011](https://doi.org/10.1080/10511482.2024.2334011)

