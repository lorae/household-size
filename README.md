# Introduction

The purpose of this project is to analyze the extent to which demographic changes in the U.S. population can account for changes in average American household size since 2000. The methodology, based upon [Galster (2024)](#galster2024), is to construct a multidimensional matrix, with each cell representing a unique combination of demographic (age, sex, race, etc) and geographic (PUMA) characteristics. The value in each cell represents the average household size among persons with that characteristic in the year 2000. The methodology is then applied to data from the year 2020 to determine which demographic groups have the largest changes.

The project uses American Community Survey (ACS) IPUMS microdata, which are freely available to the public after registering for an API key.

# Project Setup

The project is set up as an R project with `renv` to manage dependencies. When you open the project in RStudio, `renv` will automatically load the required environment, and you will have access to the correct versions of all the necessary packages.

## If you're running the code for the first time...

1. Sign up for an IPUMS account and obtain an API key.

2. Clone the repository:

    ```sh
    git clone https://github.com/yourusername/household-size.git
    ```

3. Open the R project file (`household-size.Rproj`) in RStudio. 

4. When you open the project, `renv` will automatically load the required packages. 

    To verify that the `renv` is activated, run:
  
    ```r
    renv::status()
    ```
  
5. Run the main script to perform the analysis. If you encounter any issues with missing packages, run the following command to install them:
  
    ```r
    renv::restore()
    ```

6. View the results as needed.

## If you're running the code any subsequent time...

1. Open the R project file (`household-size.Rproj`) in RStudio.

2. Run the main script to perform the analysis.

# FAQ

1. How granular is this analysis, geographically?

    There are XXX Census PUMAs in the United States. However, their size varies widely. In the state of Ohio, the smallest PUMA encompasses [what city?] and the largest PUMA encompasses [the rest of the state?]

    [map of Ohio PUMAs]

    Here's a map of the PUMAs in the United States.

    [map of all PUMAs]

2. How granular is this analysis, demographically?

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

