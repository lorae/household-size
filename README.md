# Introduction

The purpose of this project is to analyze the extent to which demographic changes in the U.S. population can account for changes in average American household size since 2000. The methodology, based upon [Galster (2024)](#galster2024), is to construct a multidimensional matrix, with each cell representing a unique combination of demographic (age, sex, race, etc) and geographic (PUMA) characteristics. The value in each cell represents the average household size among persons with that characteristic in the year 2000. The methodology is then applied to data from the year 2020 to determine which demographic groups have the largest changes.

The project uses American Community Survey (ACS) IPUMS microdata, which are freely available to the public after registering for an API key.

# Project Setup

TODO: set up an R project so the renv and working directory load up automatically upon opening the project.

## If you're running the code for the first time...

1. Sign up for an IPUMS account and API key

2. Clone the repository

3. Set your working directory into the repository:

    Shell:
    
    ```sh
    cd your/path/to/household-size
    ```
    
    R:
    
    ```r
    setwd("your/path/to/household-size")
    ```

4. Add a step to check if `renv` package is already installed and install if necessary?

5. Download and install all the needed packages using `renv` ([R virtual environment](https://rstudio.github.io/renv/articles/renv.html))

    TODO: Do you need to run `renv::activate()` first? I'm not sure.

    ```r
    renv::restore()
    ```

5. Run the main script

6. View results

## If you're running the code any subsequent time...

1. Set your working directory into the repository:

    Shell:
    
    ```sh
    cd your/path/to/household-size
    ```
    
    R:
    
    ```r
    setwd("your/path/to/household-size")
    ```

2. Restore the [`renv`](https://rstudio.github.io/renv/articles/renv.html)

    ```r
    renv::restore()
    ```

3. Run the main script


# References

- <a name="galster2024" id="galster2024"></a>**George C. Galster (2024)**. *Is There Enough Housing Production? It Matters Which Indicators Are Used to Answer*, Housing Policy Debate. DOI: 10.1080/10511482.2024.2334018

- <a name="mcclure2024" id="mcclure2024"></a>**McClure, K., & Schwartz, A. (2024)**. *Where Is the Housing Shortage?* Housing Policy Debate, 1–15. [https://doi.org/10.1080/10511482.2024.2334011](https://doi.org/10.1080/10511482.2024.2334011)
