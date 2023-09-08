# Water100 Project

## Overview
This repository contains code for the [Water100 Project](https://water100project.org/), aimed at evaluating dose-response curves from the EPA's [ECOTOX database](https://cfpub.epa.gov/ecotox/). The code is written in R.


## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/Geosyntec/water100.git
   ```


## Folder Structure

* `/data`               
     Processed data from ECOTOX



* `/R`  
     .R and .Rmd scripts for analysis

* `/xlsx`   
additional spreadsheets   
* `/docx`   
    Report (in progress)
  
## Ecotox Database
Due to its large size, the Ecotox database is not included in the GitHub repository. However, you can build it locally using the {ECOTOXr} package. 
The package documentation and source code are available at the [ECOTOXr GitHub Repository](https://github.com/pepijn-devries/ECOTOXr).

To install the ECOTOXr package and build the database, you can follow these steps:

``` r
#install Ecotoxr
install.packages("ECOTOXr")

# Load library
library(ECOTOXr)

#Build the database locally
build_ecotox_db() 
```

## Usage
#### R/ecotox_eda_v6.Rmd

```R/dose_response_example.R``` Provides an example of developing dose-response curves 
```R/ecotox_eda_v6.Rmd``` ECOTOX Exploratory Data Analysis


## Contributing

1. Fork the repository.
2. Create a new branch.
3. Commit your changes.
4. Create a pull request.



## License

This project is licensed under the MIT License. See [LICENSE.md](LICENSE.md) for details.
