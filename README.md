# Forecasting Time Series Data for EU Travel Expenditure and US Consumer Price Index

## Overview
This repository contains two separate forecasting projects:
1. **EU Travel Expenditure Forecasting** - This project focuses on forecasting the travel expenditure in the EU using various time series methods.
2. **US Consumer Price Index Forecasting** - This project aims to forecast the Consumer Price Index (CPI) in the USA up to February 2024.

## EU Travel Expenditure Forecasting

### Data Description
The dataset spans from January 2012 to December 2022, with training data up to March 2017 and testing data from April 2017 to March 2020.

### Methodology
- **Data Transformation**: Implementation of Box-Cox transformation to stabilize variance.
- **Modeling Techniques**: Utilization of Seasonal Naive, ETS, and ARIMA models.
- **Performance Evaluation**: Assessment based on RMSE, MAE, MAPE, and MASE values.

### Key Findings
- ETS and ARIMA models outperform Naive and RW Drift models in accuracy.
- The best models are identified based on the lowest RMSE and other forecasting accuracy measures.


## US Consumer Price Index Forecasting

### Data Overview
Data covers the period from January 2000 to February 2023, with training data until December 2021 and testing from January 2021 to February 2023.

### Approach
- **Seasonal Analysis**: Examination of patterns and trends over the years.
- **ARIMA Modeling**: Detailed analysis and modeling using ARIMA specifications.

### Results
- Identification of significant seasonal patterns influencing the CPI.
- ARIMA models provide robust predictions with detailed residual diagnostics.


## Technologies Used
- **R** for statistical computing and graphics.
- **Python** for data manipulation and plotting.
- **ARIMA** and **ETS** models for time series forecasting.

## Installation
To replicate the analysis:
1. Clone this repository:
2. Install required R packages and Python libraries.

## Usage
Navigate to the project directory and open the respective R or Python notebooks to view the analysis and run the models.

## Contributing
Contributions to enhance the models, improve forecasting accuracy, or extend the datasets are welcome. Please fork the repository and submit a pull request with your changes.

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for more details.

## Acknowledgments
- Data sourced from [EU Statistical Office](https://ec.europa.eu/eurostat) and [U.S. Federal Reserve Economic Data](https://fred.stlouisfed.org/series/USACPIALLMINMEI).

## Visualizations
Additional plots and model diagnostics are included within the notebooks to provide deeper insights into the forecasting models' performance and the data characteristics.
