# Advanced Business Analytics modules using R

These modules were prepared as part of *Advanced Business Analytics* graduate course work in UNC Charlotte in 2019.

### Major R Libraries used
ggplot2, tidyverse, lubridate, purrr, quanteda, stopwords, topicmodels, tidytext

### Data Analytics Techniques used
- Data wrangling - data cleanup, data merging / splitting, data type conversions, data format corrections.
- Visualizations - using ggplots.
- Forecasting - Time-series regression & ARIMA analysis.
- Text analysis - using Quanteda.

---
### How to use
`100-data-prep-all.R` is the master program that triggers all other data preparation steps.

#### Pre-requisites
- A sub-folder named 'data' containing all the 5 needed data files
	- data/product_master.csv
	- data/customer_master.csv
	- data/email_campaign.csv
	- data/product_airtime.csv
	- data/social.csv
- A sub-folder named 'rdata' for the program to store the generated Rda files.
- A sub-folder named 'csv' for the program to store the generated csv files.

---
`200-data-analysis-forecast.R` is the program that generates time-series data analysis.

#### Pre-requisites
This uses the Rda file (rdata/orders_daily_summary.Rda) generated by the previous data-preparation process.
