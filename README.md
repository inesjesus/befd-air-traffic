# Air traffic through the years

With this project we aim to analyse the air traffic in three european countries: Germany, Italy and Spain. 

We take the data mainly from Eurostat (https://ec.europa.eu/eurostat/) and answer the following questions:

- What influences the air traffic and how?
- Are the factors different across countries?

To answer these questions we do a time-series analysis using time-series linear models and dynamic regressions with ARIMA errors. To handle the bias-variance trade-off and avoid overfitting, we used the LASSO and the best subset selection approaches as feature selectors.
