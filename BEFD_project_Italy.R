# Time-series analysis of air and railway transport data for Spain (this script), Germany and Italy.

#### Reading the data and importing the libraries ####

library(dplyr)
library(lmtest) 
library(forecast)
library(fpp2)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(MuMIn)
library(lmtest)
library(glmnet)
library(leaps)
library(car)
library(lubridate)
library(seasonal)
#library(readxl)
#library(DIMORA)

setwd("D:/User/Desktop/Italia/3rd semester/Business Economic and Financial Data/BEFD-project")
rail = read.csv("Datasets/estat_rail_pa_quartal_filtered_en.csv")
air = read.csv("Datasets/estat_avia_paoc_filtered_en.csv")
population = read.csv("Datasets/estat_demo_pjan_filtered_en.csv")
gdp = read.csv("Datasets/estat_namq_10_gdp_filtered_en.csv")
prices = read.csv("Datasets/estat_prc_hicp_midx_filtered_en.csv")
railLength = read.csv("Datasets/estat_rail_if_line_na_filtered_en.csv")
area = read.csv("Datasets/estat_reg_area3_filtered_en.csv")
tourismOccupation = read.csv("Datasets/estat_tour_occ_mnor_filtered_en.csv")
restrictions = read.csv("Datasets/internal-movement-covid.csv")

#### Preliminary exploration and preprocessing ####

# There are two kinds of measurement units in each dataset. 
# For the air dataset, it is possible to see the number of passengers and the number of flights
# For the rail dataset, it is possible to see the number of passengers and the passenger*km measurement.
# I think we could analyze passengers vs passengers (straightforward approach) 
# and flights vs passenger*km (which may address better the environmental side of the analysis)

# Datasets structure
str(rail)
str(air)

# Preprocessing-------------------------------------------------------------------------------

# Reorganizing the data so we have an observation for each quarter and each country
pivotRail = rail %>%  pivot_wider(id_cols = c('TIME_PERIOD', 'geo'),
                                  names_from = 'unit',
                                  values_from = 'OBS_VALUE')


pivotAir = air %>%  pivot_wider(id_cols = c('TIME_PERIOD','geo'),
                                names_from = c('unit'),
                                values_from = 'OBS_VALUE')

# Joining both data sets to have them in a compact and organized structure
railAir = merge(pivotRail, pivotAir, by = c('TIME_PERIOD','geo'), all.y = TRUE)

# Joining the population of each country and each year to normalize the variables of interest
colnames(population)[colnames(population) == 'OBS_VALUE'] = 'POPULATION'

## We only have population up to 2022 (we would lose that data if we wanted to normalize)
## We could forecast the info for the population in 2023 with an exponential smoothing method (including a damped trend)
population = population[, c("geo", "TIME_PERIOD", "POPULATION")]
population = population %>% 
  arrange(geo, TIME_PERIOD)
pop_forecast <- function() {
  imputedPopulation = data.frame()
  forecasts = c()
  for (country in unique(population$geo)){
    # Doing the population forecast for each country
    countryPopulation = population[population$geo==country,]
    countryHolt = holt(countryPopulation$POPULATION, h = 10, level = c(80),
                       alpha = NULL, damped=T)
    
    # Plotting the forecast
    print(autoplot(countryHolt)+
            autolayer(fitted(countryHolt), series="Fitted") + 
            ylab(paste('Population forecast','-',country)) + 
            xlab("Year"))
    
    # Adding the forecast to 2023
    forecast_2023 = data.frame(geo = country,TIME_PERIOD = 2023,
                               POPULATION = countryHolt$mean[1])
    countryPopulation = bind_rows(countryPopulation, forecast_2023)
    imputedPopulation = bind_rows(imputedPopulation, countryPopulation)
  }
  return(imputedPopulation)
}
population = pop_forecast()

railAir$YEAR = as.integer(substr(as.character(railAir$TIME), 1, 4))
railAir = merge(railAir, population[,c('TIME_PERIOD','geo','POPULATION')], 
                by.x = c('YEAR','geo'), by.y = c('TIME_PERIOD','geo'), all.x = TRUE)

# Normalizing the variables of interest by the population of each country
# (the final measurement units will be given per million inhabitants)
railAir$MIO_PKM = 1000000 * railAir$MIO_PKM / railAir$POPULATION
railAir$THS_PAS = 1000000 * railAir$THS_PAS / railAir$POPULATION
railAir$FLIGHT = 1000000 * railAir$FLIGHT / railAir$POPULATION
railAir$PAS = 1000000 * railAir$PAS / railAir$POPULATION


# Changing the column names to make it more understandable (I will refer to the 'quarter' variable as 'TIME' for the plots)
# (the rail measurements were given as thousand passengers and air measurements were not, so they are normalized)
railAir$PAS = railAir$PAS/1000
colnames(railAir) = c("YEAR","COUNTRY","TIME","R_MIO_PKM","R_THS_PAS","A_FLIGHTS","A_THS_PAS","POPULATION")

# Making the TIME column and ordered factor and the COUNTRY column a factor
railAir$TIME = factor(railAir$TIME, ordered = TRUE)
railAir$COUNTRY = factor(railAir$COUNTRY)

# Adding a variable for the season (quarter)
railAir$QUARTER = factor(substring(as.character(railAir$TIME), nchar(as.character(railAir$TIME)) - 1))
railAir$QUARTER = relevel(railAir$QUARTER, ref = 'Q1')

# Joining the gdp of each country and each quarter.
# Regarding this variable, it is worth noting that We are using a version that is
# chain linked (inflation adjusted) and seasonally and calendar adjusted. 
colnames(gdp)[colnames(gdp) == 'OBS_VALUE'] = 'GDP'
railAir = merge(railAir, gdp[,c('TIME_PERIOD','geo','GDP')], 
                by.x = c('TIME','COUNTRY'), by.y = c('TIME_PERIOD','geo'), all.x = TRUE)

## Obtaining the GDP per capita for each period.
railAir$GDP_PC = 1000*railAir$GDP / railAir$POPULATION


# I found the prices for rail passenger transport, air transport
# and also just domestic flights. 
# However, unfortunately, it seems that they only started reporting the prices
# of domestic flights not so long ago, so we have a lot of missing values and I think
# it may not be useful. I add it in case you want to see it.

## For joining the price information, we first need to reorganize that dataset
pivotPrice = prices %>%  pivot_wider(id_cols = c('TIME_PERIOD','geo'),
                                     names_from = c('coicop'),
                                     values_from = 'OBS_VALUE')

## Since the price data is monthly and our other data is quarterly,
## we can group it with an average for each quarter.

### Renaming the columns for clarity and extracting the year and the quarter
colnames(pivotPrice) = c('MONTH','COUNTRY','RAIL_PRICE','AIR_PRICE','DOM_AIR_PRICE')
pivotPrice$YEAR = as.integer(substr(as.character(pivotPrice$MONTH), 1, 4))
pivotPrice = pivotPrice %>%
  mutate(QUARTER = paste0("Q",
                          quarter(as.Date(paste0(MONTH, "-01")))))
pivotPrice$QUARTER = factor(pivotPrice$QUARTER)
pivotPrice$QUARTER = relevel(pivotPrice$QUARTER, ref = 'Q1')


### Grouping the data by quarter (with an average)
quarterPrices = pivotPrice %>%
  group_by(YEAR, QUARTER, COUNTRY) %>%
  summarize(
    RAIL_PRICE = mean(RAIL_PRICE, na.rm = TRUE),
    AIR_PRICE = mean(AIR_PRICE, na.rm = TRUE),
    DOM_AIR_PRICE = mean(DOM_AIR_PRICE, na.rm = TRUE)
  )

### Joining the data to the main dataframe
railAir = merge(railAir, quarterPrices, 
                by = c('YEAR','QUARTER','COUNTRY'), all.x = TRUE)


# Adding the variables to obtain the railway density of each country

## First, we need to prepare the length data.

### The dataset includes the railway length for passengers and freight, and passengers only separately.
### I will keep them like that because we have missing values and it is easier treat each type separately and then add them.

pivotRailLength = railLength %>%  pivot_wider(id_cols = c('TIME_PERIOD','geo'),
                                              names_from = c('tra_meas'),
                                              values_from = 'OBS_VALUE')

### Some years are missing information, so we need to complete that information.
### First, we fill in the years according to what we need. 
### We do it by joining a dataframe that only contains the years (and countries) of our dataframe.
### We sort it to for a better visualization of the table.
dfYearCountry = data.frame(unique(railAir[,c('YEAR','COUNTRY')]))
pivotRailLength =  merge(dfYearCountry, pivotRailLength, 
                         by.x = c('YEAR','COUNTRY'), by.y=c('TIME_PERIOD','geo'),  all.x = TRUE)
pivotRailLength = pivotRailLength[order(pivotRailLength$COUNTRY, pivotRailLength$YEAR), ]


### Since it is infrastructure and we could expect it to be rather stable from one year to another,
### I imputed the NA values with a linear interpolation for missing values in the middle and
### with the closest observation for values at the beginning or the end of each series (hence the rule=2)
pivotRailLength = pivotRailLength %>%
  group_by(COUNTRY) %>%
  mutate(
    PAS_FR_IMPUTED = na.approx(PAS_FR, method = "linear", rule=2),
    PAS_ONL_IMPUTED = na.approx(PAS_ONL, method = "linear", rule=2)
  ) %>%
  ungroup()

### Now, the total railway kilometers for passengers are obtained by adding the length of railway
### for passengers and freight, and the length passengers only.
pivotRailLength$RAIL_LENGTH = pivotRailLength$PAS_FR_IMPUTED + pivotRailLength$PAS_ONL_IMPUTED

## Now, we prepare the land area data (does not include rivers, lakes, etc.) in a similar way.

### We also add the missing years (which in this case are only at the beginning and the end of the series)
colnames(area)[colnames(area) == 'OBS_VALUE'] = 'AREA_RAW'
area = merge(dfYearCountry, area[,c('TIME_PERIOD','geo','AREA_RAW')], 
             by.x = c('YEAR','COUNTRY'), by.y=c('TIME_PERIOD','geo'),  all.x = TRUE)
area = area[order(area$COUNTRY, area$YEAR), ]

### Of course the area is pretty much a constant so the data is just imputed as the closest observation
area = area %>%
  group_by(COUNTRY) %>%
  mutate(
    AREA = na.approx(AREA_RAW, rule=2)
  ) %>%
  ungroup()

# Finally we can obtain the rail density for passengers

## We just join the dataframes and make the operation as 1000*length/ area.
## I found on the internet that rail density is often considered in this way (length of lines operated (km) per 1000 km2)
pivotRailLength =  merge(pivotRailLength, area[,c('COUNTRY','YEAR','AREA')], 
                         by = c('COUNTRY','YEAR'),  all.x = TRUE)
pivotRailLength$RAIL_DENSITY = 1000*pivotRailLength$RAIL_LENGTH / pivotRailLength$AREA

#### Adding it to the main dataframe
railAir = merge(railAir, pivotRailLength[,c('YEAR','COUNTRY','RAIL_DENSITY')], 
                by = c('YEAR','COUNTRY'), all.x = TRUE)


# We are going to add the information about the percentage of occupancy
# in hotels and similar accommodation to have a measure of the touristic activity

## We only have data up to 2023-09, so I am going to forecast up to 2023-12
## in order to have information of the last quarter when we feed the models.

colnames(tourismOccupation)[colnames(tourismOccupation) == 'OBS_VALUE'] = 'TOURISM_OCC'
tourism_forecast <- function() {
  imputedTourism = data.frame()
  forecasts = c()
  for (country in unique(tourismOccupation$geo)){
    # Doing the tourism forecast for each country
    countryTourismOccupation = tourismOccupation[tourismOccupation$geo==country,]
    
    countryTourismOccupationTS = ts(countryTourismOccupation$TOURISM_OCC, start=2000, frequency=12)
    #countryHolt = holt(countryPopulation$POPULATION, h = 10, level = c(80),
    #                   alpha = NULL, damped=T)
    countryHoltWinters = hw(countryTourismOccupationTS, h = 10, level = c(80),
                            alpha = NULL)
    
    
    # Plotting the forecast
    print(autoplot(countryHoltWinters)+
            autolayer(fitted(countryHoltWinters), series="Fitted") + 
            ylab(paste('Tourism occupation forecast','-',country)) + 
            xlab("Time"))
    
    # Adding the forecast to what's left of 2023
    forecast_2023 = data.frame(geo = rep(country,3),
                               TIME_PERIOD = c('2023-10','2023-11','2023-12'),
                               TOURISM_OCC = countryHoltWinters$mean[1:3])
    countryTourismOccupation = bind_rows(countryTourismOccupation, forecast_2023)
    imputedTourism = bind_rows(imputedTourism, countryTourismOccupation)
  }
  return(imputedTourism)
}
tourismOccupation = tourism_forecast()
tourismOccupation = tourismOccupation[,c('geo','TIME_PERIOD','TOURISM_OCC')]

## Since the touristic occupation data is monthly and our other data is quarterly,
## we can group it with an average for each quarter.

### Renaming the columns for clarity and extracting the year and the quarter
colnames(tourismOccupation) = c('COUNTRY','MONTH','TOURISM_OCC')
tourismOccupation$YEAR = as.integer(substr(as.character(tourismOccupation$MONTH), 1, 4))
tourismOccupation = tourismOccupation %>%
  mutate(QUARTER = paste0("Q",
                          quarter(as.Date(paste0(MONTH, "-01")))))
tourismOccupation$QUARTER = factor(tourismOccupation$QUARTER)
tourismOccupation$QUARTER = relevel(tourismOccupation$QUARTER, ref = 'Q1')


### Grouping the data by quarter (with an average)
quarterTourismOccupation = tourismOccupation %>%
  group_by(COUNTRY,YEAR, QUARTER) %>%
  summarize(
    TOURISM_OCC = mean(TOURISM_OCC, na.rm = TRUE))

### Joining the data to the main dataframe
railAir = merge(railAir, quarterTourismOccupation, 
                by = c('COUNTRY','YEAR','QUARTER'), all.x = TRUE)



# Adding the alternative COVID variable defined by Our world in data (the one found by Inês)

## Filtering it for the countries we need
restrictions = restrictions[restrictions$Entity %in% c("Spain", "Germany", "Italy"), ]

## Adjusting the country label so that it matches our other datasets
restrictions$Code = factor(substring(restrictions$Code, 1, nchar(restrictions$Code) - 1))

## Extracting the year and the quarter
restrictions$YEAR = as.integer(substr(restrictions$Day, 1, 4))
restrictions$Day = as.Date(restrictions$Day)
restrictions  = restrictions  %>%
  mutate(QUARTER = paste0("Q", quarter(Day)))
restrictions$QUARTER = factor(restrictions$QUARTER)
restrictions$QUARTER = relevel(restrictions$QUARTER, ref = 'Q1')

## Grouping by country and quarter
restrictionsQuarter = restrictions %>%
  group_by(Code,YEAR,QUARTER) %>%
  summarize(COVID_AVG_RESTRICTIONS = mean(restrictions_internal_movements))

## Merging with the main dataframe
colnames(restrictionsQuarter) = c('COUNTRY','YEAR','QUARTER','COVID_AVG_RESTRICTIONS')
railAir = merge(railAir, restrictionsQuarter, 
                by = c('COUNTRY','YEAR','QUARTER'), all.x = TRUE)

## Filling with 0 the values that were out of range of the COVID period
railAir = railAir %>%
  mutate(COVID_AVG_RESTRICTIONS = ifelse(is.na(COVID_AVG_RESTRICTIONS), 0, COVID_AVG_RESTRICTIONS))

## Creating a factor column corresponding to the rounded version of the COVID_AVG_RESTRICTIONS
railAir$COVID_AVG_RESTRICTIONS_FACTOR = as.factor(round(railAir$COVID_AVG_RESTRICTIONS))




# Omitting the rows that give no information (of rail or air passengers/flights)
# TODO: Check if you're ok with this filter. 
# Also, this should be done right before separating the dataframes, just to be sure that they stay ordered  
railAir = railAir[!(is.na(railAir$R_THS_PAS) | is.na(railAir$A_THS_PAS) | is.na(railAir$A_FLIGHTS)), ]

# Sorting the dataframe (for adding the trend later)
railAir = railAir %>% 
  arrange(COUNTRY, TIME)


# Separating the data by country------------------------------------------------------------- 

# Replacing the country labels and splitting the datasets
esRailAir = railAir[railAir$COUNTRY == "ES",]
deRailAir = railAir[railAir$COUNTRY == "DE",]
itRailAir = railAir[railAir$COUNTRY == "IT",]

# Adding a variable for the trend
# (TODO: we should check if more values are removed so that the trend is assigned correctly)
esRailAir$TREND = (1:nrow(esRailAir))
deRailAir$TREND = (1:nrow(deRailAir))
itRailAir$TREND = (1:nrow(itRailAir))

# Adding the intervention variables for each country

# TODO
# We can add the dates for each country. We can try different dates according to the regulations on each country and see what works best
# Just in case, the official end of the pandemic was declared by the OMS on may 5 2023.

# Adding the intervention variable for COVID. Just trying to search for a rough start and end
#esRailAir = esRailAir %>%
#  mutate(COVID = as.integer(TIME >= "2020-Q1" & TIME <= "2021-Q2"))

#deRailAir = deRailAir %>%
#  mutate(COVID = as.integer(TIME >= "2020-Q1" & TIME <= "YYYY-QX"))

#itRailAir = itRailAir %>%
#  mutate(COVID = as.integer(TIME >= "2020-Q1" & TIME <= "YYYY-QX"))

## Adding a different version of the COVID variable that tries to capture the "strength of COVID restrictions" more precisely
## In the case of Spain, I am going to use: 
## Strong: Strongest restrictions for the whole quarter
## Medium: Medium restrictions for the whole quarter or most of it
## Low: Weak restrictions for the whole quarter OR medium (or weak) restrictions for part of the quarter OR strong restrictions for a small part of the quarter (just the first quarter)
## No: no restrictions for the whole quarter
#esRailAir = esRailAir %>%
#  mutate(COVID_ADJ = ifelse(TIME == "2020-Q2", 'Strong',
#                            ifelse((TIME >= "2020-Q4" & TIME <= "2021-Q1"), 'Medium',
#                                   ifelse((TIME == "2020-Q1" | TIME == "2020-Q3" | TIME == "2021-Q2"), 'Low', 'No'))))
#esRailAir$COVID_ADJ = factor(esRailAir$COVID_ADJ)
#esRailAir$COVID_ADJ = relevel(esRailAir$COVID_ADJ, ref = 'No')


# Adding the intervention variable for the train discount
#esRailAir = esRailAir %>%
#  mutate(TRAIN_DISCOUNT = as.integer(TIME >= "2022-Q3"))

#deRailAir = deRailAir %>%
#  mutate(TRAIN_DISCOUNT = as.integer((TIME == "2022-Q3") | (TIME >= "2023-Q2")))


# TODO: check that all variables are included
# Converting into time-series objects in case it is needed
#esRailAirTS = ts(esRailAir[, c("R_MIO_PKM","R_THS_PAS", "A_FLIGHTS", "A_THS_PAS", 
#                               "POPULATION", "COVID", "COVID_ADJ", "COVID_AVG_RESTRICTIONS","TRAIN_DISCOUNT",
#                               "GDP", "GDP_PC", "RAIL_PRICE", "AIR_PRICE", "DOM_AIR_PRICE", 
#                               "RAIL_DENSITY", "TOURISM_OCC", 
#                               "QUARTER","TREND")],
#                 start = c(2004, 1),
#                 frequency = 4)

#deRailAirTS = ts(deRailAir[, c("R_MIO_PKM","R_THS_PAS", "A_FLIGHTS", "A_THS_PAS", 
#                               "POPULATION", "COVID", "COVID_ADJ", "COVID_AVG_RESTRICTIONS","TRAIN_DISCOUNT",
#                               "GDP", "GDP_PC", "RAIL_PRICE", "AIR_PRICE", "DOM_AIR_PRICE", 
#                           esFlightsArBaseline    "RAIL_DENSITY", "TOURISM_OCC", 
#                               "QUARTER","TREND")],
#                 start = c(2004, 1),
#                 frequency = 4)



# Adding intervention variable for itRail data

itRailAir$INTERVENTION <- ifelse(itRailAir$YEAR > 2011, 1, 0)

itRailAirTS = ts(itRailAir[, c("R_MIO_PKM","R_THS_PAS", "A_FLIGHTS", "A_THS_PAS", 
                               "POPULATION", "COVID_AVG_RESTRICTIONS",
                               "GDP", "GDP_PC", "RAIL_PRICE", "AIR_PRICE", "DOM_AIR_PRICE", 
                               "RAIL_DENSITY", "TOURISM_OCC", 
                               "QUARTER","TREND","INTERVENTION")],
                 start = c(2004, 1),
                 frequency = 4)




# Applying seasonal adjustment for the required series-------------------------

seasonal_adjustments <- function(country_df, ts_data) {
  # Seasonally adjusting the predictors that might suffer from seasonality
  # A_FLIGHTS and A_THS_PAS are not included because they will always be 
  # treated as the target variable and I want to keep it as is.
  # DOM_AIR is adjusted just to see its plot, but since it has so many missing values, won't be used
  R_THS_PAS_S_ADJ = seasadj(seas(ts_data[,"R_THS_PAS"], x11=""))
  R_MIO_PKM_S_ADJ = seasadj(seas(ts_data[,"R_MIO_PKM"], x11=""))
  RAIL_PRICE_S_ADJ = seasadj(seas(ts_data[,"RAIL_PRICE"], x11=""))
  AIR_PRICE_S_ADJ = seasadj(seas(ts_data[,"AIR_PRICE"], x11=""))
  DOM_AIR_PRICE_S_ADJ = seasadj(seas(ts_data[,"DOM_AIR_PRICE"], x11=""))
  TOURISM_OCC_S_ADJ = seasadj(seas(ts_data[,"TOURISM_OCC"], x11=""))
  
  # Plotting the adjusted version of the columns to evaluate what the adjustment did
  print(autoplot(ts_data[, "R_THS_PAS"], series="Original data") +
          autolayer(R_THS_PAS_S_ADJ, series="Seasonally adjusted") +
          xlab("Time") + ylab("Thousands of passengers") +
          ggtitle("Rail - Passengers per million inhabitants") +
          scale_colour_manual(values=c("darkgray","blue"),
                              breaks=c("Original data","Seasonally adjusted")) +
          theme_minimal())
  
  print(autoplot(ts_data[, "R_MIO_PKM"], series="Original data") +
          autolayer(R_MIO_PKM_S_ADJ, series="Seasonally adjusted") +
          xlab("Time") + ylab("Millions of passenger-km") +
          ggtitle("Rail - Passenger-km per million inhabitants") +
          scale_colour_manual(values=c("darkgray","blue"),
                              breaks=c("Original data","Seasonally adjusted")) +
          theme_minimal())
  
  print(autoplot(ts_data[, "RAIL_PRICE"], series="Original data") +
          autolayer(RAIL_PRICE_S_ADJ, series="Seasonally adjusted") +
          xlab("Time") + ylab("HIPC (2015 = 100)") +
          ggtitle("Rail - Prices (HICP)") +
          scale_colour_manual(values=c("darkgray","blue"),
                              breaks=c("Original data","Seasonally adjusted")) +
          theme_minimal())
  
  print(autoplot(ts_data[, "AIR_PRICE"], series="Original data") +
          autolayer(AIR_PRICE_S_ADJ, series="Seasonally adjusted") +
          xlab("Time") + ylab("HIPC (2015 = 100)") +
          ggtitle("Air - Prices (HICP)") +
          scale_colour_manual(values=c("darkgray","blue"),
                              breaks=c("Original data","Seasonally adjusted")) +
          theme_minimal())
  
  
  print(autoplot(ts_data[, "TOURISM_OCC"], series="Original data") +
          autolayer(TOURISM_OCC_S_ADJ, series="Seasonally adjusted") +
          xlab("Time") + ylab("Rate of occupancy") +
          ggtitle("Tourism - Net occupancy rate of bed-places in hotels and similar accommodation") +
          scale_colour_manual(values=c("darkgray","blue"),
                              breaks=c("Original data","Seasonally adjusted")) +
          theme_minimal())
  
  
  # Adding the adjusted columns to the main dataframe
  country_df = cbind(country_df, 
                     R_THS_PAS_S_ADJ, R_MIO_PKM_S_ADJ,
                     RAIL_PRICE_S_ADJ, AIR_PRICE_S_ADJ,
                     TOURISM_OCC_S_ADJ)
  
  columns_to_convert = c('R_THS_PAS_S_ADJ', 'R_MIO_PKM_S_ADJ','RAIL_PRICE_S_ADJ', 'AIR_PRICE_S_ADJ','TOURISM_OCC_S_ADJ')
  country_df[columns_to_convert] <- lapply(country_df[columns_to_convert], as.numeric)
  return(country_df)
}


# TODO: execute the function with your datasets, checking that all columns are complete on the xxRailAirTS dataset definition
itRailAir = seasonal_adjustments(country_df=itRailAir, ts_data=itRailAirTS)

# Preliminary plots ----------------------------------------------------------------
# 

# Plots for all countries in the same plot each column (in process, perhaps we should normalize by number of inhabitants):

plot_allCountries <- function(y_variable, lbl_axis, lbl_plot) {
  ggplot(data=railAir, aes(x=TIME, y=.data[[y_variable]], group=COUNTRY)) +
    geom_line(aes(color=COUNTRY)) +
    ylab(lbl_axis) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_discrete(breaks = subset(railAir, grepl("-Q1$", railAir$TIME))$TIME) +
    scale_color_brewer(name = "Country", palette = "Dark2") +
    ggtitle(lbl_plot)
}


plotAllRailPas = plot_allCountries(y_variable='R_THS_PAS', lbl_axis='Thousands of passengers', lbl_plot='Rail - Passengers per million inhabitants')
plotAllAirPas = plot_allCountries(y_variable='A_THS_PAS', lbl_axis='Thousands of passengers', lbl_plot='Air - Passengers per million inhabitants')
plotAllRailPaskm = plot_allCountries(y_variable='R_MIO_PKM', lbl_axis='Millions of passenger-km', lbl_plot='Rail - Passenger-km per million inhabitants')
plotAllAirFlights = plot_allCountries(y_variable='A_FLIGHTS', lbl_axis='Number of flights', lbl_plot='Air - Flights per million inhabitants')

plotAllRailPas
plotAllAirPas
plotAllRailPaskm
plotAllAirFlights


# Plot for each country separately pass vs pass, pass*km vs flights (in process)

## Function to plot the comparison
plot_competition <- function(df, y_variable1, y_variable2,
                             lbl_country, lbl_plot,
                             lbl_axis1, lbl_axis2,scale_SecAxis = 10) {
  ggplot() +
    geom_line(data=df, aes(x=TIME, y=.data[[y_variable1]], group=1))+
    geom_line(data=df, aes(x=TIME, y=.data[[y_variable2]]*scale_SecAxis, group=1, color="red"), 
              show.legend = FALSE) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_discrete(breaks = subset(df, grepl("-Q1$", df$TIME))$TIME) +
    scale_y_continuous(name = lbl_axis1,
                       sec.axis = sec_axis( trans=~./scale_SecAxis, name=lbl_axis2)) +
    theme(axis.title.y.right = element_text(color = "red")) +
    ggtitle(paste(lbl_country,'-', lbl_plot))
}

## Plotting the Passenger pass vs pass for each country
plotEsPas = plot_competition(df=esRailAir, y_variable1='R_THS_PAS', y_variable2='A_THS_PAS',
                             lbl_country='Spain', lbl_plot='Passengers per million inhabitants',
                             lbl_axis1='Rail - Thousands of passengers', lbl_axis2='Air - Thousands of passengers')
plotItPas = plot_competition(df=itRailAir, y_variable1='R_THS_PAS', y_variable2='A_THS_PAS',
                             lbl_country='Italy', lbl_plot='Passengers per million inhabitants', 
                             lbl_axis1='Rail - Thousands of passengers', lbl_axis2='Air - Thousands of passengers')
plotDePas = plot_competition(df=deRailAir, y_variable1='R_THS_PAS', y_variable2='A_THS_PAS',
                             lbl_country='Germany', lbl_plot='Passengers per million inhabitants',
                             lbl_axis1='Rail - Thousands of passengers', lbl_axis2='Air - Thousands of passengers')
plotDePasScaled = plot_competition(df=deRailAir, y_variable1='R_THS_PAS', y_variable2='A_THS_PAS',
                                   lbl_country='Germany', lbl_plot='Passengers per million inhabitants (rescaled)',
                                   lbl_axis1='Rail - Thousands of passengers', lbl_axis2='Air - Thousands of passengers',
                                   scale_SecAxis = 100)

plotEsPas
plotItPas
plotDePas
plotDePasScaled

## Plotting the pass*km vs flights for each country
plotEsPaskmFlights = plot_competition(df=esRailAir, y_variable1='R_MIO_PKM', y_variable2='A_FLIGHTS',
                                      lbl_country='Spain', lbl_plot='Passenger-km and number of flights per million inhabitants',
                                      lbl_axis1='Rail - Millions of passenger-km', lbl_axis2='Air - Number of flights',
                                      scale_SecAxis = 0.1)
plotItPaskmFlights = plot_competition(df=itRailAir, y_variable1='R_MIO_PKM', y_variable2='A_FLIGHTS',
                                      lbl_country='Italy', lbl_plot='Passenger-km and number of flights per million inhabitants', 
                                      lbl_axis1='Rail - Millions of passenger-km', lbl_axis2='Air - Number of flights',
                                      scale_SecAxis = 0.1)
plotDePaskmFlights = plot_competition(df=deRailAir, y_variable1='R_MIO_PKM', y_variable2='A_FLIGHTS',
                                      lbl_country='Germany', lbl_plot='Passenger-km and number of flights per million inhabitants',
                                      lbl_axis1='Rail - Millions of passenger-km', lbl_axis2='Air - Number of flights',
                                      scale_SecAxis = 0.1)
plotDePaskmFlightsScaled = plot_competition(df=deRailAir, y_variable1='R_MIO_PKM', y_variable2='A_FLIGHTS',
                                            lbl_country='Germany', lbl_plot='Passenger-km vs number of flights per million inhabitants (rescaled)',
                                            lbl_axis1='Rail - Millions of passenger-km', lbl_axis2='Air - Number of flights',
                                            scale_SecAxis = 1)

plotEsPaskmFlights
plotItPaskmFlights
plotDePaskmFlights
plotDePaskmFlightsScaled


# Insights:

# 1. Note that in the case of Germany, I had to further rescale the air passengers series to make it visible,
# so, people use the train much more than the airplanes in comparison to Spain and Italy (maybe they are more environmentally friendly?).
# 2. In Spain and in Italy, peaks of air transport appear to coincide with the valleys of the rail transport
# and I think they tend to coincide with the Q3 (summer vacation?). This behavior is not present in the Germany data



#### Models for air passengers ####

# Baseline - Air passengers ----------------------------------------------------------------

# We start with a "baseline" model: a linear regression with just trend and season (and the basic COVID variable). 
# These are the 'basic' variables that we can build upon
#esPasBaseline = lm(data = esRailAir, A_THS_PAS ~ TREND + QUARTER + COVID)
itPasBaseline = lm(data = itRailAir, A_THS_PAS ~ TREND + QUARTER + COVID_AVG_RESTRICTIONS)



## Graphically, it seems we can do better, but at least we are capturing some time-dependent patterns, specially seasonality.
## We also capture some of the COVID effect (but maybe this can be improved).
## There does not seem to be a strong trend in this case.
#plot(esRailAir$A_THS_PAS)
#lines(esRailAir$A_THS_PAS)
#lines(fitted(esPasBaseline), col=2)
par(mar = c(5, 4, 4, 2) + 0.1)
plot(itRailAir$A_THS_PAS)
lines(itRailAir$A_THS_PAS)
lines(fitted(itPasBaseline), col=2)

## This is the summary of the significance and the goodness of fit of this linear model.
## We can try to improve it.
#summary(esPasBaseline)
#AICc(esPasBaseline)
summary(itPasBaseline)
AICc(itPasBaseline)
vif(itPasBaseline)


## The residuals do seem to follow a kind of harmonic pattern, it is not white noise as we would wish
## And we still have significant autocorrelation for the first lags (also for Italy)
plot(residuals(itPasBaseline))
tsdisplay(residuals(itPasBaseline), main = "Italy - Baseline residuals and autocorrelation")

# Durbin-Watson test
print(durbinWatsonTest(itPasBaseline))
# Shows significant positive autocorrelation on the first lag

# For this reason, we can fit a dynamic regression with the previous parameters.
# That way, we can capture the remaining autocorrelations (the time-dependent relations we did not capture with the predictors)

## Defining a function to fit a dynamic regression with explicit variables for the seasons (quarters) and other predictors
## The reason for this is that auto.arima requires a specific type of data as input
reg_ARIMA_errors <- function(df, y_col, predictors,
                             seasonal = TRUE) {
  # predictors can be passed as a matrix or just as the names of the columns.
  # That makes the function easy to use and robust in case a custom matrix is needed.
  # The intercept is removed because it will be added then by the ARIMA model.
  if (is.matrix(predictors)){
    X = predictors
    if('(Intercept)' %in% colnames(X)){
      X = X[, -grep('(Intercept)', colnames(X), ignore.case = TRUE)]
    } 
  }
  else{
    # Organizing the predictors in a matrix. The intercept is removed because it will be added then by the ARIMA model.
    # It is added in a different line because the other syntax is causing problems with the categorical variables.
    X = model.matrix(~ ., data = df[, predictors])
    X = X[, -1]
  }
  
  regArimaErrors = auto.arima(df[,y_col], xreg = X, seasonal = seasonal)
  return(regArimaErrors)
}

# Implementing the dynamic regression with ARIMA errors and comparing it with the simple linear model
# Both graphically and in terms of AICc, the ARIMA helps us with the fit.
  
itPasArBaseline = reg_ARIMA_errors(df=itRailAir, y_col='A_THS_PAS', predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS'))
summary(itPasArBaseline) #little better AICc

plot(itRailAir$A_THS_PAS)
lines(itRailAir$A_THS_PAS)
lines(fitted(itPasBaseline), col=2)
lines(fitted(itPasArBaseline), col=3)

# Before adding new variable, we can try to check if the baseline version could be improved somehow

# Baseline without trend  - Air passengers ----------------------------------------------------------------

# Previous model suggested that the trend is significant but let's try a model without it
itPasBaselineNoTrend = lm(data = itRailAir, A_THS_PAS ~ QUARTER + COVID_AVG_RESTRICTIONS)
summary(itPasBaselineNoTrend)
AICc(itPasBaselineNoTrend) # worse AICc


plot(residuals(itPasBaselineNoTrend))
tsdisplay(residuals(itPasBaselineNoTrend), main = "Italy - Baseline residuals and autocorrelation")

# Also we fit the version with ARIMA errors
itPasArBaselineNoTrend = reg_ARIMA_errors(df=itRailAir, y_col='A_THS_PAS', predictors=c('QUARTER','COVID_AVG_RESTRICTIONS'))
summary(itPasArBaselineNoTrend) # similar to ARIMA with trend

# we compare the two ARIMA versions graphically
plot(itRailAir$A_THS_PAS)
lines(itRailAir$A_THS_PAS)
lines(fitted(itPasArBaseline), col=4)
lines(fitted(itPasArBaselineNoTrend), col=6)

# It seems that the trend does not add anything to our ARIMA model

# Plotting models (final linear model)
## Adding the two lines for the original data and the fitted values
par(mar = c(5, 4, 4, 2) + 0.1)
plot(itRailAir$A_THS_PAS, type = "l", col = "blue", xlab = "TIME", ylab = "Air - Thousands of passengers",
     main = "Italy - Baseline linear model", xaxt = "n")
lines(fitted(itPasBaseline), col = "black",lty='dashed')

## Adding the x tick labels. Filters labels for Q1 (I tried to put the labels horizontally, 
## but it overlaps with the axis label and it's more work to fix it)
q1_labels = itRailAir$TIME[grep("-Q1$", itRailAir$TIME)]
axis(1, at = which(itRailAir$TIME %in% q1_labels), labels = q1_labels, 
     cex.axis = 1, las=1)

# Add a legend
legend("bottomleft", legend = c("Original Data", "Fitted Baseline"), 
       col = c("blue", "black"), lty = c('solid','dashed'))

# Baseline with COVID adjustment - Air passengers----------------------------------------------------------------

# We try with the alternative COVID formulation
# esPasBaselineCovidAdj = lm(data = esRailAir, A_THS_PAS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR)
# 
# ## Graphically, there seems to be an improvement regarding COVID.
# plot(esRailAir$A_THS_PAS)
# lines(esRailAir$A_THS_PAS)
# lines(fitted(esPasBaselineCovidAdj), col=5)

## We can capture the COVID effect more precisely (a coefficient for each "stage" of the COVID restrictions). 
## Also, the AICc is lower in this case even though we added some new features.
# ## This suggests that the adjusted version of the COVID variable is better, both in interpretability and in terms of fit.
# summary(esPasBaselineCovidAdj)
# AICc(esPasBaselineCovidAdj)
# # paste('% difference between AICc: ',
#       round(100*(AICc(esPasBaselineCovidAdj)-AICc(esPasBaselineNoTrend))/AICc(esPasArBaselineNoTrend),2),'%')

## Like in the previous cases, we still have significant autocorrelation for the first lags, so we also fit the dynamic regression with ARIMA errors
# plot(residuals(esPasBaselineCovidAdj))
# tsdisplay(residuals(esPasBaselineCovidAdj))
# 
# # Again, the AICc is reduced in comparison to the other dynamic regressions.
# esPasArBaselineCovidAdj = reg_ARIMA_errors(df=esRailAir, y_col='A_THS_PAS', predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR'))
# summary(esPasArBaselineCovidAdj)

## We plot the baseline model with the simple COVID variable and with the adjusted COVID variable (both without a trend, since it did not show to be significant)
# plot(esRailAir$A_THS_PAS)
# lines(esRailAir$A_THS_PAS)
# lines(fitted(esPasArBaselineNoTrend), col=4)
# lines(fitted(esPasArBaselineCovidAdj), col=6)

# As a result, we can use this last version (a model without trend and with the COVID adjustment) as 
# the "final version" of the baseline model. We can start adding variables on top of that.
# esPasBaselineFinal = esPasBaselineCovidAdj
# esPasArBaselineFinal = esPasArBaselineCovidAdj
itPasBaselineFinal = itPasBaselineNoTrend
itPasArBaselineFinal = itPasArBaselineNoTrend


#### Adding non-seasonally-adjusted predictors (approach including economic activity and other variables) ####
# We can now start adding predictors to the resulting baseline model.

# We can see what happens if we use a Lasso regression as an automatic feature selector
reg_lasso <- function(df, y_col, predictors) {
  X = model.matrix(~ ., data = df[, predictors])
  X = X[, -1]
  y = df[, y_col]
  
  # Setting a seed for reproducibility and creating the lambda grid
  set.seed(123)
  grid = 10^seq(10, -2, length=100)
  
  # Running the Lasso regression and printing its results
  # alpha = 1 is for a Lasso regression. As default it uses nfold=10 
  # and type.measure='default' (which corresponds to deviance as the loss), but I put them explicitly just for us to be aware of it
  lrLasso = cv.glmnet(X, y,
                      alpha = 1, nfold=10, type.measure='deviance',lambda = grid)
  plot(lrLasso)
  bestLamLasso = lrLasso$lambda.min
  lasso_coef = predict(lrLasso,type='coefficients',s=bestLamLasso)[,]
  print(lasso_coef)
  
  # Returning only the coefficients that are not 0
  lasso_coef = lasso_coef[lasso_coef!=0]
  return(lasso_coef)
}


# Lasso - Air passengers - Non seasonally adjusted  ----------------------------------------------------------------
# Getting the coefficients of the best Lasso and passing them to a linear model to see their significance
# We can pass all the possible predictors
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
itPasLassoCoef = reg_lasso(df=itRailAir, y_col='A_THS_PAS', 
                           predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM','R_THS_PAS',
                                        'GDP_PC','AIR_PRICE','RAIL_PRICE','RAIL_DENSITY','TOURISM_OCC'))
# For Italy, don't eliminate anything

# TODO: this must be done manually according to what is obtained from the reg_lasso function.
itPasLasso = lm(data = itRailAir, A_THS_PAS ~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + 
                  R_MIO_PKM + R_THS_PAS + GDP_PC + AIR_PRICE + RAIL_PRICE + 
                  RAIL_DENSITY + TOURISM_OCC)
summary(itPasLasso)
AICc(itPasLasso) # better
vif(itPasLasso)

## We do not have such a strong autocorrelation for the first lags, but we also fit the dynamic regression with ARIMA errors
plot(residuals(itPasLasso))
tsdisplay(residuals(itPasLasso))

# The new predictors did reduce the AICc in comparison to the other dynamic regressions.
itPasArLasso = reg_ARIMA_errors(df=itRailAir, y_col='A_THS_PAS', 
                                predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR',
                                             'R_MIO_PKM','R_THS_PAS','GDP_PC','AIR_PRICE','RAIL_PRICE',
                                             'RAIL_DENSITY','TOURISM_OCC'))
summary(itPasArLasso) # AICc 487


# Graphically (with respect to the final baseline model), we do a better job at capturing the
# initial impact of COVID and other subtle changes along the series
plot(itRailAir$A_THS_PAS)
lines(itRailAir$A_THS_PAS)
lines(fitted(itPasArBaselineFinal), col=4)
lines(fitted(itPasArLasso), col=6) #much better at fitting with COVID



# Best subset selection - Air passengers - Non seasonally adjusted ----------------------------------------------------------------

# Given that we do not have many predictors, we can perform a best subset selection
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
itPasBestObject = regsubsets(A_THS_PAS~., data=itRailAir[,c('A_THS_PAS','TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM','R_THS_PAS',
                                                            'GDP_PC','AIR_PRICE','RAIL_PRICE','RAIL_DENSITY','TOURISM_OCC')])


# From the Statistical Learning scripts:
# The regsubsets() function has a built-in plot() command  which can be used to display the selected variables for 
# the best model with a given number of predictors, ranked according to a chosen statistic. The top row of each plot 
# contains a black square for each variable selected according to the optimal model associated with that statistic.
plot(itPasBestObject,scale="adjr2")
plot(itPasBestObject,scale="Cp")
plot(itPasBestObject,scale="bic")

# We can obtain the desired the best model by fitting the regression with the coefficients resulting from the plots
# TODO: this must be done manually according to what is obtained from the plots above.
# esPasBest = lm(data = esRailAir, A_THS_PAS ~  QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + R_MIO_PKM + GDP_PC + RAIL_DENSITY)
# summary(esPasBest)
# AICc(esPasBest)

# I add the predictors this way (with a model matrix) in order to exclude the categorical dummies that are not significant
matrix = model.matrix(~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR +
                 GDP_PC + RAIL_PRICE + RAIL_DENSITY + TOURISM_OCC, data = itRailAir)
exclude_vars <- c('(Intercept)', 'QUARTER3', 'COVID_AVG_RESTRICTIONS_FACTOR1')
matrix <- matrix[, !colnames(matrix) %in% exclude_vars]

itPasBest <- lm(A_THS_PAS ~ ., data = cbind(itRailAir["A_THS_PAS"], matrix)) 
# difference from previous is no r_mio_pkm tourism and no covid factor 1 (bic)
# r_mio_pkm maybe explained by R_THS_PAS or wtv
# covid makes sense because there were only recommendations
summary(itPasBest)
AICc(itPasBest)
vif(itPasBest)

## It is practically the same model, except from the RAIL_PRICE variable, which is added by the Lasso model, 
## but is not really significant. Still, we plot the results with the dynamic regression
# plot(residuals(esPasBest))
# tsdisplay(residuals(esPasBest))
plot(residuals(itPasBest))
par(mar = c(5, 4, 4, 2) + 0.1)
tsdisplay(residuals(itPasBest))

print(durbinWatsonTest(itPasBest))

itPasArBest = reg_ARIMA_errors(df=itRailAir, y_col='A_THS_PAS', 
                               predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR',
                                            'GDP_PC','RAIL_PRICE','RAIL_DENSITY','TOURISM_OCC'))
summary(itPasArBest)

plot(itRailAir$A_THS_PAS)
lines(itRailAir$A_THS_PAS)
lines(fitted(itPasArLasso), col=4) 
lines(fitted(itPasArBest), col=6)

#### Adding seasonally adjusted predictors (approach including economic activity and other variables) ####

# Lasso - Air passengers - Seasonally adjusted ----------------------------------------------------------------
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
# Getting the coefficients of the best Lasso and passing them to a linear model to see their significance
# We can pass all the possible predictors
itPasLassoCoefSeasAdj = reg_lasso(df=itRailAir, y_col='A_THS_PAS', 
                                  predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM_S_ADJ','R_THS_PAS_S_ADJ',
                                               'GDP_PC','AIR_PRICE_S_ADJ','RAIL_PRICE_S_ADJ','RAIL_DENSITY','TOURISM_OCC_S_ADJ'))
# Remove R_MIO_PKM_S_ADJ

# TODO: this must be done manually according to what is obtained from the reg_lasso function.
# In this case, the resulting variables are really similar to what we saw with the non-seasonally adjusted versions
itPasLassoSeasAdj = lm(data = itRailAir, A_THS_PAS ~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + R_MIO_PKM_S_ADJ +
                         R_THS_PAS_S_ADJ + GDP_PC + AIR_PRICE_S_ADJ + RAIL_PRICE_S_ADJ + RAIL_DENSITY +
                         TOURISM_OCC_S_ADJ)
summary(itPasLassoSeasAdj)
AICc(itPasLassoSeasAdj) 
vif(itPasLassoSeasAdj)


## Again, we do not have such a strong autocorrelation for the first lags, but we still need to fit the dynamic regression with ARIMA errors
plot(residuals(itPasLassoSeasAdj))
par(mar = c(5, 4, 4, 2) + 0.1)
tsdisplay(residuals(itPasLassoSeasAdj))

print(durbinWatsonTest(itPasLassoSeasAdj))

itPasArLassoSeasAdj = reg_ARIMA_errors(df=itRailAir, y_col='A_THS_PAS',
                                       predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM_S_ADJ',
                                                    'R_THS_PAS_S_ADJ','GDP_PC','AIR_PRICE_S_ADJ','RAIL_PRICE_S_ADJ',
                                                    'RAIL_DENSITY','TOURISM_OCC_S_ADJ'))
summary(itPasArLassoSeasAdj) #AICc 494
plot(residuals(itPasArLassoSeasAdj))
tsdisplay(residuals(itPasArLassoSeasAdj))


# Graphically (with respect to the final baseline model), we do a better job at capturing the
# initial impact of COVID
plot(itRailAir$A_THS_PAS)
lines(itRailAir$A_THS_PAS)
lines(fitted(itPasArBaselineFinal), col=4)
lines(fitted(itPasArLassoSeasAdj), col=6)


#  Best subset selection - Air passengers - Seasonally adjusted ----------------------------------------------------------------

# Given that we do not have many predictors, we can perform a best subset selection
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
itPasBestObjectSeasAdj = regsubsets(A_THS_PAS~., data=itRailAir[,c('A_THS_PAS','TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM_S_ADJ','R_THS_PAS_S_ADJ',
                                                                   'GDP_PC','AIR_PRICE_S_ADJ','RAIL_PRICE_S_ADJ','RAIL_DENSITY','TOURISM_OCC_S_ADJ')])


# From the Statistical Learning scripts:
# The regsubsets() function has a built-in plot() command  which can be used to display the selected variables for 
# the best model with a given number of predictors, ranked according to a chosen statistic. The top row of each plot 
# contains a black square for each variable selected according to the optimal model associated with that statistic.
plot(itPasBestObjectSeasAdj,scale="adjr2")
plot(itPasBestObjectSeasAdj,scale="Cp")
plot(itPasBestObjectSeasAdj,scale="bic")

# We can obtain the desired the best model by fitting the regression with the coefficients resulting from the plots
# Two of the criteria include R_MIO_KM, while the other one does not. I include it because of the criteria of the majority
# TODO: this must be done manually according to what is obtained from the plots above.

matrix = model.matrix(~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + 
                        GDP_PC + RAIL_DENSITY + TOURISM_OCC_S_ADJ, data = itRailAir)
exclude_vars = c('(Intercept)','COVID_AVG_RESTRICTIONS_FACTOR1')
matrix <- matrix[, !colnames(matrix) %in% exclude_vars]

itPasBestSeasAdj = lm(data = cbind(itRailAir["A_THS_PAS"], matrix), A_THS_PAS ~ .)
summary(itPasBestSeasAdj)
AICc(itPasBestSeasAdj)
vif(itPasBestSeasAdj)

matrix = model.matrix(~  QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + 
                        GDP_PC + RAIL_DENSITY + TOURISM_OCC_S_ADJ, data = itRailAir)
exclude_vars = c('(Intercept)','COVID_AVG_RESTRICTIONS_FACTOR1')
matrix <- matrix[, !colnames(matrix) %in% exclude_vars]

itPasBestSeasAdj2 = lm(data = cbind(itRailAir["A_THS_PAS"], matrix), A_THS_PAS ~ .)
summary(itPasBestSeasAdj2)
AICc(itPasBestSeasAdj2)
vif(itPasBestSeasAdj2)

plot(residuals(itPasBestSeasAdj))
par(mar = c(5, 4, 4, 2) + 0.1)
tsdisplay(residuals(itPasLassoSeasAdj))

print(durbinWatsonTest(itPasLassoSeasAdj))

## This is the same model as the one obtained with Lasso, so I only do one plot (the one from the Lasso above)
itPasArBestSeasAdj = reg_ARIMA_errors(df=itRailAir, y_col='A_THS_PAS', 
                                      predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR',
                                                   'GDP_PC','RAIL_DENSITY','TOURISM_OCC_S_ADJ'))
summary(itPasArBestSeasAdj) #AICc 504


# Plotting models (final linear model)
## Adding the two lines for the original data and the fitted values
par(mar = c(5, 4, 4, 2) + 0.1)
plot(itRailAir$A_THS_PAS, type = "l", col = "blue", xlab = "TIME", ylab = "Air - Thousands of passengers",
     main = "Italy - Final model with ARIMA(0,0,1) errors", xaxt = "n")
lines(fitted(itPasArBestSeasAdj), col = "black",lty='dashed')

## Adding the x tick labels. Filters labels for Q1 (I tried to put the labels horizontally, 
## but it overlaps with the axis label and it's more work to fix it)
q1_labels = itRailAir$TIME[grep("-Q1$", itRailAir$TIME)]
axis(1, at = which(itRailAir$TIME %in% q1_labels), labels = q1_labels, 
     cex.axis = 1, las=1)

# Add a legend
legend("bottomleft", legend = c("Original Data", "Fitted ALM"), 
       col = c("blue", "black"), lty = c('solid','dashed'))


tsdisplay(residuals(itPasBestSeasAdj), main = "Italy - Final linear model - residuals and autocorrelation")
tsdisplay(residuals(itPasArBestSeasAdj), main = "Italy - Final model with ARIMA(0,0,1) errors - residuals and autocorrelation")

# Extract residuals from ARIMA model
residuals <- residuals(itPasArBestSeasAdj)

# Perform Durbin-Watson test on residuals
durbinWatsonTest(residuals)

# Summary - Air passengers ---------------------------------------------------------------------------------------
# I re-print all the results just to be clear of what happened without browsing above.
# It is the results for the linear model and the dynamic regression with ARIMA errors 
# of the Lasso and the best subset selection approaches

# I also check for collinearity just in case


# With non-seasonally adjusted variables
summary(itPasLasso) # R2 0.97
AICc(itPasLasso) # 503
summary(itPasArLasso) # aicc 486
vif(itPasLasso) 

quarter_dummies <- model.matrix(~ QUARTER - 1, data = itRailAir)
vars_of_interest <- c("TREND", "R_MIO_PKM", "R_THS_PAS", "RAIL_PRICE", "TOURISM_OCC")
correlation_matrix <- cor(cbind(quarter_dummies, itRailAir[, vars_of_interest]))
print(correlation_matrix)
#remove R_THS_PAS and TOURISM and RAIL_PRICE

correlation_df <- as.data.frame(as.table(correlation_matrix))
names(correlation_df) <- c("Variable1", "Variable2", "Correlation")

heatmap_plot <- ggplot(correlation_df, aes(Variable1, factor(Variable2, levels = rev(levels(factor(Variable2)))), fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       limits = c(-1, 1), name = "Correlation") +
  labs(title = "Correlation Matrix Heatmap",
       x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(heatmap_plot)




summary(itPasBest) # r2 0.95
AICc(itPasBest) # 528
summary(itPasArBest) # aicc 502
vif(itPasBest) # collinearity...
# remove tourism and rail_price


# With seasonally adjusted variables
summary(itPasLassoSeasAdj) # 0.96
AICc(itPasLassoSeasAdj) #516
summary(itPasArLassoSeasAdj) #aicc 476
vif(itPasLassoSeasAdj) # collinearity r_ths_pas,...


quarter_dummies <- model.matrix(~ COVID_AVG_RESTRICTIONS_FACTOR - 1, data = itRailAir)
vars_of_interest <- c("TREND", "R_MIO_PKM_S_ADJ", "RAIL_PRICE_S_ADJ", "TOURISM_OCC_S_ADJ")
correlation_matrix <- cor(cbind(quarter_dummies, itRailAir[, vars_of_interest]))
print(correlation_matrix)
#remove R_THS_PAS and TOURISM and RAIL_PRICE

correlation_df <- as.data.frame(as.table(correlation_matrix))
names(correlation_df) <- c("Variable1", "Variable2", "Correlation")

heatmap_plot <- ggplot(correlation_df, aes(Variable1, factor(Variable2, levels = rev(levels(factor(Variable2)))), fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       limits = c(-1, 1), name = "Correlation") +
  labs(title = "Correlation Matrix Heatmap",
       x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(heatmap_plot)


summary(itPasBestSeasAdj) #r2 0.95
AICc(itPasBestSeasAdj)# 518
summary(itPasArBestSeasAdj)# aicc 486 #best
vif(itPasBestSeasAdj)# no collinearity 



plot(itRailAir$A_THS_PAS)
lines(itRailAir$A_THS_PAS)
lines(fitted(itPasArBestSeasAdj), col=4) 
lines(fitted(itPasArLassoSeasAdj), col=6)



# IT seems that there might be a collinearity issue.
# I'll remove the least significant variable just to test what happens
# Remove R_THS_PAS and TOURISM and RAIL_PRICE
itPasLassoVIF = lm(data = itRailAir, A_THS_PAS ~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + R_MIO_PKM + 
                            GDP_PC + AIR_PRICE + RAIL_DENSITY)
itPasArLassoVIF = reg_ARIMA_errors(df=itRailAir, y_col='A_THS_PAS',
                                          predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM',
                                                       'GDP_PC','AIR_PRICE','RAIL_DENSITY'))

summary(itPasLassoVIF) # 0.95 R2
AICc(itPasLassoVIF)# 575
summary(itPasArLassoVIF) # AICC 509
vif(itPasLassoVIF) # no collinearity


# Now itPasBestVIF
itPasBestVIF = lm(data = itRailAir, A_THS_PAS ~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + 
                     GDP_PC + RAIL_DENSITY)
itPasArBestVIF = reg_ARIMA_errors(df=itRailAir, y_col='A_THS_PAS',
                                   predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR',
                                                'GDP_PC','RAIL_DENSITY'))

summary(itPasBestVIF) # 0.95 R2
AICc(itPasBestVIF)# 575
summary(itPasArBestVIF) # AICC 509
vif(itPasBestVIF) # no collinearity


# Now for itPasLassoSeasAdj collinearity problems
# in Italy case: remove R_THS_PAS_S_ADJ, RAIL_PRICE_S_ADJ, TOURISM
itPasLassoSeasAdjVIF = lm(data = itRailAir, A_THS_PAS ~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + 
                            R_MIO_PKM_S_ADJ + GDP_PC + AIR_PRICE_S_ADJ +
                            RAIL_DENSITY)
itPasArLassoSeasAdjVIF = reg_ARIMA_errors(df=itRailAir, y_col='A_THS_PAS',
                                         predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM_S_ADJ',
                                                      'GDP_PC','AIR_PRICE_S_ADJ','RAIL_DENSITY'))

summary(itPasLassoSeasAdjVIF) # 0.96 R2
AICc(itPasLassoSeasAdjVIF)# 525
summary(itPasArLassoSeasAdjVIF) # AICC 500
vif(itPasLassoSeasAdjVIF) # no collinearity


# So, the plot of the baseline model and the final model looks like this
plot(itRailAir$A_THS_PAS)
lines(itRailAir$A_THS_PAS)
lines(fitted(itPasArBaseline), col=4)
lines(fitted(itPasArBestSeasAdj), col=6)

summary(itPasBestSeasAdj)
AICc(itPasBestSeasAdj)
summary(itPasArBestSeasAdj)


# Plotting models (final linear model)
## Adding the two lines for the original data and the fitted values
par(mar = c(5, 4, 4, 2) + 0.1)
plot(itRailAir$A_THS_PAS, type = "l", col = "blue", xlab = "TIME", ylab = "Air - Thousands of passengers",
     main = "Italy - Final linear model", xaxt = "n")
lines(fitted(itPasBestSeasAdj), col = "black",lty='dashed')

## Adding the x tick labels. Filters labels for Q1 (I tried to put the labels horizontally, 
## but it overlaps with the axis label and it's more work to fix it)
q1_labels = itRailAir$TIME[grep("-Q1$", itRailAir$TIME)]
axis(1, at = which(itRailAir$TIME %in% q1_labels), labels = q1_labels, 
     cex.axis = 1, las=1)

# Add a legend
legend("bottomleft", legend = c("Original Data", "Fitted LM"), 
       col = c("blue", "black"), lty = c('solid','dashed'))



# Insights for air passengers:
# - The effect of the rail on the air traffic does not seem to be strong.
#   R_MIO_PKM_S_ADJ might me a little significant, but brings some collinearity issues and could be omitted.
#   In any case, if it were significant, it would have the same sign as the air passengers, 
#   maybe just giving a hint of "willingness to travel far", but I would not sell this idea strongly
# - These series are dominated by the economic activity (GDP_PC) and the seasonality. There does not seem to be a trend.
#   Just with these variables it seems to be enough to get a good fit and other variables such as 
#   the prices are not that important in terms of travelling (which goes in line with the paper I told you about, but still).
#   Rail density seems to be significant, but after doing the process with the other target variables,
#   I am not sure of how it works (also since the rail traffic does not seem to matter that much).
#   This might be a spurious correlation or it could casually coincide with some variable that is 
#   not taken into account in here, so we might need to decide how to approach the explanation for this variable.




#### Models for rail passengers ####

# Baseline - Rail passengers ----------------------------------------------------------------



# We start with a "baseline" model: a linear regression with just trend, season and the COVID variable. 
# In this case we do it directly with the COVID variable from Our world in Data
itRailPasBaseline = lm(data = itRailAir, R_THS_PAS ~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR)
itRailPasBaselineIntervention =  lm(data = itRailAir, R_THS_PAS ~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + Intervention)

## Graphically, it seems we can do better, but at least we are capturing some time-dependent patterns, specially seasonality.
## We also capture some of the COVID effect (but maybe this can be improved).
## There does not seem to be a strong trend in this case.
plot(itRailAir$R_THS_PAS)
lines(itRailAir$R_THS_PAS)
lines(fitted(itRailPasBaseline), col=2)
lines(fitted(itRailPasBaselineIntervention), col=4)

## This is the summary of the significance and the goodness of fit of this linear model.
## In tis case, it seems that the trend is significant
summary(itRailPasBaseline)
AICc(itRailPasBaseline)
summary(itRailPasBaselineIntervention)
AICc(itRailPasBaselineIntervention)

## We do the dynamic regression with ARIMA errors procedure
plot(residuals(itRailPasBaseline))
tsdisplay(residuals(itRailPasBaseline))
plot(residuals(itRailPasBaselineIntervention))
tsdisplay(residuals(itRailPasBaselineIntervention))

itRailPasArBaseline = reg_ARIMA_errors(df=itRailAir, y_col='R_THS_PAS', predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR'))
summary(itRailPasArBaseline)

plot(itRailAir$R_THS_PAS)
lines(itRailAir$R_THS_PAS)
lines(fitted(itRailPasBaseline), col=2)
lines(fitted(itRailPasArBaseline), col=3)



#### Adding non-seasonally-adjusted predictors (approach including economic activity and other variables) ####
# We can now start adding predictors to the resulting baseline model.

# Lasso - Rail passengers  - Non seasonally adjusted ----------------------------------------------------------------
# Getting the coefficients of the best Lasso and passing them to a linear model to see their significance
# We can pass all the possible predictors
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
itRailPasLassoCoef = reg_lasso(df=itRailAir, y_col='R_THS_PAS', 
                               predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR',
                                            'GDP_PC','AIR_PRICE','RAIL_PRICE','RAIL_DENSITY','TOURISM_OCC','Intervention'))

# TODO: this must be done manually according to what is obtained from the reg_lasso function.
# All columns show an "expected" behavior except from RAIL_DENSITY. Let's see what the complete procedure yields

itRailPasLasso = lm(data = itRailAir, R_THS_PAS ~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + GDP_PC + 
                      AIR_PRICE + RAIL_DENSITY + TOURISM_OCC + Intervention)
summary(itRailPasLasso)
AICc(itRailPasLasso)

## We do the Dynamic regression procedure
plot(residuals(itRailPasLasso))
tsdisplay(residuals(itRailPasLasso))

itRailPasArLasso = reg_ARIMA_errors(df=itRailAir, y_col='R_THS_PAS', 
                                    predictors=c('TREND', 'QUARTER', 'COVID_AVG_RESTRICTIONS_FACTOR', 
                                    'GDP_PC','AIR_PRICE','RAIL_PRICE','RAIL_DENSITY','TOURISM_OCC','Intervention'))
summary(itRailPasArLasso)


# Graphically (with respect to the final baseline model), we do a better job at capturing the
# initial impact of COVID and other subtle changes along the series
plot(itRailAir$R_THS_PAS)
lines(itRailAir$R_THS_PAS)
lines(fitted(itRailPasArBaseline), col=4)
lines(fitted(itRailPasArLasso), col=6)



#  Best subset selection - Rail passengers - Non seasonally adjusted ----------------------------------------------------------------

# Given that we do not have many predictors, we can perform a best subset selection
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
itRailPasBestObject = regsubsets(R_THS_PAS~., data=itRailAir[,c('R_THS_PAS','TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR',
                                                                'GDP_PC','AIR_PRICE','RAIL_PRICE','RAIL_DENSITY','TOURISM_OCC','Intervention')])


# From the Statistical Learning scripts:
# The regsubsets() function has a built-in plot() command  which can be used to display the selected variables for 
# the best model with a given number of predictors, ranked according to a chosen statistic. The top row of each plot 
# contains a black square for each variable selected according to the optimal model associated with that statistic.
plot(itRailPasBestObject,scale="adjr2")
plot(itRailPasBestObject,scale="Cp")
plot(itRailPasBestObject,scale="bic")

# We can obtain the desired the best model by fitting the regression with the coefficients resulting from the plots
# TODO: this must be done manually according to what is obtained from the plots above.

# I add the predictors this way (with a model matrix) in order to exclude the categorical dummies that are not significant
# matrix = model.matrix(~ QUARTER + RAIL_DENSITY + TOURISM_OCC + Intervention, data = itRailAir)
# exclude_vars = c('(Intercept)','COVID_AVG_RESTRICTIONS_FACTOR2', 'QUARTER4')
# matrix <- matrix[, !colnames(matrix) %in% exclude_vars]


# In this case, apart from the strange behavior in the RAIL_PRICE, we get a 
# strange behavior regarding TOURISM_OCC (the more tourist the less flights?)
# We can move on to check what the other approaches yield 
# itRailPasBest = lm(data = cbind(itRailAir["R_THS_PAS"], matrix), R_THS_PAS ~ .)
itRailPasBest = lm(data = itRailAir, R_THS_PAS ~ TREND + QUARTER + RAIL_DENSITY + TOURISM_OCC + Intervention)
summary(itRailPasBest)
AICc(itRailPasBest)

## Dynamic regression procedure
plot(residuals(itRailPasBest))
par(mar = c(5, 4, 4, 2) + 0.1)
tsdisplay(residuals(itRailPasBest))

itRailPasArBest = reg_ARIMA_errors(df=itRailAir, y_col='R_THS_PAS', 
                                   predictors=c('TREND','QUARTER','RAIL_DENSITY','TOURISM_OCC','Intervention'))
summary(itRailPasArBest)

# In terms of fit, both models are really similar and satisfactory. 
# However, the interpretation part is still confounding
plot(itRailAir$R_THS_PAS)
lines(itRailAir$R_THS_PAS)
lines(fitted(itRailPasArLasso), col=4)
lines(fitted(itRailPasArBest), col=6)

#### Adding seasonally adjusted predictors (approach including economic activity and other variables) ####

# Lasso - Rail passengers - Seasonally adjusted ----------------------------------------------------------------
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
# Getting the coefficients of the best Lasso and passing them to a linear model to see their significance
# We can pass all the possible predictors
itRailPasLassoCoefSeasAdj = reg_lasso(df=itRailAir, y_col='R_THS_PAS', 
                                      predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR',
                                                   'GDP_PC','AIR_PRICE_S_ADJ','RAIL_PRICE_S_ADJ','RAIL_DENSITY','TOURISM_OCC_S_ADJ','Intervention'))

# TODO: this must be done manually according to what is obtained from the reg_lasso function.
matrix = model.matrix(~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + GDP_PC + RAIL_DENSITY + TOURISM_OCC_S_ADJ + Intervention, data = itRailAir)
exclude_vars = c('(Intercept)','COVID_AVG_RESTRICTIONS_FACTOR2')
matrix <- matrix[, !colnames(matrix) %in% exclude_vars]


itRailPasLassoSeasAdj = lm(data = cbind(itRailAir["R_THS_PAS"],matrix), R_THS_PAS ~ .)
summary(itRailPasLassoSeasAdj)
AICc(itRailPasLassoSeasAdj)

## Dynamic regression
plot(residuals(itRailPasLassoSeasAdj))
tsdisplay(residuals(itRailPasLassoSeasAdj))

itRailPasArLassoSeasAdj = reg_ARIMA_errors(df=itRailAir, y_col='R_THS_PAS',
                                           predictors=matrix)
summary(itRailPasArLassoSeasAdj)


# Graphical results
plot(itRailAir$R_THS_PAS)
lines(itRailAir$R_THS_PAS)
lines(fitted(itRailPasArBaseline), col=4)
lines(fitted(itRailPasArLassoSeasAdj), col=6)



# Best subset selection - Rail passengers - Seasonally adjusted  ----------------------------------------------------------------

# Given that we do not have many predictors, we can perform a best subset selection
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
itRailPasBestObjectSeasAdj = regsubsets(R_THS_PAS~., data=itRailAir[,c('R_THS_PAS','TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR',
                                                                       'GDP_PC','AIR_PRICE_S_ADJ','RAIL_PRICE_S_ADJ','RAIL_DENSITY','TOURISM_OCC_S_ADJ','Intervention')])


# From the Statistical Learning scripts:
# The regsubsets() function has a built-in plot() command  which can be used to display the selected variables for 
# the best model with a given number of predictors, ranked according to a chosen statistic. The top row of each plot 
# contains a black square for each variable selected according to the optimal model associated with that statistic.
plot(itRailPasBestObjectSeasAdj,scale="adjr2")
plot(itRailPasBestObjectSeasAdj,scale="Cp")
plot(itRailPasBestObjectSeasAdj,scale="bic")

# We can obtain the desired the best model by fitting the regression with the coefficients resulting from the plots
# All criteria consider the same variables for the best model
# TODO: this must be done manually according to what is obtained from the plots above.

# I add the predictors this way (with a model matrix) in order to exclude the categorical dummies that are not significant
matrix = model.matrix(~ TREND + QUARTER + GDP_PC + RAIL_DENSITY + TOURISM_OCC_S_ADJ + Intervention, data = itRailAir)
exclude_vars = c('(Intercept)','QUARTERQ4')
matrix <- matrix[, !colnames(matrix) %in% exclude_vars]

# The model still carries the strange behavior from RAIL_PRICE_S_ADJ and TOURISM_OCC_S_ADJ 
itRailPasBestSeasAdj = lm(data = cbind(itRailAir["R_THS_PAS"],matrix), R_THS_PAS ~ .)
summary(itRailPasBestSeasAdj)
AICc(itRailPasBestSeasAdj)

## Dynamic regression procedure
plot(residuals(itRailPasBestSeasAdj))
par(mar = c(5, 4, 4, 2) + 0.1)
tsdisplay(residuals(itRailPasBestSeasAdj))

itRailPasArBestSeasAdj = reg_ARIMA_errors(df=itRailAir, y_col='R_THS_PAS',
                                          predictors=matrix)
summary(itRailPasArBestSeasAdj)

plot(itRailAir$R_THS_PAS)
lines(itRailAir$R_THS_PAS)
lines(fitted(itRailPasArBaseline), col=4)
lines(fitted(itRailPasArBestSeasAdj), col=6)


# Summary - Rail passengers ---------------------------------------------------------------------------------------
# I re-print all the results just to be clear of what happened without browsing above.
# It is the results for the linear model and the dynamic regression with ARIMA errors 
# of the Lasso and the best subset selection approaches

# I also check for collinearity just in case


# With non-seasonally adjusted variables
summary(itRailPasLasso) # r2 0.925
AICc(itRailPasLasso) # 1064
summary(itRailPasArLasso)# aicc 1028
vif(itRailPasLasso) #collinearity trend, quarter and tourism
#check correlations
quarter_dummies <- model.matrix(~ QUARTER - 1, data = itRailAir)
cor(cbind(itRailAir$TOURISM_OCC, quarter_dummies)) 
#high correlation between tourism and quarter 3
# remove tourism and trend

summary(itRailPasBest)#r2 = 0.92
AICc(itRailPasBest)#1058
summary(itRailPasArBest)# aicc 1046
vif(itRailPasBest) # no collinearity


# With seasonally adjusted variables
summary(itRailPasLassoSeasAdj)#r2 0.93
AICc(itRailPasLassoSeasAdj)# 1050
summary(itRailPasArLassoSeasAdj)#aicc 1018 #best 
vif(itRailPasLassoSeasAdj)# no collinearity

summary(itRailPasBestSeasAdj) #r2 0.93
AICc(itRailPasBestSeasAdj)# 1047
summary(itRailPasArBestSeasAdj) #aicc 1022
vif(itRailPasBestSeasAdj) # no collinearity


# Once again, there are collinearity issues.
# In this case, as much as I tried tweaking the variables, the collinearity persisted. 
itRailPasLassoVIF = lm(data = itRailAir, R_THS_PAS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + GDP_PC + AIR_PRICE + RAIL_DENSITY+ Intervention)
vif(itRailPasLassoVIF)

summary(itRailPasLassoVIF) # r2 0.849
AICc(itRailPasLassoVIF) # 1113

itRailPasArLassoVIF = reg_ARIMA_errors(df=itRailAir, y_col='R_THS_PAS', 
                                    predictors=c('QUARTER', 'COVID_AVG_RESTRICTIONS_FACTOR', 
                                                 'GDP_PC','AIR_PRICE','RAIL_DENSITY','Intervention'))
summary(itRailPasArLassoVIF) # aicc 1039


plot(itRailAir$R_THS_PAS)
lines(itRailAir$R_THS_PAS)
lines(fitted(itRailPasArLassoSeasAdj), col=6)

# I think that these models for the rail passengers are wrong and not useful.
# In the end, I did try to search for variables to model the air passengers, not the rail passengers,
# so I think it might be better to omit this part for the presentation


# Final Insights -----------------------------------------------------------------------------------
# I think that the air passengers models ara okay in my case, 
# The summary of that section should give all the information needed, I think.


# TODO:
# 1. Run the models for the air passengers and maybe the other two setries (A_FLIGHTS and R_THS_PAS)
# For me, specially the one for rail passengers, does not really make sense, I think. So I thing it would be fine to omit it
# 2. The dependent variable vs predictors plots


# Steps for 1. 
# 1.1 Fit the models with the Lasso and the best subset selection approaches.
# 1.2 Maybe try with the seasonally adjusted and the non-seasonally adjusted versions of the columns.
# In my case, there was not such a big difference, but still. I chose to stick with the seasonally adjusted versions in the end just for consistency
# 1.3. In the summary, check for collinearity and maybe try to manually improve the models regarding the interpretation

# Note: sometimes I fit the models with a matrix to skip not significant categorical dummies.
# In case it is confusing, search in this code for "mode.matrix" and there are some examples

