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

setwd("C:/Camilo/Estudio/Padova/Business, Economic and Financial Data/BEFD-project")
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

## Obtaining the GDP per capita for each period. The unit will be thousands of euro
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
esRailAir = esRailAir %>%
  mutate(COVID = as.integer(TIME >= "2020-Q1" & TIME <= "2021-Q2"))

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
esRailAir = esRailAir %>%
  mutate(COVID_ADJ = ifelse(TIME == "2020-Q2", 'Strong',
                            ifelse((TIME >= "2020-Q4" & TIME <= "2021-Q1"), 'Medium',
                                   ifelse((TIME == "2020-Q1" | TIME == "2020-Q3" | TIME == "2021-Q2"), 'Low', 'No'))))
esRailAir$COVID_ADJ = factor(esRailAir$COVID_ADJ)
esRailAir$COVID_ADJ = relevel(esRailAir$COVID_ADJ, ref = 'No')


# Adding the intervention variable for the train discount
esRailAir = esRailAir %>%
  mutate(TRAIN_DISCOUNT = as.integer(TIME >= "2022-Q3"))

deRailAir = deRailAir %>%
  mutate(TRAIN_DISCOUNT = as.integer((TIME == "2022-Q3") | (TIME >= "2023-Q2")))


# TODO: check that all variables are included
# Converting into time-series objects in case it is needed
esRailAirTS = ts(esRailAir[, c("R_MIO_PKM","R_THS_PAS", "A_FLIGHTS", "A_THS_PAS", 
                               "POPULATION", "COVID", "COVID_ADJ", "COVID_AVG_RESTRICTIONS","TRAIN_DISCOUNT",
                               "GDP", "GDP_PC", "RAIL_PRICE", "AIR_PRICE", "DOM_AIR_PRICE", 
                               "RAIL_DENSITY", "TOURISM_OCC", 
                               "QUARTER","TREND")],
                 start = c(2004, 1),
                 frequency = 4)

#deRailAirTS = ts(deRailAir[, c("R_MIO_PKM","R_THS_PAS", "A_FLIGHTS", "A_THS_PAS", 
#                               "POPULATION", "COVID", "COVID_ADJ", "COVID_AVG_RESTRICTIONS","TRAIN_DISCOUNT",
#                               "GDP", "GDP_PC", "RAIL_PRICE", "AIR_PRICE", "DOM_AIR_PRICE", 
#                               "RAIL_DENSITY", "TOURISM_OCC", 
#                               "QUARTER","TREND")],
#                 start = c(2004, 1),
#                 frequency = 4)

#itRailAirTS = ts(itRailAir[, c("R_MIO_PKM","R_THS_PAS", "A_FLIGHTS", "A_THS_PAS", 
#                               "POPULATION", "COVID", "COVID_ADJ", "COVID_AVG_RESTRICTIONS","TRAIN_DISCOUNT",
#                               "GDP", "GDP_PC", "RAIL_PRICE", "AIR_PRICE", "DOM_AIR_PRICE", 
#                               "RAIL_DENSITY", "TOURISM_OCC", 
#                               "QUARTER","TREND")],
#                 start = c(2004, 1),
#                 frequency = 4)


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
  print(autoplot(ts_data[, "R_THS_PAS"], series="Original \ndata") +
          autolayer(R_THS_PAS_S_ADJ, series="Seasonally \nadjusted") +
          xlab("Time") + ylab("Thousands of passengers") +
          ggtitle("Rail - Passengers per million inhabitants") +
          scale_colour_manual(values=c("darkgray","blue"),
                        breaks=c("Original \ndata","Seasonally \nadjusted")) +
          theme_minimal())
  
  print(autoplot(ts_data[, "R_MIO_PKM"], series="Original \ndata") +
          autolayer(R_MIO_PKM_S_ADJ, series="Seasonally \nadjusted") +
          xlab("Time") + ylab("Millions of passenger-km") +
          ggtitle("Rail - Passenger-km per million inhabitants") +
          scale_colour_manual(values=c("darkgray","blue"),
                        breaks=c("Original \ndata","Seasonally \nadjusted")) +
          theme_minimal())
  
  print(autoplot(ts_data[, "RAIL_PRICE"], series="Original \ndata") +
          autolayer(RAIL_PRICE_S_ADJ, series="Seasonally \nadjusted") +
          xlab("Time") + ylab("Price") +
          ggtitle("Rail prices - Harmonic Index of Consumer Prices (2015=100)") +
          scale_colour_manual(values=c("darkgray","blue"),
                        breaks=c("Original \ndata","Seasonally \nadjusted")) +
          theme_minimal())
  
  print(autoplot(ts_data[, "AIR_PRICE"], series="Original \ndata") +
          autolayer(AIR_PRICE_S_ADJ, series="Seasonally \nadjusted") +
          xlab("Time") + ylab("Price") +
          ggtitle("Air prices - Harmonic Index of Consumer Prices (2015=100)") +
          scale_colour_manual(values=c("darkgray","blue"),
                        breaks=c("Original \ndata","Seasonally \nadjusted")) +
          theme_minimal())
  
  
  print(autoplot(ts_data[, "TOURISM_OCC"], series="Original data") +
          autolayer(TOURISM_OCC_S_ADJ, series="Seasonally \nadjusted") +
          xlab("Time") + ylab("Rate of occupancy") +
          ggtitle("Tourism - Net occupancy rate of bed-places in hotels and similar accommodation") +
          scale_colour_manual(values=c("darkgray","blue"),
                        breaks=c("Original data","Seasonally \nadjusted")) +
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
esRailAir = seasonal_adjustments(country_df=esRailAir, ts_data=esRailAirTS)
#itRailAir = seasonal_adjustments(country_df=itRailAir, ts_data=itRailAirTS)
#deRailAir = seasonal_adjustments(country_df=deRailAir, ts_data=deRailAirTS)

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


plotAllRailDensities = plot_allCountries(y_variable='RAIL_DENSITY', lbl_axis='Rail density (km/km2)', lbl_plot="Passenger's railway length / square kilometers of land area")
plotAllRailDensities


plotAllGDP = plot_allCountries(y_variable='GDP_PC', lbl_axis='Thousands of euros', lbl_plot="GDP per capita - Chain linked volumes (2015)")
plotAllGDP


plotAllAirPrices = plot_allCountries(y_variable='AIR_PRICE', lbl_axis='Price', lbl_plot="Air price - Harmonic Index of Consumer Prices (2015=100)")
plotAllAirPrices

plotAllRailPrices = plot_allCountries(y_variable='RAIL_PRICE', lbl_axis='Price', lbl_plot="Rail price - Harmonic Index of Consumer Prices (2015=100)")
plotAllRailPrices


plotAllTourism = plot_allCountries(y_variable='TOURISM_OCC', lbl_axis='Rate of ocupancy', lbl_plot="Tourism - occupancy rate of bed-places in hotels and similar accommodation")
plotAllTourism


# Plot for each country separately pass vs pass, pass*km vs flights (in process)

## Function to plot the comparison
plot_competition <- function(df, y_variable1, y_variable2,
                             lbl_country, lbl_plot,
                             lbl_axis1, lbl_axis2,scale_SecAxis = 1/10) {
  ggplot() +
    geom_line(data=df, aes(x=TIME, y=.data[[y_variable1]], group=1), 
              color="blue", show.legend = FALSE)+
    geom_line(data=df, aes(x=TIME, y=.data[[y_variable2]]*scale_SecAxis, group=1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_discrete(breaks = subset(df, grepl("-Q1$", df$TIME))$TIME) +
    scale_y_continuous(name = lbl_axis1,
                       sec.axis = sec_axis( trans=~./scale_SecAxis, name=lbl_axis2)) +
    theme(axis.title.y.left = element_text(color = "blue")) +
    ggtitle(paste(lbl_country,'-', lbl_plot))
  }

## Plotting the Passenger pass vs pass for each country
plotEsPas = plot_competition(df=esRailAir, y_variable1='A_THS_PAS', y_variable2='R_THS_PAS',
                             lbl_country='Spain', lbl_plot='Passengers per million inhabitants',
                             lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail - Thousands of passengers')
plotItPas = plot_competition(df=itRailAir, y_variable1='A_THS_PAS', y_variable2='R_THS_PAS',
                             lbl_country='Italy', lbl_plot='Passengers per million inhabitants', 
                             lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail - Thousands of passengers')
plotDePas = plot_competition(df=deRailAir, y_variable1='A_THS_PAS', y_variable2='R_THS_PAS',
                             lbl_country='Germany', lbl_plot='Passengers per million inhabitants',
                             lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail - Thousands of passengers')
plotDePasScaled = plot_competition(df=deRailAir, y_variable1='A_THS_PAS', y_variable2='R_THS_PAS',
                                   lbl_country='Germany', lbl_plot='Passengers per million inhabitants (rescaled)',
                                   lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail - Thousands of passengers',
                                   scale_SecAxis = 1/100)

plotEsPas
plotItPas
plotDePas
plotDePasScaled

## Plotting the pass*km vs flights for each country
plotEsPaskmFlights = plot_competition(df=esRailAir, y_variable1='A_FLIGHTS', y_variable2='R_MIO_PKM',
                             lbl_country='Spain', lbl_plot='Passenger-km and number of flights per million inhabitants',
                             lbl_axis1='Air - Number of flights', lbl_axis2='Rail - Millions of passenger-km',
                             scale_SecAxis = 10)
plotItPaskmFlights = plot_competition(df=itRailAir, y_variable1='A_FLIGHTS', y_variable2='R_MIO_PKM',
                             lbl_country='Italy', lbl_plot='Passenger-km and number of flights per million inhabitants', 
                             lbl_axis1='Air - Number of flights', lbl_axis2='Rail - Millions of passenger-km',
                             scale_SecAxis = 10)
plotDePaskmFlights = plot_competition(df=deRailAir, y_variable1='A_FLIGHTS', y_variable2='R_MIO_PKM',
                             lbl_country='Germany', lbl_plot='Passenger-km and number of flights per million inhabitants',
                             lbl_axis1='Air - Number of flights', lbl_axis2='Rail - Millions of passenger-km',
                             scale_SecAxis = 10)
plotDePaskmFlightsScaled = plot_competition(df=deRailAir, y_variable1='A_FLIGHTS', y_variable2='R_MIO_PKM',
                                   lbl_country='Germany', lbl_plot='Passenger-km vs number of flights per million inhabitants (rescaled)',
                                   lbl_axis1='Air - Number of flights', lbl_axis2='Rail - Millions of passenger-km',
                                   scale_SecAxis = 1)

plotEsPaskmFlights
plotItPaskmFlights
plotDePaskmFlights
plotDePaskmFlightsScaled


## Plotting the pass vs pass*km for each country
plotEsPasAirPaskm = plot_competition(df=esRailAir, y_variable1='A_THS_PAS', y_variable2='R_MIO_PKM',
                                      lbl_country='Spain', lbl_plot='Air passengers vs rail passenger-km per million inhabitants',
                                      lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail - Millions of passenger-km',
                                      scale_SecAxis = 1)
plotItPasAirPaskm = plot_competition(df=itRailAir, y_variable1='A_THS_PAS', y_variable2='R_MIO_PKM',
                                      lbl_country='Italy', lbl_plot='Air passengers vs rail passenger-km per million inhabitants', 
                                      lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail - Millions of passenger-km',
                                      scale_SecAxis = 1)
plotDePasAirPaskm = plot_competition(df=deRailAir, y_variable1='A_THS_PAS', y_variable2='R_MIO_PKM',
                                      lbl_country='Germany', lbl_plot='Air passengers vs rail passenger-km per million inhabitants',
                                      lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail - Millions of passenger-km',
                                      scale_SecAxis = 1)
plotDePasAirPaskmScaled = plot_competition(df=deRailAir, y_variable1='A_THS_PAS', y_variable2='R_MIO_PKM',
                                            lbl_country='Germany', lbl_plot='Air passengers vs rail passenger-km per million inhabitants (rescaled)',
                                            lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail - Millions of passenger-km',
                                            scale_SecAxis = 1/5)

plotEsPasAirPaskm
plotItPasAirPaskm
plotDePasAirPaskm
plotDePasAirPaskmScaled


## Plotting the pass vs gdp for each country
plotEsPasGDP = plot_competition(df=esRailAir, y_variable1='A_THS_PAS', y_variable2='GDP_PC',
                                     lbl_country='Spain', lbl_plot='Air passengers vs GDP per capita',
                                     lbl_axis1='Air - Thousands of passengers', lbl_axis2='Thousands of euros',
                                     scale_SecAxis = 50)
plotItPasGDP = plot_competition(df=itRailAir, y_variable1='A_THS_PAS', y_variable2='GDP_PC',
                                     lbl_country='Italy', lbl_plot='Air passengers vs GDP per capita', 
                                     lbl_axis1='Air - Thousands of passengers', lbl_axis2='Thousands of euros',
                                     scale_SecAxis = 30)
plotDePasGDP = plot_competition(df=deRailAir, y_variable1='A_THS_PAS', y_variable2='GDP_PC',
                                     lbl_country='Germany', lbl_plot='Air passengers vs GDP per capita',
                                     lbl_axis1='Air - Thousands of passengers', lbl_axis2='Thousands of euros',
                                     scale_SecAxis = 10)

plotEsPasGDP
plotItPasGDP
plotDePasGDP



# Plotting the air traffic vs the prices of Rail and Air for Spain
plotEsPasAirPrice = plot_competition(df=esRailAir, y_variable1='A_THS_PAS', y_variable2='AIR_PRICE',
                                     lbl_country='Spain', lbl_plot='Air traffic vs Air price',
                                     lbl_axis1='Air - Thousands of passengers', lbl_axis2='Air price',
                                     scale_SecAxis = 4)
plotEsPasRailPrice = plot_competition(df=esRailAir, y_variable1='A_THS_PAS', y_variable2='RAIL_PRICE',
                                      lbl_country='Spain', lbl_plot='Air traffic vs Rail price',
                                      lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail price',
                                      scale_SecAxis = 3)

plotEsPasAirPrice
plotEsPasRailPrice


# Plotting the air traffic vs rail density for each country
plotEsPasRailDensity = plot_competition(df=esRailAir, y_variable1='A_THS_PAS', y_variable2='RAIL_DENSITY',
                                     lbl_country='Spain', lbl_plot='Air traffic vs Rail density',
                                     lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail density',
                                     scale_SecAxis = 10)
plotDePasRailDensity = plot_competition(df=deRailAir, y_variable1='A_THS_PAS', y_variable2='RAIL_DENSITY',
                                     lbl_country='Germany', lbl_plot='Air traffic vs Rail density',
                                     lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail density',
                                     scale_SecAxis = 0.8)
plotItPasRailDensity = plot_competition(df=itRailAir, y_variable1='A_THS_PAS', y_variable2='RAIL_DENSITY',
                                     lbl_country='Italy', lbl_plot='Air traffic vs Rail density',
                                     lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail density',
                                     scale_SecAxis = 3)

plotEsPasRailDensity
plotDePasRailDensity
plotItPasRailDensity


# Plotting the air traffic vs tourism for each country
plotEsPasTourism = plot_competition(df=esRailAir, y_variable1='A_THS_PAS', y_variable2='TOURISM_OCC',
                                        lbl_country='Spain', lbl_plot='Air traffic vs Tourism occupation',
                                        lbl_axis1='Air - Thousands of passengers', lbl_axis2='Tourism occupation',
                                        scale_SecAxis = 5)
plotDePasTourism = plot_competition(df=deRailAir, y_variable1='A_THS_PAS', y_variable2='TOURISM_OCC',
                                    lbl_country='Germany', lbl_plot='Air traffic vs Tourism occupation',
                                    lbl_axis1='Air - Thousands of passengers', lbl_axis2='Tourism occupation',
                                    scale_SecAxis = 5)
plotItPasTourism = plot_competition(df=itRailAir, y_variable1='A_THS_PAS', y_variable2='TOURISM_OCC',
                                    lbl_country='Italy', lbl_plot='Air traffic vs Tourism occupation',
                                    lbl_axis1='Air - Thousands of passengers', lbl_axis2='Tourism occupation',
                                    scale_SecAxis = 5)
plotEsPasTourism
plotDePasTourism
plotItPasTourism


# Insights:

# 1. Note that in the case of Germany, I had to further rescale the air passengers series to make it visible,
# so, people use the train much more than the airplanes in comparison to Spain and Italy (maybe they are more environmentally friendly?).
# 2. In Spain and in Italy, peaks of air transport appear to coincide with the valleys of the rail transport
# and I think they tend to coincide with the Q3 (summer vacation?). This behavior is not present in the Germany data



#### Models for air passengers ####

# Baseline - Air passengers ----------------------------------------------------------------

# We start with a "baseline" model: a linear regression with just trend and season (and the basic COVID variable). 
# These are de 'basic' variables that we can build upon
esPasBaseline = lm(data = esRailAir, A_THS_PAS ~ TREND + QUARTER + COVID)

## Graphically, it seems we can do better, but at least we are capturing some time-dependent patterns, specially seasonality.
## We also capture some of the COVID effect (but maybe this can be improved).
## There does not seem to be a strong trend in this case.
plot(esRailAir$A_THS_PAS)
lines(esRailAir$A_THS_PAS)
lines(fitted(esPasBaseline), col=2)

## This is the summary of the significance and the goodness of fit of this linear model.
## We can try to improve it.
summary(esPasBaseline)
AICc(esPasBaseline)

## The residuals do seem to follow a kind of harmonic pattern, it is not white noise as we would wish
## And we still have significant autocorrelation for the first lags
plot(residuals(esPasBaseline))
tsdisplay(residuals(esPasBaseline))

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
esPasArBaseline = reg_ARIMA_errors(df=esRailAir, y_col='A_THS_PAS', predictors=c('TREND','QUARTER','COVID'))
summary(esPasArBaseline)

plot(esRailAir$A_THS_PAS)
lines(esRailAir$A_THS_PAS)
lines(fitted(esPasBaseline), col=2)
lines(fitted(esPasArBaseline), col=3)

# Before adding new variable, we can try to check if the baseline version could be improved somehow

# Baseline without trend  - Air passengers ----------------------------------------------------------------

# Previous model suggested that the trend is not significant. Let's try a model without it
esPasBaselineNoTrend = lm(data = esRailAir, A_THS_PAS ~ QUARTER + COVID)
summary(esPasBaselineNoTrend)
AICc(esPasBaselineNoTrend)

# Also we fit the version with ARIMA errors
esPasArBaselineNoTrend = reg_ARIMA_errors(df=esRailAir, y_col='A_THS_PAS', predictors=c('QUARTER','COVID'))
summary(esPasArBaselineNoTrend)

# we compare the two ARIMA versions graphically
plot(esRailAir$A_THS_PAS)
lines(esRailAir$A_THS_PAS)
lines(fitted(esPasArBaseline), col=4)
lines(fitted(esPasArBaselineNoTrend), col=6)

# Indeed, it seems that the trend does not add anything to our model, so we can choose the simpler model (without the trend)


# Baseline with COVID adjustment - Air passengers----------------------------------------------------------------

# We try with the alternative COVID formulation
esPasBaselineCovidAdj = lm(data = esRailAir, A_THS_PAS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR)

## Graphically, there seems to be an improvement regarding COVID.


# Plotting models
## Adding the two lines for the original data and the fitted values

plot(esRailAir$A_THS_PAS, type = "l", col = "blue", xlab = "TIME", ylab = "Air - Thousands of passengers",
     main = "Spain - Baseline", xaxt = "n")
lines(fitted(esPasBaselineCovidAdj), col = "black",lty='dashed')

## Adding the x tick labels. Filters labels for Q1 (I tried to put the labels horizontally, 
## but it overlaps with the axis label and it's more work to fix it)
q1_labels = esRailAir$TIME[grep("-Q1$", esRailAir$TIME)]
axis(1, at = which(esRailAir$TIME %in% q1_labels), labels = q1_labels, 
     cex.axis = 1, las=1)

# Add a legend
legend("bottomleft", legend = c("Original Data", "Fitted baseline"), 
       col = c("blue", "black"), lty = c('solid','dashed'))











## We can capture the COVID effect more precisely (a coefficient for each "stage" of the COVID restrictions). 
## Also, the AICc is lower in this case even though we added some new features.
## This suggests that the adjusted version of the COVID variable is better, both in interpretability and in terms of fit.
summary(esPasBaselineCovidAdj)
AICc(esPasBaselineCovidAdj)
paste('% difference between AICc: ',
      round(100*(AICc(esPasBaselineCovidAdj)-AICc(esPasBaselineNoTrend))/AICc(esPasArBaselineNoTrend),2),'%')

## Like in the previous cases, we still have significant autocorrelation for the first lags, so we also fit the dynamic regression with ARIMA errors
plot(residuals(esPasBaselineCovidAdj))
tsdisplay(residuals(esPasBaselineCovidAdj), main = "Spain - Baseline residuals and autocorrelation")

# Again, the AICc is reduced in comparison to the other dynamic regressions.
esPasArBaselineCovidAdj = reg_ARIMA_errors(df=esRailAir, y_col='A_THS_PAS', predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR'))
summary(esPasArBaselineCovidAdj)

## We plot the baseline model with the simple COVID variable and with the adjusted COVID variable (both without a trend, since it did not show to be significant)
plot(esRailAir$A_THS_PAS)
lines(esRailAir$A_THS_PAS)
lines(fitted(esPasArBaselineNoTrend), col=4)
lines(fitted(esPasArBaselineCovidAdj), col=6)

# As a result, we can use this last version (a model without trend and with the COVID adjustment) as 
# the "final version" of the baseline model. We can start adding variables on top of that.
esPasBaselineFinal = esPasBaselineCovidAdj
esPasArBaselineFinal = esPasArBaselineCovidAdj


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
esPasLassoCoef = reg_lasso(df=esRailAir, y_col='A_THS_PAS', 
                           predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM','R_THS_PAS',
                                        'GDP_PC','AIR_PRICE','RAIL_PRICE','RAIL_DENSITY','TOURISM_OCC'))

# TODO: this must be done manually according to what is obtained from the reg_lasso function.
esPasLasso = lm(data = esRailAir, A_THS_PAS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR +
                  R_MIO_PKM + GDP_PC + RAIL_PRICE + RAIL_DENSITY)
summary(esPasLasso)
AICc(esPasLasso)

## We do not have such a strong autocorrelation for the first lags, but we also fit the dynamic regression with ARIMA errors
plot(residuals(esPasLasso))
tsdisplay(residuals(esPasLasso))

# The new predictors did reduce the AICc in comparison to the other dynamic regressions.
esPasArLasso = reg_ARIMA_errors(df=esRailAir, y_col='A_THS_PAS', 
                                predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM',
                                             'GDP_PC','RAIL_PRICE','RAIL_DENSITY'))
summary(esPasArLasso)


# Graphically (with respect to the final baseline model), we do a better job at capturing the
# initial impact of COVID and other subtle changes along the series
plot(esRailAir$A_THS_PAS)
lines(esRailAir$A_THS_PAS)
lines(fitted(esPasArBaselineFinal), col=4)
lines(fitted(esPasArLasso), col=6)



# Best subset selection - Air passengers - Non seasonally adjusted ----------------------------------------------------------------

# Given that we do not have many predictors, we can perform a best subset selection
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
esPasBestObject = regsubsets(A_THS_PAS~., data=esRailAir[,c('A_THS_PAS','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM','R_THS_PAS',
                                                             'GDP_PC','AIR_PRICE','RAIL_PRICE','RAIL_DENSITY','TOURISM_OCC')])


# From the Statistical Learning scripts:
# The regsubsets() function has a built-in plot() command  which can be used to display the selected variables for 
# the best model with a given number of predictors, ranked according to a chosen statistic. The top row of each plot 
# contains a black square for each variable selected according to the optimal model associated with that statistic.
plot(esPasBestObject,scale="adjr2")
plot(esPasBestObject,scale="Cp")
plot(esPasBestObject,scale="bic")

# We can obtain the desired the best model by fitting the regression with the coefficients resulting from the plots
# TODO: this must be done manually according to what is obtained from the plots above.
esPasBest = lm(data = esRailAir, A_THS_PAS ~  QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + R_MIO_PKM + GDP_PC + RAIL_DENSITY)
summary(esPasBest)
AICc(esPasBest)

## It is practically the same model, except from the RAIL_PRICE variable, which is added by the Lasso model, 
## but is not really significant. Still, we plot the results with the dynamic regression
plot(residuals(esPasBest))
tsdisplay(residuals(esPasBest))

esPasArBest = reg_ARIMA_errors(df=esRailAir, y_col='A_THS_PAS', 
                               predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM','GDP_PC','RAIL_DENSITY'))
summary(esPasArBest)

plot(esRailAir$A_THS_PAS)
lines(esRailAir$A_THS_PAS)
lines(fitted(esPasArLasso), col=4)
lines(fitted(esPasArBest), col=6)

#### Adding seasonally adjusted predictors (approach including economic activity and other variables) ####

# Lasso - Air passengers - Seasonally adjusted ----------------------------------------------------------------
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
# Getting the coefficients of the best Lasso and passing them to a linear model to see their significance
# We can pass all the possible predictors
esPasLassoCoefSeasAdj = reg_lasso(df=esRailAir, y_col='A_THS_PAS', 
                           predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM_S_ADJ','R_THS_PAS_S_ADJ',
                                        'GDP_PC','AIR_PRICE_S_ADJ','RAIL_PRICE_S_ADJ','RAIL_DENSITY','TOURISM_OCC_S_ADJ'))

# TODO: this must be done manually according to what is obtained from the reg_lasso function.
# In this case, the resulting variables are really similar to what we saw with the non-seasonally adjusted versions
esPasLassoSeasAdj = lm(data = esRailAir, A_THS_PAS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + R_MIO_PKM_S_ADJ + GDP_PC + RAIL_DENSITY)
summary(esPasLassoSeasAdj)
AICc(esPasLassoSeasAdj)

## Again, we do not have such a strong autocorrelation for the first lags, but we still need to fit the dynamic regression with ARIMA errors
plot(residuals(esPasLassoSeasAdj))
tsdisplay(residuals(esPasLassoSeasAdj))

esPasArLassoSeasAdj = reg_ARIMA_errors(df=esRailAir, y_col='A_THS_PAS',
                                       predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM_S_ADJ','GDP_PC','RAIL_DENSITY'))
summary(esPasArLassoSeasAdj)


# Graphically (with respect to the final baseline model), we do a better job at capturing the
# initial impact of COVID
plot(esRailAir$A_THS_PAS)
lines(esRailAir$A_THS_PAS)
lines(fitted(esPasArBaselineFinal), col=4)
lines(fitted(esPasLassoSeasAdj), col=4)
lines(fitted(esPasArLassoSeasAdj), col=6)



#  Best subset selection - Air passengers - Seasonally adjusted ----------------------------------------------------------------

# Given that we do not have many predictors, we can perform a best subset selection
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
esPasBestObjectSeasAdj = regsubsets(A_THS_PAS~., data=esRailAir[,c('A_THS_PAS','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM_S_ADJ','R_THS_PAS_S_ADJ',
                                                             'GDP_PC','AIR_PRICE_S_ADJ','RAIL_PRICE_S_ADJ','RAIL_DENSITY','TOURISM_OCC_S_ADJ')])


# From the Statistical Learning scripts:
# The regsubsets() function has a built-in plot() command  which can be used to display the selected variables for 
# the best model with a given number of predictors, ranked according to a chosen statistic. The top row of each plot 
# contains a black square for each variable selected according to the optimal model associated with that statistic.
plot(esPasBestObjectSeasAdj,scale="adjr2")
plot(esPasBestObjectSeasAdj,scale="Cp")
plot(esPasBestObjectSeasAdj,scale="bic")

# We can obtain the desired the best model by fitting the regression with the coefficients resulting from the plots
# Two of the criteria include R_MIO_KM, while the other one does not. I include it because of the criteria of the majority
# TODO: this must be done manually according to what is obtained from the plots above.
esPasBestSeasAdj = lm(data = esRailAir, A_THS_PAS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + R_MIO_PKM_S_ADJ + GDP_PC + RAIL_DENSITY)
summary(esPasBestSeasAdj)
AICc(esPasBestSeasAdj)

## This is the same model as the one obtained with Lasso, so I only do one plot (the one from the Lasso above)
esPasArBestSeasAdj = reg_ARIMA_errors(df=esRailAir, y_col='A_THS_PAS', 
                                      predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM_S_ADJ','GDP_PC','RAIL_DENSITY'))
summary(esPasArBestSeasAdj)

# Summary - Air passengers ---------------------------------------------------------------------------------------
# I re-print all the results just to be clear of what happened without browsing above.
# It is the results for the linear model and the dynamic regression with ARIMA errors 
# of the Lasso and the best subset selection approaches

# I also check for collinearity just in case


# With non-seasonally adjusted variables
summary(esPasLasso)
AICc(esPasLasso)
summary(esPasArLasso)
vif(esPasLasso)


summary(esPasBest)
AICc(esPasBest)
summary(esPasArBest)
vif(esPasBest)



# With seasonally adjusted variables
summary(esPasLassoSeasAdj)
AICc(esPasLassoSeasAdj)
summary(esPasArLassoSeasAdj)
vif(esPasLassoSeasAdj)


summary(esPasBestSeasAdj)
AICc(esPasBestSeasAdj)
summary(esPasArBestSeasAdj)
vif(esPasBestSeasAdj)


# IT seems that there might be a collinearity issue.
# I'll remove the least significant variable justto test what happens
esPasBestSeasAdjVIF = lm(data = esRailAir, A_THS_PAS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + GDP_PC + RAIL_DENSITY)
esPasArBestSeasAdjVIF = reg_ARIMA_errors(df=esRailAir, y_col='A_THS_PAS',
                                         predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','GDP_PC','RAIL_DENSITY'))

summary(esPasBestSeasAdjVIF)
AICc(esPasBestSeasAdjVIF)
summary(esPasArBestSeasAdjVIF)
vif(esPasBestSeasAdjVIF)

# It seems that the last model (without the R_MIO_PKM_S_ADJ column) is the best one. It is almost the same AICc, and it
# eliminates the collinearity issue. I stick with the seasonally adjusted variables in order to assess the effect of the variables
# without added effects, although the results  are very similar in this case.

# So, the plot of the baseline model and the final model looks like this
plot(esRailAir$A_THS_PAS)
lines(esRailAir$A_THS_PAS)
lines(fitted(esPasArBaselineFinal), col=4)
lines(fitted(esPasArBestSeasAdjVIF), col=6)

# Checking the residuals of the final linear model
tsdisplay(residuals(esPasBestSeasAdjVIF), main = "Spain - Final linear model - Residuals and autocorrelation")
tsdisplay(residuals(esPasArBestSeasAdjVIF), main = "Spain - Final model with ARIMA errors - Residuals and autocorrelation")


# Plotting models (final linear model)
## Adding the two lines for the original data and the fitted values
par(mar = c(5, 4, 4, 2) + 0.1)
plot(esRailAir$A_THS_PAS, type = "l", col = "blue", xlab = "TIME", ylab = "Air - Thousands of passengers",
     main = "Spain - Final linear model", xaxt = "n")
lines(fitted(esPasBestSeasAdjVIF), col = "black",lty='dashed')

## Adding the x tick labels. Filters labels for Q1 (I tried to put the labels horizontally, 
## but it overlaps with the axis label and it's more work to fix it)
q1_labels = esRailAir$TIME[grep("-Q1$", esRailAir$TIME)]
axis(1, at = which(esRailAir$TIME %in% q1_labels), labels = q1_labels, 
     cex.axis = 1, las=1)

# Add a legend
legend("bottomleft", legend = c("Original Data", "Fitted FLM"), 
       col = c("blue", "black"), lty = c('solid','dashed'))





# Plotting models (final dynamic regression with arima errors)
## Adding the two lines for the original data and the fitted values
par(mar = c(5, 4, 4, 2) + 0.1)
plot(esRailAir$A_THS_PAS, type = "l", col = "blue", xlab = "TIME", ylab = "Air - Thousands of passengers",
     main = "Spain - Final model with ARIMA errors (dynamic regression)", xaxt = "n")
lines(fitted(esPasArBestSeasAdjVIF), col = "black",lty='dashed')
lines(fitted(esPasBaselineCovidAdj), col = "orangered2",lty='dotted')

## Adding the x tick labels. Filters labels for Q1 (I tried to put the labels horizontally, 
## but it overlaps with the axis label and it's more work to fix it)
q1_labels = esRailAir$TIME[grep("-Q1$", esRailAir$TIME)]
axis(1, at = which(esRailAir$TIME %in% q1_labels), labels = q1_labels, 
     cex.axis = 1, las=1)

# Add a legend
legend("bottomleft", legend = c("Original Data", "Fitted DynReg", "Fitted baseline"), 
       col = c("blue", "black", "orangered2"), lty = c('solid', 'dashed', 'dotted'))


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






#### Models for air flights ####

# Baseline - Air flights ----------------------------------------------------------------

# We start with a "baseline" model: a linear regression with just trend, season and the COVID variable. 
# In this case we do it directly with the COVID variable from Our world in Data
esFlightsBaseline = lm(data = esRailAir, A_FLIGHTS ~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR)

## Graphically, it seems we can do better, but at least we are capturing some time-dependent patterns, specially seasonality.
## We also capture some of the COVID effect (but maybe this can be improved).
plot(esRailAir$A_FLIGHTS)
lines(esRailAir$A_FLIGHTS)
lines(fitted(esFlightsBaseline), col=2)

## This is the summary of the significance and the goodness of fit of this linear model.
## In tis case, it seems that the trend is significant
summary(esFlightsBaseline)
AICc(esFlightsBaseline)

## We do the dynamic regression with ARIMA errors procedure
plot(residuals(esFlightsBaseline))
tsdisplay(residuals(esFlightsBaseline))

esFlightsArBaseline = reg_ARIMA_errors(df=esRailAir, y_col='A_FLIGHTS', predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR'))
summary(esFlightsArBaseline)

plot(esRailAir$A_FLIGHTS)
lines(esRailAir$A_FLIGHTS)
lines(fitted(esFlightsBaseline), col=2)
lines(fitted(esFlightsArBaseline), col=3)



#### Adding non-seasonally-adjusted predictors (approach including economic activity and other variables) ####
# We can now start adding predictors to the resulting baseline model.

# Lasso - Air flights  - Non seasonally adjusted ----------------------------------------------------------------
# Getting the coefficients of the best Lasso and passing them to a linear model to see their significance
# We can pass all the possible predictors
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
esFlightsLassoCoef = reg_lasso(df=esRailAir, y_col='A_FLIGHTS', 
                           predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM','R_THS_PAS',
                                        'GDP_PC','AIR_PRICE','RAIL_PRICE','RAIL_DENSITY','TOURISM_OCC'))

# TODO: this must be done manually according to what is obtained from the reg_lasso function.
# All columns show an "expected" behavior except from RAIL_PRICE, which is negatively related to the 
# amount of flights (the higher the rail prices the less people by plane?). Let's see what the other approaches yield.
esFlightsLasso = lm(data = esRailAir, A_FLIGHTS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR +
                      GDP_PC + RAIL_PRICE + RAIL_DENSITY)
summary(esFlightsLasso)
AICc(esFlightsLasso)

## We do the Dynamic regression procedure
plot(residuals(esFlightsLasso))
tsdisplay(residuals(esFlightsLasso))

esFlightsArLasso = reg_ARIMA_errors(df=esRailAir, y_col='A_FLIGHTS', 
                                predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR',
                                             'GDP_PC','RAIL_PRICE','RAIL_DENSITY'))
summary(esFlightsArLasso)


# Graphically (with respect to the final baseline model), we do a better job at capturing the
# initial impact of COVID and other subtle changes along the series
plot(esRailAir$A_FLIGHTS)
lines(esRailAir$A_FLIGHTS)
lines(fitted(esFlightsArBaseline), col=4)
lines(fitted(esFlightsArLasso), col=6)



#  Best subset selection - Air flights - Non seasonally adjusted ----------------------------------------------------------------

# Given that we do not have many predictors, we can perform a best subset selection
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
esFlightsBestObject = regsubsets(A_FLIGHTS~., data=esRailAir[,c('A_FLIGHTS','TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM','R_THS_PAS',
                                                            'GDP_PC','AIR_PRICE','RAIL_PRICE','RAIL_DENSITY','TOURISM_OCC')])


# From the Statistical Learning scripts:
# The regsubsets() function has a built-in plot() command  which can be used to display the selected variables for 
# the best model with a given number of predictors, ranked according to a chosen statistic. The top row of each plot 
# contains a black square for each variable selected according to the optimal model associated with that statistic.
plot(esFlightsBestObject,scale="adjr2")
plot(esFlightsBestObject,scale="Cp")
plot(esFlightsBestObject,scale="bic")

# We can obtain the desired the best model by fitting the regression with the coefficients resulting from the plots
# TODO: this must be done manually according to what is obtained from the plots above.
# In this case, two criteria include TOURISM_OCC, so I add it

# I add the predictors this way (with a model matrix) in order to exclude the categorical dummies that are not significant
matrix = model.matrix(~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + GDP_PC + RAIL_PRICE + RAIL_DENSITY + TOURISM_OCC, data = esRailAir)
exclude_vars = c('(Intercept)','QUARTERQ4')
matrix = matrix[, -grep(paste(exclude_vars, collapse = "|"), colnames(matrix), ignore.case = TRUE)]

# In this case, apart from the strange behavior in the RAIL_PRICE, we get a 
# strange behavior regarding TOURISM_OCC (the more tourist the less flights?)
# We can move on to check what the other approahces yield
esFlightsBest = lm(data = esRailAir, A_FLIGHTS ~  matrix)
summary(esFlightsBest)
AICc(esFlightsBest)

## Dynamic regression procedure
plot(residuals(esFlightsBest))
tsdisplay(residuals(esFlightsBest))

esFlightsArBest = reg_ARIMA_errors(df=esRailAir, y_col='A_FLIGHTS', 
                               predictors=matrix)
summary(esFlightsArBest)

# In terms of fit, both models are really similar and satisfactory. 
# However, the interpretation part is still confounding
plot(esRailAir$A_FLIGHTS)
lines(esRailAir$A_FLIGHTS)
lines(fitted(esFlightsArLasso), col=4)
lines(fitted(esFlightsArBest), col=6)

#### Adding seasonally adjusted predictors (approach including economic activity and other variables) ####

# Lasso - Air flights - Seasonally adjusted ----------------------------------------------------------------
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
# Getting the coefficients of the best Lasso and passing them to a linear model to see their significance
# We can pass all the possible predictors
esFlightsLassoCoefSeasAdj = reg_lasso(df=esRailAir, y_col='A_FLIGHTS', 
                                  predictors=c('TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM_S_ADJ','R_THS_PAS_S_ADJ',
                                               'GDP_PC','AIR_PRICE_S_ADJ','RAIL_PRICE_S_ADJ','RAIL_DENSITY','TOURISM_OCC_S_ADJ'))

# TODO: this must be done manually according to what is obtained from the reg_lasso function.
# In this case, the resulting variables are really similar to what we saw with the non-seasonally adjusted versions
esFlightsLassoSeasAdj = lm(data = esRailAir, A_FLIGHTS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + GDP_PC + RAIL_PRICE_S_ADJ + RAIL_DENSITY)
summary(esFlightsLassoSeasAdj)
AICc(esFlightsLassoSeasAdj)

## Dynamic regression
plot(residuals(esFlightsLassoSeasAdj))
tsdisplay(residuals(esFlightsLassoSeasAdj))

esFlightsArLassoSeasAdj = reg_ARIMA_errors(df=esRailAir, y_col='A_FLIGHTS',
                                       predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','GDP_PC','RAIL_PRICE_S_ADJ','RAIL_DENSITY'))
summary(esFlightsArLassoSeasAdj)


# Graphical results
plot(esRailAir$A_FLIGHTS)
lines(esRailAir$A_FLIGHTS)
lines(fitted(esFlightsArBaseline), col=4)
lines(fitted(esFlightsArLassoSeasAdj), col=6)



# Best subset selection - Air flights - Seasonally adjusted  ----------------------------------------------------------------

# Given that we do not have many predictors, we can perform a best subset selection
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
esFlightsBestObjectSeasAdj = regsubsets(A_FLIGHTS~., data=esRailAir[,c('A_FLIGHTS','TREND','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM_S_ADJ','R_THS_PAS_S_ADJ',
                                                                   'GDP_PC','AIR_PRICE_S_ADJ','RAIL_PRICE_S_ADJ','RAIL_DENSITY','TOURISM_OCC_S_ADJ')])


# From the Statistical Learning scripts:
# The regsubsets() function has a built-in plot() command  which can be used to display the selected variables for 
# the best model with a given number of predictors, ranked according to a chosen statistic. The top row of each plot 
# contains a black square for each variable selected according to the optimal model associated with that statistic.
plot(esFlightsBestObjectSeasAdj,scale="adjr2")
plot(esFlightsBestObjectSeasAdj,scale="Cp")
plot(esFlightsBestObjectSeasAdj,scale="bic")

# We can obtain the desired the best model by fitting the regression with the coefficients resulting from the plots
# All criteria consider the same variables for the best model
# TODO: this must be done manually according to what is obtained from the plots above.

# I add the predictors this way (with a model matrix) in order to exclude the categorical dummies that are not significant
matrix = model.matrix(~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + GDP_PC + RAIL_PRICE_S_ADJ + RAIL_DENSITY + TOURISM_OCC_S_ADJ, data = esRailAir)
exclude_vars = c('(Intercept)','QUARTERQ4')
matrix = matrix[, -grep(paste(exclude_vars, collapse = "|"), colnames(matrix), ignore.case = TRUE)]

# The model still carries the strange behavior from RAIL_PRICE_S_ADJ and TOURISM_OCC_S_ADJ 
esFlightsBestSeasAdj = lm(data = esRailAir, A_FLIGHTS ~ matrix)
summary(esFlightsBestSeasAdj)
AICc(esFlightsBestSeasAdj)

## Dynamic regression procedure
plot(residuals(esFlightsBestSeasAdj))
tsdisplay(residuals(esFlightsBestSeasAdj))

esFlightsArBestSeasAdj = reg_ARIMA_errors(df=esRailAir, y_col='A_FLIGHTS',
                                          predictors=matrix)
summary(esFlightsArBestSeasAdj)

plot(esRailAir$A_FLIGHTS)
lines(esRailAir$A_FLIGHTS)
lines(fitted(esFlightsArBaseline), col=4)
lines(fitted(esFlightsArBestSeasAdj), col=6)


# Summary - Air flights ---------------------------------------------------------------------------------------
# I re-print all the results just to be clear of what happened without browsing above.
# It is the results for the linear model and the dynamic regression with ARIMA errors 
# of the Lasso and the best subset selection approaches

# I also check for collinearity just in case


# With non-seasonally adjusted variables
summary(esFlightsLasso)
AICc(esFlightsLasso)
summary(esFlightsArLasso)
vif(esFlightsLasso)


summary(esFlightsBest)
AICc(esFlightsBest)
summary(esFlightsArBest)
vif(lm(data = esRailAir, A_FLIGHTS ~   QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + GDP_PC + RAIL_PRICE + RAIL_DENSITY + TOURISM_OCC)) # Since the model was with a matrix, the VIF in this case has to be approximated this way



# With seasonally adjusted variables
summary(esFlightsLassoSeasAdj)
AICc(esFlightsLassoSeasAdj)
summary(esFlightsArLassoSeasAdj)
vif(esFlightsLassoSeasAdj)


summary(esFlightsBestSeasAdj)
AICc(esFlightsBestSeasAdj)
summary(esFlightsArBestSeasAdj)
vif(lm(data = esRailAir, A_FLIGHTS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + GDP_PC + RAIL_PRICE_S_ADJ + RAIL_DENSITY + TOURISM_OCC_S_ADJ)) # Since the model was with a matrix, the VIF in this case has to be approximated this way


# Again, it seems that there might be a collinearity issue.
# I'll remove the least significant variable just to test what happens
# I add the predictors this way (with a model matrix) in order to exclude the categorical dummies that are not significant
esFlightsBestSeasAdjVIF = lm(data = esRailAir, A_FLIGHTS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + GDP_PC + RAIL_DENSITY + TOURISM_OCC_S_ADJ)
esFlightsArBestSeasAdjVIF = reg_ARIMA_errors(df=esRailAir, y_col='A_FLIGHTS',
                                         predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','GDP_PC','RAIL_DENSITY','TOURISM_OCC_S_ADJ'))

summary(esFlightsBestSeasAdjVIF)
AICc(esFlightsBestSeasAdjVIF)
summary(esFlightsArBestSeasAdjVIF)
vif(esFlightsBestSeasAdjVIF)

# The collinearity issue is not solved yet and the TOURISM_OCC_S_ADJ does not seem significant.
# I will also remove that variable
esFlightsBestSeasAdjVIF2 = lm(data = esRailAir, A_FLIGHTS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + GDP_PC + RAIL_DENSITY)
esFlightsArBestSeasAdjVIF2 = reg_ARIMA_errors(df=esRailAir, y_col='A_FLIGHTS',
                                         predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','GDP_PC','RAIL_DENSITY'))

summary(esFlightsBestSeasAdjVIF2)
AICc(esFlightsBestSeasAdjVIF2)
summary(esFlightsArBestSeasAdjVIF2)
vif(esFlightsBestSeasAdjVIF2)




# The last model has a worse AICc, but it removes the collinearity problem. The best AICc was obtained
# by the Lasso approach with the seasonally adjusted variables,but it includes a hard to interpret RAIL_PRICE_S_ADJ
# For that reason, since the increase in the AICc is not that dramatic (and we still have adjusted R2 close to 1),
# I think it is better to pick the model with no collinearity problems which is more straightforward to interpret, so the last one 

# The plot of the baseline model and the final model looks like this
plot(esRailAir$A_FLIGHTS)
lines(esRailAir$A_FLIGHTS)
lines(fitted(esFlightsArBaseline), col=4)
lines(fitted(esFlightsBestSeasAdjVIF2), col=6)


# Insights:
# This model did not show so consistent results. Perhaps the amount of flights have other
# dynamics that involve the airlines decisions or things like that.
# I think that the air passengers models work better and are easier to interpret,
# so I would prefer to do that in the presentation.




# Final Insights -----------------------------------------------------------------------------------
# I think that the air passengers models ara okay in my case, 
# The summary of that section should give all the information needed, I think.
