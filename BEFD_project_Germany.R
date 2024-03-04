# Time-series analysis of air and railway transport data for Germany
#### Reading the data and importing the libraries ####

#library(readxl)
library(dplyr)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(MuMIn)
library(glmnet)
library(leaps)
library(car)
library(lubridate)
library(seasonal)

setwd("/Users/Oksana/Documents/BEFD_project")
rail = read.csv("Datasets/estat_rail_pa_quartal_filtered_en.csv")
air = read.csv("Datasets/estat_avia_paoc_filtered_en.csv")
population = read.csv("Datasets/estat_demo_pjan_filtered_en.csv")
gdp = read.csv('Datasets/estat_namq_10_gdp_filtered_en.csv')
prices = read.csv('Datasets/estat_prc_hicp_midx_filtered_en.csv')
railLength = read.csv('Datasets/estat_rail_if_line_na_filtered_en.csv')
area = read.csv('Datasets/estat_reg_area3_filtered_en.csv')
tourismOccupation = read.csv('Datasets/estat_tour_occ_mnor_filtered_en.csv')
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

#_____________________________________________________
# Joining both data sets to have them in a compact and organized structure
railAir = merge(pivotRail, pivotAir, by = c('TIME_PERIOD','geo'), all.y = TRUE)

# Joining the population of each country and each year to normalize the variables of interest
colnames(population)[colnames(population) == 'OBS_VALUE'] = 'POPULATION'

#__________________________________FORECAST POPULATION________________________________
## We only have population up to 2022 (we would lose that data if we want to normalize)
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
#_______________________________________________________________________________

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

#___________________________________

# Joining the gdp of each country and each quarter
# This version is chain linked (inflation adjusted) and seasonally and calendar adjusted
colnames(gdp)[colnames(gdp) == 'OBS_VALUE'] = 'GDP'
railAir = merge(railAir, gdp[, c('TIME_PERIOD', 'geo', 'GDP')],
                by.x = c('TIME', 'COUNTRY'), by.y = c('TIME_PERIOD', 'geo'), all.x = TRUE)

## Obtaining the GDP per capita for each period
railAir$GDP_PC = 1000*railAir$GDP / railAir$POPULATION
#___________________________________

# Prices for rail passenger transport, air transport and just domestic flights
# BUT a lot of missing data (not sure if it can be used)

## For joining the price info, we first need to reorganize that dataset
pivotPrice = prices %>% pivot_wider(id_cols = c('TIME_PERIOD', 'geo'),
                                    names_from = c('coicop'),
                                    values_from = 'OBS_VALUE')

## Price data is monthly and other data is quarterly -> group it with an average for each quarter
## Renaming the columns for clarity and extracting the year and the quarter 
colnames(pivotPrice) = c('MONTH', 'COUNTRY', 'RAIL_PRICE', 'AIR_PRICE', 'DOM_AIR_PRICE')
pivotPrice$YEAR = as.integer(substr(as.character(pivotPrice$MONTH), 1, 4))
pivotPrice = pivotPrice %>%
  mutate(QUARTER = paste0('Q',
                          quarter(as.Date(paste0(MONTH, '-01')))))
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
railAir = merge(railAir, quarterPrices, by = c('YEAR', 'QUARTER', 'COUNTRY'), all.x = TRUE)
#___________________________________

#Adding the variables to obtain the railway density of each country

## Prepare the length data
### Dataset includes the railway length for passengers and freight, and only passengers separatly
### Keep them that way because there are missing values 

pivotRailLength = railLength %>% pivot_wider(id_cols = c('TIME_PERIOD', 'geo'),
                                             names_from = c('tra_meas'),
                                             values_from = 'OBS_VALUE')

dfYearCountry = data.frame(unique(railAir[, c('YEAR', 'COUNTRY')]))
pivotRailLength = merge(dfYearCountry, pivotRailLength,
                        by.x = c('YEAR', 'COUNTRY'), by.y = c('TIME_PERIOD', 'geo'), all.x = TRUE)
pivotRailLength = pivotRailLength[order(pivotRailLength$COUNTRY, pivotRailLength$YEAR), ]


# Since it's infrastructure, we can expect it to be rather stable from one year to another 
# NA values are imputed with a linear interpolation for missing values in the middle and with
# the closest observation for values at the beginning or the end of each series (hence the rule=2)
pivotRailLength = pivotRailLength %>%
  group_by(COUNTRY) %>%
  mutate(
    PAS_FR_IMPUTED = na.approx(PAS_FR, method = 'linear', rule=2),
    PAS_ONL_IMPUTED = na.approx(PAS_ONL, method = 'linear', rule=2)
  ) %>%
  ungroup()

pivotRailLength$RAIL_LENGTH = pivotRailLength$PAS_FR_IMPUTED + pivotRailLength$PAS_ONL_IMPUTED


#___________________________________________

# Prepare the land area data in a similar way 
colnames(area)[colnames(area) == 'OBS_VALUE'] = 'AREA_RAW'
area = merge(dfYearCountry, area[, c('TIME_PERIOD', 'geo', 'AREA_RAW')],
             by.x = c('YEAR', 'COUNTRY'), by.y = c('TIME_PERIOD', 'geo'), all.x = TRUE)
area = area[order(area$COUNTRY, area$YEAR), ]

# The data is imputed as the closest observation
area = area %>%
  group_by(COUNTRY) %>%
  mutate(
    AREA = na.approx(AREA_RAW, rule=2)
  ) %>%
  ungroup()

## Obtaining the rail density for passengers 
# Rail density is often considered as length of lines operated (km) per 1000 km2
# Joining dataframes and have an operation as 1000*length/area
pivotRailLength = merge(pivotRailLength, area[, c('COUNTRY', 'YEAR', 'AREA')],
                        by = c('COUNTRY', 'YEAR'), all.x = TRUE)
pivotRailLength$RAIL_DENSITY = 1000*pivotRailLength$RAIL_LENGTH / pivotRailLength$AREA

railAir = merge(railAir, pivotRailLength[, c('YEAR', 'COUNTRY', 'RAIL_DENSITY')],
                by = c('YEAR', 'COUNTRY'), all.x = TRUE)

#___________________________________________
# Info about the percentage of occupancy in hotels to calculate the touristic activity
# We have data up to 2023-09, so forecasting up to 2023-12 is done

colnames(tourismOccupation)[colnames(tourismOccupation) == 'OBS_VALUE'] = 'TOURISM_OCC'
tourism_forecast <- function() {
  imputedTourism = data.frame()
  forecasrs = c()
  for (country in unique(tourismOccupation$geo)) {
    countryTourismOccupation = tourismOccupation[tourismOccupation$geo == country,]
    
    countryTourismOccupationTS = ts(countryTourismOccupation$TOURISM_OCC, start=2000, frequency=12)
    countryHoltWinters = hw(countryTourismOccupationTS, h = 10, level = c(80), alpha = NULL)
    
    # Plotting the forecast
    print(autoplot(countryHoltWinters)+
            autolayer(fitted(countryHoltWinters), series = 'Fitted') +
            ylab(paste('Tourism occupation forecast', '-', country)) + 
            xlab('Time'))
    
    forecast_2023 = data.frame(geo = rep(country, 3),
                               TIME_PERIOD = c('2023-10', '2023-11', '2023-12'),
                               TOURISM_OCC = countryHoltWinters$mean[1:3])
    countryTourismOccupation = bind_rows(countryTourismOccupation, forecast_2023)
    imputedTourism = bind_rows(imputedTourism, countryTourismOccupation)
  }
  return(imputedTourism)
}
tourismOccupation = tourism_forecast()
tourismOccupation = tourismOccupation[, c('geo', 'TIME_PERIOD', 'TOURISM_OCC')]

# Tourist occupation data is monthly, so we group it with an average for each quarter
colnames(tourismOccupation) = c('COUNTRY', 'MONTH', 'TOURISM_OCC')
tourismOccupation$YEAR = as.integer(substr(as.character(tourismOccupation$MONTH), 1, 4))
tourismOccupation = tourismOccupation %>%
  mutate(QUARTER = paste0('Q',
                         quarter(as.Date(paste0(MONTH, '-01')))))
tourismOccupation$QUARTER = factor(tourismOccupation$QUARTER)
tourismOccupation$QUARTER = relevel(tourismOccupation$QUARTER, ref = 'Q1')

quarterTourismOccupation = tourismOccupation %>%
  group_by(COUNTRY, YEAR, QUARTER) %>% 
  summarise(
    TOURISM_OCC = mean(TOURISM_OCC, na.rm = TRUE)
  )

# Joining the data to the main dataframe
railAir = merge(railAir, quarterTourismOccupation,
                by = c('COUNTRY', 'YEAR', 'QUARTER'), all.x = TRUE)


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


#___________________________________________
# Omitting the rows that give no information (of rail or air passengers/flights)
railAir = railAir[!(is.na(railAir$R_THS_PAS) | is.na(railAir$A_THS_PAS) | is.na(railAir$A_FLIGHTS)), ]
railAir = railAir %>% 
  arrange(COUNTRY, TIME)

#___________________________________________
# Separating the data by country
# Replacing the country labels and splitting the datasets
esRailAir = railAir[railAir$COUNTRY == "ES",]
deRailAir = railAir[railAir$COUNTRY == "DE",]
itRailAir = railAir[railAir$COUNTRY == "IT",]

# Adding a variable for the trend
esRailAir$TREND<- (1:nrow(esRailAir))
deRailAir$TREND<- (1:nrow(deRailAir))
itRailAir$TREND<- (1:nrow(itRailAir))

#___________________________________________COVID (first version)_________________________________
# Adding the intervention variables for each country
# Just in case, the official end of the pandemic was declared by the OMS on may 5 2023.

# Adding the intervention variable for COVID. Just trying to search for a rough start and end
esRailAir = esRailAir %>%
  mutate(COVID = as.integer(TIME >= "2020-Q1" & TIME <= "2021-Q2"))
deRailAir = deRailAir %>%
  mutate(COVID = as.integer(TIME >= "2020-Q1" & TIME <= "2022-Q1"))
#itRailAir = itRailAir %>%
#  mutate(COVID = as.integer(TIME >= "2020-Q1" & TIME <= "YYYY-QX"))


## Adding a different version of the COVID variable that tries to capture the "strength of COVID restrictions" more precisely
## In the case of Spain, I am going to use: 
## Strong: Strongest restrictions for the whole quarter
## Medium: Medium restrictions for the whole quarter or most of it
## Weak: Weak restrictions for the whole quarter OR medium (or weak) restrictions for part of the quarter OR strong restrictions for a small part of the quarter (just the first quarter)
## No: no restrictions for the whole quarter
esRailAir <- esRailAir %>%
  mutate(COVID_ADJ = ifelse(TIME == "2020-Q2", 'Strong',
                            ifelse((TIME >= "2020-Q4" & TIME <= "2021-Q1"), 'Medium',
                                   ifelse((TIME == "2020-Q1" | TIME == "2020-Q3" | TIME == "2021-Q2"), 'Weak', 'No'))))
esRailAir$COVID_ADJ = factor(esRailAir$COVID_ADJ, ordered = TRUE, levels = c('No',"Weak", "Medium", "Strong"))
esCovidDummies = model.matrix(~esRailAir$COVID_ADJ - 1)
esCovidDummies = esCovidDummies[, -grep("No", colnames(esCovidDummies))]

# Same scale for Germany was used
deRailAir <- deRailAir %>%
  mutate(COVID_ADJ = ifelse((TIME == '2020-Q2' | TIME == '2021-Q1'), 'Strong',
                            ifelse((TIME >= '2020-Q3' & TIME <= '2020-Q4'), 'Medium',
                                   ifelse((TIME == '2021-Q3'| TIME == '2021-Q4'), 'Weak', 'No'))))

deRailAir$COVID_ADJ = factor(deRailAir$COVID_ADJ, ordered = TRUE, levels = c('No', 'Weak', 'Medium', 'Strong'))
deCovidDummies = model.matrix(~deRailAir$COVID_ADJ - 1)
deCovidDummies = deCovidDummies[, -grep('No', colnames(deCovidDummies))]

# Adding the intervention variable for the train discount
esRailAir = esRailAir %>%
  mutate(TRAIN_DISCOUNT = as.integer(TIME >= "2022-Q3"))
deRailAir = deRailAir %>%
  mutate(TRAIN_DISCOUNT = as.integer((TIME == "2022-Q3") | (TIME >= "2023-Q2")))

# Converting into time-series objects in case it is needed
esRailAirTS = ts(esRailAir[, c("R_MIO_PKM","R_THS_PAS", "A_FLIGHTS", "A_THS_PAS", 
                               "POPULATION", "COVID", "COVID_ADJ", "COVID_AVG_RESTRICTIONS","TRAIN_DISCOUNT",
                               "GDP", "GDP_PC", "RAIL_PRICE", "AIR_PRICE", "DOM_AIR_PRICE", 
                               "RAIL_DENSITY", "TOURISM_OCC", 
                               "QUARTER","TREND")],
                 start = c(2004, 1),
                 frequency = 4)
deRailAirTS = ts(deRailAir[, c("R_MIO_PKM","R_THS_PAS", "A_FLIGHTS", "A_THS_PAS", 
                               "POPULATION", "COVID", "COVID_ADJ", "COVID_AVG_RESTRICTIONS","TRAIN_DISCOUNT",
                               "GDP", "GDP_PC", "RAIL_PRICE", "AIR_PRICE", "DOM_AIR_PRICE", 
                               "RAIL_DENSITY", "TOURISM_OCC", 
                               "QUARTER","TREND")],
                 start = c(2004, 1),
                 frequency = 4)
itRailAirTS = ts(itRailAir[, c("R_MIO_PKM","R_THS_PAS", "A_FLIGHTS", "A_THS_PAS", 
                               "POPULATION", "COVID", "COVID_ADJ", "TRAIN_DISCOUNT",
                               "QUARTER","TREND")],
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


esRailAir = seasonal_adjustments(country_df=esRailAir, ts_data=esRailAirTS)
itRailAir = seasonal_adjustments(country_df=itRailAir, ts_data=itRailAirTS)
deRailAir = seasonal_adjustments(country_df=deRailAir, ts_data=deRailAirTS)

# Preliminary plots ----------------------------------------------------------------

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
                             lbl_axis1, lbl_axis2,scale_SecAxis = 1/10) {
  ggplot() +
    geom_line(data=df, aes(x=TIME, y=.data[[y_variable1]], group=1),
              color='blue', show.legend = FALSE)+
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
                             lbl_axis1='Rail - Thousands of passengers', lbl_axis2='Air - Thousands of passengers')
plotItPas = plot_competition(df=itRailAir, y_variable1='A_THS_PAS', y_variable2='R_THS_PAS',
                             lbl_country='Italy', lbl_plot='Passengers per million inhabitants', 
                             lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail - Thousands of passengers')
plotDePas = plot_competition(df=deRailAir, y_variable1='A_THS_PAS', y_variable2='R_THS_PAS',
                             lbl_country='Germany', lbl_plot='Passengers per million inhabitants',
                             lbl_axis1='Air - Thousands of passengers', lbl_axis2='Air - Thousands of passengers')
plotDePasScaled = plot_competition(df=deRailAir, y_variable1='A_THS_PAS', y_variable2='R_THS_PAS',
                                   lbl_country='Germany', lbl_plot='Passengers per million inhabitants (rescaled)',
                                   lbl_axis1='Air - Thousands of passengers', lbl_axis2='Rail - Thousands of passengers',
                                   scale_SecAxis = 1/100)

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


#### Models ####

#### Models for air passengers ####

# Baseline - Air passengers ----------------------------------------------------------------

# "Baseline" model: a linear regression with just trend, season, and 
# an intervention variable for the COVID. These are de 'basic' variables 
dePasBaseline = lm(data = deRailAir, A_THS_PAS ~ TREND + QUARTER + COVID)


## Graphically, it seems we can do better, but at least we are capturing some time-dependent patterns, specially seasonality.
## We also capture some of the COVID effect (but maybe this can be improved).
## There does not seem to be a strong trend in this case.
plot(deRailAir$A_THS_PAS, type = 'l', col = 'blue', xlab = TRUE, ylab = TRUE,
     main = 'Germany - Baseline', xaxt = 'n')
lines(fitted(dePasBaseline), col='black', lty = 'dashed')

q1_labels = deRailAir$TIME[grep('-Q1$', deRailAir$TIME)]
axis(1, at = which(deRailAir$TIME %in% q1_labels), labels = q1_labels,
     cex.axis = 1, las = 1)

legend('bottomleft', legend = c('Original data', 'Fitted baseline'),
       col = c('blue', 'black'), lty = c('solid', 'dashed'))

summary(dePasBaseline)
AICc(dePasBaseline)

## The residuals do seem to follow a kind of harmonic pattern, it is not white noise as we would wish
## And we still have significant autocorrelation for the first lags
plot(residuals(dePasBaseline))
tsdisplay(residuals(dePasBaseline), main = 'Germany - Baseline residuals and autocorrelation')

#____________________________________________________________
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

dePasArBaseline = reg_ARIMA_errors(df=deRailAir, y_col='A_THS_PAS', predictors = c('TREND', 'QUARTER', 'COVID'))
summary(dePasArBaseline)

plot(deRailAir$A_THS_PAS)
lines(deRailAir$A_THS_PAS)
lines(fitted(dePasBaseline), col=3)
lines(fitted(dePasArBaseline), col=2)

# Before adding new variable, we can try to check if the baseline version could be improved somehow


# Baseline without trend  - Air passengers ----------------------------------------------------------------

# Previous model suggested that the trend is not significant. Let's try a model without it
dePasBaselineNoTrend = lm(data = deRailAir, A_THS_PAS ~ QUARTER + COVID)
summary(dePasBaselineNoTrend)
AICc(dePasBaselineNoTrend)

# Also we fit the version with ARIMA errors
dePasArBaseNoTr = reg_ARIMA_errors(df=deRailAir, y_col='A_THS_PAS', predictors=c('QUARTER', 'COVID'))
summary(dePasArBaseNoTr)

# we compare the two ARIMA versions graphically
plot(deRailAir$A_THS_PAS)
lines(deRailAir$A_THS_PAS)
lines(fitted(dePasArBaseline), col=2)
lines(fitted(dePasArBaseNoTr), col=4)

# Seems that the trend does not add anything to our model, so we can choose the simpler model (without the trend)


# Baseline with COVID adjustment - Air passengers----------------------------------------------------------------

# We try with the alternative COVID formulation
dePasBaseCovAdj = lm(data = deRailAir, A_THS_PAS ~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR)

## Graphically, there seems to be an improvement regarding COVID.
#plot(deRailAir$A_THS_PAS)
#lines(deRailAir$A_THS_PAS)
#lines(fitted(dePasBaseCovAdj), col=5)

plot(deRailAir$A_THS_PAS, type = 'l', col = 'blue', xlab = 'TIME', ylab = 'Air - Thousands of passengers',
     main = 'Germany - Baseline', xaxt = 'n')
lines(fitted(dePasBaseCovAdj), col='black', lty = 'dashed')

q1_labels = deRailAir$TIME[grep('-Q1$', deRailAir$TIME)]
axis(1, at = which(deRailAir$TIME %in% q1_labels), labels = q1_labels,
     cex.axis = 1, las = 1)

legend('bottomleft', legend = c('Original data', 'Fitted baseline'),
       col = c('blue', 'black'), lty = c('solid', 'dashed'))

## We can capture the COVID effect more precisely (a coefficient for each "stage" of the COVID restrictions). 
## Also, the AICc is lower in this case even though we added some new features.
## This suggests that the adjusted version of the COVID variable is better, both in interpretability and in terms of fit.
summary(dePasBaseCovAdj)
AICc(dePasBaseCovAdj)
paste('% difference between AICc: ',
      round(100*(AICc(dePasBaseCovAdj) - AICc(dePasArBaseNoTr))/AICc(dePasArBaseNoTr), 2), '%')

durbinWatsonTest(dePasBaseCovAdj)

plot(residuals(dePasBaseCovAdj))
tsdisplay(residuals(dePasBaseCovAdj), main = 'Germany - Baseline residuals and autocorrelation')

dePasArBaseCovAdj = reg_ARIMA_errors(df=deRailAir, y_col='A_THS_PAS', predictors=c('QUARTER', 'COVID_AVG_RESTRICTIONS_FACTOR'))
summary(dePasArBaseCovAdj)

## We plot the baseline model with the simple COVID variable and with the adjusted COVID variable (both without a trend, since it did not show to be significant)
plot(deRailAir$A_THS_PAS)
lines(deRailAir$A_THS_PAS)
lines(fitted(dePasArBaseNoTr), col = 4)
lines(fitted(dePasArBaseCovAdj), col=6)

# As a result, we can use this last version (a model without trend and with the COVID adjustment) as 
# the "final version" of the baseline model. We can start adding variables on top of that.
dePasBaseFinal = dePasBaseCovAdj
dePasArBaseFinal = dePasArBaseCovAdj

#________________________________ADDING PREDICTORS____________________________

# Lasso - Air passengers 
reg_lasso <- function(df, y_col, predictors) {
  X = model.matrix(~ ., data = df[, predictors])
  X = X[, -1]
  y = df[, y_col]
  
  set.seed(123)
  grid = 10^seq(10, -2, length=100)
  
  lrLasso = cv.glmnet(X, y,
                      alpha=1, nfold=10, type.measure = 'deviance', lambda = grid)
  plot(lrLasso)
  bestLamLasso = lrLasso$lambda.min
  lasso_coef = predict(lrLasso, type='coefficients', s=bestLamLasso)[,]
  print(lasso_coef)
  
  lasso_coef = lasso_coef[lasso_coef != 0]
  return(lasso_coef)
}

dePasLassoCoef = reg_lasso(df=deRailAir, y_col='A_THS_PAS',
                           predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM','R_THS_PAS',
                                        'GDP_PC','AIR_PRICE','RAIL_PRICE','RAIL_DENSITY','TOURISM_OCC'))

#TODO: this must be done manually
dePasLasso = lm(data = deRailAir, A_THS_PAS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + AIR_PRICE + 
                  R_THS_PAS + GDP_PC + RAIL_DENSITY)
summary(dePasLasso)
AICc(dePasLasso)

plot(residuals(dePasLasso))
tsdisplay(residuals(dePasLasso))

# AICc improved w.r.t. other dynamic models
dePasArLasso = reg_ARIMA_errors(df=deRailAir, y_col='A_THS_PAS',
                                predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_THS_PAS',
                                             'GDP_PC', 'AIR_PRICE','RAIL_DENSITY'))
summary(dePasArLasso)

plot(deRailAir$A_THS_PAS)
lines(deRailAir$A_THS_PAS)
lines(fitted(dePasArBaseFinal), col=4)
lines(fitted(dePasArLasso), col=6)

vif(dePasLasso)

#_______________________________
# Adding predictors with the best subset selection - Air Passengers 
dePasBestObj = regsubsets(A_THS_PAS~., data = deRailAir[, c('A_THS_PAS','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM','R_THS_PAS',
                                                            'GDP_PC','AIR_PRICE','RAIL_PRICE','RAIL_DENSITY','TOURISM_OCC')])

plot(dePasBestObj, scale = 'adjr2')
plot(dePasBestObj, scale='Cp')
plot(dePasBestObj, scale = 'bic')

# TODO: this must be done manually according to what is obtained from the plots above.
dePasBest = lm(data = deRailAir, A_THS_PAS ~ QUARTER + R_THS_PAS + GDP_PC + AIR_PRICE + RAIL_PRICE + RAIL_DENSITY)
summary(dePasBest)
AICc(dePasBest)

plot(residuals(dePasBest))
tsdisplay(residuals(dePasBest))


dePasArBest = reg_ARIMA_errors(df=deRailAir, y_col='A_THS_PAS',
                               predictors = c('QUARTER','R_THS_PAS', 'GDP_PC', 'AIR_PRICE', 'RAIL_PRICE', 'RAIL_DENSITY'))
summary(dePasArBest)


plot(deRailAir$A_THS_PAS)
lines(deRailAir$A_THS_PAS)
lines(fitted(dePasArLasso), col=4)
lines(fitted(dePasArBest), col=6)



#### Adding seasonally adjusted predictors (approach including economic activity and other variables) ####

# Lasso - Air passengers - Seasonally adjusted ----------------------------------------------------------------
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
# Getting the coefficients of the best Lasso and passing them to a linear model to see their significance
# We can pass all the possible predictors
dePasLassoCoefSeasAdj = reg_lasso(df=deRailAir, y_col='A_THS_PAS', 
                           predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM_S_ADJ','R_THS_PAS_S_ADJ',
                                        'GDP_PC','AIR_PRICE_S_ADJ','RAIL_PRICE_S_ADJ','RAIL_DENSITY','TOURISM_OCC_S_ADJ'))

# TODO: this must be done manually according to what is obtained from the reg_lasso function.
# In this case, the resulting variables are really similar to what we saw with the non-seasonally adjusted versions
dePasLassoSeasAdj = lm(data = deRailAir, A_THS_PAS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + R_THS_PAS_S_ADJ + GDP_PC + AIR_PRICE_S_ADJ + RAIL_DENSITY)
summary(dePasLassoSeasAdj)
AICc(dePasLassoSeasAdj)

dePasLasso2SeasAdj = lm(data = deRailAir, A_THS_PAS ~ QUARTER + R_THS_PAS_S_ADJ + GDP_PC + AIR_PRICE_S_ADJ + RAIL_DENSITY)
summary(dePasLasso2SeasAdj)
AICc(dePasLasso2SeasAdj)

## Again, we do not have such a strong autocorrelation for the first lags, but we still need to fit the dynamic regression with ARIMA errors
plot(residuals(dePasLassoSeasAdj))
tsdisplay(residuals(dePasLassoSeasAdj))

plot(residuals(dePasLasso2SeasAdj))
tsdisplay(residuals(dePasLasso2SeasAdj))

plot(deRailAir$A_THS_PAS)
lines(deRailAir$A_THS_PAS)
lines(fitted(dePasArLassoSeasAdj), col=4)
lines(fitted(dePasArBestSeasAdj), col=6)

plot(residuals(dePasArBestSeasAdj))
tsdisplay(residuals(dePasArBestSeasAdj))


plot(deRailAir$A_THS_PAS, type = 'l', col = 'blue', xlab = 'TIME', ylab = 'Air - Thousands of passengers',
     main = 'Germany - Linear model', xaxt = 'n')
lines(fitted(dePasLassoSeasAdj), col='black', lty = 'dashed')
lines(fitted(dePasLasso2SeasAdj), col = 'green', lty = 'dashed')

q1_labels = deRailAir$TIME[grep('-Q1$', deRailAir$TIME)]
axis(1, at = which(deRailAir$TIME %in% q1_labels), labels = q1_labels,
     cex.axis = 1, las = 1)

legend('bottomleft', legend = c('Original data', 'Fitted with COVID', 'Fitted without COVID'),
       col = c('blue', 'black', 'green'), lty = c('solid', 'dashed', 'dashed'))

dePasArLassoSeasAdj = reg_ARIMA_errors(df=deRailAir, y_col='A_THS_PAS',
                                       predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_THS_PAS_S_ADJ','GDP_PC', 'AIR_PRICE_S_ADJ',
                                                    'RAIL_PRICE_S_ADJ', 'RAIL_DENSITY'))
summary(dePasArLassoSeasAdj)



# Graphically (with respect to the final baseline model), we do a better job at capturing the
# initial impact of COVID
plot(deRailAir$A_THS_PAS)
lines(deRailAir$A_THS_PAS)
lines(fitted(dePasArBaseFinal), col=4)
lines(fitted(dePasArLassoSeasAdj), col=6)



#  Best subset selection - Air passengers - Seasonally adjusted ----------------------------------------------------------------

# Given that we do not have many predictors, we can perform a best subset selection
# TODO: I think that TRAIN_DISCOUNT is confounding because it coincides with the end of COVID.
# The price reductions are better captured by the RAIL_PRICE variable. That is why TRAIN_DISCOUNT is not included
dePasBestObjectSeasAdj = regsubsets(A_THS_PAS~., data=deRailAir[,c('A_THS_PAS','QUARTER','COVID_AVG_RESTRICTIONS_FACTOR','R_MIO_PKM_S_ADJ','R_THS_PAS_S_ADJ',
                                                             'GDP_PC','AIR_PRICE_S_ADJ','RAIL_PRICE_S_ADJ','RAIL_DENSITY','TOURISM_OCC_S_ADJ')])


# From the Statistical Learning scripts:
# The regsubsets() function has a built-in plot() command  which can be used to display the selected variables for 
# the best model with a given number of predictors, ranked according to a chosen statistic. The top row of each plot 
# contains a black square for each variable selected according to the optimal model associated with that statistic.
plot(dePasBestObjectSeasAdj,scale="adjr2")
plot(dePasBestObjectSeasAdj,scale="Cp")
plot(dePasBestObjectSeasAdj,scale="bic")

# We can obtain the desired the best model by fitting the regression with the coefficients resulting from the plots
# Two of the criteria include R_MIO_KM, while the other one does not. I include it because of the criteria of the majority
# TODO: this must be done manually according to what is obtained from the plots above.
dePasBestSeasAdj = lm(data = deRailAir, A_THS_PAS ~ TREND + QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + GDP_PC + RAIL_PRICE_S_ADJ + RAIL_DENSITY)
summary(dePasBestSeasAdj)
AICc(dePasBestSeasAdj)
vif(dePasBestSeasAdj)

## This is the same model as the one obtained with Lasso, so I only do one plot (the one from the Lasso above)
dePasArBestSeasAdj = reg_ARIMA_errors(df=deRailAir, y_col='A_THS_PAS', 
                                      predictors=c('QUARTER', 'COVID_AVG_RESTRICTIONS_FACTOR', 'R_THS_PAS_S_ADJ',
                                                   'GDP_PC', 'AIR_PRICE_S_ADJ', 'RAIL_PRICE_S_ADJ','RAIL_DENSITY'))


plot(deRailAir$A_THS_PAS, type = 'l', col = 'blue', xlab = 'TIME', ylab = 'Air - Thousands of passengers',
     main = 'Germany - Final model with ARIMA errors', xaxt = 'n')
lines(fitted(dePasArBestSeasAdj), col = 'black', lty = 'dashed')

q1_labels = deRailAir$TIME[grep('-Q1$', deRailAir$TIME)]
axis(1, at = which(deRailAir$TIME %in% q1_labels), labels = q1_labels,
     cex.axis = 1, las = 1)

legend('bottomleft', legend = c('Original data', 'Fitted'),
       col = c('blue', 'black'), lty = c('solid', 'dashed'))



dePasSeas = reg_ARIMA_errors(df = deRailAir, y_col = 'A_THS_PAS',
                             predictors = c('QUARTER', 'COVID_AVG_RESTRICTIONS_FACTOR', 'R_THS_PAS_S_ADJ', 'AIR_PRICE_S_ADJ', 'GDP_PC',
                                            'RAIL_PRICE_S_ADJ','RAIL_DENSITY'))
summary(dePasArBestSeasAdj)
summary(dePasSeas)

plot(deRailAir$A_THS_PAS)
lines(deRailAir$A_THS_PAS)
lines(fitted(dePasArLassoSeasAdj), col=4)
lines(fitted(dePasArBestSeasAdj), col=6)

plot(residuals(dePasArBestSeasAdj))
tsdisplay(residuals(dePasArBestSeasAdj), main = 'Germany - Final model with ARIMA errors - Residuals and autocorrelation')

plot(residuals(dePasBestSeasAdjVIF))
tsdisplay(residuals(dePasBestSeasAdjVIF), main = 'Germany - Final linear model - Residuals and autocorrelation')


plot(deRailAir$A_THS_PAS, type = 'l', col = 'blue', xlab = 'TIME', ylab = 'Air - Thousands of passengers',
     main = 'Germany - Final Linear model', xaxt = 'n')
lines(fitted(dePasBestSeasAdjVIF), col = 'black', lty = 'dashed')

q1_labels = deRailAir$TIME[grep('-Q1$', deRailAir$TIME)]
axis(1, at = which(deRailAir$TIME %in% q1_labels), labels = q1_labels,
     cex.axis = 1, las = 1)

legend('bottomleft', legend = c('Original data', 'Fitted'),
       col = c('blue', 'black'), lty = c('solid', 'dashed'))
# Summary - Air passengers ---------------------------------------------------------------------------------------
# I re-print all the results just to be clear of what happened without browsing above.
# It is the results for the linear model and the dynamic regression with ARIMA errors 
# of the Lasso and the best subset selection approaches

# I also check for collinearity just in case


# With non-seasonally adjusted variables
summary(dePasLasso)
AICc(dePasLasso)
summary(dePasArLasso)
vif(dePasLasso)


summary(dePasBest)
AICc(dePasBest)
summary(dePasArBest)
vif(dePasBest)


# With seasonally adjusted variables
summary(dePasLassoSeasAdj)
AICc(dePasLassoSeasAdj)
summary(dePasArLassoSeasAdj)
vif(dePasLassoSeasAdj)


summary(dePasBestSeasAdj)
AICc(dePasBestSeasAdj)
summary(dePasArBestSeasAdj)
vif(dePasBestSeasAdj)

AICc(dePasArBestSeasAdj)
AICc(dePasArLasso)
AICc(dePasArBest)
AICc(dePasArLassoSeasAdj)
AICc(dePasArBestSeasAdjVIF)


# IT seems that there might be a collinearity issue.
# I'll remove the least significant variable just to test what happens
dePasBestSeasAdjVIF = lm(data = deRailAir, A_THS_PAS ~ QUARTER + COVID_AVG_RESTRICTIONS_FACTOR + AIR_PRICE_S_ADJ + RAIL_PRICE_S_ADJ + GDP_PC)
summary(dePasBestSeasAdjVIF)
AICc(dePasBestSeasAdjVIF)
vif(dePasBestSeasAdjVIF)

dePasArBestSeasAdjVIF = reg_ARIMA_errors(df=deRailAir, y_col='A_THS_PAS',
                                         predictors=c('QUARTER','COVID_AVG_RESTRICTIONS_FACTOR', 'R_THS_PAS_S_ADJ','GDP_PC','RAIL_DENSITY'))
summary(dePasArBestSeasAdjVIF)


# # So, the plot of the baseline model and the final model looks like this
plot(deRailAir$A_THS_PAS)
lines(deRailAir$A_THS_PAS)
lines(fitted(dePasArBaseFinal), col=4)
lines(fitted(dePasArBestSeasAdj), col=6)


#______-GDP _______
plotDeGDP = plot_competition(df=deRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'GDP_PC',
                             lbl_country = 'Germany', lbl_plot = 'GDP (per capita)',
                             lbl_axis1 = 'Air traffic', lbl_axis2 = 'GDP',
                             scale_SecAxis = 10)

plotEsGDP = plot_competition(df=esRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'GDP_PC',
                             lbl_country = 'Spain', lbl_plot = 'GDP (per capita)',
                             lbl_axis1 = 'Air traffic', lbl_axis2 = 'GDP',
                             scale_SecAxis = 100)

plotItGDP = plot_competition(df=itRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'GDP_PC',
                             lbl_country = 'Italy', lbl_plot = 'GDP (per capita)',
                             lbl_axis1 = 'Air traffic', lbl_axis2 = 'GDP',
                             scale_SecAxis = 10)
plotDeGDP
plotEsGDP
plotItGDP

#_____- Density _______

plotDeRDensity = plot_competition(df=deRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'RAIL_DENSITY',
                             lbl_country = 'Germany', lbl_plot = 'Rail density',
                             lbl_axis1 = 'Air traffic', lbl_axis2 = 'Rail Density',
                             scale_SecAxis = 1)

plotEsRDensity = plot_competition(df=esRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'RAIL_DENSITY',
                                  lbl_country = 'Spain', lbl_plot = 'Rail density',
                                  lbl_axis1 = 'Air traffic', lbl_axis2 = 'Rail Density',
                                  scale_SecAxis = 1)

plotItRDensity = plot_competition(df=itRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'RAIL_DENSITY',
                                  lbl_country = 'Italy', lbl_plot = 'Rail density',
                                  lbl_axis1 = 'Air traffic', lbl_axis2 = 'Rail Density',
                                  scale_SecAxis = 1)
plotDeRDensity
plotEsRDensity
plotItRDensity
#_____-
plotDeRMio = plot_competition(df=deRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'R_MIO_PKM',
                                  lbl_country = 'Germany', lbl_plot = 'Air passengers vs rail passengers - km per million inhabitants',
                                  lbl_axis1 = 'Air - Thousands of passengers', lbl_axis2 = 'Rail - Millions of passengers - km',
                              scale_SecAxis = 1)

plotEsRMio = plot_competition(df=esRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'R_MIO_PKM',
                              lbl_country = 'Spain', lbl_plot = 'Air passengers vs rail passengers - km per million inhabitants',
                              lbl_axis1 = 'Air - Thousands of passengers', lbl_axis2 = 'Rail - Millions of passengers - km',
                              scale_SecAxis = 1)

plotItRMio = plot_competition(df=itRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'R_MIO_PKM',
                              lbl_country = 'Italy', lbl_plot = 'Air passengers vs rail passengers - km per million inhabitants',
                              lbl_axis1 = 'Air - Thousands of passengers', lbl_axis2 = 'Rail - Millions of passengers - km',
                              scale_SecAxis = 1)
plotDeRMio
plotEsRMio
plotItRMio

#_____-
plotDeAirPrice = plot_competition(df=deRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'AIR_PRICE',
                              lbl_country = 'Germany', lbl_plot = 'Air traffic vs Air Price',
                              lbl_axis1 = 'Air traffic', lbl_axis2 = 'Air Price',
                              scale_SecAxis = 1)

plotEsAirPrice = plot_competition(df=esRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'AIR_PRICE',
                                  lbl_country = 'Spain', lbl_plot = 'Air traffic vs Air Price',
                                  lbl_axis1 = 'Air traffic', lbl_axis2 = 'Air Price',
                                  scale_SecAxis = 1)

plotItAirPrice = plot_competition(df=itRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'AIR_PRICE',
                                  lbl_country = 'Italy', lbl_plot = 'Air traffic vs Air Price',
                                  lbl_axis1 = 'Air traffic', lbl_axis2 = 'Air Price',
                                  scale_SecAxis = 1)
plotDeAirPrice
plotEsAirPrice
plotItAirPrice

#____- Rail Price _____

plotDeRailPrice = plot_competition(df=deRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'RAIL_PRICE',
                                lbl_country = 'Germany', lbl_plot = 'Air traffic vs Rail Price',
                                lbl_axis1 = 'Air traffic', lbl_axis2 = 'Rail Price',
                                scale_SecAxis = 1)

plotEsRailPrice = plot_competition(df=esRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'RAIL_PRICE',
                                   lbl_country = 'Spain', lbl_plot = 'Air traffic vs Rail Price',
                                   lbl_axis1 = 'Air traffic', lbl_axis2 = 'Rail Price',
                                   scale_SecAxis = 1)

plotItRailPrice = plot_competition(df=itRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'RAIL_PRICE',
                                   lbl_country = 'Italy', lbl_plot = 'Air traffic vs Rail Price',
                                   lbl_axis1 = 'Air traffic', lbl_axis2 = 'Rail Price',
                                   scale_SecAxis = 1)
plotDeRailPrice
plotEsRailPrice
plotItRailPrice

#____- Tourism _____-

plotDeTour = plot_competition(df=deRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'TOURISM_OCC',
                                lbl_country = 'Germany', lbl_plot = 'Tourism Occupation',
                                lbl_axis1 = 'Air traffic', lbl_axis2 = 'Tourism occupation',
                                scale_SecAxis = 1)

plotEsTour = plot_competition(df=esRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'TOURISM_OCC',
                              lbl_country = 'Spain', lbl_plot = 'Tourism Occupation',
                              lbl_axis1 = 'Air traffic', lbl_axis2 = 'Tourism occupation',
                              scale_SecAxis = 1)

plotItTour = plot_competition(df=itRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'TOURISM_OCC',
                              lbl_country = 'Italy', lbl_plot = 'Tourism Occupation',
                              lbl_axis1 = 'Air traffic', lbl_axis2 = 'Tourism occupation',
                              scale_SecAxis = 1)
plotDeTour
plotEsTour
plotItTour

# _________Covid ________-

plotDeCovid = plot_competition(df=deRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'COVID_AVG_RESTRICTIONS',
                                lbl_country = 'Germany', lbl_plot = 'Covid Restrictions',
                                lbl_axis1 = 'Air traffic', lbl_axis2 = 'Covid Restrictions',
                                scale_SecAxis = 10)

plotEsCovid = plot_competition(df=esRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'COVID_AVG_RESTRICTIONS',
                               lbl_country = 'Spain', lbl_plot = 'Covid Restrictions',
                               lbl_axis1 = 'Air traffic', lbl_axis2 = 'Covid Restrictions',
                               scale_SecAxis = 10)

plotItCovid = plot_competition(df=itRailAir, y_variable1 = 'A_THS_PAS', y_variable2 = 'COVID_AVG_RESTRICTIONS',
                               lbl_country = 'Italy', lbl_plot = 'Covid Restrictions',
                               lbl_axis1 = 'Air traffic', lbl_axis2 = 'Covid Restrictions',
                               scale_SecAxis = 10)
plotDeCovid
plotEsCovid
plotItCovid

#_______ Price vs Price _______- 

plotDePrices = plot_competition(df=deRailAir, y_variable1 = 'AIR_PRICE', y_variable2 = 'RAIL_PRICE',
                               lbl_country = 'Germany', lbl_plot = 'Prices - Harmonic Index of Consumer Prices (2015=100)',
                               lbl_axis1 = 'Air Prices', lbl_axis2 = 'Rail Prices',
                               scale_SecAxis = 1)

plotEsPrices = plot_competition(df=esRailAir, y_variable1 = 'AIR_PRICE', y_variable2 = 'RAIL_PRICE',
                                lbl_country = 'Spain', lbl_plot = 'Prices - Harmonic Index of Consumer Prices (2015=100)',
                                lbl_axis1 = 'Air Prices', lbl_axis2 = 'Rail Prices',
                                scale_SecAxis = 1)

plotItPrices = plot_competition(df=itRailAir, y_variable1 = 'AIR_PRICE', y_variable2 = 'RAIL_PRICE',
                                lbl_country = 'Italy', lbl_plot = 'Prices - Harmonic Index of Consumer Prices (2015=100)',
                                lbl_axis1 = 'Air Prices', lbl_axis2 = 'Rail Prices',
                                scale_SecAxis = 1)
plotDePrices
plotEsPrices
plotItPrices


plotallPrices = plot_allCountries(y_variable = 'RAIL_PRICE', lbl_axis = 'Price', lbl_plot = 'Rail Prices')
plotallPrices





