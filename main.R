
#Load packages

install.packages('tidyverse')
library(tidyverse)
library(lubridate)

#read in data 

Global_means<-read_csv("~/Documents/Environmental_Informatics/git/global_climate_change_analysis", skip = 1)

global_temp_annual<- Global_means %>%
  select('Year', "J-D")colnames(global_temp_annual)<- c('year', 'annual_temp')

global_temp_annual <- global_temp_annual %>%
  mutate(annual_temp = as.numeric(annual_temp))

ggplot(data = global_temp_annual, aes(x = year, y = annual_temp)) +
  geom_smooth()+
  geom_point()+
  labs(title= 'Global Mean Temperature Change Over Time', 
       x = 'Time (yrs)', y = 'Annual Temperature')


# modeling data to get rate of change

temp_fit_line<- lm(annual_temp ~ year, global_temp_annual)
summary(temp_fit_line)$r.squared

#Model data
#Many scientists claim that drastic changes in global temperature 
#began around 1950 when fossil-fuel-powered transportation became a reality.
post_1950 <- global_temp_annual %>%
  filter(year > 1950)

rate_of_change_post1950 <- lm(formula = annual_temp ~ year, data = post_1950)

ggplot(data = post_1950, aes(x = year, y = annual_temp))+
  geom_abline(slope = rate_of_change_post1950$coefficients[2], intercept = rate_of_change_post1950$coefficients[1])
              geom_point()+
              geom_smooth()+
              labs(title = "Annual Temperature Change (1951-2017)",
                   x= "Year", y = "Annual Temperature Anomaly")
              

#reading in Mauna Loa c02 data              
co2_data <- read_delim('~/Documents/Environmental_Informatics/git/global_climate_change_analysis/co2_annmean_mlo.txt',delim =" ",comment = '#')

colnames(co2_data) <- c("year", "co2_mean","unc")

co2_data <- co2_data %>%
  mutate(co2_mean= as.numeric(co2_mean),
         year = as.numeric(year),
         unc = as.numeric(unc))

ggplot(data = co2_data,aes(x = year, y = co2_mean)) +
        geom_point()+
        geom_line()+
        labs(title = 'Atmospheric Concentrations in Mauna Loa, Hawaii (1959- 2019)', 
        x = 'Year', y = 'Mean C02')


#model data
co2_fit <- lm(co2_mean ~ year, data = co2_data)

summary(co2_fit)$r.square


co2_model_plot <- ggplot(data = temp_co2_data,
       aes(x = co2_mean, y = annual_temp))+
        geom_point()+
        labs(title = 'Relationship Between Mean CO2 Concentrations and Temperature Values', 
             y = 'Annual Tempefrom 1950 to 1980 (Celsius)', 
             x = 'C02 Concentration(ppm)')

#linear regression
co2_temp_fit <- lm(annual_temp ~ co2_mean, data = temp_co2_data)
co2_temp_fit

summary(co2_temp_fit)$r.squared


#vostok ice core data

vostok_temp_data <-read_delim('~/Documents/Environmental_Informatics/git/global_climate_change_analysis/ice_core_data', 
                                delim = '  ', skip =1)
vostok_temp_data <- vostok_temp_data %>%
  slice(-c(1,2))

colnames(vostok_temp_data) <- c("Depth(m)","ice_age", "delta_D", "Temperature variation")

vostok_temp_data<- vostok_temp_data %>
  %mutate(delta_D = as.numeric(delta_D),
          ice_age = as.numeric(ice_age))

head(vostok_temp_data)

#calculating temp
vostok_temp_data<- vostok_temp_data %>%
  mutate(temperature = -55.5 + (delta_D + 440) / 6)

vostok_plot <- ggplot(data = vostok_temp_data,
               mapping = aes(x = ice_age, y = temperature)) +
                geom_point()+
                labs(title = 'Pre-historic Ice-core temperature data' , 
                x = 'Age of ice (year bp)', 
                y= 'Temperature')



        
              