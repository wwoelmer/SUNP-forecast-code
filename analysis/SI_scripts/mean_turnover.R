# read in forecasts 40 days before turnover in each year, 
# calculate schmidt stability leading up to turnover
# OR calculate temp difference between surface and bottom (<1C is turnover)
# how many days ahead of turnover did forecasts predict <1C

library(tidyverse)
library(ggpubr)

lake_directory <- here::here()
folder <- 'SUNP_fcasts_temp_DO'
mix_dates <- c('2021-10-04', '2022-09-23')
start_dates <- as.Date(mix_dates) - days(40)

sub_dates <- c(seq.Date(start_dates[1], as.Date(mix_dates[1]) + 7, by = 'day'),
                    seq.Date(start_dates[2], as.Date(mix_dates[2]) + 7, by = 'day'))
sub_dates

forecast_folder <- file.path(lake_directory, 'forecasts/sunp/', folder)
filenames <- list.files(path = forecast_folder, pattern = "*.csv.gz")

# Extract dates from filenames using a regular expression
extract_dates <- function(filenames) {
  # Regular expression to match dates in YYYY-MM-DD format
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Extract dates from filenames
  date_strings <- regmatches(filenames, regexpr(date_pattern, filenames))
  
  # Convert the extracted date strings to Date objects
  extracted_dates <- as.Date(date_strings)
  
  return(extracted_dates)
}

extracted_dates <- extract_dates(filenames)
sub_files <- filenames[extracted_dates %in% sub_dates]

###########################################################
# some subsetting
vars <- c('temperature')
depths <- c(0.1, 10)
horizons <- c(1:35)


## read in teh first files
files1 <- read.csv(paste0(forecast_folder, "/", sub_files[1]))

# reaad in the first file
files1 <- files1 %>% 
  filter(variable %in% vars,
         forecast > 0,
         depth %in% depths) %>% 
  mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days')) %>% 
  pivot_wider(names_from = 'depth', values_from = 'prediction', names_glue = "depth_{depth}") %>% 
  group_by(horizon, reference_datetime) %>% 
  mutate(temp_diff = depth_0.1 - depth_10)


files_summ <- files1 %>% 
  group_by(horizon, reference_datetime, datetime) %>% 
  summarise(mean = mean(prediction)) %>% 
  ungroup() 

summ_wide <- files_summ %>% 
  pivot_wider(names_from = 'depth', values_from = 'mean', names_glue = "depth_{depth}") %>% 
  group_by(horizon, reference_datetime) %>% 
  mutate(temp_diff = depth_0.1 - depth_10)


# loop through the rest
for(i in 2:length(sub_files)){
  print(sub_files[i])
  temp <- read.csv(paste0(forecast_folder, "/", sub_files[i]))
  
  temp <- temp %>% 
    filter(variable %in% vars,
           forecast > 0,
           depth %in% depths) %>% 
    mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days'))
  
  if(temp$horizon[1] <1){
    temp$horizon <- round(temp$horizon)
  }

  
  temp_summ <- temp %>% 
    group_by(depth, variable, horizon, reference_datetime, datetime) %>% 
    summarise(mean = mean(prediction)) %>% 
    ungroup() 
  
  temp_wide <- temp_summ %>% 
    pivot_wider(names_from = 'depth', values_from = 'mean', names_glue = "depth_{depth}") %>% 
    group_by(horizon, reference_datetime) %>% 
    mutate(temp_diff = depth_0.1 - depth_10)
  
  summ_wide <- rbind(summ_wide, temp_wide)
}

summ_wide <- summ_wide %>% 
  mutate(year = year(reference_datetime))
  
summ_wide <- summ_wide %>% 
  mutate(mix_date = ifelse(year==2021, '2021-10-04', '2022-09-23'))


library(plotly)
ggplot(summ_wide, aes(x = as.Date(datetime), y = temp_diff, color = reference_datetime)) +
  geom_line() +
  geom_vline(aes(xintercept = as.Date(mix_date))) +
  geom_hline(aes(yintercept = 1)) +
  facet_wrap(~year, scales = 'free') +
  theme_bw() +
  theme(legend.position = "none") +
  ylab('Predicted difference between 0.1 m and 10 m temperature')

dates21 <- seq.Date(as.Date(unique(summ_wide$mix_date[1])), as.Date(unique(summ_wide$mix_date[1])) + 7, by = 'day')
dates22 <- seq.Date(as.Date(unique(summ_wide$mix_date)[2]), as.Date('2022-09-23') + 7, by = 'day')
dates <- c(dates21, dates22)


turn <- summ_wide %>% 
  filter(as.Date(datetime) %in% unique(mix_dates)) %>% 
  mutate(horizon = round(horizon),
         days_before_turnover = as.Date(mix_date) - as.Date(datetime))

ggplot(turn, aes(x = horizon, y = temp_diff)) +
  geom_point() +
  scale_x_reverse() +
  facet_wrap(~year, scales = 'free')



ggplot(turn, aes(x = horizon, y = temp_diff)) +
  geom_point() +
  geom_line() +
  scale_x_reverse() +
  geom_hline(aes(yintercept = 1)) +
  facet_wrap(~year, scales = 'free') +
  xlab('Days before turnover') +
  ylab('Predicted difference between 0.1 m and 10 m temperature') +
  theme_bw()





obs <- read.csv('./targets/sunp/SUNP_fcasts_temp_DO/sunp-targets-insitu.csv') %>% 
  filter(as.Date(time) > as.Date('2021-06-08'),
         variable=='temperature') %>% 
  mutate(year = year(time),
         date = as.Date(time))


obs <- obs %>% 
  filter(year!='2023') %>% 
  group_by(date) %>% 
  arrange(depth) %>% 
  filter(depth == 10| depth ==0.1) %>% 
  pivot_wider(names_from = 'depth', values_from = 'observed', names_glue = "depth_{depth}") %>% 
  mutate(temp_diff = depth_0.1 - depth_10)

ggplot(obs, aes(x = as.Date(date), y = temp_diff)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept = 1)) +
  facet_wrap(~year, scales = 'free')

