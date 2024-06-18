## plot skill metrics over doy

library(lubridate)
library(tidyverse)
library(ggpubr)
library(arrow)
library(scales)
library(scoringRules)
library(Metrics)


lake_directory <- here::here()

# some subsetting variables
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
horizons <- c(1:35)
sim_name <- 'SUNP_fcasts_temp_DO' 

# read in the scores and calculate variance
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", sim_name))

sc <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  collect() 



# vector of dates when obs are available (before buoy is taken into harbor)
##### make these the same dates for each year for equal comparison
buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))

sc <- sc %>% 
  mutate(doy = yday(datetime),
         year = year(datetime)) %>% 
  select(-c(family, site_id)) %>% 
  filter(horizon > 0,#) %>% 
         as.Date(reference_datetime) %in% buoy_dates,
         doy < 291) %>%
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

# convert oxy crps and obs to mg/L
sc <- sc %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000)),
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)),
         mean = ifelse(variable=='temperature', mean, (mean*32/1000)),
         sd = ifelse(variable=='temperature', sd, (sd*32/1000)))

# add month day
sc <- sc %>% 
  mutate(mo_day = format(datetime, "%m-%d"))


################################################3
# add mixing dates to dataframe
mix_21 <- "10-04"
mix_22 <- "09-23"

df <- sc %>% 
  select(datetime, variable, depth, crps, horizon) %>% 
  mutate(time = as.Date(datetime),
         year = year(datetime)) %>% 
  mutate(mix_date = ifelse(year=='2021', as.Date('2021-10-04'), as.Date('2022-09-23'))) %>% 
  ungroup() %>% 
  select(time, depth, year, horizon, variable, crps,  mix_date)

#####################################################################################
### calculate days before turnover to set x-axis
df <- df %>% 
  mutate(dbt = time -mix_date)

#####################################################################################
# plot select horizons

o1 <- df %>% 
  filter(horizon==1,
         variable=="oxygen") %>% 
  ggplot(aes(x = dbt, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_vline( aes(xintercept = 0))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (mg/L)') +
  ggtitle('Oxygen, 1 Day') +
  xlab('Days Before Turnover') +
  guides(linetype = "none") +
  theme_bw()
o1

t1 <- df %>% 
  filter(horizon==1,
         variable=="temperature") %>% 
  ggplot(aes(x = dbt, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_vline( aes(xintercept = 0))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  ylim(0, 2.2) +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (°C)') +
  ggtitle('Temperature, 1 Day') +
  xlab('Days Before Turnover') +
  guides(linetype = "none") +
  theme_bw()
t1

o7 <- df %>% 
  filter(horizon==7,
         variable=="oxygen") %>% 
  ggplot(aes(x = dbt, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_vline( aes(xintercept = 0))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  ylim(0, 2.3) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (mg/L)') +
  ggtitle('Oxygen, 7 Days') +
  xlab('Days Before Turnover') +
  guides(linetype = "none") +
  theme_bw()
o7

t7 <- df %>% 
  filter(horizon==7,
         variable=="temperature") %>% 
  ggplot(aes(x = dbt, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_vline( aes(xintercept = 0))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (°C)') +
  ylim(0, 2.2) +
  ggtitle('Temperature, 7 Days') +
  xlab('Days Before Turnover') +
  guides(linetype = "none") +
  theme_bw()
t7

ggarrange(t1, t7, o1, o7, common.legend = TRUE, nrow = 1)

o21 <- df %>% 
  filter(horizon==21,
         variable=="oxygen") %>% 
  ggplot(aes(x = dbt, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_vline( aes(xintercept = 0))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  ylim(0, 2.3) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (mg/L)') +
  ggtitle('Oxygen, 21 Days') +
  xlab('Days Before Turnover') +
  guides(linetype = "none") +
  theme_bw()
o21

t21 <- df %>% 
  filter(horizon==21,
         variable=="temperature") %>% 
  ggplot(aes(x = dbt, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_vline( aes(xintercept = 0))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (°C)') +
  ylim(0, 2.2) +
  ggtitle('Temperature, 21 Days') +
  xlab('Days Before Turnover') +
  guides(linetype = "none") +
  theme_bw()
t21

ggarrange(t1, t7, t21, common.legend = TRUE, nrow = 1, align = "v")
ggarrange(o1, o7, o21, common.legend = TRUE, nrow = 1, align = "v")

######################################################################################################
md21 <- df %>% 
  filter(time %in% md$time,
         horizon==21) %>% 
  distinct(time, depth, horizon, variable, .keep_all = TRUE) %>% 
  mutate(mix_day = format(as.Date(time), "%m-%d"),
         year = year(time))


#######
# figures
brks <- seq.Date(as.Date('08-04', "%m-%d"), as.Date('10-17', '%m-%d'), by = "1 month")

t21 <- sc %>% 
  filter(horizon %in% c(21),
         variable=='temperature (C)',
         doy > 238) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  #geom_point(data = md21[md21$variable=='temperature (C)',], size = 3, color = 'black', shape = 8, aes(x = as.Date(mix_day, format = "%m-%d"), y = crps))  +
  geom_vline(data = md21[md21$variable=='temperature (C)',],  aes(xintercept = as.Date(mix_day, format = "%m-%d"), color = as.factor(year)))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (°C)') +
  ggtitle('Temperature, 21 Days') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  theme_bw()
t21
o21 <- sc %>% 
  filter(horizon %in% c(21),
         variable=='oxygen (mg/L)',
         doy > 238) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  #geom_point(data = md21[md21$variable=='oxygen (mg/L)',], size = 3, color = 'black', shape = 8, aes(x = as.Date(mix_day, format = "%m-%d"), y = crps))  +
  geom_vline(data = md21[md21$variable=='oxygen (mg/L)',],  aes(xintercept = as.Date(mix_day, format = "%m-%d"), color = as.factor(year)))  +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (mg/L)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  theme_bw() +
  ggtitle('Oxygen, 21 Days')
ggarrange(t21, o21, common.legend = TRUE)

minmax <-  sc %>% 
  filter(horizon %in% c(21),
         doy > 238) %>% 
  select(reference_datetime:crps, doy, year) %>% 
  group_by(variable, year, depth) %>% 
  mutate(min_doy = min(crps),
         max_doy = max(crps),
         change = max(crps) - min(crps)) %>% 
  distinct(variable, depth, year, .keep_all = TRUE)

minmax

#########################################################
md7 <- scjoin %>% 
  filter(time %in% md$time,
         horizon==7) %>% 
  distinct(time, depth, horizon, variable, .keep_all = TRUE) %>% 
  mutate(mix_day = format(as.Date(time), "%m-%d"),
         year = year(time))

t7 <- sc %>% 
  filter(horizon %in% c(7),
         variable=='temperature (C)',
         doy > 238) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  geom_vline(data = md7[md7$variable=='temperature (C)',],  aes(xintercept = as.Date(mix_day, format = "%m-%d"), color = as.factor(year)))  +
  #geom_point(data = md7[md7$variable=='temperature (C)',], size = 3, color = 'black', shape = 8, aes(x = as.Date(mix_day, format = "%m-%d"), y = crps))  +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (°C)') +
  ggtitle('Temperature, 7 Days') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  theme_bw()

o7 <- sc %>% 
  filter(horizon %in% c(7),
         variable=='oxygen (mg/L)',
         doy > 238) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  #geom_point(data = md7[md7$variable=='oxygen (mg/L)',],  size = 2, shape = 23, aes(x = as.Date(mix_day, format = "%m-%d"), y = crps, fill = as.factor(year)))  +
  geom_vline(data = md7[md7$variable=='oxygen (mg/L)',],  aes(xintercept = as.Date(mix_day, format = "%m-%d"), color = as.factor(year)))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (mg/L)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  theme_bw() +
  ggtitle('Oxygen, 7 Days')
o7

ggarrange(t7, o7, common.legend = TRUE)
ggarrange(t21, o21, common.legend = TRUE)

######################################
# horizon = 1
md1 <- scjoin %>% 
  filter(time %in% md$time,
         horizon==1) %>% 
  distinct(time, depth, horizon, variable, .keep_all = TRUE) %>% 
  mutate(mix_day = format(as.Date(time), "%m-%d"),
         year = year(time))

t1 <- sc %>% 
  filter(horizon %in% c(1),
         variable=='temperature (C)') %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  #geom_vline(data = md1[md1$variable=='temperature (C)',],  aes(xintercept = as.Date(mix_day, format = "%m-%d"), color = as.factor(year)))  +
  geom_point(data = md1[md1$variable=='temperature (C)',], size = 3, color = 'black', shape = 8, aes(x = as.Date(mix_day, format = "%m-%d"), y = crps))  +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (°C)') +
  ggtitle('Temperature, 1 Day') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  theme_bw()

o1 <- sc %>% 
  filter(horizon %in% c(1),
         variable=='oxygen (mg/L)') %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_point(data = md1[md1$variable=='oxygen (mg/L)',],  size = 3, color = 'black', shape = 8, aes(x = as.Date(mix_day, format = "%m-%d"), y = crps))  +
  #geom_vline(data = md7[md7$variable=='oxygen (mg/L)',],  aes(xintercept = as.Date(mix_day, format = "%m-%d"), color = as.factor(year)))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (mg/L)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  theme_bw() +
  ggtitle('Oxygen, 1 Day')
o1

ggarrange(t1, o1, common.legend = TRUE)

###############################################################################################
## misc figs below, not in MS
#################################################################################################################
sc %>% 
  filter(horizon %in% c(1)) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  ylab('Forecast Performance (CRPS)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  ggtitle('horizon = 1') +
  theme_bw()

sc %>% 
  filter(horizon %in% c(10)) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  ylab('Forecast Performance (CRPS)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  ggtitle('horizon = 10')+
  theme_bw()

sc %>% 
  filter(horizon %in% c(21)) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  ylab('Forecast Performance (CRPS)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  ggtitle('horizon = 21')+
  theme_bw()

sc %>% 
  filter(horizon %in% c(35)) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  ylab('Forecast Performance (CRPS)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  ggtitle('horizon = 35')+
  theme_bw()

##########################################################################
## bias
sc %>% 
  filter(horizon == 1) %>% 
ggplot(aes(x =  as.Date(mo_day, format = "%m-%d"), y = observation - mean, color = as.factor(year))) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_grid(depth~variable, scales = 'free')
  

##############################################################################
# misc figs

### select horizons for all metrics
ggplotly(sc %>% 
           filter(horizon %in% c(1, 7, 14, 21, 35)) %>% 
           ggplot(aes(x = doy, y = crps, color = as.factor(horizon), linetype = as.factor(year))) +
           geom_line() +
           facet_grid(depth~variable))

sc %>% 
  filter(horizon %in% c(1, 7, 14, 21, 35)) %>% 
  ggplot(aes(x = doy, y = logs, color = as.factor(horizon), linetype = as.factor(year))) +
  geom_line() +
  facet_grid(depth~variable)

#####################
# plot crps with pattern of observations from each year
sc %>% 
  filter(horizon %in% c(1)) %>% 
  ggplot(aes(x = doy, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'horizon', linetype = 'year') +
  facet_wrap(depth~variable, scales = 'free') +
  geom_line(aes(y = observation/5, color = as.factor(year), linetype = 'obs'))+
  scale_y_continuous(name = 'CRPS', sec.axis = sec_axis(trans = ~.*5, name = 'observations'))








