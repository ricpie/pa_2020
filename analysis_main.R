# RP 2020.  AP?

# Let's look at PurpleAir indoor and outdoor and ratio data for COVID times compared to pre-covid times.  
# Maybe add in some infection/mortality/hospitalization/quarantine restrictions on top later.

library(ggplot2)
library(tidyverse)
library(tidyfast)
library(DT)
library(data.table)
library(tools)
library(plyr)
library(lubridate)


# Define rough regions. 
denver_lat_range = c(39,41)
denver_lon_range = c(-106,-104)
nyc_lat_range = c(40,42)
nyc_lon_range = c(-76,-73)


#Import PA data. Use api later?
all_pa_paths <- list.files(
  path = "pa_data",
  recursive = TRUE,
  full.names = TRUE
) %>%
  grep("csv", x = ., ignore.case = TRUE, value = TRUE) %>%
  grep("xls", x = ., ignore.case = TRUE, value = TRUE, invert = TRUE) %>%
  print()


#Import purpleair data, format the time stamp, get rid of cruft
import_pa <-function(x){
  asdf<-readr::read_csv(x,
                        col_names = TRUE,
                        col_types = paste0(paste0(replicate(1,'c'),collapse = ''),
                                           paste0(replicate(7,'d'),collapse = ''),'c',collapse = '')) %>%
    dplyr::mutate(filename = x,
                  trimmed_filename = sub(".+?.*\\(", "", file_path_sans_ext(basename(filename))),
                  location_name = sub("\\(.*$", "", file_path_sans_ext(basename(filename))),
                  trimmed_filename = sub("\\)", "", trimmed_filename),
                  trimmed_filename = sub("\\(", "", trimmed_filename),
                  trimmed_filename = sub("\\)", "", trimmed_filename)) %>% 
    separate(trimmed_filename,sep = " ",into = c("enviro","lat","lon","sensor","aveperiod","c","d")) %>% 
    dplyr::mutate(enviro = tolower(enviro))
}

pa_data <- ldply(all_pa_paths,import_pa,.parallel = TRUE) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(datetime = as.POSIXct(created_at,tz = "UTC"),
                lat = as.numeric(lat),
                lon = as.numeric(lon),
                year = as.factor(year(datetime))) %>% 
  dplyr::filter(`PM1.0_CF1_ug/m3` <4000,
                `PM2.5_CF1_ug/m3` <4000,
                `PM10.0_CF1_ug/m3`<4000) %>% 
  dplyr::select(-c("created_at","IAQ","ADC","c","d","UptimeMinutes")) %>% 
  dplyr::group_by(location_name,datetime,enviro) %>% 
  dplyr::mutate(`PM1.0_CF1_ug/m3` = mean(`PM1.0_CF1_ug/m3`,na.rm = T),
                `PM2.5_CF1_ug/m3` = mean(`PM2.5_CF1_ug/m3`,na.rm = T),
                `PM10.0_CF1_ug/m3` = mean(`PM10.0_CF1_ug/m3`,na.rm = T)) %>% 
  dplyr::filter(row_number()==1) %>% 
  dplyr::mutate(region = case_when(lat %between% denver_lat_range &
                                     lon %between% denver_lon_range ~ "denver",
                                   lat %between% nyc_lat_range &
                                     lon %between% nyc_lon_range ~ "nyc",
                                   TRUE ~ "Other"))


plot_timeseries <- pa_data %>% 
  dplyr::filter(!region %in% "Other") %>% 
  dplyr::group_by(datetime,region,enviro,year) %>% 
  dplyr::summarise(meanpm25 = mean(`PM2.5_CF1_ug/m3`,na.rm = T)) %>% 
  ggplot(aes(x=datetime,y=meanpm25,color = year)) + 
  geom_line(alpha = 0.1) +
  geom_smooth(alpha = 0.1) +
  facet_grid(region~enviro) +
  ylim(c(0,100)) +
  ggtitle("Regional averaged PM2.5")
plot_timeseries

plot_box <- pa_data %>% 
  dplyr::filter(!region %in% "Other") %>% 
  dplyr::group_by(datetime,region,enviro,year) %>% 
  dplyr::summarise(meanpm25 = mean(`PM2.5_CF1_ug/m3`,na.rm = T)) %>% 
  ggplot(aes(x=year,y=meanpm25)) + 
  geom_jitter(alpha = 0.1,width = .1) +
  geom_boxplot(alpha = 0.2) +
  ylim(c(0,100)) +
  facet_grid(region~enviro) 
plot_box


plot_tod <- pa_data %>% 
  dplyr::filter(region %in% "nyc") %>% 
  dplyr::group_by(datetime,region,enviro,year) %>% 
  dplyr::summarise(meanpm25 = mean(`PM2.5_CF1_ug/m3`,na.rm = T)) %>% 
  dplyr::mutate(hour = hour(datetime)) %>% 
  ggplot(aes(x=hour,y=meanpm25,color = region)) + 
  geom_smooth(alpha = 0.2) +
  ylim(c(0,30)) +
  facet_grid(year~enviro) 
plot_tod

plot_tod <- pa_data %>% 
  dplyr::filter(region %in% "denver") %>% 
  dplyr::group_by(datetime,region,enviro,year) %>% 
  dplyr::summarise(meanpm25 = mean(`PM2.5_CF1_ug/m3`,na.rm = T)) %>% 
  dplyr::mutate(hour = hour(datetime)) %>% 
  ggplot(aes(x=hour,y=meanpm25,color = region)) + 
  geom_smooth(alpha = 0.2) +
  ylim(c(0,30)) +
  facet_grid(year~enviro) 
plot_tod

