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
registerDoParallel(cores=detectCores()-1)


# Define rough regions. 
fence = data.frame(region = c("denver","nyc"),
                  denver_lat_range = c(39,41),
                  denver_lon_range = c(-104,-106),
                  nyc_lat_range = c(40,42),
                  nyc_lon_range = c(-74,-76))

#Import PA data. Use with api later?
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
    separate(trimmed_filename,sep = " ",into = c("enviro","lat","lon","sensor","aveperiod","c","d"))
}

pa_data <- ldply(all_pa_paths,import_pa,.parallel = TRUE) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(datetime = as.POSIXct(created_at,tz = "UTC"),
                lat = as.numeric(lat),
                lon = as.numeric(lon),
                year = year(datetime)) %>% 
  dplyr::select(-c("created_at","IAQ","ADC","c","d","UptimeMinutes")) %>% 
  dplyr::group_by(location_name,datetime) %>% 
  dplyr::mutate(`PM1.0_CF1_ug/m3` = mean(`PM1.0_CF1_ug/m3`,na.rm = T),
                `PM2.5_CF1_ug/m3` = mean(`PM2.5_CF1_ug/m3`,na.rm = T),
                `PM10.0_CF1_ug/m3` = mean(`PM10.0_CF1_ug/m3`,na.rm = T)) %>% 
  dplyr::filter(row_number()==1) %>% 
  dplyr::mutate(region = case_when(lat %between% fence$denver_lat_range &
                                     lon %between% fence$denver_lon_range ~ "denver",
                                   lat %between% fence$nyc_lat_range &
                                     lon %between% fence$nyc_lon_range ~ "nyc",
                                   TRUE ~ "Other"))


#Use the average of these channels as per Mailings et al. 2019b.  Also filter out data above 4000 when doing the average
# pa_data[, pm2_5_cf_1_ave := dt_case_when(pm2_5_cf_1 <4000 & pm2_5_cf_1_b<4000 ~ rowMeans(pa_data[,c('pm2_5_cf_1','pm2_5_cf_1_b')],na.rm = TRUE) , #PM25 kitchen uses mostly ECM, but some corrected PATS data.
#                                          pm2_5_cf_1 <4000  ~ pm2_5_cf_1,
#                                          pm2_5_cf_1_b <4000  ~ pm2_5_cf_1_b)]

plot_scatter_all <- pa_data %>% 
  ggplot(aes(x=datetime,y=`PM2.5_CF1_ug/m3`,color = )) + 
  geom_point(alpha = 0.1) +
  geom_smooth(alpha = 0.2) +
  facet_grid(year~enviro) +
  ggtitle(paste0(path_region," PM2.5"))
plot_scatter_all

