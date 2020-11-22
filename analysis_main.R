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


# Import data from region. Use with api later?
path_region = "denver"


#Import PA data
all_pa_paths <- list.files(
  path = path_region,
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
                  col_types = paste0(paste0(replicate(1,'c'),collapse = ''),paste0(replicate(7,'d'),collapse = ''),'c',collapse = '')
  ) %>%
    dplyr::mutate(filename = x,
                  trimmed_filename = sub(".*\\(", "", file_path_sans_ext(basename(filename))),
                  trimmed_filename = sub("\\)", "", trimmed_filename)) %>% 
    separate(trimmed_filename,sep = " ",into = c("lat","lon","sensor","aveperiod","c","d"))
}

pa_data <- ldply(all_pa_paths,import_pa,.parallel = TRUE) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(datetime = created_at,tz = "UTC",
                lat = as.numeric(lat),
                lon = as.numeric(lon)) %>% 
  dplyr::select(-created_at)
  as.data.table()

pa_data$UTCDateTime = gsub("T"," ", pa_data$UTCDateTime)
pa_data$UTCDateTime = gsub("z","", pa_data$UTCDateTime)
pa_data = pa_data[!grepl("[+]", pa_data$UTCDateTime),]
pa_data = pa_data[str_length(pa_data$UTCDateTime)>15,]
pa_data = pa_data[!grepl("[[:alpha:]]", pa_data$pm10_0_cf_1),]
pa_data = pa_data[!grepl("[[:alpha:]]", pa_data$pm2_5_cf_1_b),]
pa_data = pa_data[!grepl("[[:alpha:]]", pa_data$pm10_0_cf_1_b),]
pa_data = pa_data[!grepl("[[:alpha:]]", pa_data$pm2.5_aqi_atm),]
pa_data = pa_data[!grepl("[[:alpha:]]", pa_data$pm1_0_atm_b),]
pa_data = pa_data[!grepl("[[:alpha:]]", pa_data$p_10_0_um_b),]
pa_data = pa_data[pa_data$p_0_5_um != "",]


#Convert to numeric to get rid of strange characters in some cases.
# lst <- lapply(pa_data, grep, pattern="http", value=TRUE, invert=TRUE)
# temp = pa_data[,sapply(pa_data[5:41],
#               function(x) !grepl("[[:alpha:]]", x))]

# suppressWarnings(pa_data[,( colnames(pa_data)[5:41]):= lapply(.SD, as.numeric), .SDcols =  colnames(pa_data)[5:41]])

#Deal with timestamps
pa_data[,UTCDateTime := as.POSIXct(UTCDateTime,"UTC", "%Y/%m/%d %H:%M:%S")]
pa_data = na.omit(pa_data,"UTCDateTime")

#Merge inventory and data
pa_data = merge(pa_data,all_pa_inventories,by.x="mac_address",by.y = "instrument_id")

#Use the average of these channels as per Mailings et al. 2019b.  Also filter out data above 4000 when doing the average
pa_data[, pm2_5_cf_1_ave := dt_case_when(pm2_5_cf_1 <4000 & pm2_5_cf_1_b<4000 ~ rowMeans(pa_data[,c('pm2_5_cf_1','pm2_5_cf_1_b')],na.rm = TRUE) , #PM25 kitchen uses mostly ECM, but some corrected PATS data.
                                         pm2_5_cf_1 <4000  ~ pm2_5_cf_1,
                                         pm2_5_cf_1_b <4000  ~ pm2_5_cf_1_b)]

#Flag points above 4000 for inspection
pa_data[, flag_maxedout := ifelse(pm2_5_cf_1 > 4000 | pm2_5_cf_1_b > 4000, 1, 0)]
pa_data[, flag_sensor_diff := ifelse(abs(pm2_5_cf_1 - pm2_5_cf_1_b) < 5, 0,1)]
