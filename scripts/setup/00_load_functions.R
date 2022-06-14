#-------------------------------------------------------------------------------
# Functions from Anne
#-------------------------------------------------------------------------------
#### Function that for each extract returns number of plumes, used in loop ####
get_plumes = function(dt) {
  dt = as.data.table(dt)
  dt = dt[, .(light=sum(Density==5), medium=sum(Density==16), 
              dense=sum(Density==27), total=.N), by=date]
  dt = dt[, total := rowSums(.SD), .SDcols=2:4]
  return(dt)
}

#### For plumes before 2011 ####
get_plumes_pre2011 = function(dt) {
  dt = as.data.table(dt)
  dt = dt[, .(smoke_day=1, total=.N), by=date]
  return(dt)
}

#### Anne's ggplot theme ####
theme_anne = function(font="Avenir", size=10) {
  theme_tufte(base_size=size, base_family=font) %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="transparent", colour=NA), 
      axis.line.x = element_line(color="black", size = .2), 
      axis.line.y = element_line(color="black", size = .2), 
      plot.title = element_text(hjust = 0.5)
    )
}

#-------------------------------------------------------------------------------
# Functions from Jessica
#-------------------------------------------------------------------------------
#### Print time ####
print_time <- function(start, unit = "auto", message = "Time elapsed:") {
  end <- Sys.time()
  d <- difftime(time1 = end, time2 = start, units = unit)
  t <- round(d, digits = 1)
  u <- units(d)
  
  print(paste("Start time:", start))
  print(paste("End time:", end))
  message <- paste(message, t, u)
  print(message)
  return(d)
}

#### Get the start time ####
get_start_time <- function(message = "Time started:") {
  t <- Sys.time()
  print(paste(message, t))
  return(t)
}

#### Split data into chunks ####
split_chunks <- function(v, chunks) {
  return(split(v, cut(seq_along(v), chunks, labels = FALSE)))
}


#-------------------------------------------------------------------------------
# Functions from Sam
#-------------------------------------------------------------------------------
#### Wrapper to execute the clean_sensor() function ####
run_clean_stations <- function(stationid, pa_path = pa_path){
  clean_sensor(read_csv(
    paste(pa_path,stationid,".csv",sep=""), 
    col_types = "cnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnicnncc", 
    progress = F))
}


#### Functions required for clean_sensor() ####
clean_sensor <- function(sensor.file, 
                         aggregate = 60, 
                         ab_tolerance_scale = 0.1, 
                         ab_tolerance_level = 10, 
                         spike.tolerance = 200, 
                         topcut = 1500){
  if(nrow(sensor.file) == 0){
    output.data <- data.frame(Name = NA, 
                              ID = NA, 
                              Location = NA, 
                              Lon = NA, 
                              Lat = NA, 
                              year = NA, 
                              month = NA, 
                              day = NA, 
                              hour = NA, 
                              time_hours= NA, 
                              temperature = NA, 
                              humidity = NA, 
                              pressure = NA, 
                              dropped_ab_scale = NA, 
                              dropped_ab_level = NA, 
                              dropped_tso = NA, 
                              dropped_pm25 = NA, 
                              pm25 = NA, 
                              dual_sensor = NA)
  } else {
    dualsensor <- TRUE
    if(sum(!is.na(sensor.file[,"PM2.5_CF_ATM_ug/m3_B"])) == 0) {
      dualsensor <- FALSE
    }
    
    # [1] Read in and clean up data
    output.data <- sensor.file %>% 
      mutate(
        time = ymd_hms(as.character(created_at)), 
        time_hours = ceiling_date(time, unit = "hours")
      ) %>% 
      dplyr::select(Name, ID, Location, Lon, Lat, starts_with("time"), 
                    temperature = Temperature_F_A, 
                    humidity = `Humidity_%_A`, 
                    pressure = Atmos_Pres_B,starts_with("PM2.5"), 
                    ends_with("dl_A"),ends_with("dl_B")) %>% 
      rename(pm25_cf_a = "PM2.5_CF_1_ug/m3_A", pm25_atm_a = "PM2.5_CF_ATM_ug/m3_A",pm25_cf_b = "PM2.5_CF_1_ug/m3_B",pm25_atm_b = "PM2.5_CF_ATM_ug/m3_B" ,
             pc0_3_a = "0.3um/dl_A", pc0_5_a = "0.5um/dl_A", pc1_0_a ="1.0um/dl_A", pc2_5_a ="2.5um/dl_A", pc5_0_a ="5.0um/dl_A" , pc10_0_a = "10.0um/dl_A"  ,
             pc0_3_b = "0.3um/dl_B", pc0_5_b = "0.5um/dl_B", pc1_0_b ="1.0um/dl_B", pc2_5_b ="2.5um/dl_B", pc5_0_b ="5.0um/dl_B" , pc10_0_b = "10.0um/dl_B")
    
    output.data$dual_sensor <- as.numeric(sum(is.na(output.data$pm25_cf_b)) < nrow(output.data))
    
    # [2] Check for consistency across sensors in outdoor sensors and assign PM 
    # from obs with appropriate correction (varies for indoor vs outdoor)
    
    if (dualsensor == T) {
      # New ones are outdoor dual sensor sensors. Old ones may be single
      output.data <-  output.data %>% 
        rowwise() %>% # Ensure we pull lower PM value between cf and atm corrections since sometimes mislabeled
        mutate(
          pm25_a = min(c(pm25_atm_a, pm25_cf_a)),
          pm25_b = min(c(pm25_atm_b, pm25_cf_b))
        ) %>% 
        mutate(    
          diff_a_b_scale = abs(pm25_a - pm25_b)/pm25_a,
          diff_a_b_level = abs(pm25_a - pm25_b),
          pm25 = (pm25_a + pm25_b)/2,
          dropped_pm25 = pm25,
          dropped_scale = as.numeric(diff_a_b_scale > ab_tolerance_scale & pm25_a >= 100 & pm25_b >= 100 & !is.na(pm25_a) & !is.na(pm25_b) & !is.na(diff_a_b_scale) & pm25_a !=0),
          dropped_level = as.numeric(diff_a_b_level > ab_tolerance_level & (pm25_a < 100 | pm25_b < 100)& !is.na(diff_a_b_level) & !is.na(pm25_a) & !is.na(pm25_b)),
          pm25 = replace(pm25, dropped_level == 1 & dropped_scale == 1, NA), # Only drop if both levels and % off [follows Tryner et al 2020 + Borkjohn et al 2020]
          pm25 = replace(pm25, dual_sensor == 0, pm25_a[dual_sensor == 0]), # For sensors with 1 sensor just set to single sensor value
          dropped_pm25 = replace(dropped_pm25, dual_sensor == 0, pm25_a[dual_sensor == 0]),
          pc0_3 = (pc0_3_a + pc0_3_b)/2,
          pc0_5 = (pc0_5_a + pc0_5_b)/2,
          pc1_0 = (pc1_0_a + pc1_0_b)/2,
          pc2_5 =(pc2_5_a + pc2_5_b)/2,
          pc5_0 =(pc5_0_a + pc5_0_b)/2,
          pc10_0 =(pc10_0_a + pc10_0_b)/2,
          pc0_3 = replace(pc0_3, dropped_pm25 != pm25, NA),
          pc0_5 = replace(pc0_5, dropped_pm25 != pm25, NA),
          pc1_0 = replace(pc1_0, dropped_pm25 != pm25, NA),
          pc2_5 = replace(pc2_5, dropped_pm25 != pm25, NA),
          pc5_0 = replace(pc5_0, dropped_pm25 != pm25, NA),
          pc10_0 = replace(pc10_0, dropped_pm25 != pm25, NA)
        )
    } else { 
      # Indoor single sensor sensors
      output.data <-  output.data %>% 
        rowwise() %>% 
        mutate(
          pm25 = min(pm25_cf_a, pm25_atm_a),
          dropped_pm25 = pm25,
          pc0_3 = pc0_3_a,
          pc0_5 = pc0_5_a, 
          pc1_0 = pc1_0_a,
          pc2_5 = pc2_5_a, 
          pc5_0 =pc5_0_a, 
          pc10_0 =pc10_0_a,
          dropped_scale = 0, 
          dropped_level = 0,
        )
    }
    
    # [3] Filling missing values so we have 1 obs for every 10 minutes 
    # (Otherwise hard to detect outliers because adjacent obs could be different time periods)
    output.data <- output.data %>% 
      ungroup() %>% 
      tidyr::complete(time = seq(min(time), max(time), by = "10 min"), fill = list(value= NA))
    
    # [4] Check for outliers that are too big to be believable
    # Drop observations with extreme jumps up and back down (or down and back up) 
    # in sequential 10 minute intervals
    output.data <- output.data %>% 
      mutate(
        pmlag = lag(dropped_pm25), # Use the version of PM before any values were dropped (later on this variable is set to NA for values that weren't dropped but for now it's a complete record of all observed PM)
        pmlead = lead(dropped_pm25),# Use the version of PM before any values were dropped (later on this variable is set to NA for values that weren't dropped but for now it's a complete record of all observed PM)
        pmlagratio = pm25/pmlag,
        pmleadratio = pm25/pmlead,
        pmlagdiff = pm25-pmlag,
        pmleaddiff = pmlead-pm25,
        dropped_tso = as.numeric(((pmlagdiff > spike.tolerance & pmleaddiff < -spike.tolerance) | 
                                    (pmlagdiff < -spike.tolerance & pmleaddiff > spike.tolerance)) & 
                                   (!is.na(pmlagdiff) & !is.na(pmleaddiff))),
        pm25 = replace(pm25, dropped_tso == 1, NA),
        pc0_3 = replace(pc0_3, dropped_tso == 1, NA),
        pc0_5 = replace(pc0_5, dropped_tso == 1, NA),
        pc1_0 = replace(pc1_0, dropped_tso == 1, NA),
        pc2_5 = replace(pc2_5, dropped_tso == 1, NA),
        pc5_0 = replace(pc5_0, dropped_tso == 1, NA),
        pc10_0 = replace(pc10_0, dropped_tso == 1, NA)
      ) 
    
    
    # [6] Only keep what we need before aggregating
    output.data <- output.data %>% 
      dplyr::select(Name, ID, Location, Lon, Lat, time, time_hours, 
                    temperature, humidity,pressure, starts_with("dropped"), pm25, 
                    pc0_3, pc0_5, pc1_0, pc2_5, pc5_0, pc10_0)
    
    # [7] Drop anything above PA measurement threshold
    output.data <- output.data %>% 
      mutate(
        pm2high = as.numeric(pm25 > topcut)) %>% 
      mutate(
        pm25 = replace(pm25, pm2high == 1, NA),
        dropped_pm25 = replace(pm25, pm2high == 1, 500),
        pc0_3 = replace(pc0_3, pm2high==1, NA),
        pc0_5 = replace(pc0_5, pm2high==1, NA),
        pc1_0 = replace(pc1_0, pm2high==1, NA),
        pc2_5 = replace(pc2_5, pm2high==1, NA),
        pc5_0 = replace(pc5_0, pm2high==1, NA),
        pc10_0 = replace(pc10_0, pm2high==1, NA)
      ) #%>% 
    # mutate(dropped_pm25 = replace(dropped_pm25, !is.na(pm25), NA))
    
    # [8] Implement temp and humidity corrections from Barkjohn et al
    output.data <- output.data %>% 
      mutate(
        temperature = replace(temperature, temperature > 1000, NA), # Barkjohn et al recommendation page 4
        humidity = replace(humidity, humidity > 100 | humidity < 0, NA), # Drop invalid values for humidity
        pressure = replace(pressure,  pressure < 0, NA) # Drop invalid values for air pressure
      )
    
    # [9] Aggregate to hourly or daily
    if(aggregate == 60) {
      output.data <- output.data %>%
        group_by(Name, ID, Location, Lon, Lat, time_hours) %>%
        summarise(
          temperature = mean(temperature, na.rm = T),
          humidity = mean(humidity, na.rm = T),
          pressure = mean(pressure, na.rm = T),
          dropped_scale = sum(dropped_scale, na.rm = T), 
          dropped_level = sum(dropped_level, na.rm = T), 
          dropped_tso = sum(dropped_tso, na.rm = T), 
          dropped_topcut = sum(pm2high, na.rm = T),
          dropped_pm25 = mean(dropped_pm25, na.rm = T), 
          dropped_missing = sum(is.na(pm25) & 
                                  dropped_scale == 0 & 
                                  dropped_level == 0 & 
                                  dropped_tso==0 & 
                                  dropped_topcut==0),
          pm25 = mean(pm25), 
          pc0_3 = mean(pc0_3),
          pc0_5 = mean(pc0_5),
          pc1_0 = mean(pc1_0),
          pc2_5 = mean(pc2_5),
          pc5_0 = mean(pc5_0),
          pc10_0 = mean(pc10_0),
          .groups = 'drop') %>% 
        ungroup() %>% 
        tidyr::complete(time_hours = seq(min(time_hours, na.rm =T), 
                                         max(time_hours, na.rm = T), by = "60 min"), 
                        fill = list(value= NA)) %>% # Complete means everything gets is NA for new rows
        mutate(
          dropped_scale = replace(dropped_scale, is.na(dropped_scale), 0),
          dropped_level = replace(dropped_level, is.na(dropped_level), 0),
          dropped_tso = replace(dropped_tso, is.na(dropped_tso), 0),
          dropped_topcut = replace(dropped_topcut, is.na(dropped_topcut), 0),
          dropped_missing = replace(dropped_missing, is.na(dropped_missing), 6),
          year = as.numeric(format(as.Date(time_hours,format = "%Y-%m-%d" ), "%Y")),
          month = as.numeric(format(as.Date(time_hours,format = "%Y-%m-%d" ), "%m")),
          day = as.numeric(format(as.Date(time_hours,format = "%Y-%m-%d" ), "%d")),
          hour = as.numeric(hour(time_hours)),                   
          pm25 = replace(pm25, is.nan(pm25), NA),
          pc0_3 = replace(pc0_3, is.nan(pc0_3), NA),
          pc0_5 = replace(pc0_5, is.nan(pc0_5), NA),
          pc1_0 = replace(pc1_0, is.nan(pc1_0), NA),
          pc2_5 = replace(pc2_5, is.nan(pc2_5), NA),
          pc5_0 = replace(pc5_0, is.nan(pc5_0), NA),
          pc10_0 = replace(pc10_0, is.nan(pc10_0), NA),
          dropped_pm25 = replace(dropped_pm25, is.nan(dropped_pm25), NA),
          nodata = 0,
          nodata = replace(nodata, is.na(Name),1)
        ) %>% 
        dplyr::select(Name, ID, Location, Lon, Lat, 
                      year, month, day, hour, time_hours, 
                      temperature, humidity, pressure, 
                      dropped_ab_scale = dropped_scale, 
                      dropped_ab_level = dropped_level, 
                      dropped_tso,dropped_topcut,
                      dropped_missing, 
                      pm25_dropped = dropped_pm25, pm25, starts_with("pc"))
      
      output.data$Name <- output.data$Name[!is.na(output.data$Name)][1]
      output.data$Location <- output.data$Location[!is.na(output.data$Location)][1]
      #output.data$Lon <- output.data$Lon[!is.na(output.data$Lon)][1]
      #output.data$Lat <- output.data$Lat[!is.na(output.data$Lat)][1]
      output.data$ID <- output.data$ID[!is.na(output.data$ID)][1]
      output.data <- output.data %>% dplyr::filter(!is.na(year))
    }
    
    if (aggregate == 10) {
      output.data <- output.data %>%
        tidyr::complete(time = seq(min(time), max(time), by = "10 min"), fill = list(value= NA)) %>% 
        arrange(time) %>% 
        mutate(
          time_hours = ceiling_date(time, unit = "hours"),
          year = as.numeric(format(as.Date(time_hours,format = "%Y-%m-%d" ), "%Y")),
          month = as.numeric(format(as.Date(time_hours,format = "%Y-%m-%d" ), "%m")),
          day = as.numeric(format(as.Date(time_hours,format = "%Y-%m-%d" ), "%d")),
          hour = as.numeric(hour(time)),                   
          dropped_topcut = sum(pm2high, na.rm = T),
          pm25 = replace(pm25, is.nan(pm25), NA),
          dropped_pm25 = replace(dropped_pm25, is.nan(dropped_pm25), NA),
          nodata = 0,
          nodata = replace(nodata, is.na(Name),1)
        ) %>% 
        dplyr::select(Name, ID, Location, Lon, Lat, 
                      year, month, day, hour, time_hours, 
                      temperature, humidity, pressure, 
                      dropped_ab_scale = dropped_scale, 
                      dropped_ab_level = dropped_level, 
                      dropped_tso, 
                      dropped_pm25, pm25, starts_with("pc"))
      
      output.data$Name <- output.data$Name[!is.na(output.data$Name)][1]
      output.data$Location <- output.data$Location[!is.na(output.data$Location)][1]
      output.data$ID <- output.data$ID[!is.na(output.data$ID)][1]
    }
    output.data$dual_sensor <- dualsensor
    output.data <- output.data %>% dplyr::filter(!is.na(year))
  }
  return(output.data)
}

#### Function for downloading PurpleAir sensor data ####
# Source: https://github.com/jianzhaobi/bjzresc/blob/master/R/purpleairDownload.R
#' Download Purple Air PM2.5 data
#'
#' Download Purple Air PM2.5 data and save as csv files (each PurpleAir site per file). The indoor sites are not included by default.
#'
#' @param site.csv a data frame of a site list or a absolute path to the site CSV file (from \code{getPurpleairLst}).
#' @param start.date the beginning date in the format "YYYY-MM-DD".
#' @param end.date the end date in the format "YYYY-MM-DD".
#' @param output.path the path to output CSV files.
#' @param average get average of this many minutes, valid values: 10, 15, 20, 30, 60, 240, 720, 1440, "daily". "daily" is not recommended as the daily values can only be calculated at the UTC time.
#' @param time.zone time zone specification to be used for the conversion, but "" is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning. For more time zones, see \link{https://www.mathworks.com/help/thingspeak/time-zones-reference.html}.
#' @param indoor whether includes indoor sites (TRUE by default).
#' @param n.thread number of parallel threads used to download the data (1 by default).
#'
#' @examples
#' purpleairDownload(site.csv = '/absolute/path/to/the/sensorlist.csv',
#'     start.date = '2017-01-01',
#'     end.date = '2017-12-31',
#'     output.path = '/output_path',
#'     average = 60,
#'     time.zone = 'America/Los_Angeles')
#' @export

purpleairDownload <- function(site.csv, 
                              start.date, 
                              end.date, 
                              output.path, 
                              average, 
                              time.zone = 'GMT', 
                              indoor = T, 
                              n.thread = 1) {
  
  if (!require('httpuv')) {
    install.packages('httpuv')
    library(httpuv)
  }
  if (!require('foreach')) {
    install.packages('foreach')
    library(foreach)
  }
  if (!require('doMC')) {
    install.packages('doMC')
    library(doMC)
  }
  registerDoMC(n.thread)
  
  # Read the latest sensor list
  if (class(site.csv) == 'data.frame') {
    sites <- site.csv
  } else if (class(site.csv) == 'character') {
    sites <- read.csv(site.csv, as.is = T)
  } else {
    stop('Illegal CSV variable name!')
  }
  
  # Start date and end date
  start_date <- as.Date(start.date)
  end_date <- as.Date(end.date)
  
  # Output directory
  out.path <- output.path
  if (!file.exists(out.path)) {
    dir.create(out.path, recursive = T)
  }
  
  # Time zone
  timezone <- time.zone
  
  # Average level
  # Don't use 'daily' since the time is UTC !!!
  average <- average
  
  # Field names
  # Primary
  fieldnames.pri.A <- c("PM1.0_CF_1_ug/m3_A","PM2.5_CF_1_ug/m3_A","PM10.0_CF_1_ug/m3_A","Uptime_Minutes_A","RSSI_dbm_A","Temperature_F_A","Humidity_%_A","PM2.5_CF_ATM_ug/m3_A")
  fieldnames.pri.B <- c("PM1.0_CF_1_ug/m3_B","PM2.5_CF_1_ug/m3_B","PM10.0_CF_1_ug/m3_B","HEAP_B","ADC0_voltage_B","Atmos_Pres_B","Not_Used_B","PM2.5_CF_ATM_ug/m3_B")
  # Secondary
  fieldnames.sec.A <- c("0.3um/dl_A","0.5um/dl_A","1.0um/dl_A","2.5um/dl_A","5.0um/dl_A","10.0um/dl_A","PM1.0_CF_ATM_ug/m3_A","PM10_CF_ATM_ug/m3_A")
  fieldnames.sec.B <- c("0.3um/dl_B","0.5um/dl_B","1.0um/dl_B","2.5um/dl_B","5.0um/dl_B","10.0um/dl_B","PM1.0_CF_ATM_ug/m3_B","PM10_CF_ATM_ug/m3_B")
  
  #--------------Run--------------#
  # For each site
  foreach (i = 1 : nrow(sites)) %dopar% {
    
    if (!file.exists(file.path(out.path, paste(sites$ID[i], '.csv', sep = '')))) { # Skip existing files
      if ((is.na(sites$ParentID[i])) & (!is.na(sites$DEVICE_LOCATIONTYPE[i]))) { # Skip Channel B sensors
        if (indoor | sites$DEVICE_LOCATIONTYPE[i] == 'outside') { # Skip indoor sensors
          
          # --- Site information ---
          name <- trimws(sites$Label[i]) # Remove Leading/Trailing Whitespace
          Lat <- sites$Lat[i]
          Lon <- sites$Lon[i]
          Location <- sites$DEVICE_LOCATIONTYPE[i]
          Type <- sites$Type[i]
          # Channel A (Primary)
          ID.A <- sites$ID[i]
          channelID.A <- sites$THINGSPEAK_PRIMARY_ID[i]
          channelKey.A <- sites$THINGSPEAK_PRIMARY_ID_READ_KEY[i]
          channelID.A.sec <- sites$THINGSPEAK_SECONDARY_ID[i]
          channelKey.A.sec <- sites$THINGSPEAK_SECONDARY_ID_READ_KEY[i]
          # Channel B
          ib <- which(sites$ParentID == ID.A)
          if (length(ib) > 1) { # If there are multiple Channel B
            ib.min <- ib[which(sites$AGE[ib] == min(sites$AGE[ib]))]
            ib.min <- ib.min[1] # If there are multiple channel B with the same AGE, select the first one
            channelID.B <- sites$THINGSPEAK_PRIMARY_ID[ib.min]
            channelKey.B <- sites$THINGSPEAK_PRIMARY_ID_READ_KEY[ib.min]
            channelID.B.sec <- sites$THINGSPEAK_SECONDARY_ID[ib.min]
            channelKey.B.sec <- sites$THINGSPEAK_SECONDARY_ID_READ_KEY[ib.min]
          } else {
            channelID.B <- sites$THINGSPEAK_PRIMARY_ID[ib]
            channelKey.B <- sites$THINGSPEAK_PRIMARY_ID_READ_KEY[ib]
            channelID.B.sec <- sites$THINGSPEAK_SECONDARY_ID[ib]
            channelKey.B.sec <- sites$THINGSPEAK_SECONDARY_ID_READ_KEY[ib]
          }
          
          print(ID.A)
          
          # --- Channel A & B --- #
          # Initialization of primary data frame
          dat.final <- data.frame()
          
          for (j in 0 : as.numeric(end_date - start_date)) {
            
            this.day <- start_date + j
            cat(as.character(this.day), '\r')
            
            # --- Time range for a day --- #
            starttime <- httpuv::encodeURI(paste(this.day, '00:00:00')) # UTC Time !!!
            endtime <- httpuv::encodeURI(paste(this.day, '23:59:59')) # UTC Time !!!
            
            # --- URL --- #
            # Channel A
            url.csv.A <- paste("https://thingspeak.com/channels/", channelID.A, "/feed.csv?api_key=", channelKey.A, '&average=', average, "&round=3&start=", starttime, "&end=", endtime, '&timezone=', timezone, sep = '')
            url.csv.A.sec <- paste("https://thingspeak.com/channels/", channelID.A.sec, "/feed.csv?api_key=", channelKey.A.sec, '&average=', average, "&round=3&start=", starttime, "&end=", endtime, '&timezone=', timezone, sep = '')
            # Channel B
            url.csv.B <- paste("https://thingspeak.com/channels/", channelID.B, "/feed.csv?api_key=", channelKey.B, '&average=', average, "&round=3&start=", starttime, "&end=", endtime, '&timezone=', timezone, sep = '')
            url.csv.B.sec <- paste("https://thingspeak.com/channels/", channelID.B.sec, "/feed.csv?api_key=", channelKey.B.sec, '&average=', average, "&round=3&start=", starttime, "&end=", endtime, '&timezone=', timezone, sep = '')
            
            # --- Load CSV data --- #
            # Download URL A
            url.idx <- T
            while (url.idx) {
              try.txt <- try(expr = { # Try to connect the link
                dat.A <- read.csv(url.csv.A)
              }, silent = T)
              closeAllConnections()
              if (!inherits(try.txt, 'try-error')) url.idx <- F
            }
            url.idx <- T
            while (url.idx) {
              try.txt <- try(expr = { # Try to connect the link
                dat.A.sec <- read.csv(url.csv.A.sec)
              }, silent = T)
              closeAllConnections()
              if (!inherits(try.txt, 'try-error')) url.idx <- F
            }
            
            if (length(ib) != 0) { # If Channel B exists
              # Download URL B
              url.idx <- T
              while (url.idx) {
                try.txt <- try(expr = { # Try to connect the link
                  dat.B <- read.csv(url.csv.B)
                }, silent = T)
                closeAllConnections()
                if (!inherits(try.txt, 'try-error')) url.idx <- F
              }
              url.idx <- T
              while (url.idx) {
                try.txt <- try(expr = { # Try to connect the link
                  dat.B.sec <- read.csv(url.csv.B.sec)
                }, silent = T)
                closeAllConnections()
                if (!inherits(try.txt, 'try-error')) url.idx <- F
              }
            } else {
              dat.B <- dat.A
              dat.B.sec <- dat.A.sec
              if (nrow(dat.B) != 0) {
                dat.B[,] <- NA
                dat.B.sec[,] <- NA
                dat.B$created_at <- dat.A$created_at
                dat.B.sec$created_at <- dat.A.sec$created_at
              }
            }
            # Change the column names
            names(dat.A)[2 : ncol(dat.A)] <- c(fieldnames.pri.A)
            names(dat.A.sec)[2 : ncol(dat.A.sec)] <- c(fieldnames.sec.A)
            names(dat.B)[2 : ncol(dat.B)] <- c(fieldnames.pri.B)
            names(dat.B.sec)[2 : ncol(dat.B.sec)] <- c(fieldnames.sec.B)
            # Combine Channel A & B
            dat <- merge(dat.A, dat.A.sec, by = c('created_at'), all = T)
            dat <- merge(dat, dat.B, by = c('created_at'), all = T)
            dat <- merge(dat, dat.B.sec, by = c('created_at'), all = T)
            
            # --- Combine data frame --- #
            dat.final <- rbind(dat.final, dat)
            
          }
          
          # --- Add basic information --- #
          if (nrow(dat.final) != 0) {
            dat.final$ID <- ID.A
            dat.final$Name <- name
            dat.final$Lat <- Lat
            dat.final$Lon <- Lon
            dat.final$Location <- Location
            dat.final$Type <- Type
          }
          
          # --- Save CSV data --- #
          file.name <- paste(ID.A, '.csv', sep = '')
          write.csv(dat.final, file.path(out.path, file.name), row.names = F)
          
        } # if
      } # if
    } # if
    
  }
  #--------------Run--------------#
  
}


#### Function for downloading PurpleAir sensor list ####
# Source: https://github.com/jianzhaobi/bjzresc/blob/master/R/getPurpleairLst.R
#'
#' Get a list of the latest PurpleAir sensors from PurpleAir JSON (\link{https://www.purpleair.com/json}) and save as a CSV file.
#'
#' @param output.path the path of the output CSV file. By default, this variable equals \code{NULL} and the list is saved as a data frame.
#'
#' @return The latest PurpleAir sensor list as the data frame format
#'
#' @examples
#' # Save as a CSV file
#' getPurpleairLst('/absolute/path/to/the/csv/file')
#' # Save as a data frame variable
#' sensor.lst <- getPurpleairLst()
#' @export

getPurpleairLst <- function(output.path = NULL) {
  
  if(!require('jsonlite')) {
    install.packages('jsonlite')
    library(jsonlite)
  }
  
  # Make output path
  if(!is.null(output.path) && !file.exists(output.path)) {
    dir.create(output.path, recursive = T)
  }
  
  # Load JSON from URL
  idx <- T
  while(idx) {
    tryCatch(expr = {
      Sys.sleep(2) # Pause for 2 seconds to prevent HTTP Error 429
      json.file <- jsonlite::fromJSON('https://www.purpleair.com/json')
      idx <- F
    },
    error = function(e) {
      # print(e)
      Sys.sleep(2)
      idx <- T
    })
  }
  
  # Load sensor list
  sensor.df <- json.file$results
  # Write CSV
  if (!is.null(output.path)) {
    write.csv(sensor.df, file = paste(output.path, '/sensorlist', '_', Sys.Date(), '.csv', sep = ''), row.names = F)
  }
  
  return(sensor.df)
  
}

#### Add histograms to bottom of plots ####
#### Step 1: create data frame with hist heights ####
get_hst_obj <- function(values, min, max, width, cutoff = 1, type = "count") {
  x_hist_bar <- seq(min, max, width) 
  hist_ht <- data.frame(x_hist_bar - width/2 , x_hist_bar + width/2, NA)       
  for (i in 1:nrow(hist_ht)) {
    hist_ht[i,3] <- sum(values > hist_ht[i,1] & 
                          values <= hist_ht[i,2], na.rm = T)
  }
  hist_ht[1,3] <- hist_ht[1,3] + sum(values < hist_ht[1,1], na.rm = T)
  hist_ht[nrow(hist_ht),3] <- hist_ht[nrow(hist_ht),3] +  
    sum(values > hist_ht[nrow(hist_ht),2], na.rm = T)
  if (type == "share") {
    hist_ht[,3] <- hist_ht[,3]/sum(hist_ht[,3],na.rm = T)
  }
  names(hist_ht) = c("left", "right", "count")
  hist_ht <- subset(hist_ht, right <= quantile(as.matrix(values), cutoff, na.rm = T))
  return(hist_ht)
}

#### Step 1b: create data frame with hist heights for higher unit counting ####
get_hst_obj2 <- function(values, min, max, width, cutoff = 1,type = "count") {
  x_hist_bar <- seq(min, max, width) 
  hist_ht <- data.frame(x_hist_bar - width/2 , x_hist_bar + width/2, NA)
  values = values %>% dplyr::select(pre_exp, everything())
  for(i in 1:nrow(hist_ht)){
    hist_ht[i,3] <- values %>% 
      filter(pre_exp>=hist_ht[i,1] & 
               pre_exp<hist_ht[i,2]) %>% dplyr::select(2) %>%
      distinct() %>% 
      nrow()
  }
  hist_ht[1,3] <- hist_ht[1,3] + values %>% 
    filter(pre_exp<=hist_ht[1,1]) %>% 
    dplyr::select(2) %>% 
    distinct() %>% 
    nrow()
  hist_ht[nrow(hist_ht),3] <- hist_ht[nrow(hist_ht),3] + values %>% 
    filter(pre_exp>=hist_ht[nrow(hist_ht),1]) %>% 
    dplyr::select(2) %>% 
    distinct() %>% 
    nrow()
  if (type == "share") {
    hist_ht[,3] <- hist_ht[,3]/sum(hist_ht[,3],na.rm = T)
  }
  names(hist_ht) = c("left", "right", "count")
  hist_ht <- subset(hist_ht, right <= quantile(as.matrix(values$pre_exp), cutoff,na.rm = T))
  return(hist_ht)
}

#### Step 2: plot histogram at bottom of figure ####
plotHist <- function(hst_obj, col, alpha, bottom, height, norm, border.col = col) {
  rect(xleft = hst_obj[,1], xright =  hst_obj[,2], 
       ybottom = bottom, ytop = (hst_obj[,3]/norm)*height + bottom, 
       col = add.alpha(col, alpha), border = border.col)
}

#### More plotting functions ####
#### Plot y histogram ####
plotHistY <- function(hst_obj, col, alpha, bottom, height, norm, border.col = col) {
  rect(ybottom = hst_obj[,1], ytop = hst_obj[,2], 
       xleft = bottom, xright = (hst_obj[,3]/norm)*height + bottom, 
       col = add.alpha(col, alpha), border = border.col)
}

#### Add transparency to any color ####
add.alpha <- function(col, alpha = 1) {
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

#### Add correction to purple air data ####
pm_correction <- function(pm_raw, relative_humidity, temperature, correction = "AirNow"){
  if(correction %in% c("AirNow","Smoke")== F){stop("Not a valid correction. Please choose between `AirNow` and `Smoke` correction")}
  
  # Correction from Barkjohn et al 2020 used by AirNow: https://doi.org/10.5194/amt-2020-413
  if(correction == "AirNow"){
    pm_corrected <- 0.524*pm_raw - 0.0852*relative_humidity + 5.72 #equation (8) on page 11
  }
  
  # Smoke specific correction from Holder et al 2020: https://www.mdpi.com/1424-8220/20/17/4796
  if (correction == "Smoke"){
    pm_corrected <- 0.79*pm_raw + 0.3*temperature - 0.041*relative_humidity + -13.68 # Table (6) 4th row of PA (cf = ATM) section page 10 has highest r2 & min mae
  }
  
  return(pm_corrected)
}

#### Get pm2.5 from the particle counts ####
count2pm <- function(
  X0.3um.dl_A, X0.5um.dl_A, X1.0um.dl_A, X2.5um.dl_A,X5.0um.dl_A, X10.0um.dl_A
){
  N0.3A = as.numeric(X0.3um.dl_A)
  N0.5A = as.numeric(X0.5um.dl_A)
  N1A = as.numeric(X1.0um.dl_A)
  N2.5A = as.numeric(X2.5um.dl_A)
  N5A = as.numeric(X5.0um.dl_A)
  N10A = as.numeric(X10.0um.dl_A)
  
  N5A = N5A - N10A
  N2.5A = N2.5A - N5A
  N1A = N1A - N2.5A 
  N0.5A = N0.5A - N1A
  N0.3A = N0.3A - N0.5A 
  
  # Geometric mean
  d0.3 = sqrt(0.3 * 0.5)
  d0.5 = sqrt(0.5 * 1)
  d1 = sqrt(1 * 2.5)
  d2.5 = sqrt(2.5 * 5)
  d5 = sqrt(5 * 10)
  
  # If rho_H2O = 997 kg/m^3
  C0.3A = pi/6 * 10^-5 * 997 * N0.3A * d0.3^3
  C0.5A = pi/6 * 10^-5 * 997 * N0.5A * d0.5^3
  C1A = pi/6 * 10^-5 * 997 * N1A * d1^3
  C2.5A = pi/6 * 10^-5 * 997 * N2.5A * d2.5^3
  C5A = pi/6 * 10^-5 * 997 * N5A * d5^3
  
  PM1A = C0.3A + C0.5A
  PM2.5A = PM1A + C1A
  
  return(PM2.5A)
}
