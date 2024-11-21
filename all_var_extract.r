library(writexl)
library(readxl)
library(ncdf4)
library(fields)
library(ncdf4.helpers)
library(stringr)
### Input Section to be updated by the user
input_location="C:\\Users\\Asus\\Desktop\\Climate change Training\\01_fclimate_projection\\01_input\\02_extract_bias_corr_analysis"
output_location="C:\\Users\\Asus\\Desktop\\Climate change Training\\01_fclimate_projection\\03_output\\02_extract_bias_corr_analysis"
hist_start=1980
hist_end=2014
future_start=2015
future_end=2100
# Creating a sample list
my_dict <- list(
  ppt = "pr",
  tmax = "tasmax",
  tmin = "tasmin"
)
vars <- names(my_dict) ## To be defined based on your folder name
scenarios=c('historical','ssp245','ssp585') # Should match with the name of nc file in input section
#### Code Start
#var='ppt'
for (var in vars){
  met_folder=file.path(input_location,var,"Met_Station_list.csv")
  stn_loc=read.csv(met_folder)
  GCM_dir=file.path(input_location,var,"GCM")
  gcm <- basename(list.dirs(GCM_dir, recursive = FALSE, full.names = FALSE))
  for (k in 1:length(gcm)){
    gcm_folder=file.path(input_location,var,'GCM',gcm[k])
    for (scenario in scenarios){
      scenario_path=file.path(gcm_folder,paste0(scenario,".nc"))
      f1=nc_open(scenario_path)
      calendar <- ncatt_get(f1, "time", "calendar")
      calendar1=calendar$value
      if (calendar1=='julian'){
        time <- ncvar_get(f1, varid = "time")
        obs_unit <- ncatt_get(f1, "time", "units")
        obs_unit_value=obs_unit$value
        # Remove the "days since " part
        numeric_part <- gsub("days since ", "", obs_unit_value)
        
        # Convert time to Date format
        date_hist <- as.Date(time-1, origin =numeric_part)  #numeric_part
        
      } else{
        date_hist <- nc.get.time.series(f = f1,time.dim.name = "time")
      }
      if (scenario=='historical'){
        start_year=hist_start
        end_year=hist_end
      }else{
        start_year=future_start
        end_year=future_end
      }
      start_time_index <- which(substr(date_hist, 1, 4) == as.character(start_year))
      end_time_index <- which(substr(date_hist, 1, 4) == as.character(end_year))
      date_req=date_hist[start_time_index[1]:end_time_index[length(end_time_index)]]
      date_req <- sub("^(\\d{4}-\\d{2}-\\d{2}).*", "\\1", date_req)
      # Calculate the count for the time dimension
      time_count <- end_time_index[length(end_time_index)] - start_time_index[1] + 1
      lat=ncvar_get(f1,"lat")
      lon=ncvar_get(f1,"lon")
      for(i in 1:nrow(stn_loc)){
        stn_lat=stn_loc[i,3]
        stn_lon=stn_loc[i,4]
        sq_diff_lat=(lat-stn_lat)**2
        lat_index=which.min(sq_diff_lat)
        sq_diff_lon=(lon-stn_lon)**2
        lon_index=which.min(sq_diff_lon)
        data=ncvar_get(f1,my_dict[[var]],start = c(lon_index,lat_index,start_time_index[1]),count=c(1,1,time_count))   #change needed
        if (var=='ppt'){
          data=round(data*86400,1)
        }else{
          data=round(data-273.15,2)
        }
        #change to mm unit from kg/m2/s
        #data=data*86400
        date_req=cbind(date_req,data)
      }
      colnames(date_req)=c("Date",stn_loc$INDEX)
      max_temp=as.data.frame(date_req)
      numeric_columns <- names(max_temp)[-1]  # Exclude the first column (Date)
      max_temp[, numeric_columns] <- lapply(max_temp[, numeric_columns], as.numeric)
      output_folder=file.path(output_location,var,'GCM',gcm[k])
      # Check if the folder exists, and create it if not
      if (!file.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
      }
      write_xlsx(max_temp,paste0(output_folder,"\\",scenario,"_raw.xlsx"))
    }
  }
}


