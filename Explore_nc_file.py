# -*- coding: utf-8 -*-
"""
Created on Thu Jan  2 17:50:01 2025

@author: Aayush
"""

import xarray as xr
import numpy as np
import pandas as pd

# Input section to be defined by the user
file_path = r'D:\cwrs_climate_prjt1\01_fclimate_projection\01_input\01_gcm_output_global_netcdf\pr_day_ACCESS-CM2_historical_r1i1p1f1_gn_1980.nc'

# Code starts here
ds = xr.open_dataset(file_path, engine='netcdf4')  # Specify the engine
print(ds)

lon_values = ds['lon'].values.tolist()
print(f"The longitude values are \n {lon_values}")

lat_values = ds['lat'].values.tolist()
print(f"The latitude values are \n {lat_values}")

# Assuming 'time' is a datetime64 variable
time_values = pd.to_datetime(ds['time'].values).strftime('%Y-%m-%d %H:%M:%S').tolist()
print(f"The time values are \n {time_values}")

z1 = ds['pr'][:, :, :]
