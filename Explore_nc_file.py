import xarray as xr
import numpy as np
import pandas as pd
#Input section to be defined by the user
file_path='path to your nc file'
#Code starts here
ds=xr.open_dataset(file_path)
print(ds)
lon_values = ds['lon'].values.tolist()
print(f"The longitude values are \n {lon_values}")
lat_values = ds['lat'].values.tolist()
print(f"The latitude values are \n {lat_values}")
# Assuming 'time' is a datetime64 variable
time_values = pd.to_datetime(ds['time'].values).strftime('%Y-%m-%d %H:%M:%S').tolist()
print(f"The time are \n {time_values}")
z1=ds['pr'][:,:,:]
