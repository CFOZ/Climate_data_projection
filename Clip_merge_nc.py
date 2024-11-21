import xarray as xr
import os
# Input section to be defined by the user
#Input Folder Location
folder_path = "C:/Users/Asus/Desktop/Climate change Training/01_fclimate_projection/01_input/01_gcm_output_global_netcdf"
# Output_FolderLocation
output_folder = "C:/Users/Asus/Desktop/Climate change Training/01_fclimate_projection/03_output/01_gcm_local_netcdf"

# Specify latitude and longitude bounds for clipping
lat_bounds = (25, 35)
lon_bounds = (78, 91)

# Time Dimension Name
time_dim_name="time"

### Code Starts here
# Function to clip a NetCDF file based on latitude and longitude bounds
def clip_nc(file_path, output_path, lat_bounds, lon_bounds):
    ds = xr.open_dataset(file_path)    
    # Clip based on latitude and longitude bounds
    clipped_ds = ds.sel(lat=slice(*lat_bounds), lon=slice(*lon_bounds))
    
    # Save the clipped data to a new NetCDF file
    clipped_ds.to_netcdf(output_path)
    
    ds.close()

# Function to merge two NetCDF files into a single file
def merge_nc(file1_path, file2_path, output_path):
    ds1 = xr.open_dataset(file1_path)
    ds2 = xr.open_dataset(file2_path)
    
    # Merge the two datasets along the time dimension
    merged_ds = xr.concat([ds1, ds2], dim=time_dim_name)
    
    # Save the merged data to a new NetCDF file
    merged_ds.to_netcdf(output_path)
    
    ds1.close()
    ds2.close()



# Clip each file individually
for filename in os.listdir(folder_path):
    if filename.endswith(".nc"):
        file_path = os.path.join(folder_path, filename)
        output_path = os.path.join(output_folder, f"clipped_{filename}")
        clip_nc(file_path, output_path, lat_bounds, lon_bounds)

# List of clipped files
clipped_files = [os.path.join(output_folder, f"{filename}") for filename in os.listdir(output_folder)]

# Merge the clipped files into a single file
merge_output_path = os.path.join(output_folder, "merged_output.nc")
merge_nc(*clipped_files, merge_output_path)
