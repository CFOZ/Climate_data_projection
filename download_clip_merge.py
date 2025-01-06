import os
import requests
from netCDF4 import Dataset
import xarray as xr

# Configuration: Base settings
base_url = "https://nex-gddp-cmip6.s3.us-west-2.amazonaws.com/NEX-GDDP-CMIP6"
target_directory = r"D:\cwrs_climate_prjt1\01_fclimate_projection\01_input\01_gcm_output_global_netcdf"


# GCM and scenario parameters
gcm_name = "ACCESS-CM2"  # Example: ACCESS-CM2
scenario = "ssp585"  # Example: historical, ssp245, ssp585
variable = "pr"          # Example: pr, tas, etc.
folder_var = "ppt"#  Change for tmax, tmin
ensemble = "r1i1p1f1"    # Example: r1i1p1f1
year_range = range(2015, 2101)  # Example year range
target_output = r"D:\cwrs_climate_prjt1\01_fclimate_projection\01_input\02_extract_bias_corr_analysis"
output_folder = f"{target_output}/{folder_var}/GCM/{gcm_name}"
# Create directories if they don't exist
os.makedirs(target_directory, exist_ok=True)
os.makedirs(output_folder, exist_ok=True)

# Specify latitude and longitude bounds for clipping
lat_bounds = (29, 30)
lon_bounds = (81, 83)

# Time dimension name
time_dim_name = "time"

def download_and_slice_file(file_url, local_file_path):
    # Check if the file already exists
    if os.path.exists(local_file_path):
        print(f"File already exists: {local_file_path}. Skipping download.")
        return
    
    # Download the file
    print(f"Downloading: {file_url}")
    response = requests.get(file_url, stream=True)
    if response.status_code == 200:
        with open(local_file_path, "wb") as f:
            f.write(response.content)
        print(f"Downloaded: {local_file_path}")
    else:
        print(f"Failed to download {file_url}. HTTP Status: {response.status_code}")

def clip_nc(file_path, output_path, lat_bounds, lon_bounds):
    ds = xr.open_dataset(file_path, engine='netcdf4')
    clipped_ds = ds.sel(lat=slice(*lat_bounds), lon=slice(*lon_bounds))
    clipped_ds.to_netcdf(output_path)
    ds.close()


def merge_nc(file_paths, output_path):
    datasets = [xr.open_dataset(file_path) for file_path in file_paths]
    merged_ds = xr.concat(datasets, dim=time_dim_name)
    merged_ds.to_netcdf(output_path)
    for ds in datasets:
        ds.close()
    print(f"Merged file saved: {output_path}")

# Generate file list and download
file_list = [
    f"{variable}_day_{gcm_name}_{scenario}_{ensemble}_gn_{year}.nc"
    for year in year_range
]

for file_name in file_list:
    file_url = f"{base_url}/{gcm_name}/{scenario}/{ensemble}/{variable}/{file_name}"
    local_file_path = os.path.join(target_directory, file_name)
    download_and_slice_file(file_url, local_file_path)

# Clip each downloaded file
for filename in os.listdir(target_directory):
    if filename.endswith(".nc"):
        file_path = os.path.join(target_directory, filename)
        output_path = os.path.join(output_folder, f"clipped_{filename}")
        clip_nc(file_path, output_path, lat_bounds, lon_bounds)

# Merge all clipped files
clipped_files = [os.path.join(output_folder, f) for f in os.listdir(output_folder) if f.startswith("clipped_")]
merge_output_path = os.path.join(output_folder, f"{scenario}.nc")
merge_nc(clipped_files, merge_output_path)
