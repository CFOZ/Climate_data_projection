#### Code to download GCM for NEPAL
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.support.ui import Select
import xarray as xr
from io import BytesIO
import requests
from selenium.webdriver.common.by import By
import glob
# Set the path to the ChromeDriver executable
driver_path = "D:\\chrome_driver\\chromedriver.exe"

# Create a Service object
service = Service(driver_path)

# Start the WebDriver service
service.start()

# Create a WebDriver instance using the Service object
#options = webdriver.ChromeOptions()
#options.add_argument("--headless")
driver = webdriver.Chrome(service=service)
driver.maximize_window()
#driver.get("https://nex-gddp-cmip6.s3.us-west-2.amazonaws.com/index.html?fbclid=IwAR2DmLTQdB1g5-ROLMctOMz_tktQOd5PkOObM9kED2nwyPR1hw2EiZljIsE#NEX-GDDP-CMIP6/IITM-ESM/ssp585/r1i1p1f1/pr/")

# Find the dropdown element using its name attribute
#dropdown = Select(driver.find_element("name", "tb-s3objects_length"))

# Select the option with visible text '100'
#dropdown.select_by_visible_text('100')
#Object	Last Modified	Timestamp	Size
#pr_day_IITM-ESM_ssp585_r1i1p1f1_gn_2015.nc
#p1 = "pr_day_IITM-ESM_ssp585_r1i1p1f1_gn_"
#for number in range(2099,2101):
#    p2=p1+str(number)+'.nc'  
#    nc_link = driver.find_element("link text", p2)
#    #nc_link = driver.find_element(By.LINK_TEXT, p2)
    #nc_link = driver.find_element_by_link_text(p2)
 #   nc_url = nc_link.get_attribute('href')
 #   response = requests.get(nc_url)  #,timeout=60
 #   nc_content = response.content
 #   nc_data = xr.open_dataset(BytesIO(nc_content),engine='h5netcdf')
    # Define the latitude and longitude ranges for clipping
#    lat_range = (25, 35)
 #   lon_range = (78, 90)
    # Clip the NetCDF data based on the latitude and longitude ranges
#   clipped_data = nc_data.sel(lat=slice(*lat_range), lon=slice(*lon_range))
    # Save the clipped data to a new NetCDF file
 #   p3='D:\\new_gcm\\ppt\\ssp585\\IITM-ESM\\clipped_ppt'
 #   p4=str(number)
#    p5='.nc'
#    p6=p3+p4+p5
#    clipped_data.to_netcdf(p6)

###Merge Code
# Specify the directory where the NetCDF files are located
directory = 'D:\\new_gcm\\ppt\\historical\\bcc'

# Get a list of all NetCDF files in the directory
files = glob.glob(directory + '\\*.nc')

# Load the NetCDF files into a list of xarray Datasets
datasets = [xr.open_dataset(file) for file in files]

# Concatenate the datasets along the time dimension
merged_dataset = xr.concat(datasets, dim='time')

# Save the merged dataset to a new NetCDF file
merged_dataset.to_netcdf('D:\\new_gcm\\ppt\\historical\\bcc\\historical.nc')

#package_update
#pip install --upgrade h5py (also do for other packages such as numpy, scipy, h5netcdf, netcdf4, xarray)
