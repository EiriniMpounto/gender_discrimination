"WOMEN TRAVELLERS IN A MANS WORLD"

# import initial libraries
import os
import pandas as pd
import matplotlib.pyplot as plt

# import secondary libraries
from file_loader import DataLoader
from data_cleaner import DataCleaner
from statsmodels.tsa.stattools import kpss
from statsmodels.tsa.stattools import coint
from descriptive_statistics import DescriptiveStatistics


# print all the rows in the results
#pd.set_option('display.max_rows', None)

# load data function
data_loader = DataLoader()

# setting the output
working_directory = data_loader.working_directory
output_folder = os.path.join(working_directory, "Output")
os.chdir(working_directory)
os.makedirs(output_folder, exist_ok=True)

# load the data
raw_ceo_data = data_loader.ceo_official_data

# read the data
raw_ceo_data = pd.read_csv(raw_ceo_data, 
                           chunksize=10000, header=0)
raw_ceo_data = pd.concat(raw_ceo_data, 
                         ignore_index=True)
# print(raw_ceo_data)

# clean the data
clean_data_funtion = DataCleaner()
ceo_data = clean_data_funtion.clean_data(raw_ceo_data)
ceo_data.to_csv(os.path.join(output_folder, 
                             "./clean_ceo_data.csv"))
# print(ceo_data)

# Renaming the data for an easier calculation
trs1yr = ceo_data["TRS1YR"]
tdc2 = ceo_data["TDC2"]
prcc = ceo_data["PRCC"]
age = ceo_data["AGE"]
lagtrs1yr = ceo_data["LAG_TRS1YR"]
roa = ceo_data["ROA"]


# Start calculating the descriptives
data_counts = DescriptiveStatistics.data_counts(ceo_data)
gender_comparisons = DescriptiveStatistics.gender_comparisons(ceo_data)
ceo_gender_comparisons = DescriptiveStatistics.ceo_gender_comparisons(ceo_data)
statistical_calculations = DescriptiveStatistics.statistical_calculations(ceo_data)
