import pandas as pd
import numpy as np


class DataCleaner:
    def __init__(self):
        pass
    
    # implementing a function to create dummies
    def to_dummies(self, raw_categories):
        stripped_categories = raw_categories.str.strip()
        categories = pd.Categorical(
            stripped_categories.astype('category'))
        dummies = categories.codes
        
        return dummies
    
    # cleaning data function
    def clean_data(self, raw_ceo_data):
        clean_ceo_data = pd.DataFrame(raw_ceo_data)
        
        # create dummies
        clean_ceo_data['GENDER']= pd.get_dummies(
            clean_ceo_data['GENDER'])
        
        clean_ceo_data['REASON']= pd.get_dummies(
            clean_ceo_data['REASON'])
        
        clean_ceo_data['SPCODE']= pd.get_dummies(
            clean_ceo_data['SPCODE'])
        
        clean_ceo_data['CEO_NEW']= pd.get_dummies(
            clean_ceo_data['CEO_NEW'])
        
        clean_ceo_data['CFO_NEW']= pd.get_dummies(
            clean_ceo_data['CFO_NEW'])
        
        #clean_ceo_data['STATE']=pd.get_dummies(
            # clean_ceo_data['STATE'])
        
        
        # replace the missing values with zeros
        clean_ceo_data['STATE'] = clean_ceo_data[
            'STATE'].replace(np.nan, 0)
        
        clean_ceo_data['BECAMECEO'] = clean_ceo_data[
            'BECAMECEO'].replace(np.nan, 0)
        
        clean_ceo_data['JOINED_CO'] = clean_ceo_data[
            'JOINED_CO'].replace(np.nan, 0)
        
        clean_ceo_data['LEFTOFC'] = clean_ceo_data[
            'LEFTOFC'].replace(np.nan, 0)
        
        clean_ceo_data['LEFTCO'] = clean_ceo_data[
            'LEFTCO'].replace(np.nan, 0)
        
        clean_ceo_data['REJOIN'] = clean_ceo_data[
            'REJOIN'].replace(np.nan, 0)
        
        clean_ceo_data['RELEFT'] = clean_ceo_data[
            'RELEFT'].replace(np.nan, 0)
        
        clean_ceo_data['REASON'] = clean_ceo_data[
            'REASON'].replace(np.nan, 0)
        
        return clean_ceo_data