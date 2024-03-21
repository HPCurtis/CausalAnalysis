import pandas as pd
import numpy as np

def data_loader():
    berlin_url = "https://raw.githubusercontent.com/DominikDeffner/Cross-Cultural-Generalizability/main/data/Berlin-2020.csv"
    vanautu_ulr = "https://raw.githubusercontent.com/DominikDeffner/Cross-Cultural-Generalizability/main/data/Vanuatu-2019.csv"
    adult_url = "https://raw.githubusercontent.com/DominikDeffner/Cross-Cultural-Generalizability/main/data/Model_1a_1b_1c_data.csv"
    child_url = "https://raw.githubusercontent.com/DominikDeffner/Cross-Cultural-Generalizability/main/data/Model_4a_4b_4c_4d_data.csv"
    
    berlin_data = pd.read_csv(berlin_url)
    vanautu_data = pd.read_csv(vanautu_ulr)
    adult_data = pd.read_csv(adult_url)
    child_data = pd.read_csv(child_url)
    return berlin_data, vanautu_data, adult_data, child_data
 

def data_cleaner(berlin_data, vanautu_data, adult_data, child_data):
    # Remove empty data. 
    vanautu_data.drop(20, inplace=True)
    pop_berlin_raw = berlin_data.values[:,1:]
    pop_berlin = (pop_berlin_raw / np.sum(pop_berlin_raw)) * 100
    pop_vanautu_raw = vanautu_data.values[:,1:]
    pop_vanautu = (pop_vanautu_raw / np.sum(pop_vanautu_raw)) * 100
    
    # Select useful columns
    adult_data = adult_data[["SUBJECT_ID","GENDER_1female","fieldid", "AGE_in_years","T1_ad_choice_1yes"]]
    child_data = child_data[["SUBJECT_ID","GENDER_1female","fieldid", "AGE_in_years","T1_choice_1yes"]]
    
    adult_data["choice"] = adult_data["T1_ad_choice_1yes"]
    child_data["choice"] = child_data["T1_choice_1yes"]
    
    adult_data.drop("T1_ad_choice_1yes", inplace = True, axis =1 )
    child_data.drop("T1_choice_1yes", inplace = True, axis = 1)
    
    
    comb_data = pd.concat([child_data, adult_data], ignore_index=True)
    comb_data = comb_data.sort_values(by=['fieldid', 'AGE_in_years'])

    comb_data["gender"] = comb_data["GENDER_1female"] + 1
    comb_data.drop("GENDER_1female", inplace = True, axis =1 )
    MA = np.max(round(comb_data["AGE_in_years"]))
    d_berlin = comb_data[comb_data['fieldid'] == 1]
    d_vanautu = comb_data[comb_data['fieldid'] == 7]
    d_berlin["Age_binned"] = [np.argmax(1 / (np.arange(5, 101, 5) - age)) for age in d_berlin['AGE_in_years']]
    d_vanautu["Age_binned"] = [np.argmax(1 / (np.arange(5, 101, 5) - age)) for age in d_vanautu['AGE_in_years']]
    d_berlin["Age_binned"] = d_berlin["Age_binned"] + 1 
    d_vanautu["Age_binned"] = d_vanautu["Age_binned"] + 1
    return d_berlin, d_vanautu, MA,pop_berlin, pop_berlin_raw, pop_vanautu_raw, pop_vanautu

def sample_dist(d_berlin, d_vanautu):
    # Create matrices with zeros
    sample_berlin = np.zeros((20, 2))
    sample_vanuatu = np.zeros((20, 2))

    # Populate the matrices
    for i in range(1, 21):
        for g in range(0, 2):
            sample_berlin[i-1, g-1] = np.sum((d_berlin['Age_binned'] == i) & (d_berlin['gender'] == g))
            sample_vanuatu[i-1, g-1] = np.sum((d_vanautu['Age_binned'] == i) & (d_vanautu['gender'] == g))
    
    return sample_berlin, sample_vanuatu