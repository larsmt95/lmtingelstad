# -*- coding: utf-8 -*-
"""
Created on Mon Mar  2 12:11:14 2026

@author: larsmt
"""

import os
print(os.getcwd())

## Koble til DB og hente data 

import pandas as pd
from sqlalchemy import create_engine
import statsmodels.api as sm
import numpy as np

engine = create_engine(
    "postgresql+psycopg2://postgres:legandary007@localhost:5432/e-commerce"
)

query = """
SELECT 
    low_rating_flag,
    delay_flag,
    total_order_value,
    product_category_name_english
FROM public.fact_orders_v5
"""

df = pd.read_sql(query, engine)

df = df.rename(columns={"product_category_name_english": "category"})

## Klargjøre variabler

df["category"] = df["category"].astype("category")
df["low_rating_flag"] = df["low_rating_flag"].astype(int)
df["delay_flag"] = df["delay_flag"].astype(int)

# Baseline
df["low_rating_flag"].mean()

## Logistisk regresjon

X = pd.get_dummies(
    df[["delay_flag", "total_order_value", "category"]],
    drop_first=True
)

X = sm.add_constant(X)

# Tving alt til numerisk 
    # R håndterer faktorer automatisk.
    # (Python krever eksplisitt numerisk designmatrise.)
    
X = X.astype(float)

y = df["low_rating_flag"].astype(int)

model = sm.Logit(y, X).fit()
print(model.summary())

# Odds ratios
np.exp(model.params)


## Lage makrokategorier
def map_category(cat):
    if cat in ["computers","computers_accessories","electronics",
               "telephony","tablets_printing_image","consoles_games",
               "audio","fixed_telephony"]:
        return "electronics_tech"

    elif cat in ["fashion_male_clothing","fashion_female_clothing",
                 "fashion_shoes","fashion_bags_accessories",
                 "fashion_underwear_beach","fashion_childrens_clothes",
                 "watches_gifts"]:
        return "fashion"

    elif cat in ["furniture_decor","furniture_living_room",
                 "furniture_bedroom","housewares","home_appliances",
                 "home_construction",
                 "kitchen_dining_laundry_garden_furniture",
                 "bed_bath_table","garden_tools"]:
        return "home_furniture"

    elif cat in ["health_beauty","perfumery",
                 "diapers_and_hygiene"]:
        return "beauty_health"

    elif cat in ["sports_leisure","toys",
                 "cool_stuff","musical_instruments"]:
        return "sports_leisure"

    elif cat in ["books_general_interest","books_technical",
                 "books_imported","dvds_blu_ray",
                 "cds_dvds_musicals","music"]:
        return "books_media"

    elif cat in ["food","food_drink","drinks"]:
        return "food_drinks"

    else:
        return "other"

df["category_macro"] = df["category"].apply(map_category).astype("category")


## Modell med makrokategorier
X_macro = pd.get_dummies(
    df[["delay_flag", "total_order_value", "category_macro"]],
    drop_first=True
)

X_macro = sm.add_constant(X_macro)

# 🔥 Viktig linje
X_macro = X_macro.astype(float)

y = df["low_rating_flag"].astype(int)

model_macro = sm.Logit(y, X_macro).fit()
print(model_macro.summary())

np.exp(model_macro.params)

## Effekt i prosentpoeng
mean_value = df["total_order_value"].mean()
base_category = df["category"].cat.categories[0]

new_data = pd.DataFrame({
    "delay_flag": [0, 1],
    "total_order_value": mean_value,
    "category": base_category
})

new_data = pd.get_dummies(new_data, drop_first=True)
new_data = sm.add_constant(new_data, has_constant="add")

# Match eksakt modellstruktur
new_data = new_data.reindex(columns=X.columns, fill_value=0)

pred = model.predict(new_data)

print(pred)
print("Effekt i prosentpoeng:", pred.iloc[1] - pred.iloc[0])