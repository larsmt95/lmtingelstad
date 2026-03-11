# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import sqlalchemy
import statsmodels.api as sm
from sqlalchemy import create_engine
import psycopg2

# Koble til PostgreSQL og hente data
engine = create_engine(
    "postgresql+psycopg2://postgres:legandary007@localhost:5432/e-commerce"
)

query = """
SELECT 
    order_purchase_timestamp,
    low_rating_flag,
    delay_flag,
    delay_days,
    total_order_value,
    avg_review_score
FROM public.fact_orders_v5
"""

df = pd.read_sql(query, engine)


# Logistisk regresjon: Delay → Low rating
import numpy as np

X = df[['delay_days', 'total_order_value']]
X = sm.add_constant(X)
y = df['low_rating_flag']

model_delay = sm.Logit(y, X).fit()
print(model_delay.summary())

# Odds ratio
odds_ratios = np.exp(model_delay.params)
print(odds_ratios)

# Tolkning
np.exp(model_delay.params['delay_days'])

# Predikert sannsynlighet (prosentpoeng)
new_data = pd.DataFrame({
    'delay_days': [0, 5, 10],
    'total_order_value': df['total_order_value'].mean()
})

new_data = sm.add_constant(new_data, has_constant='add')

print(new_data.columns)  # Sjekk at 'const' finnes

pred = model_delay.predict(new_data)
print(pred)

# Kvadratledd (ikke-lineær effekt)
df['delay_days_sq'] = df['delay_days'] ** 2

X_quad = df[['delay_days', 'delay_days_sq', 'total_order_value']]
X_quad = sm.add_constant(X_quad)

model_quad = sm.Logit(y, X_quad).fit()
print(model_quad.summary())

# Visualisere sannsynlighetskurve
import matplotlib.pyplot as plt

delay_seq = pd.DataFrame({
    'delay_days': range(0, 21),
    'total_order_value': df['total_order_value'].mean()
})

delay_seq = sm.add_constant(delay_seq, has_constant='add')

# Tving identisk struktur som treningsdata
delay_seq = delay_seq[model_delay.model.exog_names]

delay_seq['pred'] = model_delay.predict(delay_seq)

plt.plot(delay_seq['delay_days'], delay_seq['pred'])
plt.xlabel("Delay days")
plt.ylabel("Predicted probability of low rating")
plt.show()



# Interaksjon: Delay × Order value
df['interaction'] = df['delay_days'] * df['total_order_value']

X_int = df[['delay_days', 'total_order_value', 'interaction']]
X_int = sm.add_constant(X_int)

model_interaction = sm.Logit(y, X_int).fit()
print(model_interaction.summary())


# Visualisere interaksjon
q_vals = df['total_order_value'].quantile([0.25, 0.75]).values

delay_seq = pd.DataFrame({
    'delay_days': list(range(0, 16)) * 2,
    'total_order_value': [q_vals[0]] * 16 + [q_vals[1]] * 16
})

delay_seq['interaction'] = delay_seq['delay_days'] * delay_seq['total_order_value']
delay_seq = sm.add_constant(delay_seq)

delay_seq['pred'] = model_interaction.predict(delay_seq)

plt.figure()

labels = [f"Low order value ({round(q_vals[0],0)})",
          f"High order value ({round(q_vals[1],0)})"]

for val, label in zip(q_vals, labels):
    subset = delay_seq[delay_seq['total_order_value'] == val]
    plt.plot(subset['delay_days'], subset['pred'], label=label)

plt.xlabel("Delay days")
plt.ylabel("Predicted probability of low rating")
plt.legend()
plt.show()

# Effekt i prosentpoeng

new_data_2 = pd.DataFrame({
    'delay_days': [0, 10, 0, 10],
    'total_order_value': [q_vals[0], q_vals[0], q_vals[1], q_vals[1]]
})

new_data_2['interaction'] = new_data_2['delay_days'] * new_data_2['total_order_value']
new_data_2 = sm.add_constant(new_data_2)

new_data_2['pred'] = model_interaction.predict(new_data_2)

effects = (
    new_data_2
    .groupby('total_order_value')['pred']
    .apply(lambda x: x.iloc[1] - x.iloc[0])
)

print(effects)