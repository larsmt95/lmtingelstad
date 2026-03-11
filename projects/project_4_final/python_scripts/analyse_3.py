# -*- coding: utf-8 -*-
"""
Created on Mon Mar  2 14:12:33 2026

@author: larsmt
"""

## Koble til DB og hente data
import pandas as pd
from sqlalchemy import create_engine
import matplotlib.pyplot as plt
import statsmodels.api as sm
import seaborn as sns

sns.set_theme(style="white")
plt.rcParams["figure.figsize"] = (12,6)

engine = create_engine(
    "postgresql+psycopg2://postgres:legandary007@localhost:5432/e-commerce"
)

df = pd.read_sql("""
SELECT low_rating_flag,
       delay_flag,
       delay_days,
       total_order_value,
       avg_review_score
FROM public.fact_orders_v5
""", engine)

df_mr = pd.read_sql("SELECT * FROM public.montly_revenue", engine)
df_drm = pd.read_sql("SELECT * FROM public.delay_rate_montly", engine)
aov = pd.read_sql("SELECT * FROM public.monthly_aov", engine)
monthly_kpis = pd.read_sql("SELECT * FROM public.monthly_kpis", engine)

## AOV over tid + lineær trend
aov = aov.sort_values("year_month")

    # Lag numerisk tidsindeks for trend
aov["time_index"] = range(len(aov))
X = sm.add_constant(aov["time_index"])
model = sm.OLS(aov["aov"], X).fit()
aov["trend"] = model.predict(X)

fig, ax = plt.subplots()

sns.lineplot(data=aov,
             x="year_month",
             y="aov",
             linewidth=2,
             ax=ax,
             label="Actual")

sns.lineplot(data=aov,
             x="year_month",
             y="trend",
             linestyle="--",
             ax=ax,
             label="Trend")

# Fjern top og right spine
sns.despine()

ax.set_title("Average Order Value Over Time", fontsize=14, weight="bold")
ax.set_xlabel("")
ax.set_ylabel("AOV")
plt.xticks(rotation=0)
plt.tight_layout()
plt.show()

## Revenue over tid + trend
df_mr = df_mr.sort_values("year_month")
df_mr["time_index"] = range(len(df_mr))

X = sm.add_constant(df_mr["time_index"])
model_rev = sm.OLS(df_mr["total_revenue"], X).fit()
df_mr["trend"] = model_rev.predict(X)

fig, ax = plt.subplots()

sns.lineplot(data=df_mr,
             x="year_month",
             y="total_revenue",
             linewidth=2,
             ax=ax,
             label="Revenue")

sns.lineplot(data=df_mr,
             x="year_month",
             y="trend",
             linestyle="--",
             ax=ax,
             label="Trend")

sns.despine()
ax.set_title("Monthly Revenue Trend", fontsize=14, weight="bold")
ax.set_xlabel("")
ax.set_ylabel("Revenue")
plt.xticks(rotation=0)
plt.tight_layout()
plt.show()

## Delay rate over tid 
df_drm_filtered = df_drm[df_drm["delay_rate"] < 1].sort_values("year_month")

plt.figure()
sns.lineplot(data=df_drm_filtered,
             x="year_month",
             y="delay_rate",
             linewidth=2)

sns.despine()
plt.title("Monthly Delay Rate", weight="bold")
plt.xlabel("")
plt.ylabel("Delay Rate")
plt.xticks(rotation=0)
plt.tight_layout()
plt.show()


## Low rating over tid 
monthly_kpis_filtered = monthly_kpis[monthly_kpis["low_rating_rate"] < 1]

plt.figure()
plt.plot(monthly_kpis_filtered["year_month"],
         monthly_kpis_filtered["low_rating_rate"])
plt.xlabel("Year-Month")
plt.ylabel("Low Rating Rate")
plt.show()

## Lineær trendmodell (tilsvarer lm() i R)

    # må bruke numerisk tidsindeks:
df_mr["time_index"] = range(len(df_mr))

X = sm.add_constant(df_mr["time_index"])
trend_model = sm.OLS(df_mr["total_revenue"], X).fit()

print(trend_model.summary())