# -*- coding: utf-8 -*-
"""
Created on Mon Mar  2 14:29:07 2026

@author: larsmt
"""

import pandas as pd
import numpy as np
import statsmodels.api as sm
import seaborn as sns
import matplotlib.pyplot as plt

from sqlalchemy import create_engine

engine = create_engine(
    "postgresql+psycopg2://postgres:legandary007@localhost:5432/e-commerce"
)

monthly_kpis = pd.read_sql(
    "SELECT * FROM public.monthly_kpis",
    engine
)

monthly_kpis["year_month"] = pd.to_datetime(monthly_kpis["year_month"])
monthly_kpis = monthly_kpis.sort_values("year_month").reset_index(drop=True)


## Klargjør trend + sesong
monthly_kpis["time_index"] = range(1, len(monthly_kpis)+1)
monthly_kpis["month"] = monthly_kpis["year_month"].dt.month.astype("category")

## Bygg modeller 

    # Kun trend
X_trend = sm.add_constant(monthly_kpis["time_index"])

print(X_trend)

model_trend = sm.OLS(
    monthly_kpis["total_revenue"], 
    X_trend
).fit()

print(model_trend.summary())

    # Trend + sesong (dummy variabler)
X = pd.get_dummies(
    monthly_kpis[["time_index", "month"]],
    drop_first=True,
    dtype=float
    )

X = sm.add_constant(X)

print(X)

model_trend_season = sm.OLS(
    monthly_kpis["total_revenue"],
    X
).fit()

print(model_trend_season.summary())


## Lag fremtidige måneder (12 mnd)

h = 12 

last_time = monthly_kpis["time_index"].max()
last_date = monthly_kpis["year_month"].max()

future_dates = pd.date_range(
    last_date + pd.offsets.MonthBegin(),
    periods=h,
    freq="MS"
    )

future_data = pd.DataFrame({
    "year_month": future_dates,
    "time_index": range(last_time+1, last_time+h+1)
    })

future_data["month"] = future_data["year_month"].dt.month.astype("category")


## Match modellstruktur

future_X = pd.get_dummies(
    future_data[["time_index", "month"]],
    drop_first=True,
    dtype = float
    )

future_X = sm.add_constant(future_X)

    # Viktig: identiske kolonner som treningsdata
future_X = future_X.reindex(columns=X.columns, fill_value=0)

## Forecast med 95 % prediskjonsintervall

future_pred = model_trend_season.get_prediction(future_X)
future_ci = future_pred.summary_frame(alpha=0.05)

future_data["forecast"] = future_ci["mean"]
future_data["lower"] = future_ci["obs_ci_lower"]
future_data["upper"] = future_ci["obs_ci_upper"]

## Profesjonell visualisering (McKinsey clean)

sns.set_theme(style="white")
plt.rcParams["figure.figsize"] = (12,6)

fig, ax = plt.subplots()

# Historisk
sns.lineplot(
    data=monthly_kpis,
    x="year_month",
    y="total_revenue",
    ax=ax,
    linewidth=2,
    label="Historical"
)

# Forecast-linje
sns.lineplot(
    data=future_data,
    x="year_month",
    y="forecast",
    ax=ax,
    linestyle="--",
    linewidth=2,
    label="Forecast"
)

# Interval
ax.fill_between(
    future_data["year_month"],
    future_data["lower"],
    future_data["upper"],
    alpha=0.15
)

sns.despine()
ax.grid(False)

ax.set_title("Revenue Forecast (Trend + Monthly Seasonality)",
             fontsize=14,
             weight="bold")

ax.set_xlabel("")
ax.set_ylabel("Total Revenue")

plt.xticks(rotation=0)
plt.tight_layout()
plt.show()

## Modellvalidering

plt.figure()
plt.plot(model_trend_season.resid)
plt.title("Residuals")
plt.show()


## AIC-sammenligning
print("AIC trend only:", model_trend.aic)
print("AIC trend + season:", model_trend_season.aic)