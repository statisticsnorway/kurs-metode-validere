# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#   kernelspec:
#     display_name: kurs-metode-validere2
#     language: python
#     name: kurs-metode-validere2
# ---

# %% [markdown]
# # Henter pakker vi skal bruke

# %%
import pandas as pd
import numpy as np
import pandera.pandas as pa
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
from pandera.pandas import DataFrameModel
from pandera.pandas import Field
from pandera.typing import Series
from pandera import Check
from klass import get_classification
from vaskify import Detect
from sklearn.linear_model import HuberRegressor

# %% [markdown]
# # Lager data vi skal bruke

# %%
# Lager data

np.random.seed(42)
n = 150
data = {
    "id": range(1, n + 1),
    "forbruk_vann": np.round(np.random.normal(loc=150, scale=30, size=n), 1),
    "alder_anlegg": np.round(np.random.normal(loc=2000, scale=20, size=n), 0),
}
df = pd.DataFrame(data)

print(df)


# %% [markdown]
# # Legger inn feil i datasettet

# %%
# Lager feil
# To rapporterer i negative verdier
df.loc[df["id"] == 1, "forbruk_vann"] *= -1 
df.loc[df["id"] == 2, "forbruk_vann"] *= -1

# To rapporterer i liter istedenfor i kubic meter
df.loc[df["id"] == 3, "forbruk_vann"] *= 1000
df.loc[df["id"] == 4, "forbruk_vann"] *= 1000

# En vet ikke hvor gammelt annlegget er
df.loc[df["id"] == 5, "alder_anlegg"] = pd.NA


print(df)


# %% [markdown]
# # Eksemel på logiske kontroller med pakken Pandera

# %%
# Setter opp regler for hver variabel
class ReglerVann(pa.DataFrameModel):

    id: Series[int] = Field(nullable=False, unique=True)
    forbruk_vann: Series[float]=Field(nullable=False, ge=0, le=1000 )
    alder_anlegg: Series[float]=Field(nullable=False)
   


# %%
# Kjører kontrollene på datasettet

try: 
    validert_df = ReglerVann.validate(df, lazy=True) 
    print("Alle kontroller bestått")

except pa.errors.SchemaErrors as e:
    feil = e.failure_cases
    print(feil)

 

# %%
# Analyse av kontrollene

antall_feil = len(feil)
antall_obs = len(df)
andel_feil=antall_feil/antall_obs

andel_feil


# %%
# antall feil per variabel
antall_feil_variabel = feil["column"].value_counts()
antall_feil_variabel


# %%
# antall feil per observasjon 
antall_feil_obs = feil["index"].value_counts()

antall_feil_obs

# %%
# antall feil per regel

antall_feil_regel = feil["check"].value_counts()
antall_feil_regel

# %%
# Grafikk
fig, ax = plt.subplots(figsize=(8, 4))
antall_feil_variabel.plot( kind="barh", color="steelblue", ax=ax )
ax.set_title("Antall feil per variabel")
ax.set_xlabel("Antall feil")
ax.set_ylabel("Variabler")
ax.xaxis.set_major_locator( MaxNLocator(integer=True) )

fig.tight_layout()
plt.show()


# %%
# Legger til innledene kontroller på antall observasjoner og antall variabler

class ReglerVann(pa.DataFrameModel):

    @pa.dataframe_check 
    def minst_125_observasjoner(cls, df): 
        return len(df) > 125 
        
    @pa.dataframe_check 
    def tre_variabler(cls, df): 
        return df.shape[1] == 3
    
    id: Series[int] = Field(nullable=False, unique=True)
    
    forbruk_vann: Series[float]=Field(nullable=False, ge=0, le=1000 )
    
    alder_anlegg: Series[float]=Field(nullable=False)
    
  

# %%
# Kontroll opp mot kodelister og standarder som ligger i Klass
# Vi bruke pakken ssb-klass-python til å hente kodelisten
# kort form: gyldige_koder = get_classification(6).get_codes().data["code"].tolist()

naering = get_classification(6)
koder = naering.get_codes()
kodeverk = koder.data
gyldige_koder = kodeverk["code"].tolist()

class ReglerBedrift(pa.DataFrameModel): 
    naering: Series[str] = Field( isin=gyldige_koder )



# %% [markdown]
# # Selektiv editering - mistenkelige observasjoner


# %% [markdown]
# ## Les inn data

# %%
kirkedata_0 = pd.read_parquet("../../data/kirkedata_0.parquet")
kirkedata = pd.read_parquet("../../data/kirkedata.parquet")

# %%
kirkedata_0.head()

# %%
kirkedata.head()

# %% [markdown]
# # Kvatilmetde

# %%
det = Detect(kirkedata_0, id_nr = "region")
resultat = det.quartile_error(x_var = "konfirmanter", y_var = "personer15", pkl=2, pku=2)
resultat.head()

# %%
outliere = resultat[resultat["flag_quartile"] == 1 ] 
outliere.head(20)

# %%
# Lager figur over kvartilmetoden

lower = resultat["lower_limit"].iloc[0]
upper = resultat["upper_limit"].iloc[0]

fig, ax = plt.subplots(figsize=(8, 4)) 
ax.hist( resultat["ratio"], bins=30, color="steelblue", edgecolor="black" )
ax.set_title("Fordeling av ratio=konfirmanter/15 åringer") 
ax.set_xlabel("Ratio") 
ax.set_ylabel("Antall observasjoner")
ax.axvline( lower, color="red", linestyle="--", linewidth=2, label="Nedre grense" )
ax.axvline( upper, color="red", linestyle="--", linewidth=2, label="Øvre grense" )

# %%
# Alternativ figur

fig, ax = plt.subplots(figsize=(8, 6))
ax.scatter(resultat["ranking"], resultat["ratio"], color="steelblue", alpha=0.7 )
ax.set_title("Ratio mot ranking") 
ax.set_xlabel("Ranking") 
ax.set_ylabel("Ratio")
ax.axhline( lower, color="red", linestyle="--", linewidth=2, label=f"Nedre grense = {lower:.2f}" )
ax.axhline( upper, color="red", linestyle="--", linewidth=2, label=f"Øvre grense = {upper:.2f}" )

# %% [markdown]
# # HB-metoden

# %%
resultat_HB = det.hb(y_var = ["konfirmanter", "konfirmanter_1"], pc = 8, pu = 0.75, pa = 0.05)
resultat_HB.head()

# %%
resultat_HB_sort = ( resultat_HB .sort_values("konfirmanter") )

fig, ax = plt.subplots(figsize=(8, 6))
ax.scatter( resultat_HB_sort["konfirmanter"], resultat_HB_sort["ratio"], color="steelblue", alpha=0.7 )
ax.set_title("HB metoden") 
ax.set_xlabel("Konfirmanter") 
ax.set_ylabel("Ratio")
ax.plot(resultat_HB_sort["konfirmanter"], resultat_HB_sort["lower_limit"], color="red", linewidth=1, label="Nedre grense" )
ax.plot(resultat_HB_sort["konfirmanter"], resultat_HB_sort["upper_limit"], color="red", linewidth=1, label="Øvre grense" )
# ax.fill_between( resultat_HB_sort["konfirmanter"], resultat_HB_sort["lower_limit"], resultat_HB_sort["upper_limit"], color="lightgrey", alpha=0.3 )
plt.show()


# %% [markdown]
# # Robust regresjon

# %%
X = kirkedata_0[["personer15"]]
y = kirkedata_0["konfirmanter"]
modell = HuberRegressor()
modell.fit(X, y)
kirkedata_0["predikert"] = modell.predict(X)

# Beregner residualer og stanadiserte residualer
kirkedata_0["residual"] = ( kirkedata_0["konfirmanter"] - kirkedata_0["predikert"] )
residual_std = kirkedata_0["residual"].std()
kirkedata_0["std_residual"] = ( kirkedata_0["residual"] / residual_std )
kirkedata_0["abs_std_residual"] = ( kirkedata_0["std_residual"] .abs() )

# Lager outlier variabel
grense = 2
kirkedata_0["outlier"] = ( kirkedata_0["abs_std_residual"] > grense )
kirkedata_0.head()


# %%
# Lager figur
data_sort = ( kirkedata_0 .sort_values("personer15") )
fig, ax = plt.subplots(figsize=(8, 6)) 
#ax.scatter( data_sort["personer15"], data_sort["konfirmanter"], alpha=0.5 )
ax.scatter( data_sort.loc[ ~data_sort["outlier"], "personer15" ], data_sort.loc[ ~data_sort["outlier"], "konfirmanter" ], color="steelblue", alpha=0.5, label="Vanlige observasjoner" )
ax.scatter( data_sort.loc[ data_sort["outlier"], "personer15" ], data_sort.loc[ data_sort["outlier"], "konfirmanter" ], color="red", alpha=0.8, label="Outliere" )
ax.plot( data_sort["personer15"], data_sort["predikert"], color="red", linewidth=2, label="Huber-regresjon" )
ax.set_title("Huber-regresjon") 
ax.set_xlabel("Personer 15 år") 
ax.set_ylabel("Konfirmanter") 
ax.legend() 
plt.show()


# %%
# Analyse av outlier
print( kirkedata_0["outlier"].sum() )

# %%
# se på outlierne
outliere = kirkedata_0[ kirkedata_0["outlier"] ] 
print(outliere)


# %% [markdown]
# # Selektiv editering - innflytelse på statistikken

# %%
# Størrelse - observasjons andel av totalen

kirkedata["andel_av_total"] = ( kirkedata["konfirmanter"] / kirkedata["konfirmanter"].sum() )

# ser på de 5 som har størst betydning for totalen
print( kirkedata.sort_values( "andel_av_total", ascending=False ) .head(5) )

# %%
# Endring - bidrag til endring.

# Beregner endringen
kirkedata["endring"] = ( kirkedata["konfirmanter"] - kirkedata["konfirmanter_1"] )
total_endring = ( kirkedata["endring"] .sum() )
pros_endring= total_endring*100/(kirkedata["konfirmanter_1"].sum())

# Beregner hvor mye hver kommune bidrar til endringen
kirkedata["bidrag"] = ( kirkedata["endring"] / total_endring )
kirkedata["abs_bidrag"] = ( kirkedata["bidrag"] .abs() )
pros_endring= total_endring*100/(kirkedata["konfirmanter_1"].sum())

print(pros_endring)
print( kirkedata.sort_values( "abs_bidrag", ascending=False ) .head(10) )


# %%
# Innflytelse - kombinere størrelse og endring
kirkedata["innflytelse"] = ( kirkedata["personer15"] * kirkedata["endring"].abs() )
print( kirkedata .sort_values( "innflytelse", ascending=False ) .head(10) )

# %%
# Figur
topp10 = ( kirkedata .sort_values( "innflytelse", ascending=False ) .head(10) )

fig, ax = plt.subplots(figsize=(8, 6))
ax.scatter( kirkedata["personer15"], kirkedata["endring"], alpha=0.6)
ax.scatter( topp10["personer15"], topp10["endring"], color="red", s=100, label="10 største" )
ax.set_title( "Størrelse og endring" )
ax.set_xlabel( "Personer 15 år" ) 
ax.set_ylabel( "Endring" )
ax.legend()
plt.show()

# %%
# Figur 2 bobleplott

kirkedata["abs_endring"] = ( kirkedata["endring"].abs() )
fig, ax = plt.subplots(figsize=(8, 6))
ax.scatter( kirkedata["personer15"], kirkedata["endring"], s=kirkedata["innflytelse"] / 100, alpha=0.5, color="steelblue" )
ax.set_title( "Innflytelse på endringstall" )
ax.set_xlabel( "Personer 15 år" ) 
ax.set_ylabel( "Endring i antall konfirmanter" )
plt.show()


# %% [markdown]
# # Maskinlæring for å finne outliere

# %%
# Isolation forest

from sklearn.ensemble import IsolationForest

X = kirkedata[ [ "personer15", "konfirmanter" ] ]

modell = IsolationForest( contamination=0.02, random_state=42 )
# contamination=0.02 omtrent 2 % av observasjonene forventes å være outliere.

modell.fit(X)
kirkedata["iforest"] = ( modell.predict(X) )
kirkedata["outlier_if"] = ( kirkedata["iforest"] == -1 )
kirkedata["anomaly_score"] = ( modell.score_samples(X) )
outliere = kirkedata[ kirkedata["outlier_if"] ] 

print( outliere[ [ "name", "personer15", "konfirmanter","anomaly_score" , "kostragr"] ] )


# %%
# Local Outlier Factor
from sklearn.neighbors import LocalOutlierFactor

X = kirkedata_0[ [ "personer15", "konfirmanter" ] ]
lof = LocalOutlierFactor( n_neighbors=20,contamination=0.02)
kirkedata_0["lof"] = lof.fit_predict(X)
kirkedata_0["outlier_lof"] = ( kirkedata_0["lof"] == -1 )

outliere = kirkedata_0[ kirkedata_0["outlier_lof"] ] 

print( outliere[ [ "name", "personer15", "konfirmanter", "kostragr"] ] )


# %%
# Random forest


# %% [markdown]
# ### Eksempel på tusenfeil

# %%
print(thousand_result["flag_thousand_1"].dtype)

# %%
det = Detect(kirkedata_0, id_nr = "region")
thousand_result = det.thousand_error(y_var = ["konfirmanter", "konfirmanter_1"], lower_bound=-0.5, upper_bound=0.5)
diff = thousand_result["konfirmanter"] - thousand_result["konfirmanter_1"]
thousand_result["diffLog10"] = (np.sign(diff) * np.log10(np.abs(diff) + 1))


# %%
outliere = thousand_result[ thousand_result["flag_thousand_1"] == 1 ] 
print(outliere)

# %%
avvik = thousand_result[ (thousand_result["diffLog10"] < -1) | (thousand_result["diffLog10"] > 1) ]
avvik.head()

# %%
thousand_result.head()

# %%
fig, ax = plt.subplots(figsize=(8, 6))
# scatter = ax.scatter( thousand_result["konfirmanter"], thousand_result["diffLog10"] )
ax.scatter( thousand_result.loc[ thousand_result["flag_thousand_1"] == 0.0, "konfirmanter" ], 
            thousand_result.loc[ thousand_result["flag_thousand_1"] == 0.0, "diffLog10" ], 
            color="steelblue", label="Ikke outlier" )

ax.scatter( thousand_result.loc[ thousand_result["flag_thousand_1"] == 1.0, "konfirmanter" ], 
            thousand_result.loc[ thousand_result["flag_thousand_1"] == 1.0, "diffLog10" ], 
            color="red", label="Outlier" )

ax.set_title("Tusenfeil")

ax.set_xlabel("Konfirmanter")

ax.set_ylabel("logaritmen til differansen")



# %%
# Add upper limit line
fig.add_trace(
    go.Scatter(
        x=thousand_result["konfirmanter"],
        y=thousand_result["upperLimit"],
        mode="lines",
        name="Øvre grense"
    )
)

# Add lower limit line
fig.add_trace(
    go.Scatter(
        x=thousand_result["konfirmanter"],
        y=thousand_result["lowerLimit"],
        mode="lines",
        name="Nedre grense"
    )
)



# %%

# %%
det.thousand_error(y_var = ["konfirmanter", "konfirmanter_1"], lower_bound=-0.5, upper_bound=0.5, output_scope = "outliers")

# %%

# %% [markdown]
# ### Eksempel på HB-metoden

# %%
det.hb(y_var = ["konfirmanter", "konfirmanter_1"], pc = 8, pu = 0.75, pa = 0.05)

# %%

# %% [markdown]
# ## Eksempel på kvartile-metoden

# %%
det.quartile_error(x_var = "konfirmanter", y_var = "personer15", pkl=2, pku=2)


# %%
