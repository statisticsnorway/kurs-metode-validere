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

# %%
# Lager data

# %%
# Henter biblioteker
import pandas as pd
import numpy as np
import pandera.pandas as pa
from pandera.pandas import DataFrameModel
from pandera.pandas import Field
from pandera.typing import Series

from vaskify import Detect

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


# %%
print(df.dtypes)

# %%
# Lager feil
# To rapporterer i negative verdier
df.loc[df["id"] == 1, "forbruk_vann"] *= -1 
df.loc[df["id"] == 2, "forbruk_vann"] *= -1
df.loc[df["id"] == 3, "forbruk_vann"] *= 1000
df.loc[df["id"] == 4, "forbruk_vann"] *= 1000
df.loc[df["id"] == 5, "alder_anlegg"] = pd.NA

# df.loc[df["id"] == "1", "forbruk_vann"] = (df.loc[df["id"] == "1", "forbruk_vann"]*-1)
# df.loc[df["id"] == "2", "forbruk_vann"] = (df.loc[df["id"] == "2", "forbruk_vann"]*-1)

# To rapporterer i liter istedenfor i m*3
# df.loc[df["id"] == "3", "forbruk_vann"] = (df.loc[df["id"] == "3", "forbruk_vann"] * 1000)
# df.loc[df["id"] == "4", "forbruk_vann"] = ( df.loc[df["id"] == "4", "forbruk_vann"] * 1000)

# En vet ikke hvor gammelelt annlegget er
# df.loc[df["id"] == "5", "alder_anlegg"] = "."

print(df.dtypes)
print(df)


# %%
class ReglerVann(pa.DataFrameModel):
    """Schema for validating water consumption inndata."""

    id: Series[int] = Field(nullable=False, unique=True)
    forbruk_vann: Series[float]=Field(nullable=False, ge=0, le=1000 )
    alder_anlegg: Series[float]=Field(nullable=False)
   


# %%
# Kjører kontrollene på datasettet

try: 
    validert_df = ReglerVann.validate(df, lazy=True) 
    print("Alle kontroller bestått")

except pa.errors.SchemaErrors as e:
    feil=e.failure_cases
    print(feil)

 

# %%
antall_feil = len(feil)
antall_obs = len(df)
andel_feil=antall_feil/antall_obs
andel_feil
antall_feil_variabel = feil["column"].value_counts()
antall_feil_obs = feil["index"].value_counts()
antall_feil_regel = feil["check"].value_counts()


# %%
# Setter opp innledene kontroller
regler_start = pa.DataFrameSchema( columns={ "id": Column(int), 
                                          "forbruk_vann": Column(float), 
                                          "alder_anlegg": Column(str), }, 
                                   checks=[ Check( lambda df: len(df) > 125, error="Antall observasjoner må være større enn 125" ), 
                                         Check( lambda df: df.shape[1] == 3, error="Datasettet må ha nøyaktig 3 variabler" ), ], 
                                  strict=True, )


# %%
df.describe()
#df.info

# %%
try:
    regler_start.validate(df, lazy=True) 
    print("Alle kontroller bestått")

except pa.errors.SchemaErrors as e:
    print(e.failure_cases)


# %%
class ReglerPerson(pa.DataFrameModel):

    person_id: Series[str] = Field(unique=True, nullable=False)

    alder: Series[int] = Field(ge=0, le=120)

    inntekt: Series[float] = Field(ge=0)

    kjonn: Series[str] = Field(isin=["K", "M"])




# %% [markdown]
# # Validerings eksempler

# %%
import pandas as pd
from vaskify import Detect

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
# ### Eksempel på tusenfeil

# %%
det = Detect(kirkedata_0, id_nr = "region")
thousand_result = det.thousand_error(y_var = ["konfirmanter", "konfirmanter_1"], lower_bound=-0.5, upper_bound=0.5)

# %%
thousand_result.head()

# %%
import plotly.express as px
import plotly.graph_objects as go

# Main scatter plot
fig = px.scatter(
    thousand_result,
    x="konfirmanter",
    y="diffLog10",
    color="outlier",
    title="Tusenfeil",
    labels={
        "konfirmanter": "Konfirmanter",
        "diffLog10": "logaritmen til differansen",
        "outlier": "Outlier:"
    },
    hover_data={
        "id": True,
        "konfirmanter": True,
        "konfirmanter_1": True,
        "diffLog10": False  # already shown via hovertemplate
    }
)
fig.show()


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
import numpy as np
import pandas as pd

# Sørger for at tilfeldige tall blir like hver gang koden kjøres
np.random.seed(42)

# Genererer 100 observasjoner
n = 100


# %%
data = {
    # Unik ID fra 1 til 100
    "id": range(1, n + 1),
    # Simulerer vannforbruk i m*3 med et gjennomsnitt på 150 og standardavvik på 30L
    "forbruk_vann": np.round(np.random.normal(loc=150, scale=30, size=n), 1),
}

# Oppretter datasettet (DataFrame)
df = pd.DataFrame(data)


# %%
print(df.head())

# %%
# 2. Legger inn 2 negative observasjoner (ID 11 og ID 21)
df.loc[df["id"] == 11, "forbruk_vann"] = -45.0
df.loc[df["id"] == 21, "forbruk_vann"] = -12.5

# 3. Legger inn 2 observasjoner som rapporterer i liter i stedet for m3 (ID 51 og ID 61)
# Verdien ganges med 1000 for å simulere at tallet ble tastet inn i liter (f.eks. 150 000 istedenfor 150)
df.loc[df["id"] == 51, "forbruk_vann"] = (
    df.loc[df["id"] == 51, "forbruk_vann"] * 1000
)
df.loc[df["id"] == 61, "forbruk_vann"] = (
    df.loc[df["id"] == 61, "forbruk_vann"] * 1000
)


# %%
import pandera as pa

# %%
