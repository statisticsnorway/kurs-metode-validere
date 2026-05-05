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
