"""

    Parker Dunn
    7 March 2022

"""

# %% IMPORTS

import pandas as pd
import dask.dataframe as dd
import time


# %% STRUCTURE TO STORE GAME INFO

# Creating a class to track some basic data
class GameData:
    def __init__(self, h1_start, h1_end, h2_start, h2_end):
        self.h1_start = h1_start
        self.h2_start = h2_start
        self.h1_end = h1_end
        self.h2_end = h2_end
        self.dfs = []

    def store_df_features(self, name, df):
        self.dfs.append({name: df.columns})


# %% LOADING DATA INTO SCRIPT

def load_to_dask_df(csvfile, partitions=4, header=0, index_col=None):
    df = pd.read_csv("raw_data/" + csvfile, header=header, index_col=index_col)
    return dd.from_pandas(df, npartitions=partitions)


# %% BASIC DATA PREPARATION OPERATIONS

def split_into_halves(gamedata, full_game_dd):
    h1_dd = full_game_dd[
                            full_game_dd["cs_time_unix"] >= gamedata.h1_start or
                            full_game_dd["cs_time_unix"] <= gamedata.h1_end
                            ]
    h2_dd = full_game_dd[
                            full_game_dd["cs_time_unix"] >= gamedata.h2_start or
                            full_game_dd["cs_time_unix"] <= gamedata.h2_end
                            ]
    return h1_dd, h2_dd
