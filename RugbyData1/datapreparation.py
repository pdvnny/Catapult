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
        self.dfs = {}

    def store_df_features(self, name, cols):
        self.dfs[name] = cols

    def print_df_features(self):
        for key, value in self.dfs.items():
            print(f"df name: {key} -> {value}")

    def unix_to_gametime(self, time, half):
        if half == 1:
            return time - self.h1_start
        else:
            return time - self.h2_start


# %% LOADING DATA INTO SCRIPT

def load_to_dask_df(csvfile, partitions=4, header=0, index_col=None):
    df = pd.read_csv("raw_data/" + csvfile, header=header, index_col=index_col)
    return dd.from_pandas(df, npartitions=partitions)


# %% BASIC DATA PREPARATION OPERATIONS

def split_into_halves(gamedata, full_game_dd):
    # half1_by_time = lambda df: df["cs_time_unix"] <= (gamedata.h1_end*100)
    # half2_by_time = lambda df: df["cs_time_unix"] >= (gamedata.h2_start*100)

    # Split AT the half
    h1_dd = full_game_dd.loc[full_game_dd["cs_time_unix"] <= (gamedata.h1_end*100), :]
    h2_dd = full_game_dd.loc[full_game_dd["cs_time_unix"] >= (gamedata.h2_start*100), :]

    # For debugging... FIXED IT I THINK
    #print(h1_dd.head())
    #print(h2_dd.head())

    return (h1_dd.loc[h1_dd["cs_time_unix"] >= (gamedata.h1_start*100), :].compute().reset_index(drop=True),
            h2_dd.loc[h2_dd["cs_time_unix"] <= (gamedata.h2_end*100), :].compute().reset_index(drop=True))

# %% RANDOM FUNCTION FOR HANDLING COMMON OPERATIONS

def centiseconds_to_clock(time_in_cs):
    time_ms = time_in_cs * 10
    ms = time_ms % 1000
    time_sec = time_ms // 1000
    sec = time_sec % 60
    minutes = time_sec // 60
    return minutes, sec, ms
