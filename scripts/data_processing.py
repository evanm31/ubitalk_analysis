#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 29 17:18:48 2019

@author: mm06832
"""
import os
import sys
import numpy as np
import pandas as pd

abspath = os.path.abspath('C:\\Users\\house\\Documents\\GitHub\\ubitalk_analysis\\data\\Alex.txt')
dname = os.path.dirname(abspath)
os.chdir(dname)

files = os.listdir()
files = [file for file in files if file not in ('.DS_Store', 'final_data.csv')]

def gather_data():
    first = True #if first, create dataframe for future concatenations
    for participant in files:
        dat = open(participant).read().split()
        df = pd.DataFrame([line.split(",") for line in dat])
        df.columns = ['n_words', 'n_words_total', 'speed', 'time']
        df = df.assign(participant = participant.split('.')[0])
        if (first):
            df_final = df.copy()
            first = False
        else:
            df_final = pd.concat([df_final, df])
    return df_final

df = gather_data()
df.to_csv(path_or_buf = os.getcwd() + "/final_data.csv", index=False)