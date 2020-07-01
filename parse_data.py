'''

Scrapes data from database, then parses it to anonymize and make good for analysis

'''

from __future__ import division, print_function
import os
import json
import sys
import h5py
import argparse
import numpy as np
import pandas as pd
from sqlalchemy import create_engine, MetaData, Table

import pprint


DBFL = "data/participants.db"
EXPERIMENT_FLAGS = ["1.0"] # Allowable codeversion flags to download
TROUT = "data/parsed_trials.csv"
QOUT = "data/parsed_questions.csv"

# Mostly from http://psiturk.readthedocs.io/en/latest/retrieving.html
def read_db(db_path, codeversions):
    table_name = "mot_live"
    data_column_name = "datastring"
    mode = "live"
    engine = create_engine("sqlite:///" + db_path)
    metadata = MetaData()
    metadata.bind = engine
    table = Table(table_name, metadata, autoload=True)
    s = table.select()
    rows = s.execute()

    rawdata = []
    statuses = [3, 4, 5, 7]
    for row in rows:
        if (row['status'] in statuses and
            row['mode'] == mode and
            row['codeversion'] in codeversions):
            str_data = row[data_column_name]
            proc_data = json.loads(str_data)
            rawdata.append(proc_data)

    conddict = {}
    for part in rawdata:
        uniqueid = part['workerId'] + ':' + part['assignmentId']
        conddict[uniqueid] = part['condition']
    data = [part['data'] for part in rawdata]

    for part in data:
        for record in part:
            record['trialdata']['uniqueid'] = record['uniqueid']
            record['trialdata']['condition'] = conddict[record['uniqueid']]

    trialdata = pd.DataFrame([record['trialdata'] for part in data for
                              record in part if
                              ('IsInstruction' in record['trialdata'] and
                               not record['trialdata']['IsInstruction'])])

    qdat = []
    for part in rawdata:
        thispart = part['questiondata']
        thispart['uniqueid'] = part['workerId'] + ':' + part['assignmentId']
        qdat.append(thispart)
    questiondata = pd.DataFrame(qdat)

    return trialdata, questiondata

def parse_rawname(trialname, f):
    fullname = os.path.splitext(trialname)[0]
    scene, dot = fullname.split('_')
    dataset = f['dataset']
    params = {k:dataset[scene][k].value for k in ['inertia',
                                      'spring',
                                      'sigma_w',
                                      'sigma_x',
                                      'sigma_v',
                                      'num_targets',
                                      'num_observations']}
    params.update({'scene': int(scene), 'dot': int(dot)})
    return params

def main():

    parser = argparse.ArgumentParser(description = "Parses MOT Exp:0 data")
    parser.add_argument("dataset", type = str, help = "Path to trial dataset",
                        default = 'participants.db')

    args = parser.parse_args()

    trs, qs = read_db(DBFL, EXPERIMENT_FLAGS)

    print(trs)

    qs = qs.rename(index=str, columns={'uniqueid': 'WID'})


    h5f = h5py.File('data/exp_0.h5','r')
    trs = trs.merge(trs.TrialName.apply(
        lambda s: pd.Series(parse_rawname(s, h5f))),
                    left_index=True, right_index=True)
    trs = trs.dropna()

    trs = trs.rename(index=str,
                     columns={'ReactionTime':'RT',
                              'uniqueid':'WID'})

    """Make sure we have 150 observations per participant"""
    trialsbyp = trs.WID.value_counts()
    # trialsbyp = trialsbyp[trialsbyp == 150]
    trialsbyp = trialsbyp[trialsbyp == 128]
    good_wids = trialsbyp.index
    trs = trs[trs.WID.isin(good_wids)]
    # good_wids = trs.index

    """Assign random identifiers to each participant"""
    wid_translate = {}
    for i, wid in enumerate(good_wids):
        wid_translate[wid] = "Participant_" + str(i)

    trs["ID"] = trs.WID.apply(lambda x: wid_translate[x])

    trs = trs.drop('WID', 1)
    trs.to_csv(TROUT, index=False)

    print(trs)
    cl_qs = qs[qs.WID.isin(good_wids)].copy()
    cl_qs["ID"] = cl_qs.WID.apply(lambda x: wid_translate[x])
    print(cl_qs)
    cl_qs[["ID", "instructionloops", "comments"]].to_csv(QOUT, index=False)

if __name__ == '__main__':
    main()
