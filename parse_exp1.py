'''

Scrapes data from database, then parses it to anonymize and make good for analysis

'''

from __future__ import division, print_function
import os
import json
import sys
import argparse
# import numpy as np
import pandas as pd
from sqlalchemy import create_engine, MetaData, Table

import pprint



# Mostly from http://psiturk.readthedocs.io/en/latest/retrieving.html
def read_db(db_path, table_name, codeversions, mode):
    data_column_name = "datastring"
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

def main():

    parser = argparse.ArgumentParser(description = "Parses MOT Exp:1 data",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
   
    parser.add_argument("--dataset", type = str, help = "Path to trial dataset",
                        default = 'data/participants.db')
    parser.add_argument("--table_name", type = str, default = "mot_live",
                        help = 'Table name')
    parser.add_argument("--exp_flag", type = str, default = "1.0",
                        help = 'Experiment version flag')
    parser.add_argument("--mode", type = str, default = "debug",
                        choices = ['debug', 'live'],
                        help = 'Experiment mode')
    parser.add_argument("--trialsbyp", type = int, default = 40,
                        help = 'Number of trials expected per subject')
    parser.add_argument("--trialdata", type = str, default = 'data/parsed_trials.csv',
                        help = 'Filename to dump parsed trial data')
    parser.add_argument("--questiondata", type = str, default = 'data/parsed_questions.csv',
                        help = 'Filename to dump parsed trial data')



    args = parser.parse_args()

    trs, qs = read_db(args.dataset, args.table_name, args.exp_flag, args.mode)


    qs = qs.rename(index=str, columns={'uniqueid': 'WID'})

    trs = trs.dropna()
    trs = trs.rename(index=str,
                     columns={'ReactionTime':'RT',
                              'uniqueid':'WID'})

    print(trs)
    # Make sure we have 150 observations per participant
    trialsbyp = trs.WID.value_counts()
    print(trialsbyp)
    trialsbyp = trialsbyp[trialsbyp == args.trialsbyp]
    good_wids = trialsbyp.index
    trs = trs[trs.WID.isin(good_wids)]

    print(trs)

    """Assign random identifiers to each participant"""
    wid_translate = {}
    for i, wid in enumerate(good_wids):
        wid_translate[wid] = "Participant_" + str(i)

    trs["ID"] = trs.WID.apply(lambda x: wid_translate[x])

    print(trs)
    # trs = trs.drop('WID', 1)

    trs.to_csv(args.trialdata, index=False)

    cl_qs = qs[qs.WID.isin(good_wids)].copy()
    cl_qs["ID"] = cl_qs.WID.apply(lambda x: wid_translate[x])
    cl_qs[["ID", "instructionloops", "comments"]].to_csv(args.questiondata, index=False)

if __name__ == '__main__':
    main()
