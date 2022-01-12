'''

Scrapes data from database, then parses it to anonymize and make good for analysis

'''

from __future__ import division, print_function
import os
import json
import sys
import argparse
import numpy as np
import pandas as pd
from sqlalchemy import create_engine, MetaData, Table

import pprint



def read_json(file_path):
    with open(file_path) as json_file:
        return json.load(json_file)

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
    statuses =  [3, 4, 5, 7]
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

def parse_rawname(trialname):
    fullname = os.path.splitext(trialname[0])[0]
    rot_angle = int(trialname[1])

    trial_params = {'rot_angle': rot_angle}
    return trial_params


def classify(probe_timing, spacebar):
    PROBE_WINDOW = 900
    ds = spacebar - probe_timing
    return np.logical_and(ds >= 0, ds <= PROBE_WINDOW)


def parse_row(row, dataset):
    # target designation
    pred_targets = row.Target
    gt_targets = dataset[row.scene-1]["aux_data"]["targets"]
    td_acc = sum(pred_targets and gt_targets)/sum(gt_targets)
    target_idxs = np.where(gt_targets)[0]

    print(row.TrialName[:2])
    
    # some extra data for exp1_difficulty
    vel = dataset[row.scene-1]["aux_data"]["vel"]
    n_dist = dataset[row.scene-1]["aux_data"]["n_distractors"]

    # probes
    gt_probes = row.TrialName[2]
    probe_trackers = np.full(4, -1)
    response_frames = []

    if gt_probes:
        probe_trackers, probe_frames = zip(*gt_probes)
        FRAME_DURATION = 41.6667;
        probe_timings = np.array(probe_frames)*FRAME_DURATION
        
        spacebar = np.array(row.Probe)
        response_frames = np.floor(spacebar / FRAME_DURATION).astype(int)

    template = {
        'WID' : row.WID,
        'scene' : row.TrialName[0],
        'probe_id_1' : probe_trackers[0],
        'probe_id_2' : probe_trackers[1],
        'probe_id_3' : probe_trackers[2],
        'probe_id_4' : probe_trackers[3],
        'td_1' : row.Target[target_idxs[0]],
        'td_2' : row.Target[target_idxs[1]],
        'td_3' : row.Target[target_idxs[2]],
        'td_4' : row.Target[target_idxs[3]],
        'vel' : vel,
        'n_dist' : n_dist
    }

    cols = [{'response_frame' : rf, **template} for rf in response_frames]
    # in case no response is recorded
    if len(cols) == 0:
        cols = [template]
    return pd.DataFrame(cols)
    

def main():

    parser = argparse.ArgumentParser(description = "Parses MOT Exp1-difficulty data",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
   
    parser.add_argument("--database", type = str, help = "Path to participant database",
                        default = 'data/exp1_difficulty.db')
    parser.add_argument("--dataset", type = str, help = "Path to trial dataset",
                        default = 'data/exp1_difficulty_dataset.json')
    parser.add_argument("--table-name", type = str, default = "exp1_difficulty",
                        help = 'Table name')
    parser.add_argument("--exp-flag", type = str, nargs ='+', default = ["1.0"],
                        help = 'Experiment version flag')
    parser.add_argument("--mode", type = str, default = "debug",
                        choices = ['debug', 'sandbox', 'live'],
                        help = 'Experiment mode')
    parser.add_argument("--trialsbyp", type = int, default = 25,
                        help = 'Number of trials expected per subject')
    parser.add_argument("--trialdata", type = str, default = '../data/exp2/parsed_trials.csv',
                        help = 'Filename to dump parsed trial data')
    parser.add_argument("--questiondata", type = str, default = '../data/exp2/parsed_questions.csv',
                        help = 'Filename to dump parsed trial data questions')

    args = parser.parse_args()
    trs, qs = read_db(args.database, args.table_name, args.exp_flag, args.mode)
    dataset = read_json(args.dataset)

    qs = qs.rename(index=str, columns={'uniqueid': 'WID'})

    trs = trs.dropna()
    print(trs)
    trs = trs.rename(index=str,
                     columns={'ReactionTime':'RT',
                              'uniqueid':'WID'})
    trs['scene'] = trs['TrialName'].apply(lambda r: r[0])

    row_data = pd.concat(trs.apply(parse_row, axis=1, args=(dataset,)).tolist())
    trs = trs[['scene', 'WID', 'RT', 'condition', 'TrialOrder']]
    trs = trs.merge(row_data, on = ['scene', 'WID'])

    # Make sure we have 120 observations per participant
    trialsbyp = trs.groupby('WID').aggregate({"scene" : lambda x : len(x.unique())})
    print(trialsbyp)
    trialsbyp = trialsbyp[trialsbyp.scene  == args.trialsbyp]
    good_wids = trialsbyp.index
    trs = trs[trs.WID.isin(good_wids)]

    """Assign random identifiers to each participant"""
    wid_translate = {}
    for i, wid in enumerate(good_wids):
        wid_translate[wid] = i

    trs["ID"] = trs.WID.apply(lambda x: wid_translate[x])

    trs.to_csv(args.trialdata, index=False)

    cl_qs = qs[qs.WID.isin(good_wids)].copy()
    cl_qs["ID"] = cl_qs.WID.apply(lambda x: wid_translate[x])
    #cl_qs[["ID", "instructionloops", "comments"]].to_csv(args.questiondata, index=False)
    cl_qs[["ID", "comments"]].to_csv(args.questiondata, index=False)

if __name__ == '__main__':
    main()
