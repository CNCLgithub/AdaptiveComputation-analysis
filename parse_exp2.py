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

import scipy.stats
import matplotlib.pyplot as plt


def clamp(n, smallest, largest): return max(smallest, min(n, largest))

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
    statuses = list(range(3, 8)) # [3, 4, 5, 7]
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
    PROBE_WINDOW = 1000
    ds = spacebar - probe_timing
    return np.logical_and(ds >= 0, ds <= PROBE_WINDOW)

def parse_row(row, n_frames, frame_duration):
    # target designation
    td_acc = sum(row.Target[:4]) / 4.0
    
    #n_frames_CONV = 10 # number of frames to use to do a moving average
    # diff_array = np.array(row.DifficultyArray)
    # times = [diff[0]/frame_duration for diff in diff_array]
    # diffs = [diff[1] for diff in diff_array]
    # difficulty = scipy.stats.binned_statistic(times, diffs, statistic='mean', bins=n_frames, range=(0,n_frames))[0]

    #difficulty = np.convolve(difficulty[0], np.ones((n_frames_CONV,))/n_frames_CONV, mode='same')
    
    #plt.plot(range(1,n_frames+1), difficulty)
    
    # recreating the difficulty from spacebar presses
    spacebar = np.array(row.Probe) # TODO change name saved to Spacebar
    spacebar = spacebar / frame_duration
    
    # actually, this doesn't matter too much:
    # further scaling and smoothing can be done in R
    DIFF_BORDER_TIME = frame_duration;
    # DIFF_UP = 0.3; # how much difficulty goes up when SPACEBAR pressed
    # DIFF_DOWN = 0.05; # adjusted to match...

    DIFF_DOWN = 0.06 # how much difficulty goes down automatically within DIFF_BORDER_TIME
    DIFF_UP = 0.3 # how much difficulty goes up when SPACEBAR pressed

    difficulty = 0.0
    difficulty_array = []
    for frame in range(1, n_frames+1):
        # for every spacebar press in that timestep, increase difficulty
        n_ups = sum((spacebar >= frame-1) & (spacebar < frame))
        for i in range(n_ups):
            difficulty = min(difficulty + DIFF_UP, 1.0)

        # decrease difficulty every frame
        difficulty = max(difficulty - DIFF_DOWN * frame_duration / DIFF_BORDER_TIME, 0.0)
        difficulty_array.append(difficulty)
    
    #plt.plot(range(1,n_frames+1), difficulty_array)
    #plt.show()
    
    df = pd.DataFrame()
    df['frame'] = range(1, n_frames+1)
    df['difficulty'] = difficulty_array
    df['WID'] = row.WID
    df['scene'] = row.TrialName[0]
    df['td_acc'] = td_acc
    
    return df


def main():

    parser = argparse.ArgumentParser(description = "Parses MOT Exp:1 data",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter)
   
    parser.add_argument("--dataset", type = str, help = "Path to trial dataset",
                        default = 'data/participants.db')
    parser.add_argument("--table_name", type = str, default = "exp1_live",
                        help = 'Table name')
    parser.add_argument("--exp_flag", type = str, nargs ='+', default = ["5.02"],
                        help = 'Experiment version flag')
    parser.add_argument("--mode", type = str, default = "debug",
                        choices = ['debug', 'sandbox', 'live'],
                        help = 'Experiment mode')
    parser.add_argument("--trialsbyp", type = int, default = 40,
                        help = 'Number of trials expected per subject')
    parser.add_argument("--trialdata", type = str, default = 'data/parsed_trials.csv',
                        help = 'Filename to dump parsed trial data')
    parser.add_argument("--questiondata", type = str, default = 'data/parsed_questions.csv',
                        help = 'Filename to dump parsed trial data')
    parser.add_argument("--n_frames", type = int, default = 490,
                        help = 'How many frames to extract from behavioral data, behavioral recording seems to lag a bit so 490 > actual 480 frames of the scene')
    parser.add_argument("--frame_duration", type = float, default = 41.6667,
                        help = 'The duration of one frame in milliseconds')

    args = parser.parse_args()

    trs, qs = read_db(args.dataset, args.table_name, args.exp_flag, args.mode)
    
    qs = qs.rename(index=str, columns={'uniqueid': 'WID'})

    trs = trs.dropna()
    trs = trs.rename(index=str,
                     columns={'ReactionTime':'RT',
                              'uniqueid':'WID'})
    trs['scene'] = trs['TrialName'].apply(lambda r: r[0])

    row_data = pd.concat(trs.apply(parse_row, axis=1, args=(args.n_frames, args.frame_duration)).tolist())
    trs = trs[['scene', 'WID', 'RT', 'condition', 'TrialOrder']]
    trs = trs.merge(row_data, on = ['scene', 'WID'])

    # Make sure we have a certain number of trials per participant
    trialsbyp = trs.WID.value_counts()/args.n_frames
    trialsbyp = trialsbyp[trialsbyp == args.trialsbyp]
    good_wids = trialsbyp.index
    trs = trs[trs.WID.isin(good_wids)]
    
    print(trs)

    """Assign random identifiers to each participant"""
    wid_translate = {}
    for i, wid in enumerate(good_wids):
        wid_translate[wid] = "Participant_" + str(i)

    trs["ID"] = trs.WID.apply(lambda x: wid_translate[x])

    # trs = trs.drop('WID', 1)

    trs.to_csv(args.trialdata, index=False)

    cl_qs = qs[qs.WID.isin(good_wids)].copy()
    cl_qs["ID"] = cl_qs.WID.apply(lambda x: wid_translate[x])
    # cl_qs[["ID", "instructionloops", "comments"]].to_csv(args.questiondata, index=False)
    cl_qs[["ID", "comments"]].to_csv(args.questiondata, index=False)

if __name__ == '__main__':
    main()
