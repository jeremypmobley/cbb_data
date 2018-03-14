
### Helper functions

import pandas as pd
import datetime
import numpy as np



def read_in_file_to_df(file_name):
    df = pd.read_csv(file_name + ".csv")
    print(file_name, df.shape)
    return df




def create_poss_fields(df):
    """ Function to create possession fields within the detailed results dataframe """
    df['WPoss'] = (df['WFGA'] - df['WOR'] + df['WTO'] + (0.475*df['WFTA']) )
    df['LPoss'] = (df['LFGA'] - df['LOR'] + df['LTO'] + (0.475*df['LFTA']) )
    df['TotPoss'] = df['WPoss'] + df['LPoss']
    return df


def create_ppp_fields(df):
    """ Function to create possession fields within the detailed results dataframe """
    df['WPPP'] = (df['WScore'] / ((df['WPoss'] + df['LPoss'])/2) )
    df['LPPP'] = (df['LScore'] / ((df['WPoss'] + df['LPoss'])/2) )
    return df




def add_advanced_metrics(df):
    """ Function to add advanced metrics to detailed results dataframe """
    #Offensive efficiency (OffRtg) = 100 x (Points / Possessions)
    df['WOffRtg'] = df.apply(lambda row: 100 * (row.WScore / row.TotPoss), axis=1)
    df['LOffRtg'] = df.apply(lambda row: 100 * (row.LScore / row.TotPoss), axis=1)
    #Defensive efficiency (DefRtg) = 100 x (Opponent points / Opponent possessions)
    df['WDefRtg'] = df.LOffRtg
    df['LDefRtg'] = df.WOffRtg
    #Net Rating = Off.eff - Def.eff
    df['WNetRtg'] = df.apply(lambda row:(row.WOffRtg - row.LDefRtg), axis=1)
    df['LNetRtg'] = df.apply(lambda row:(row.LOffRtg - row.LDefRtg), axis=1)
    #Assist Ratio : Percentage of team possessions that end in assists
    df['WAstR'] = df.apply(lambda row: 100 * row.WAst / (row.WFGA + 0.44*row.WFTA + row.WAst + row.WTO), axis=1)
    df['LAstR'] = df.apply(lambda row: 100 * row.LAst / (row.LFGA + 0.44*row.LFTA + row.LAst + row.LTO), axis=1)
    #Turnover Ratio: Number of turnovers of a team per 100 possessions used.
    #(TO * 100) / (FGA + (FTA * 0.44) + AST + TO
    df['WTOR'] = df.apply(lambda row: 100 * row.LAst / (row.LFGA + 0.44*row.LFTA + row.LAst + row.LTO), axis=1)
    df['LTOR'] = df.apply(lambda row: 100 * row.LAst / (row.LFGA + 0.44*row.LFTA + row.LAst + row.LTO), axis=1)
    #The Shooting Percentage : Measure of Shooting Efficiency (FGA/FGA3, FTA)
    df['WTSP'] = df.apply(lambda row: 100 * row.WScore / (2 * (row.WFGA + 0.44 * row.WFTA)), axis=1)
    df['LTSP'] = df.apply(lambda row: 100 * row.LScore / (2 * (row.LFGA + 0.44 * row.LFTA)), axis=1)
    #eFG% : Effective Field Goal Percentage adjusting for the fact that 3pt shots are more valuable 
    df['WeFGP'] = df.apply(lambda row:(row.WFGM + 0.5 * row.WFGM3) / row.WFGA, axis=1)      
    df['LeFGP'] = df.apply(lambda row:(row.LFGM + 0.5 * row.LFGM3) / row.LFGA, axis=1)   
    #FTA Rate : How good a team is at drawing fouls.
    df['WFTAR'] = df.apply(lambda row: row.WFTA / row.WFGA, axis=1)
    df['LFTAR'] = df.apply(lambda row: row.LFTA / row.LFGA, axis=1)
    #OREB% : Percentage of team offensive rebounds
    df['WORP'] = df.apply(lambda row: row.WOR / (row.WOR + row.LDR), axis=1)
    df['LORP'] = df.apply(lambda row: row.LOR / (row.LOR + row.WDR), axis=1)
    #DREB% : Percentage of team defensive rebounds
    df['WDRP'] = df.apply(lambda row: row.WDR / (row.WDR + row.LOR), axis=1)
    df['LDRP'] = df.apply(lambda row: row.LDR / (row.LDR + row.WOR), axis=1)                                      
    #REB% : Percentage of team total rebounds
    df['WRP'] = df.apply(lambda row: (row.WDR + row.WOR) / (row.WDR + row.WOR + row.LDR + row.LOR), axis=1)
    df['LRP'] = df.apply(lambda row: (row.LDR + row.WOR) / (row.WDR + row.WOR + row.LDR + row.LOR), axis=1)
    return df







def add_in_team_names(df, teams):
    """ Function to add in team names to an existing pandas dataframe with WTeamID and LTeamID fields """
    df = pd.merge(left=df, right=teams[['TeamID', 'TeamName']], 
        left_on='WTeamID', right_on='TeamID', how='inner')
    df = df.rename( columns = { 'TeamName' : 'WTeamName'})
    df = df.drop('TeamID', 1)
    df = pd.merge(left=df, right=teams[['TeamID', 'TeamName']], 
        left_on='LTeamID', right_on='TeamID', how='inner')
    df = df.rename( columns = { 'TeamName' : 'LTeamName'})
    df = df.drop('TeamID', 1)
    return df


def add_in_date(df, seasons):
    df = pd.merge(left=df, right=seasons[['Season', 'DayZero']], 
                                left_on='Season', right_on='Season', how='inner')
    df['game_date'] = df.apply(lambda row: datetime.datetime.strptime(row['DayZero'],"%m/%d/%Y") + datetime.timedelta(days=row['DayNum']), axis=1)
    df = df.drop('DayZero', 1)
    return df




def create_master_train(all_detailed_results):
    """ 
    Function to create master training data set from all detailed results - regular season and tournament 
    Returns pandas dataframe
    """

    def clean_winner_column_names(col_name):
        if col_name=="WLoc":
            col_name="t1_Loc"
        else:
            col_name = col_name.replace("W", "t1_")
            col_name = col_name.replace("L", "t2_")
        return col_name

    def clean_loser_column_names(col_name):
        if col_name=="WLoc":
            col_name="t1_Loc"
        else:
            col_name = col_name.replace("W", "t2_")
            col_name = col_name.replace("L", "t1_")
        return col_name


    winners = all_detailed_results.copy()
    winner_column_names = [clean_winner_column_names(col_name) for col_name in all_detailed_results.columns]
    winners.columns = winner_column_names
    winners['Result'] = 1.0

    losers = all_detailed_results.copy()
    loser_column_names = [clean_loser_column_names(col_name) for col_name in all_detailed_results.columns]
    losers.columns = loser_column_names
    losers['Result'] = 0.0
    losers['score_diff'] = (-1.0 * losers['score_diff'])

    def translate_loc(t1_loc):
        if t1_loc=="H":
            return "A"
        if t1_loc=="A":
            return "H"
        else:
            return "N"

    losers['t1_Loc'] = [translate_loc(loc) for loc in winners['t1_Loc']]
    train_df = pd.concat( [winners, losers]).reset_index(drop = True)

    return train_df




# I don't think this is the approach I want to use to 

def create_rolling_avg_feature(df, field_to_agg, num_games=5):

    # Possible window types to look into
    # https://docs.scipy.org/doc/scipy/reference/signal.html#window-functions

    """ Function to calculate rolling features """
    df = df.sort_values(['t1_TeamID', 'Season','DayNum']).reset_index()
    col_name = field_to_agg + "_rolling_avg_" + str(num_games) + "gm"
    df[col_name] = df.groupby(['Season', 't1_TeamID'])[[field_to_agg]].shift(1) \
        .rolling(window=num_games, min_periods=num_games).mean()
    return df






def calc_logloss(preds, actuals):
    step1 = (actuals * np.log(preds)) + ((1-actuals)*(np.log(1-preds)))
    return (-1/len(step1) * sum(step1))






def add_in_massey_ordinal_field(df, massey_ranking_name, massey_ordinals_df):
    """ Function to add in most recent massey ordinal rank for each team in train """
    df = pd.merge_asof(left=df.sort_values('DayNum'), 
                      right=massey_ordinals_df[massey_ordinals_df['SystemName']==massey_ranking_name].sort_values('DayNum'), 
                      left_on=['DayNum'], right_on=['DayNum'], 
                      left_by=['t1_TeamID', 'Season'],
                      right_by=['TeamID', 'Season'],
                      allow_exact_matches=False)
    t1_col_name = ("t1_" + massey_ranking_name + "_rnk")
    df.rename(columns={'OrdinalRank' : t1_col_name}, inplace=True)
    df.drop(columns='SystemName', inplace=True)
    df.drop(columns='TeamID', inplace=True)
    df = pd.merge_asof(left=df.sort_values('DayNum'), 
                      right=massey_ordinals_df[massey_ordinals_df['SystemName']==massey_ranking_name].sort_values('DayNum'), 
                      left_on=['DayNum'], right_on=['DayNum'], 
                      left_by=['t2_TeamID', 'Season'],
                      right_by=['TeamID', 'Season'],
                      allow_exact_matches=False)
    t2_col_name = ("t2_" + massey_ranking_name + "_rnk")
    df.rename(columns={'OrdinalRank' : t2_col_name}, inplace=True)
    df.drop(columns='SystemName', inplace=True)
    df.drop(columns='TeamID', inplace=True)
    diff_col_name = ("t1_" + massey_ranking_name + "_rnk_diff")
    df[diff_col_name] = df[t1_col_name] - df[t2_col_name]
    return df





def calc_vegas_line_preds(row):
    if row['vegas_line'] >= 0:
        return abs(scipy.stats.norm(0,10.5).cdf(row['vegas_line']) - 1)
    else:
        return 1 - scipy.stats.norm(0,10.5).cdf(row['vegas_line'])








