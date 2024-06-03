#Importing the required modules
import numpy as np
import pandas as pd
import cfbd
import csv

#Configure our API Key
configuration = cfbd.Configuration()
configuration.api_key['Authorization'] = 'YOUR API KEY HERE'
configuration.api_key_prefix['Authorization'] = 'Bearer'

api_config = cfbd.ApiClient(configuration)

#%%
#Putting our requests into a DataFrame
response = games = cfbd.GamesApi(api_config).get_games(year=2023, season_type='both')

data = pd.DataFrame.from_records([g.to_dict() for g in games])
data.head()

#%%
#Getting rid of FCS games and leaving us with FBS teams only
data = data[
    (pd.notna(data['home_pregame_elo'])) #
    & (pd.notna(data['away_pregame_elo']))
]

data = data[
    (data['home_points'] == data['home_points'])
    & (data['away_points'] == data['away_points'])
    & (pd.notna(data['home_conference']))
    & (pd.notna(data['away_conference']))
]

#%%
#Adding homefield advantage column to our DataFrame
data['home_spread'] = np.where(data['neutral_site'] == True, data['home_points'] - data['away_points'], (data['home_points'] - data['away_points'] - 3))
data['away_spread'] = -data['home_spread']
data.head()

#%%
#Capping the Margin of Vicotry to 28 points (this can be modified up and down to your liking)
teams = pd.concat([
    data[['home_team', 'home_spread', 'away_team']].rename(columns={'home_team': 'team', 'home_spread': 'spread', 'away_team': 'opponent'}),
    data[['away_team', 'away_spread', 'home_team']].rename(columns={'away_team': 'team', 'away_spread': 'spread', 'home_team': 'opponent'})
])

teams.head()

teams['spread'] = np.where(teams['spread'] > 28, 28, teams['spread']) # cap the upper bound scoring margin at +28 points
teams['spread'] = np.where(teams['spread'] < -28, -28, teams['spread']) # cap the lower bound scoring margin at -28 points

#%%
spreads = teams.groupby('team').spread.mean()
spreads.head()

#%%
# create empty arrays
terms = []
solutions = []

for team in spreads.keys():
    row = []
    # get a list of team opponents
    opps = list(teams[teams['team'] == team]['opponent'])

    for opp in spreads.keys():
        if opp == team:
        	# coefficient for the team should be 1
            row.append(1)
        elif opp in opps:
        	# coefficient for opponents should be 1 over the number of opponents
            row.append(-1.0/len(opps))
        else:
        	# teams not faced get a coefficient of 0
            row.append(0)

    terms.append(row)

    # average game spread on the other side of the equation
    solutions.append(spreads[team])

solutions = np.linalg.solve(np.array(terms), np.array(solutions))

#%%

ratings = list(zip( spreads.keys(), solutions ))
srs = pd.DataFrame(ratings, columns=['team', 'rating'])
srs.head()
SRS_rankings = srs.sort_values('rating', ascending=False).reset_index()[['team', 'rating']]

#%%
SRS_rankings.to_csv('C:\\Users\\ryann\\Downloads\\SRS_rankings.csv')
