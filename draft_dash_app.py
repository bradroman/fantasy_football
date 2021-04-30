#Import libraries
import pandas as pd
import numpy as np
import dash
import dash_core_components as dcc
import dash_html_components as html
import dash_table
from dash.dependencies import Input, Output, State
#Import plotly
import plotly.graph_objects as go
import plotly.express as px
import scipy.stats as stats

#Create the app

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css'] 

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

# Reading in the draft csv 
player_rank_df = pd.read_csv('input/2021_draft_player_rankings.csv')
# Create a random draft order   
player_rank_df['samp_rank'] = stats.truncnorm(
    (player_rank_df.BEST - player_rank_df.AVG) / player_rank_df.STD_DEV, (player_rank_df.WORST - player_rank_df.AVG) / player_rank_df.STD_DEV, 
    loc=player_rank_df.AVG, 
    scale=player_rank_df.STD_DEV).rvs()
draft_df = player_rank_df.sort_values('samp_rank')
draft_df['draft_order'] = np.arange(draft_df.shape[0]) + 1

#filter the columns to show
draft_dash_df = draft_df[['PLAYER NAME','TEAM','AVG','BEST','WORST','position','pos_rank']]

app.layout = html.Div([
    # app headings
    html.H1('Brad\'s Mock Draft App'),
    html.H3('A guide for the yahoo fantasy league'),
    html.Br(),
    # button to select a player and remove them from the draft board
    html.H6('Select Player'),
    dcc.Input(id='input-player', type='text', value=''),
    html.Button(id='submit-button-state', children='Submit'),
    html.Div(id='output-state'),
    html.Br(),
    # table of available players left 
    dash_table.DataTable(id='table',
    columns=[{"name": i, "id": i} for i in draft_dash_df.columns],
    page_action='none',
    style_table={'height': '500px', 'overflowY': 'auto'},
    fixed_rows={'headers': True},
    # need to fix width of total table but this spaces it evenly
    style_cell={
        'minWidth': 95, 'maxWidth': 95, 'width': 35
    },
    # figure this out and print the sorting type?
    sort_action="native",
    filter_action='native',
    row_selectable='single',   
    data=draft_dash_df.to_dict('records')),
    html.Div(id='output_div')
])

@app.callback(Output('output-state', 'children'),
              Input('submit-button-state', 'children'))
def update_output(input1):
    return u'''
        Player Selected: {0}
    '''.format(input1)

@app.callback(
    Output('output_div', 'children'),
    Input('table', 'active_cell'),
    State('table', 'data')
)
def getActiveCell(active_cell, data):
    if active_cell:
        col = active_cell['column_id']
        row = active_cell['row']
        cellData = data[row][col]
        return html.P(f'row: {row}, col: {col}, value: {cellData}')
    return html.P('Select Player')

if __name__ == '__main__':
    app.run_server(debug=True)

