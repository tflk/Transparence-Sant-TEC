# -*- coding: utf-8 -*-
"""
Created on Thu Mar  5 14:59:19 2020

@author: utilisateur
"""

# # -*- coding: utf-8 -*-
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import math
import dash
import dash_core_components as dcc
import dash_html_components as html

#===========================================================================================================

avantage_ = pd.read_csv("C:/Users/utilisateur/Desktop/Brief projet/declaration_avantage_2020_02_19_04_00.csv", sep=";")
convention_ = pd.read_csv("C:/Users/utilisateur/Desktop/Brief projet/declaration_convention_2020_02_19_04_00.csv", sep=";")
remuneration_ = pd.read_csv("C:/Users/utilisateur/Desktop/Brief projet/declaration_remuneration_2020_02_19_04_00.csv", sep=";")
entreprise_ = pd.read_csv("C:/Users/utilisateur/Desktop/Brief projet/entreprise_2020_02_19_04_00.csv", sep=",")

avantage = avantage_.drop_duplicates()
convention = convention_.drop_duplicates()
remuneration = remuneration_.drop_duplicates()
entreprise = entreprise_.drop_duplicates()

#=================================================

secteur_entreprise = entreprise["secteur"].value_counts()

labels = [x for x in secteur_entreprise.index]
values = [x for x in secteur_entreprise.values]
title = "Nombre d'entreprise par secteurs"
fig1 = px.pie(entreprise, values = values, names = labels, title = title)

#=========

top10_pays_entreprise = entreprise.pays.value_counts()[entreprise.pays.value_counts()>10]

labels = ['France', 'Etranger']
values = [x for x in entreprise.pays.value_counts()]
val = [values[0], sum(values[1:])]
fig2 = px.pie(entreprise, values = val, names = labels, title = "Répartition des entreprises françaises et étrangères")

#=========

benef_avant_categorie = avantage.benef_categorie_code.value_counts()
labels = [x for x in benef_avant_categorie.index]
values3 = [math.log(x, 10) for x in benef_avant_categorie.values]

benef_rem_categorie = remuneration.benef_categorie_code.value_counts()
values1 = [math.log(x, 10) for x in benef_rem_categorie.values]

benef_conv_categorie = convention.benef_categorie_code.value_counts()
values2 = [math.log(x, 10) for x in benef_conv_categorie.values]


fig3 = go.Figure(go.Bar(x=labels, y=values1, name='Rémunération'))
fig3.add_trace(go.Bar(x=labels, y=values2, name='Convention'))
fig3.add_trace(go.Bar(x=labels, y=values3, name='Avantage'))

fig3.update_layout(barmode='stack', xaxis={'categoryorder':'total descending'}, title = "Nombre de bénéficiaires par catégorie")

#=========

a = remuneration[["categorie", "remu_montant_ttc"]].groupby(["categorie"]).sum()#.sort_values(by = "remu_montant_ttc", ascending = False)
labels = [x for x in a.index]
values1 = [int(x) for x in a.values]

aa = convention[["categorie", "conv_montant_ttc"]].groupby(["categorie"]).sum()#.sort_values(by = "conv_montant_ttc", ascending = False)
values2 = [int(x) for x in aa.values]

aaa = avantage[["categorie", "avant_montant_ttc"]].groupby(["categorie"]).sum()#.sort_values(by = "avant_montant_ttc", ascending = False)
values3 = [int(x) for x in aaa.values]


fig4 = go.Figure(go.Bar(x=labels, y=values1, name='Rémunération'))
fig4.add_trace(go.Bar(x=labels, y=values2, name='Convention'))
fig4.add_trace(go.Bar(x=labels, y=values3, name='Avantage'))

fig4.update_layout(barmode='stack', xaxis={'categoryorder':'total descending'}, title = "Montant investi par catégorie d'entreprise")

#=========

b = remuneration[["pays", "remu_montant_ttc"]].groupby(["pays"]).mean()#.sort_values(by = "remu_montant_ttc", ascending = False)
labels = [x for x in b.index]
values1 = [int(x) for x in b.values]

bb = convention[["pays", "conv_montant_ttc"]].groupby(["pays"]).mean()#.sort_values(by = "conv_montant_ttc", ascending = False)
values2 = [float(x) for x in bb.values]

bbb = avantage[["pays", "avant_montant_ttc"]].groupby(["pays"]).mean()#.sort_values(by = "avant_montant_ttc", ascending = False)
values3 = [int(x) for x in bbb.values]


fig5 = go.Figure(go.Bar(x=labels, y=values1, name='Rémunération'))
fig5.add_trace(go.Bar(x=labels, y=values2, name='Convention'))
fig5.add_trace(go.Bar(x=labels, y=values3, name='Avantage'))

fig5.update_layout(barmode='stack', xaxis={'categoryorder':'total descending'}, title = "Investissement moyen par pays")

#=========
#%%
c = remuneration[["benef_categorie_code", "remu_montant_ttc"]].groupby(["benef_categorie_code"]).sum()#.sort_values(by = "remu_montant_ttc", ascending = False)
labels = [x for x in c.index]
values1 = [int(x) for x in c.values]

cc = convention[["benef_categorie_code", "conv_montant_ttc"]].groupby(["benef_categorie_code"]).sum()#.sort_values(by = "conv_montant_ttc", ascending = False)
values2 = [int(x) for x in cc.values]

ccc = avantage[["benef_categorie_code", "avant_montant_ttc"]].groupby(["benef_categorie_code"]).sum()#.sort_values(by = "avant_montant_ttc", ascending = False)
values3 = [int(x) for x in ccc.values]


fig6 = go.Figure(go.Bar(x=labels, y=values1, name='Rémunération'))
fig6.add_trace(go.Bar(x=labels, y=values2, name='Convention'))
fig6.add_trace(go.Bar(x=labels, y=values3, name='Avantage'))

fig6.update_layout(barmode='stack', xaxis={'categoryorder':'total descending'}, title = "Montant perçu par catégorie de bénéficiaire")


#=========

e = remuneration[["entreprise_identifiant", "remu_montant_ttc"]].groupby(["entreprise_identifiant"]).sum()#.sort_values(by = "remu_montant_ttc", ascending = False).head(10)
labels = [x for x in e.index]
values1 = [int(x) for x in e.values]

ee = convention[["entreprise_identifiant", "conv_montant_ttc"]].groupby(["entreprise_identifiant"]).sum()#.sort_values(by = "conv_montant_ttc", ascending = False).head(10)
values2 = [int(x) for x in ee.values]

eee = avantage[["entreprise_identifiant", "avant_montant_ttc"]].groupby(["entreprise_identifiant"]).sum()#.sort_values(by = "avant_montant_ttc", ascending = False).head(10)
values3 = [int(x) for x in eee.values]


fig7 = go.Figure(go.Bar(x=labels, y=values1, name='Rémunération'))
fig7.add_trace(go.Bar(x=labels, y=values2, name='Convention'))
fig7.add_trace(go.Bar(x=labels, y=values3, name='Avantage'))

fig7.update_layout(barmode='stack', xaxis={'categoryorder':'total descending'}, title = "Investissement total par entreprise")

#===========================================================================================================

#%%

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

app.layout = html.Div(children=[
    html.H1(children='Transparence - Santé'),
    html.H3(children='TEC'),
    html.Img(src="/assets/entreprises_france.PNG"),
    dcc.Graph(
        id='example-graph1',
        figure= fig2
    ),
    dcc.Graph(
        id='example-graph2',
        figure= fig1
    ),
    dcc.Graph(
        id='example-graph3',
        figure= fig3
    ),
    dcc.Graph(
        id='example-graph4',
        figure= fig4
    ),
    dcc.Graph(
        id='example-graph5',
        figure= fig5
    ),
    dcc.Graph(
        id='example-graph6',
        figure= fig6
    ),
    dcc.Graph(
        id='example-graph7',
        figure= fig7
    ),
    html.Img(src="/assets/boxplot_remun_year_petite_echelle_-_color.png"),
    html.Img(src="/assets/nombre_de_conventions_par_an_et_par_categorie.png"),
    html.Img(src="/assets/moyennes_des_remunerations_par_mois.png"),
    html.Img(src="/assets/moyennes_des_totaux_de_remunerations_par_mois.png")   
])

if __name__ == '__main__':
    app.run_server(debug=False)
    
#%%