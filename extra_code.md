# Removed from network_analysis.ipynb

Cells removed while fixing the same-item correlation bug (see `fix_item_response.md`)
and cleaning up the notebook. These were dead/broken given the current
`data_clean/horizontial_df.csv` and `data_clean/vertical_extension.csv` column names
(most reference columns from an older version of `scripts/5_prepare_network.R`,
e.g. `Q73_Defence_security`, `send_weapons_SA`, `security_focused`, `Russian_collab`,
`Q9_1`, `Q13`), or were bare variable dumps / superseded duplicates of the two
working pipelines that remain in the notebook (horizontal -> `horizontial_coherence_network.gexf`,
vertical -> `vertical_extension_network.gexf`). Kept here in case any of the
logic or hardcoded column groupings is still wanted for reference.

## Bare variable dumps

```python
df
```

```python
dictionary = df.iloc[0,:].copy()
```

```python
dictionary
```

```python
pos
```

```python
x_pca
```

```python
xcor_att
```

```python
x_pca_2
```

## First-draft graph build (superseded by the final `fcadab26`/`debfbfe7` pipelines; also had the `c not in [fa_nodes, 'Unnamed: 0']` bug, which never actually excluded `fa_nodes` since it compared each column name against a list containing the whole `fa_nodes` list as one element)

```python
remove_name = None

metrics = pears_pos

fa_nodes     = ['Defence_Focused', 'Normalizaiton_Focused']
list_of_nodes = [c for c in df.columns if c not in [fa_nodes, 'Unnamed: 0']]

G = make_graph_(df, list_of_nodes, metrics, exclude_same_question=True, print_=True)
key_list = list(G.nodes)
```

```python
print(G.number_of_edges())  # if 0, that's your problem
print(G.number_of_nodes())
```

```python
edges_to_remove = [(u, v) for u, v, d in G.edges(data=True) if np.isnan(d['weight'])]
G.remove_edges_from(edges_to_remove)
```

```python
print([(u, v, d) for u, v, d in G.edges(data=True) if d.get('weight') != d.get('weight')])
```

## Item-level coding for an older 5-point Likert dataset (`Q9_1`/`Q9_3`, "Strongly Agree" etc.) that no longer exists in the current data. `node.split('')` also raises `ValueError: empty separator` in Python.

```python
dic_ = dict()

for node in G.nodes:
    name = node.split('')
    
    if name[0] == "Q9_3" or name[0] == "Q9_1":
        if name[1] == "Strongly Agree":
            dic_[node] = 5
        if name[1] == "Somewhat Agree":
            dic_[node] = 4
        if name[1] == "Neutral":
            dic_[node] = 3
        if name[1] == "Somewhat Disagree":
            dic_[node] = 2
        if name[1] == "Strongly Disagree":
            dic_[node] = 1
    else:
        if name[1] == "Strongly Agree":
            dic_[node] = 1
        if name[1] == "Somewhat Agree":
            dic_[node] = 2
        if name[1] == "Neutral":
            dic_[node] = 3
        if name[1] == "Somewhat Disagree":
            dic_[node] = 4
        if name[1] == "Strongly Disagree":
            dic_[node] = 5

nx.set_node_attributes(G, dic_, "level")
```

## Self-identification correlation using `df["Q13"]`, a column that doesn't exist in the current `horizontial_df.csv`

```python
dic_ = dict()

for type_ in ["continuous","discrete"]:
    for id_ in ['Republican', 'Independent', 'Democrat']:
        col_id = df["Q13"]==id_
        for node in G.nodes:
            col_node = df[node]

            r = stt.pearsonr(col_node,col_id)[0]
            
            if type_=="discrete":
                dic_[node] = np.sign(r)
            else:
                dic_[node] = r

        nx.set_node_attributes(G, dic_, id_+"_"+type_)
```

## Layout + PCA feeding the dead first-draft graph above

```python
pos = nx.spring_layout(G,iterations=50000*10)

pos2 = [[],[]]
key_list = []
for key in pos:
    pos2[0].append(pos[key][0])
    pos2[1].append(pos[key][1])
    key_list.append(key)
    
pos3 = []
keys_vec = []
for key in pos:
    pos3.append([pos[key][0],pos[key][1]])
    keys_vec.append(key)
```

```python
pca = PCA(n_components=2)
pca.fit(pos3)
x_pca = pca.transform(pos3)
```

## Exploratory plot using an older column set (`Q73_*`, `send_weapons_*`, `sanctions_*`, `US_trust`, `China_trust`, `Terrorism_threat`) not present in the current `horizontial_df.csv` -- would render every node gray since none of the color-map keys match

```python
# Show the result


# Step 3: derive dominant FA per node (for colour)


plt.figure()
xx = x_pca[:,0]
yy = x_pca[:,1]

mm = min(xx)*1.1
MM = max(xx)*1.1

koef = 3
for i1, key1 in enumerate(key_list):
    for i2, key2 in enumerate(key_list):
        if (key1, key2) in G.edges:
            w = G.get_edge_data(key1,key2)['weight']

            if w > 0.:
                plt.plot([xx[i1],xx[i2]],[yy[i1],yy[i2]],'k',linewidth=koef*w**2,zorder=1)

trust_nodes = ['US_trust', 'Russia_trust', 'Ukraine_trust', 'China_trust', 'Trust_EU']
threat_nodes = ['Terrorism_threat', 'Russia_threat', 'US_threat', 'China_threat', 
                'Nuclear_threat', 'Other_threat', 'DK_threat']
fa_nodes     = [ 'Q73_Defence_security',
         'Q73_trade_diplomacy',
         'Q73_DK',
         'send_weapons_SA',
         'send_weapons_A',
         'send_weapons_D',
         'send_weapons_SD',
         'sanctions_SA',
         'sanctions_A',
         'sanctions_D',
         'sanctions_SD']
other_nodes  = ['Referendum', 'NATO_support']

color_map = {}
for k in trust_nodes:  color_map[k] = 'steelblue'
for k in threat_nodes: color_map[k] = 'tomato'
for k in fa_nodes:     color_map[k] = 'seagreen'
for k in other_nodes:  color_map[k] = 'gold'

texts = []
for i, key in enumerate(key_list):
    c = color_map.get(key, 'gray')
    plt.scatter(xx[i], yy[i], s=50, c=c, zorder=2)
    texts.append(plt.text(xx[i], yy[i], key, fontsize=8))
    
adjust_text(texts, arrowprops=dict(arrowstyle='-', color='gray', lw=0.5))

index = np.array(range(0,len(xx)))+1

plt.xlim([mm, MM])
plt.ylim([mm, MM])
#plt.grid()
```

## Rebuilt layout + broken duplicate plot (crashes with `KeyError` on re-run: `fa_nodes` here is left over as the old 11-item list set in the cell above, but `fa_colors` only has `Defence_Focused`/`Normalizaiton_Focused`, so `fa_colors[dominant_fa]` fails)

```python
# Rebuild layout with the new G (FA nodes excluded)
pos = nx.spring_layout(G, iterations=50000*10, seed=1998)

pos2 = [[],[]]
key_list = []
for key in pos:
    pos2[0].append(pos[key][0])
    pos2[1].append(pos[key][1])
    key_list.append(key)
    
pos3 = []
keys_vec = []
for key in pos:
    pos3.append([pos[key][0],pos[key][1]])
    keys_vec.append(key)

pca = PCA(n_components=2)
pca.fit(pos3)
x_pca = pca.transform(pos3)

xx = x_pca[:, 0]
yy = x_pca[:, 1]
```

```python

mm = min(xx) * 1.1
MM = max(xx) * 1.1
koef = 3

fa_colors = {
    'Defence_Focused': 'steelblue',
    'Normalizaiton_Focused':  'tomato'
}

node_colors = {}
for node in G.nodes:
    attrs = nx.get_node_attributes(G, node)  # won't work — use this instead:
    correlations = {fa: pearsonr(df[node], df[fa])[0] for fa in fa_nodes}
    dominant_fa = max(correlations, key=correlations.get)
    node_colors[node] = fa_colors[dominant_fa]

# Step 1: exclude FA nodes from graph (they aren't attitudes, they're factors)
list_of_nodes = [c for c in df.columns if c not in fa_nodes]
G = make_graph_(df, list_of_nodes, pears_pos, exclude_same_question=True)

# Step 2: attach FA correlations as node attributes (following the heatmap pattern)
for fa in fa_nodes:
    dic_ = {}
    for node in G.nodes:
        r = pearsonr(df[node], df[fa])[0]
        dic_[node] = r
    nx.set_node_attributes(G, dic_, fa)   # e.g. attribute "security_focused"


node_colors = {}
for node in G.nodes:
    attrs = nx.get_node_attributes(G, node)  # won't work — use this instead:
    correlations = {fa: pearsonr(df[node], df[fa])[0] for fa in fa_nodes}
    dominant_fa = max(correlations, key=correlations.get)
    node_colors[node] = fa_colors[dominant_fa]

plt.figure(figsize=(10, 10))

# Edges
for i1, key1 in enumerate(key_list):
    for i2, key2 in enumerate(key_list):
        if (key1, key2) in G.edges:
            w = G.get_edge_data(key1, key2)['weight']
            if w > 0.:
                plt.plot(
                    [xx[i1], xx[i2]], [yy[i1], yy[i2]],
                    color='black',
                    linewidth=koef * w,
                    alpha=1, zorder=1
                )

# Nodes + labels
texts = []
for i, key in enumerate(key_list):
    plt.scatter(xx[i], yy[i], s=80,
                c=node_colors[key], zorder=2,
                edgecolors='k', linewidths=0.5)
    texts.append(plt.text(xx[i], yy[i], key, fontsize=8))

adjust_text(texts, arrowprops=dict(arrowstyle='-', color='gray', lw=0.5))

# Legend
from matplotlib.patches import Patch
legend_elements = [Patch(facecolor=c, label=fa) for fa, c in fa_colors.items()]
plt.legend(handles=legend_elements, loc='best')

plt.xlim([mm, MM])
plt.ylim([mm, MM])
plt.tight_layout()
plt.show()
```

## Normalized x-coordinate attribute, computed from the dead first-draft graph/layout above and never attached to `G` or saved anywhere

```python
xcor_att = dict()
ycor_att = dict()

min_ = 1000
max_ = -min_

for i,key in enumerate(keys_vec):
    xcor_att[key] = xx[i]
    ycor_att[key] = yy[i]
    
    if xx[i]<min_:
        min_ = xx[i]
        
    if xx[i]>max_:
        max_= xx[i]
```

```python
for key in xcor_att:
    val = xcor_att[key]
    
    val1 = (val-min_)/(max_-min_)
    val2 = (val1*2) - 1
    
    xcor_att[key] = val2
```

## Duplicate `df2` read (redundant with the one kept before the vertical pipeline)

```python
df2 = pd.read_csv("data_clean/vertical_extension.csv")
```

## Vertical "unpooled" graph (G2) block -- built around `fa_nodes = ['security_focused', 'Russian_collab', 'conditional', 'domestic']`, none of which exist in the current `data_clean/vertical_extension.csv` (would `KeyError` as soon as `df2[fa]` is looked up); also has the same `c not in [fa_nodes, 'Unnamed: 0']` bug as the first horizontal draft

```python
fa_nodes     = ['security_focused', 'Russian_collab', 'conditional','domestic']
list_of_nodes = [c for c in df2.columns if c not in [fa_nodes, 'Unnamed: 0']]

G2 = make_graph_(df2, list_of_nodes, metrics, exclude_same_question=True, print_=True)
```

```python
pos_2 = nx.spring_layout(G2,iterations=50000*10)

pos2_2 = [[],[]]
key_list_2 = []
for key in pos_2:
    pos2_2[0].append(pos_2[key][0])
    pos2_2[1].append(pos_2[key][1])
    key_list_2.append(key)
    
pos3_2 = []
keys_vec_2 = []
for key in pos_2:
    pos3_2.append([pos_2[key][0],pos_2[key][1]])
    keys_vec_2.append(key)
```

```python
pca_2 = PCA(n_components=2)
pca_2.fit(pos3_2)
x_pca_2 = pca_2.transform(pos3_2)
```

```python
fa_colors = {
    'security_focused': 'steelblue',
    'Russian_collab':   'tomato',
    'conditional':      'seagreen',
    'domestic':         'gold'
}

for fa in fa_nodes:
    dic_ = {}
    for node in G2.nodes:
        r = pearsonr(df2[node], df2[fa])[0]
        dic_[node] = r
    nx.set_node_attributes(G2, dic_, fa)   # e.g. attribute "security_focused"

# Step 3: derive dominant FA per node (for colour)
node_colors_2 = {}
for node in G2.nodes:
    attrs = nx.get_node_attributes(G2, node)  # won't work — use this instead:
    correlations = {fa: pearsonr(df2[node], df2[fa])[0] for fa in fa_nodes}
    dominant_fa = max(correlations, key=correlations.get)
    node_colors_2[node] = fa_colors[dominant_fa]

# --- Plot ---
xx_2 = x_pca_2[:, 0]
yy_2 = x_pca_2[:, 1]
mm_2 = min(xx_2) * 1.1
MM_2 = max(xx_2) * 1.1
koef = 3

plt.figure(figsize=(10, 10))

# Edges
for i1, key1 in enumerate(key_list_2):
    for i2, key2 in enumerate(key_list_2):
        if (key1, key2) in G2.edges:
            w = G2.get_edge_data(key1, key2)['weight']
            if w > 0.:
                plt.plot(
                    [xx_2[i1], xx_2[i2]], [yy_2[i1], yy_2[i2]],
                   # color=edge_color_2(key1, key2),
                    linewidth=koef * w,
                    alpha=1, zorder=1
                )

# Nodes + labels
texts = []
for i, key in enumerate(key_list_2):
    plt.scatter(xx_2[i], yy_2[i], s=80,
                c=node_colors_2[key], zorder=2,
                edgecolors='k', linewidths=0.5)
    texts.append(plt.text(xx_2[i], yy_2[i], key, fontsize=8))

adjust_text(texts, arrowprops=dict(arrowstyle='-', color='gray', lw=0.5))

# Legend
from matplotlib.patches import Patch
legend_elements = [Patch(facecolor=c, label=fa) for fa, c in fa_colors.items()]
plt.legend(handles=legend_elements, loc='best')

plt.xlim([mm_2, MM_2])
plt.ylim([mm_2, MM_2])
plt.tight_layout()
plt.show()
```

```python

plt.figure()
xx = x_pca_2[:,0]
yy = x_pca_2[:,1]

mm = min(xx)*1.1
MM = max(xx)*1.1

koef = 3
for i1, key1 in enumerate(key_list_2):
    for i2, key2 in enumerate(key_list_2):
        if (key1, key2) in G2.edges:
            w = G2.get_edge_data(key1,key2)['weight']

            if w > 0.:
                plt.plot([xx[i1],xx[i2]],[yy[i1],yy[i2]],'k',linewidth=koef*w**2,zorder=1)

democracy_nodes = ['Support_aggrandizement', 'Neutral_aggrandizement',
         'Oppose_aggrandizement',
         'Low_democracy',
         'Middle_democracy',
         'High_democracy']
trust_nodes = ['General_trust_trust',
         'General_trust_notrust']
EU_punish_nodes = [ 'EU_punish_nointerference',
         'EU_punish_financial',
         'EU_punish_voting',
         'EU_punish_other']
threat_nodes = [ 'Immigration_Support_Low',
         'Immigration_Support_Mid',
         'Immigration_Support_High']
fa_nodes     = ['security_focused', 'Russian_collab', 'conditional','domestic']
other_nodes  = ['Climate', 'Energy', 'Inflation', 'Unnamed: 0']

color_map = {}
for k in trust_nodes:  color_map[k] = 'steelblue'
for k in threat_nodes: color_map[k] = 'tomato'
for k in fa_nodes:     color_map[k] = 'seagreen'
for k in other_nodes:  color_map[k] = 'gold'
for k in democracy_nodes:  color_map[k] = 'orange'
for k in EU_punish_nodes:  color_map[k] = 'purple'


texts = []
for i, key in enumerate(key_list_2):
    c = color_map.get(key, 'gray')
    plt.scatter(xx[i], yy[i], s=50, c=c, zorder=2)
    texts.append(plt.text(xx[i], yy[i], key, fontsize=7))
    
adjust_text(texts, arrowprops=dict(arrowstyle='-', color='gray', lw=1))

index = np.array(range(0,len(xx)))+1

plt.xlim([mm, MM])
plt.ylim([mm, MM])
#plt.grid()
```
