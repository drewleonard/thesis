#!/usr/bin/env python
# coding: utf-8

# In[15]:


from __future__ import division
import pandas as pd
import sys


# In[17]:


df = pd.read_csv('/Users/drewnleonard/Documents/thesis/data/csv/fb_gold.csv')


# In[18]:


# Remove unavailable
df = df[df["AccountGroup"] != 'Unavailable']
df = df[df["Interests"] != 'unavailable']
df = df[['AccountGroup','Interests']]


# In[19]:


group_to_interest = {}
for n, e in df.iterrows():
    
    group = e['AccountGroup']
    interest = e['Interests']
    
    if group not in group_to_interest:
        group_to_interest[group] = {}
    
    if interest not in group_to_interest[group]:
        group_to_interest[group][interest] = 0
    
    group_to_interest[group][interest] += 1


# In[20]:


group_to_group = {}
for group,interest_map in group_to_interest.iteritems():
    for interest,count in interest_map.iteritems():
        
        for inner_group,inner_interest_map in group_to_interest.iteritems():
            for inner_interest,inner_count in inner_interest_map.iteritems():
                
                if group != inner_group and interest == inner_interest:
                    shared = min(count,inner_count)
                    
                    if inner_group not in group_to_group:
                        group_to_group[inner_group] = {}
                    if group not in group_to_group:
                        group_to_group[group] = {}
                    
                    if group not in group_to_group[inner_group]:
                        group_to_group[inner_group][group] = 0
                    if inner_group not in group_to_group[group]:
                        group_to_group[group][inner_group] = 0
                    
                    group_to_group[inner_group][group] += shared
                    group_to_group[group][inner_group] += shared


# In[21]:


group1l = []
group2l = []
weightl = []

for group1, group1_map in group_to_group.iteritems():
    for group2, weight in group1_map.iteritems():
        
        group1l.append(group1)
        group2l.append(group2)
        weightl.append(weight)


# In[22]:


group_to_group_df = pd.DataFrame()
group_to_group_df['group1'] = group1l
group_to_group_df['group2'] = group2l
group_to_group_df['weight'] = weightl


# In[23]:


# Get max weights for normalization
group_weights = {}
for n, e in group_to_group_df.iterrows():
    
    g1 = e['group1']
    g2 = e['group2']
    w = e['weight']
    
    if g1 not in group_weights:
        group_weights[g1] = {}
    if g2 not in group_weights:
        group_weights[g2] = {}
    
    if 'min' not in group_weights[g1]:
        group_weights[g1]['min'] = sys.maxsize
    if 'min' not in group_weights[g2]:
        group_weights[g2]['min'] = sys.maxsize
    
    if 'max' not in group_weights[g1]:
        group_weights[g1]['max'] = -sys.maxsize
    if 'max' not in group_weights[g2]:
        group_weights[g2]['max'] = -sys.maxsize
    
    if w < group_weights[g1]['min']:
        group_weights[g1]['min'] = w
    if w < group_weights[g2]['min']:
        group_weights[g2]['min'] = w
    
    if w > group_weights[g1]['max']:
        group_weights[g1]['max'] = w
    if w > group_weights[g2]['max']:
        group_weights[g2]['max'] = w


# In[24]:


node_name = []
node_id = []
n = 0
seen = set()
for n, e in df.iterrows():
    group = e['AccountGroup']
    if group not in seen:
        node_name.append(group)
        node_id.append(n)
        n += 1
    seen.add(group)


# In[25]:


node_df = pd.DataFrame()
node_df['node'] = node_name
node_df['id'] = node_id


# In[26]:


node_df.to_csv('/Users/drewnleonard/Documents/thesis/data/csv/group_node.csv',index=False)
group_to_group_df.to_csv('/Users/drewnleonard/Documents/thesis/data/csv/group_to_group.csv',index=False)


# In[27]:


node_df.shape


# In[28]:


group_to_group_df.shape


# In[ ]:


# Normalize weights
# normalized = []
# for n, e in group_to_group_df.iterrows():
    
#     g1 = e['group1']
#     g2 = e['group2']
#     w = e['weight']
    
#     g1weights = group_weights[g1]
#     g2weights = group_weights[g2]
    
#     if g1weights['max'] == g1weights['min']:
#         g1norm = 0.5
#     else:
#         g1norm = (w - g1weights['min'])/(g1weights['max'] - g1weights['min'])
    
#     if g2weights['max'] == g2weights['min']:
#         g2norm = 0.5
#     else:
#         g2norm = (w - g2weights['min'])/(g2weights['max'] - g2weights['min'])
    
#     avg_norm = (g1norm + g2norm) / 2
#     normalized.append(avg_norm)
    
# group_to_group_df['weight'] = normalized

