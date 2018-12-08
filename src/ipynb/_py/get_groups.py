#!/usr/bin/env python
# coding: utf-8

# In[1]:


import re
import os
import json
import pandas as pd
from difflib import SequenceMatcher


# In[2]:


def load_ad(path):
    with open(path) as f:
        return json.load(f)


# In[3]:


def get_ad_group(ad):
    
    if 'second' not in ad:
        return None
    
    if ad == 'black matters':
        return ad
    
    # Index down to get ad text
    ad_text = ad['second']['text']['textAnnotations'][0]['description'].splitlines()
    
    if 'instagram' in ad_text[0].lower() or 'sponsored' in ad_text[0].lower() or 'suggested' in ad_text[0].lower():
        return ad_text[1]
    return ad_text[0]


# In[4]:


def get_ad_id(ad):
    
    # Index down
    ad_text = ad['first']['text']['fullTextAnnotation']['text']
    ad_id_line = ad_text.splitlines()[0]
    
    # Parse id
    ad_id = re.search(r'\d+', ad_id_line).group()
    
    return ad_id


# In[5]:


def clean_first_page_text(text):
    
    remove_tokens = ['shared', 'added', 'updated']
    for token in remove_tokens:
        if token in text:
            text = text.split(token,1)[0]
    return text


# In[6]:


def similar(a, b):
    return SequenceMatcher(None, a, b).ratio()


# In[7]:


groups = set([
 u'black matters',
 u'afrokingdom',
 u'american.made',
 u'american.veterans',
 u'angry eagle',
 u'army of jesus',
 u'back the badge',
 u'baltimore.blackvoice',
 u'being patriotic',
 u'bernie sanders for president',
 u'black baptist church',
 u'black edification',
 u'black excellence',
 u'black guns matter',
 u'black matters',
 u'black matters us',
 u'black4black',
 u'blackluive',
 u'blacks go viral',
 u'blackstagram',
 u'blacktivist',
 u'bm',
 u'born black',
 u'born liberal',
 u'brown power',
 u'cop block us',
 u'defend the 2nd',
 u"don't shoot",
 u'donald trump america',
 u'fit black',
 u'gov spending',
 u'heart of texas',
 u'hell_and_back',
 u'instagnan',
 u'isagan',
 u'justice for ezell ford and donnell thompson',
 u'l for life',
 u'lgbt united',
 u'liberty_rising',
 u'make america great again donald j trump',
 u'mbm',
 u'melanie black',
 u'melanie panther',
 u'memopolis',
 u'mericanfury',
 u'muslim voice',
 u'musliminst',
 u'native americans united',
 u"nefertiti's community",
 u'pan-african roots move',
 u'panther melanie',
 u"patriot's heart",
 u'pray4police',
 u'proud blacks',
 u'rebeltexas',
 u'secured borders',
 u'sincerely_black',
 u'south united',
 u'southern.rebel.pride',
 u'stand for freedom',
 u'stop a.l.',
 u'stop refugees',
 u'the red pill',
 u'trumpsters united',
 u'united muslims of america',
 u'veterans come first',
 u'watch the police',
 u'williams&kalvin',
 u'woke blacks'])


# In[18]:


exclude = set([
u'ad creation date 03/17/16 04:21:38 am pdt',
 u'ad creation date 07/21/15 06:54:01 am pdt',
 u'ad end date 06/24/15 07:03:47 am pdt',
 u'ad impressions 1,439',
 u'ad impressions 19,055',
 u'ad impressions 306',
 u'age: 18-65+'
])


# In[41]:


results = {}
candidates = set()
rootdir = '/Users/drewnleonard/Documents/thesis/data/json/ads/'
for subdir, dirs, files in os.walk(rootdir):
    for file in files:
            
        path = os.path.join(subdir, file)
        if '.json' not in path:
            continue
        ad = load_ad(path)
        
        if 'second' not in ad:
            continue
        
        ad_text = ad['second']['text']['textAnnotations'][0]['description'].splitlines()
        found_group = clean_first_page_text(ad_text[0]).lower()
        
        if found_group in exclude:
            continue
        
        next_page_tokens = ['like page', 'instagram', 'suggested page', 'sponsored', 'post not exist']
        for next_page_token in next_page_tokens:
            if next_page_token in found_group or found_group in next_page_token or similar(found_group, next_page_token) > 0.9:
                found_group = ad_text[1].lower()
                
        if found_group in ['post not exist: post that is used for', 'sponsored', 'this preview does not exist.']:
            continue
                
        if found_group == 'mbm' or found_group == 'atch':
            found_group = 'bm'
        
        if found_group == 'atch':
            found_group = 'watch the police'
        
        max_group = {
            "name": "",
            "score": 0
        }
        
        for group in groups:
            if similar(group, found_group) > max_group['score']:
                max_group['score'] = similar(group, found_group)
                max_group['name'] = group
        
        found_group = max_group['name']
        
        if found_group == '':
            continue
        
        results[get_ad_id(ad)] = found_group


# In[42]:


json.dump(results, open('/Users/drewnleonard/Documents/thesis/data/json/group_keys.json', 'wb'))

