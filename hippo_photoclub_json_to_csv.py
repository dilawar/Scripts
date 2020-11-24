"""
USAGE:
    python3 hippo_photoclub_json_to_csv.py 8 > data.csv

where 8 is the id of the competetion.

"""
import sys
import json
import urllib.request
import pandas as pd

url = f"https://ncbs.res.in/hippo/v1/pub/photographyclub_data/{sys.argv[1]}"
res = urllib.request.urlopen(url)
if res.getcode() == 200:
    x = res.read()
    data = json.loads(x)
    entries = data['entries']
    df = pd.DataFrame(entries)

df.to_csv(sys.stdout, index=False)
