import requests
import pprint
import json
import time

creds = {
            "username": "dvb",
            "password": "start123"
        }

r = requests.post('http://localhost:9998/rpc/login', data=creds)
token = r.json()["token"]
heads = {
            'Authorization': 'Bearer {}'.format(token)
        }

def check_jobs():
    time.sleep(4)
    return requests.post('http://localhost:9998/rpc/getJobsState', headers=heads, json={"jobs":[{"job_id": "EXA_STAGE_J_DEFAULT"}]}).json()


r6 = requests.post('http://localhost:9998/rpc/initiateJob', headers=heads, json={"job_id": "EXA_STAGE_J_DEFAULT"})

checker = ''
while checker != 'Succeeded' and checker != 'Incomplete':
    checker=check_jobs()['jobs_state'][0]['job_load_state']

print("\nJob status: {}".format(checker))
