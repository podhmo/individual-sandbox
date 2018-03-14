import bson
import uuid
from elasticsearch import Elasticsearch
from elasticsearch import helpers

es = Elasticsearch()
actions = []
for i in range(1000):
    doc = {"id": str(bson.ObjectId()), "value": uuid.uuid4().hex, "workspaceId": str(bson.ObjectId())}
    actions.append({'_index': 'values', '_type': 'values', '_source': doc})
helpers.bulk(es, actions)
