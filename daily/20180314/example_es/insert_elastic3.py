import bson
import uuid
from elasticsearch import Elasticsearch
from elasticsearch import helpers

es = Elasticsearch()
actions = []
workspaceId = str(bson.ObjectId())
for i in range(1000):
    doc = {"id": str(bson.ObjectId()), "value": uuid.uuid4().hex, "workspaceId": workspaceId}
    actions.append({'_index': 'values2', '_type': 'values', '_source': doc})
helpers.bulk(es, actions)
