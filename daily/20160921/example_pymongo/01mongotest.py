import pymongo
import unittest
import datetime


def get_client():
    return pymongo.MongoClient("mongodb://localhost:27017/")


def clear_db(client, name):
    return client.drop_database(name)


def get_db(client, name):
    return client[name]


class MongoDBTestCase(unittest.TestCase):
    dbname = ""
    collections = []

    @classmethod
    def setUpClass(cls):
        cls.client = get_client()
        cls.db = get_db(cls.client, cls.dbname)

    @classmethod
    def tearDownClass(cls):
        cls.client.drop_database(cls.dbname)

    def tearDown(self):
        for name in self.collections:
            self.db.drop_collection(name)


class PostDataTests(MongoDBTestCase):
    dbname = "test"
    collections = ["posts"]

    def _getCollection(self):
        return self.db.posts

    def test_insert_one(self):
        post = {
            "author": "Mike",
            "text": "My first blog post",
            "tags": ["mongodb", "python", "pymongo"],
            "date": datetime.datetime.now()
        }
        coll = self._getCollection()
        self.assertEqual(coll.find().count(), 0)
        coll.insert_many([post])
        self.assertEqual(coll.find().count(), 1)

    def test_query_with_condition(self):
        candidates = []
        for i in range(10):
            post = {
                "author": "Mike",
                "i": i,
                "text": "My first blog post",
                "tags": ["mongodb", "python", "pymongo"],
                "date": datetime.datetime.now()
            }
            candidates.append(post)

        coll = self._getCollection()
        coll.insert_many(candidates)
        self.assertEqual(coll.find().count(), 10)
        self.assertEqual(coll.find({"i": {"$lt": 5}}).count(), 5)


if __name__ == "__main__":
    unittest.main()
