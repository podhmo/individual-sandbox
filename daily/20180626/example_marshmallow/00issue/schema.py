import json
from marshmallow import Schema, fields, post_load


class Version(object):
    def __init__(self, value, archived, label):
        self.value = value
        self.archived = archived
        self.label = label

    class VersionSchema(Schema):
        value = fields.String()
        archived = fields.Boolean()
        label = fields.String()

        @post_load
        def make_object(self, data):
            return Version(**data)

    def __str__(self):
        return ' \n'.join(self.__dict__)

    def __repr__(self):
        for i, v in self.__dict__.items():
            print('{} : {}'.format(i, v))
        return ''


class Versions(object):
    def __init__(self, type, hasAccessToSoftware, unreleasedVersions, releasedVersions=None):
        self.type = type
        self.hasAccessToSoftware = hasAccessToSoftware
        self.unreleasedVersions = unreleasedVersions
        self.releasedVersions = releasedVersions

    class VersionsSchema(Schema):
        type = fields.String(default='')
        hasAccessToSoftware = fields.Boolean(default=False)
        unreleasedVersions = fields.Nested(Version.VersionSchema, many=True, default=[])
        releasedVersions = fields.Nested(Version.VersionSchema, many=True, default=[])

        @post_load
        def make_object(self, data):
            return Versions(**data)

    def __str__(self):
        return ' \n'.join(self.__dict__)

    def __repr__(self):
        for i, v in self.__dict__.items():
            if type(i) is str:
                print(i.__repr__())
            else:
                print('{} : {}'.format(i, v))
        return ''


# TODO Test
# schema = Versions.VersionsSchema(partial=True)
with open("versions.json") as rf:
    zephyr_object = json.load(rf)
    result = Versions.VersionsSchema(partial=True).load(zephyr_object)
# print(result.__str__())
print(result[0].releasedVersions)
