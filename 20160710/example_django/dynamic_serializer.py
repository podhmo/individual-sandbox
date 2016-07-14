# -*- coding:utf-8 -*-
"""
contextに従ってrelation先のsub serializerを変更したかった
e.g. DynamicSerializer(context={"foo__bar": MyFooBarSerializer})
"""

from django.conf import settings
settings.configure(
    DEBUG=True,
    ALLOWED_HOSTS=['*'],
    INSTALLED_APPS=[
        "django.contrib.staticfiles",
        "django.contrib.contenttypes",
        "django.contrib.auth",
        "rest_framework",
    ],
    STATIC_URL='/static/',
    MIDDLEWARE_CLASSES=(
        'django.middleware.common.CommonMiddleware',
    ),
    REST_FRAMEWORK={
        "DEFAULT_PERMISSION_CLASS": [
            "rest_framework.permissions.AllowAny"
        ]
    },
    DATABASES={"default": {
        "ENGINE": "django.db.backends.sqlite3",
        "NAME": ":memory:"
    }},
    CACHES={
        'default': {
            'BACKEND': 'django.core.cache.backends.locmem.LocMemCache',
        }
    }
)

from rest_framework import serializers  # NOQA


# darui
def get_dynamic_fields(parent_serializer):
    pass


class DynamicSerializer(serializers.Serializer):
    def get_fields(self):
        pass
