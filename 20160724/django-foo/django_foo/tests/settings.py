DEBUG = True
SECRET_KEY = "test"
INSTALLED_APPS = [
    'django_foo.tests',
]
DATABASES = {"default": {
    "ENGINE": "django.db.backends.sqlite3",
    "NAME": ":memory:"
}}
CACHES = {
    'default': {
        'BACKEND': 'django.core.cache.backends.locmem.LocMemCache',
    }
}
