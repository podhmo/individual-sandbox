# -*- coding:utf-8 -*-

"""
prefetch related with generic foreign key (selfhand)
"""
import django
import contextlib
from django.db import models
from django.conf import settings
from django.db import connections


settings.configure(
    DEBUG=True,
    DATABASES={"default": {
        "ENGINE": "django.db.backends.sqlite3",
        "NAME": ":memory:"
    }},
    INSTALLED_APPS=[__name__, "django.contrib.contenttypes"]
)
django.setup()


def create_table(model):
    connection = connections['default']
    with connection.schema_editor() as schema_editor:
        schema_editor.create_model(model)


class K(models.Model):
    name = models.CharField(max_length=32, default="", blank=False)

    class Meta:
        db_table = "k"
        app_label = __name__


class A(models.Model):
    name = models.CharField(max_length=32, default="", blank=False)
    k = models.ForeignKey(K)

    class Meta:
        db_table = "a"
        app_label = __name__


class B(models.Model):
    name = models.CharField(max_length=32, default="", blank=False)
    k = models.ForeignKey(K)

    class Meta:
        db_table = "b"
        app_label = __name__


class C(models.Model):
    name = models.CharField(max_length=32, default="", blank=False)
    k = models.ForeignKey(K)

    class Meta:
        db_table = "c"
        app_label = __name__


class CustomPrefetchDescriptor(object):
    def __init__(self, cache_name, choices):
        self.cache_name = cache_name
        self.value_to_model = {k: v for k, v in choices}
        self.model_to_value = {v: k for k, v in choices}

    def is_cached(self, instance):
        return False  # xxx

    def get_prefetch_queryset(self, objs, qs):
        if qs is not None:
            raise ValueError("Custom queryset can't be used for this lookup.")

        from collections import defaultdict
        d = defaultdict(list)
        for ob in objs:
            d[self.value_to_model[ob.content_type]].append(ob.object_id)

        result = []
        for model, id_list in d.items():
            result.extend(model.objects.filter(id__in=id_list))

        return (
            result,
            self.get_rel_obj_attr,
            self.get_instance_attr,
            True,
            self.cache_name
        )

    def get_rel_obj_attr(self, relobj):
        return (self.model_to_value[relobj.__class__], relobj._get_pk_val())

    def get_instance_attr(self, obj):
        return (obj.content_type, obj.object_id)


class Feed(models.Model):
    class Meta:
        db_table = "feed"
        unique_together = ("content_type", "object_id")

    object_id = models.PositiveIntegerField()
    content_type = models.PositiveIntegerField(choices=([(1, "a"), (2, "b"), (3, "c")]))
    content_set = CustomPrefetchDescriptor("content_set", [(1, A), (2, B), (3, C)])

    @property
    def content(self):
        # cached_property?
        return self.content_set.value_to_model[self.content_type].objects.get(id=self.object_id)

    @classmethod
    def with_prefetch(cls, to_attr):
        from django.db.models import Prefetch
        return Prefetch("content_set", to_attr=to_attr)


@contextlib.contextmanager
def with_clear_connection(c, message):
    print("\n========================================")
    print(message)
    print("========================================")
    c.queries_log.clear()
    yield


if __name__ == "__main__":
    create_table(K)
    create_table(A)
    create_table(B)
    create_table(C)
    create_table(Feed)
    import logging
    for name in ['django.db.backends']:
        logger = logging.getLogger(name)
        logger.setLevel(logging.DEBUG)
        logger.addHandler(logging.StreamHandler())

    k = K(name="k")
    k.save()
    k.refresh_from_db()

    A.objects.bulk_create([
        A(name="a0", id=1, k=k),
        A(name="a1", id=2, k=k),
        A(name="a2", id=3, k=k)
    ])
    B.objects.bulk_create([
        B(name="b0", id=10, k=k),
        B(name="b1", id=20, k=k),
        B(name="b2", id=30, k=k)
    ])
    C.objects.bulk_create([
        C(name="c0", id=100, k=k),
        C(name="c1", id=200, k=k),
        C(name="c2", id=300, k=k)
    ])
    Feed.objects.bulk_create(
        [Feed(content_type=1, object_id=a.id) for a in A.objects.all()]
        + [Feed(content_type=2, object_id=b.id) for b in B.objects.all()]
        + [Feed(content_type=3, object_id=c.id) for c in C.objects.all()]
    )

    c = connections["default"]
    with with_clear_connection(c, "n + 1"):
        print(len(c.queries))
        content_list = []
        for feed in Feed.objects.all():
            content_list.append(feed.content)
        print(len(c.queries))  # => 1 + 9 * 1 = 10
        print([(o.__class__.__name__, o.id, o.k) for o in content_list])

    with with_clear_connection(c, "prefetch"):
        print(len(c.queries))
        content_list = []
        qs = Feed.objects.all().prefetch_related(Feed.with_prefetch("content2"), "content2__k")
        for feed in qs:
            content_list.append(feed.content2)
        print(len(c.queries))  # => 1 + 3 * 1 = 10
        print([(o.__class__.__name__, o.id, o.k) for o in content_list])
