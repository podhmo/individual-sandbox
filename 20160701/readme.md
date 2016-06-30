# djangoのprefetch relatedはgeneric foreign keyにも対応していた。

memo. 以下のようなことが必要

```python
settings.configure(
    DEBUG=True,
    DATABASES={"default": {
        "ENGINE": "django.db.backends.sqlite3",
        "NAME": ":memory:"
    }},
    INSTALLED_APPS=[__name__, "django.contrib.contenttypes"]
)


class Feed(models.Model):
    class Meta:
        db_table = "feed"
        unique_together = ("content_type", "object_id")

    object_id = models.PositiveIntegerField()
    content_type = models.ForeignKey(ContentType)
    content = GenericForeignKey('content_type', 'object_id')
```

てきとうにmodelを作る

```python
class A(models.Model):
    name = models.CharField(max_length=32, default="", blank=False)

    class Meta:
        db_table = "a"
        app_label = __name__


class B(models.Model):
    name = models.CharField(max_length=32, default="", blank=False)

    class Meta:
        db_table = "b"
        app_label = __name__


class C(models.Model):
    name = models.CharField(max_length=32, default="", blank=False)

    class Meta:
        db_table = "c"
        app_label = __name__
```

テキトウにインスタンスを生成する

```python
A.objects.bulk_create([
    A(name="a0", id=1),
    A(name="a1", id=2),
    A(name="a2", id=3)
])
B.objects.bulk_create([
    B(name="b0", id=10),
    B(name="b1", id=20),
    B(name="b2", id=30)
])
C.objects.bulk_create([
    C(name="c0", id=100),
    C(name="c1", id=200),
    C(name="c2", id=300)
])
Feed.objects.bulk_create(
    [Feed(content=a) for a in A.objects.all()]
    + [Feed(content=b) for b in B.objects.all()]
    + [Feed(content=c) for c in C.objects.all()]
)
```

通常(n +1)とprefetch

```python
@contextlib.contextmanager
def with_clear_connection(c, message):
    print("\n========================================")
    print(message)
    print("========================================")
    c.queries_log.clear()
    yield

c = connections["default"]
with with_clear_connection(c, "n + 1"):
    print(len(c.queries))
    content_list = []
    for feed in Feed.objects.all():
        content_list.append(feed.content)
    print(len(c.queries))  # => 1 + 9 * 1 = 10
    print([(o.__class__.__name__, o.id) for o in content_list])

with with_clear_connection(c, "prefetch"):
    print(len(c.queries))
    content_list = []
    for feed in Feed.objects.all().prefetch_related("content"):
        content_list.append(feed.content)
    print(len(c.queries))  # => 1 + 3 * 1 = 10
    print([(o.__class__.__name__, o.id) for o in content_list])
```
