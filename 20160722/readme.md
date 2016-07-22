# python3でsuper classのclassmethodをsuperだけを使って呼べたっけ？

普通に `super()` でよかった。

```python
class A:
    @classmethod
    def for_class(cls):
        print("A", cls)

class B(A):
    @classmethod
    def for_class(cls):
        super().for_class()
        print("B", cls)
```

# django adminの使い方

[やり方はここに書いてある](https://docs.djangoproject.com/ja/1.9/ref/contrib/admin/#overview)

## 各 django appにadmin appを作る

ModelAdminクラスをadmin.pyに作る。

```python
@admin.register(<Model>)
class <Model>Admin(admin.ModelAdmin):
    pass
```

## ログイン可能にする方法

```
update <user model> set is_superuser = 1 where id = <id>
```

## adminのカスタマイズ

### admin画面でのfieldをoptionalにする方法

`blank=True, null=True` にしたものはoptionalになった。 `blank=True` も必要。

```python
    judged_time = models.DateTimeField("審査終了日時", null=True, blank=True)
```

### admin画面の一覧表示で表示されるフィールドを指定する方法

defaultはmodelを `__str__()` した結果が表示されるだけっぽい。`list_display` を指定する。

```python
@admin.register(<Model>)
class <Model>Admin(admin.ModelAdmin):
    readonly_fields = ('ctime', 'utime')
    list_display = [
        "id",
        "order",
        "name",
        "start_time",
        "end_time"
    ]
```

### 一覧画面の特定のフィールドをリンクとして機能させたい

`list_display_links` を指定すれば良い模様。defaultは左端だけがリンクとして機能している。

```python
@admin.register(<Model>)
class <Model>Admin(admin.ModelAdmin):
    list_display_links = ['id', 'name']
```

表示やリンク先をフルにカスタマイズしたい場合には以下の様な感じにする。

```python
@admin.register(<Model>)
class <Model>Admin(admin.ModelAdmin):
    def article_title(ca):
        return '<a href="{href}">{text}</a>'.format(
            href=reverse('admin:article_article_change', args=(ca.article_id,)),
            text="{}{}".format(ca.article.title, "(下書き)" if not ca.article.is_public else "")
        )
    article_title.allow_tags = True
    article_title.short_description = "article_title"

    list_display = [
        "order", "status", article_title
    ]
```

### データの生成を直接行うことは禁止したい

has_add_permissionというmethodが使われるみたい。

```python
@admin.register(<Model>)
class <Model>Admin(admin.ModelAdmin):
    def has_add_permission(self, request):
        """データの生成を直接行うことは禁止したい"""
        return False
```

### 特別な処理を追加したい

actionsというoptionがあるらしい。

```python
@admin.register(<Model>)
class <Model>Admin(admin.ModelAdmin):
    def to_rollback(modeladmin, request, queryset):
        for obj in queryset:
            api.to_rollback_judging(obj)
    to_rollback.short_description = '状態を戻す'
    actions = [to_rollback]
```

### 特別な検索条件で一覧表示したい

list_filterというオプションが有るらしい。joinした条件も行ける。

```python
@admin.register(<Model>)
class <Model>Admin(admin.ModelAdmin):
    list_filter = ["status", "article__title"]
```
[自分で定義するのもそんなに大変じゃない](https://docs.djangoproject.com/ja/1.9/ref/contrib/admin/#django.contrib.admin.ModelAdmin.list_filter)

### データの作成ができないようにしたい

has_add_permissionでFalseを返せば良い

```python
@admin.register(<Model>)
class <Model>Admin(admin.ModelAdmin):
    def has_add_permission(self, request):
        """データの生成を直接行うことは禁止したい"""
        return False
```


### 編集画面中に関連した特殊なフレームを用意したい

inlinesというoptionがあるらしい。

```python
class <Model2>Inline(admin.TabularInline):
    model = <Model2>
    fk_name = 'user'
    fields = ('<model2>', )
    raw_id_fields = ('<model2>', )
    extra = 0

@admin.register(<Model>)
class <Model>Admin(admin.ModelAdmin):
    inlines = [MyInline]
```

inlinesで行われるモデルの生成にフックを仕掛けるのはあまり綺麗なフックポイントが見つからなかった。親の方のオブジェクトのsave_formsetを見るというのが無難かもしれない。

```python
class <Model2>Inline(admin.TabularInline):
    model = <Model2>
    # snip..

@admin.register(<Model>)
class <Model>Admin(admin.ModelAdmin):
    inlines = [MyInline]

    def save_formset(self, request, form, formset, change):
        sub_items = formset.save()
        for sub_item in sub_items:
            if isinstance(sub_item, <Model2>):
                do_something(sub_item)
        return sub_items
```
