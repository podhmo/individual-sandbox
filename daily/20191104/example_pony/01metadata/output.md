## Category

```
{'_columns': <sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98d79d0>,
 '_extra_dependencies': set(),
 '_prefixes': [],
 'columns': <sqlalchemy.sql.base.ImmutableColumnCollection object at 0x7fe3f98f6a00>,
 'constraints': {PrimaryKeyConstraint(Column('id', INTEGER(), table=<Category>, primary_key=True, nullable=False)),
                 UniqueConstraint(Column('name', TEXT(), table=<Category>, nullable=False))},
 'description': 'Category',
 'dialect_kwargs': <sqlalchemy.sql.base._DialectArgView object at 0x7fe3f98d72d0>,
 'dialect_options': {},
 'dispatch': <sqlalchemy.event.base.DDLEventsDispatch object at 0x7fe3f98ecef0>,
 'foreign_keys': set(),
 'fullname': 'Category',
 'implicit_returning': True,
 'indexes': set(),
 'metadata': MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)),
 'name': 'Category',
 'primary_key': PrimaryKeyConstraint(Column('id', INTEGER(), table=<Category>, primary_key=True, nullable=False)),
 'schema': None}
```

## Customer

```
{'_columns': <sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98d7890>,
 '_extra_dependencies': set(),
 '_prefixes': [],
 'columns': <sqlalchemy.sql.base.ImmutableColumnCollection object at 0x7fe3f98ee0a0>,
 'constraints': {PrimaryKeyConstraint(Column('id', INTEGER(), table=<Customer>, primary_key=True, nullable=False)),
                 UniqueConstraint(Column('email', TEXT(), table=<Customer>, nullable=False))},
 'description': 'Customer',
 'dialect_kwargs': <sqlalchemy.sql.base._DialectArgView object at 0x7fe3f98e4650>,
 'dialect_options': {},
 'dispatch': <sqlalchemy.event.base.DDLEventsDispatch object at 0x7fe3f98ec290>,
 'foreign_keys': set(),
 'fullname': 'Customer',
 'implicit_returning': True,
 'indexes': set(),
 'metadata': MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)),
 'name': 'Customer',
 'primary_key': PrimaryKeyConstraint(Column('id', INTEGER(), table=<Customer>, primary_key=True, nullable=False)),
 'schema': None}
```

## Product

```
{'_columns': <sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98d7950>,
 '_extra_dependencies': set(),
 '_prefixes': [],
 'columns': <sqlalchemy.sql.base.ImmutableColumnCollection object at 0x7fe3f98e8f50>,
 'constraints': {PrimaryKeyConstraint(Column('id', INTEGER(), table=<Product>, primary_key=True, nullable=False))},
 'description': 'Product',
 'dialect_kwargs': <sqlalchemy.sql.base._DialectArgView object at 0x7fe3f98d7850>,
 'dialect_options': {},
 'dispatch': <sqlalchemy.event.base.DDLEventsDispatch object at 0x7fe3f9a9b560>,
 'foreign_keys': set(),
 'fullname': 'Product',
 'implicit_returning': True,
 'indexes': set(),
 'metadata': MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)),
 'name': 'Product',
 'primary_key': PrimaryKeyConstraint(Column('id', INTEGER(), table=<Product>, primary_key=True, nullable=False)),
 'schema': None}
```

## sqlite_sequence

```
{'_columns': <sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98fa450>,
 '_extra_dependencies': set(),
 '_prefixes': [],
 'columns': <sqlalchemy.sql.base.ImmutableColumnCollection object at 0x7fe3f9902fa0>,
 'constraints': {PrimaryKeyConstraint()},
 'description': 'sqlite_sequence',
 'dialect_kwargs': <sqlalchemy.sql.base._DialectArgView object at 0x7fe3f98ffd50>,
 'dialect_options': {},
 'dispatch': <sqlalchemy.event.base.DDLEventsDispatch object at 0x7fe3f98870e0>,
 'foreign_keys': set(),
 'fullname': 'sqlite_sequence',
 'implicit_returning': True,
 'indexes': set(),
 'metadata': MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)),
 'name': 'sqlite_sequence',
 'primary_key': PrimaryKeyConstraint(),
 'schema': None}
```

## CartItem

```
{'_columns': <sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f9aa1410>,
 '_extra_dependencies': set(),
 '_prefixes': [],
 'columns': <sqlalchemy.sql.base.ImmutableColumnCollection object at 0x7fe3f98c8140>,
 'constraints': {ForeignKeyConstraint(<sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98e4e90>, None, link_to_name=True, table=Table('CartItem', MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)), Column('id', INTEGER(), table=<CartItem>, primary_key=True, nullable=False), Column('quantity', INTEGER(), table=<CartItem>, nullable=False), Column('customer', INTEGER(), ForeignKey('Customer.id'), table=<CartItem>, nullable=False), Column('product', INTEGER(), ForeignKey('Product.id'), table=<CartItem>, nullable=False), schema=None)),
                 ForeignKeyConstraint(<sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98d7e50>, None, link_to_name=True, table=Table('CartItem', MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)), Column('id', INTEGER(), table=<CartItem>, primary_key=True, nullable=False), Column('quantity', INTEGER(), table=<CartItem>, nullable=False), Column('customer', INTEGER(), ForeignKey('Customer.id'), table=<CartItem>, nullable=False), Column('product', INTEGER(), ForeignKey('Product.id'), table=<CartItem>, nullable=False), schema=None)),
                 PrimaryKeyConstraint(Column('id', INTEGER(), table=<CartItem>, primary_key=True, nullable=False))},
 'description': 'CartItem',
 'dialect_kwargs': <sqlalchemy.sql.base._DialectArgView object at 0x7fe3f9aa1e90>,
 'dialect_options': {},
 'dispatch': <sqlalchemy.event.base.DDLEventsDispatch object at 0x7fe3f9a83a70>,
 'foreign_keys': {ForeignKey('Customer.id'), ForeignKey('Product.id')},
 'fullname': 'CartItem',
 'implicit_returning': True,
 'indexes': {Index('idx_cartitem__product', Column('product', INTEGER(), ForeignKey('Product.id'), table=<CartItem>, nullable=False)),
             Index('idx_cartitem__customer', Column('customer', INTEGER(), ForeignKey('Customer.id'), table=<CartItem>, nullable=False))},
 'metadata': MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)),
 'name': 'CartItem',
 'primary_key': PrimaryKeyConstraint(Column('id', INTEGER(), table=<CartItem>, primary_key=True, nullable=False)),
 'schema': None}
```

## Category_Product

```
{'_columns': <sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98d7710>,
 '_extra_dependencies': set(),
 '_prefixes': [],
 'columns': <sqlalchemy.sql.base.ImmutableColumnCollection object at 0x7fe3f98f8410>,
 'constraints': {ForeignKeyConstraint(<sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98fa150>, None, link_to_name=True, table=Table('Category_Product', MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)), Column('category', INTEGER(), ForeignKey('Category.id'), table=<Category_Product>, primary_key=True, nullable=False), Column('product', INTEGER(), ForeignKey('Product.id'), table=<Category_Product>, primary_key=True, nullable=False), schema=None)),
                 ForeignKeyConstraint(<sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98f4c90>, None, link_to_name=True, table=Table('Category_Product', MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)), Column('category', INTEGER(), ForeignKey('Category.id'), table=<Category_Product>, primary_key=True, nullable=False), Column('product', INTEGER(), ForeignKey('Product.id'), table=<Category_Product>, primary_key=True, nullable=False), schema=None)),
                 PrimaryKeyConstraint(Column('category', INTEGER(), ForeignKey('Category.id'), table=<Category_Product>, primary_key=True, nullable=False), Column('product', INTEGER(), ForeignKey('Product.id'), table=<Category_Product>, primary_key=True, nullable=False))},
 'description': 'Category_Product',
 'dialect_kwargs': <sqlalchemy.sql.base._DialectArgView object at 0x7fe3f98f4a50>,
 'dialect_options': {},
 'dispatch': <sqlalchemy.event.base.DDLEventsDispatch object at 0x7fe3f98e93b0>,
 'foreign_keys': {ForeignKey('Category.id'), ForeignKey('Product.id')},
 'fullname': 'Category_Product',
 'implicit_returning': True,
 'indexes': {Index('idx_category_product', Column('product', INTEGER(), ForeignKey('Product.id'), table=<Category_Product>, primary_key=True, nullable=False))},
 'metadata': MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)),
 'name': 'Category_Product',
 'primary_key': PrimaryKeyConstraint(Column('category', INTEGER(), ForeignKey('Category.id'), table=<Category_Product>, primary_key=True, nullable=False), Column('product', INTEGER(), ForeignKey('Product.id'), table=<Category_Product>, primary_key=True, nullable=False)),
 'schema': None}
```

## Order

```
{'_columns': <sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98f4e50>,
 '_extra_dependencies': set(),
 '_prefixes': [],
 'columns': <sqlalchemy.sql.base.ImmutableColumnCollection object at 0x7fe3f98f8be0>,
 'constraints': {ForeignKeyConstraint(<sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98ff310>, None, link_to_name=True, table=Table('Order', MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)), Column('id', INTEGER(), table=<Order>, primary_key=True, nullable=False), Column('state', TEXT(), table=<Order>, nullable=False), Column('date_created', DATETIME(), table=<Order>, nullable=False), Column('date_shipped', DATETIME(), table=<Order>), Column('date_delivered', DATETIME(), table=<Order>), Column('total_price', DECIMAL(precision=12, scale=2), table=<Order>, nullable=False), Column('customer', INTEGER(), ForeignKey('Customer.id'), table=<Order>, nullable=False), schema=None)),
                 PrimaryKeyConstraint(Column('id', INTEGER(), table=<Order>, primary_key=True, nullable=False))},
 'description': 'Order',
 'dialect_kwargs': <sqlalchemy.sql.base._DialectArgView object at 0x7fe3f98f4210>,
 'dialect_options': {},
 'dispatch': <sqlalchemy.event.base.DDLEventsDispatch object at 0x7fe3f98e9b00>,
 'foreign_keys': {ForeignKey('Customer.id')},
 'fullname': 'Order',
 'implicit_returning': True,
 'indexes': {Index('idx_order__customer', Column('customer', INTEGER(), ForeignKey('Customer.id'), table=<Order>, nullable=False))},
 'metadata': MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)),
 'name': 'Order',
 'primary_key': PrimaryKeyConstraint(Column('id', INTEGER(), table=<Order>, primary_key=True, nullable=False)),
 'schema': None}
```

## OrderItem

```
{'_columns': <sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98f4b50>,
 '_extra_dependencies': set(),
 '_prefixes': [],
 'columns': <sqlalchemy.sql.base.ImmutableColumnCollection object at 0x7fe3f9902050>,
 'constraints': {ForeignKeyConstraint(<sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98ffc90>, None, link_to_name=True, table=Table('OrderItem', MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)), Column('quantity', INTEGER(), table=<OrderItem>, nullable=False), Column('price', DECIMAL(precision=12, scale=2), table=<OrderItem>, nullable=False), Column('order', INTEGER(), ForeignKey('Order.id'), table=<OrderItem>, primary_key=True, nullable=False), Column('product', INTEGER(), ForeignKey('Product.id'), table=<OrderItem>, primary_key=True, nullable=False), schema=None)),
                 ForeignKeyConstraint(<sqlalchemy.sql.base.ColumnCollection object at 0x7fe3f98ffb90>, None, link_to_name=True, table=Table('OrderItem', MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)), Column('quantity', INTEGER(), table=<OrderItem>, nullable=False), Column('price', DECIMAL(precision=12, scale=2), table=<OrderItem>, nullable=False), Column('order', INTEGER(), ForeignKey('Order.id'), table=<OrderItem>, primary_key=True, nullable=False), Column('product', INTEGER(), ForeignKey('Product.id'), table=<OrderItem>, primary_key=True, nullable=False), schema=None)),
                 PrimaryKeyConstraint(Column('order', INTEGER(), ForeignKey('Order.id'), table=<OrderItem>, primary_key=True, nullable=False), Column('product', INTEGER(), ForeignKey('Product.id'), table=<OrderItem>, primary_key=True, nullable=False))},
 'description': 'OrderItem',
 'dialect_kwargs': <sqlalchemy.sql.base._DialectArgView object at 0x7fe3f98fa850>,
 'dialect_options': {},
 'dispatch': <sqlalchemy.event.base.DDLEventsDispatch object at 0x7fe3f98fe710>,
 'foreign_keys': {ForeignKey('Product.id'), ForeignKey('Order.id')},
 'fullname': 'OrderItem',
 'implicit_returning': True,
 'indexes': {Index('idx_orderitem__product', Column('product', INTEGER(), ForeignKey('Product.id'), table=<OrderItem>, primary_key=True, nullable=False))},
 'metadata': MetaData(bind=Engine(sqlite:///../00example/estore.sqlite)),
 'name': 'OrderItem',
 'primary_key': PrimaryKeyConstraint(Column('order', INTEGER(), ForeignKey('Order.id'), table=<OrderItem>, primary_key=True, nullable=False), Column('product', INTEGER(), ForeignKey('Product.id'), table=<OrderItem>, primary_key=True, nullable=False)),
 'schema': None}
```

