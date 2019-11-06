Category:
  id:
    type: INTEGER
    primary_key: true
  name:
    type: TEXT
    primary_key: false
Customer:
  id:
    type: INTEGER
    primary_key: true
  email:
    type: TEXT
    primary_key: false
  password:
    type: TEXT
    primary_key: false
  name:
    type: TEXT
    primary_key: false
  country:
    type: TEXT
    primary_key: false
  address:
    type: TEXT
    primary_key: false
Product:
  id:
    type: INTEGER
    primary_key: true
  name:
    type: TEXT
    primary_key: false
  description:
    type: TEXT
    primary_key: false
  picture:
    type: BLOB
    primary_key: false
  price:
    type: DECIMAL
    primary_key: false
  quantity:
    type: INTEGER
    primary_key: false
CartItem:
  id:
    type: INTEGER
    primary_key: true
  quantity:
    type: INTEGER
    primary_key: false
  customer:
    type: INTEGER
    primary_key: false
  product:
    type: INTEGER
    primary_key: false
Category_Product:
  category:
    type: INTEGER
    primary_key: true
  product:
    type: INTEGER
    primary_key: true
Order:
  id:
    type: INTEGER
    primary_key: true
  state:
    type: TEXT
    primary_key: false
  date_created:
    type: DATETIME
    primary_key: false
  date_shipped:
    type: DATETIME
    primary_key: false
  date_delivered:
    type: DATETIME
    primary_key: false
  total_price:
    type: DECIMAL
    primary_key: false
  customer:
    type: INTEGER
    primary_key: false
OrderItem:
  quantity:
    type: INTEGER
    primary_key: false
  price:
    type: DECIMAL
    primary_key: false
  order:
    type: INTEGER
    primary_key: true
  product:
    type: INTEGER
    primary_key: true
