CREATE TABLE IF NOT EXISTS "Category" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "name" TEXT UNIQUE NOT NULL
);
CREATE TABLE sqlite_sequence(name,seq);
CREATE TABLE IF NOT EXISTS "Customer" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "email" TEXT UNIQUE NOT NULL,
  "password" TEXT NOT NULL,
  "name" TEXT NOT NULL,
  "country" TEXT NOT NULL,
  "address" TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS "Order" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "state" TEXT NOT NULL,
  "date_created" DATETIME NOT NULL,
  "date_shipped" DATETIME,
  "date_delivered" DATETIME,
  "total_price" DECIMAL(12, 2) NOT NULL,
  "customer" INTEGER NOT NULL REFERENCES "Customer" ("id") ON DELETE CASCADE
);
CREATE TABLE IF NOT EXISTS "Product" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "name" TEXT NOT NULL,
  "description" TEXT NOT NULL,
  "picture" BLOB,
  "price" DECIMAL(12, 2) NOT NULL,
  "quantity" INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS "CartItem" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "quantity" INTEGER NOT NULL,
  "customer" INTEGER NOT NULL REFERENCES "Customer" ("id") ON DELETE CASCADE,
  "product" INTEGER NOT NULL REFERENCES "Product" ("id") ON DELETE CASCADE
);
CREATE TABLE IF NOT EXISTS "Category_Product" (
  "category" INTEGER NOT NULL REFERENCES "Category" ("id") ON DELETE CASCADE,
  "product" INTEGER NOT NULL REFERENCES "Product" ("id") ON DELETE CASCADE,
  PRIMARY KEY ("category", "product")
);
CREATE TABLE IF NOT EXISTS "OrderItem" (
  "quantity" INTEGER NOT NULL,
  "price" DECIMAL(12, 2) NOT NULL,
  "order" INTEGER NOT NULL REFERENCES "Order" ("id") ON DELETE CASCADE,
  "product" INTEGER NOT NULL REFERENCES "Product" ("id") ON DELETE CASCADE,
  PRIMARY KEY ("order", "product")
);
CREATE INDEX "idx_order__customer" ON "Order" ("customer");
CREATE INDEX "idx_cartitem__customer" ON "CartItem" ("customer");
CREATE INDEX "idx_cartitem__product" ON "CartItem" ("product");
CREATE INDEX "idx_category_product" ON "Category_Product" ("product");
CREATE INDEX "idx_orderitem__product" ON "OrderItem" ("product");
