PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS "Category" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "name" TEXT UNIQUE NOT NULL
);
INSERT INTO Category VALUES(1,'Tablets');
INSERT INTO Category VALUES(2,'USB Flash Drives');
INSERT INTO Category VALUES(3,'Solid State Drives');
INSERT INTO Category VALUES(4,'Data Storage');
CREATE TABLE IF NOT EXISTS "Customer" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "email" TEXT UNIQUE NOT NULL,
  "password" TEXT NOT NULL,
  "name" TEXT NOT NULL,
  "country" TEXT NOT NULL,
  "address" TEXT NOT NULL
);
INSERT INTO Customer VALUES(1,'john@example.com','***','John Smith','USA','address 1');
INSERT INTO Customer VALUES(2,'matthew@example.com','***','Matthew Reed','USA','address 2');
INSERT INTO Customer VALUES(3,'chuanqin@example.com','***','Chuan Qin','China','address 3');
INSERT INTO Customer VALUES(4,'rebecca@example.com','***','Rebecca Lawson','USA','address 4');
INSERT INTO Customer VALUES(5,'oliver@example.com','***','Oliver Blakey','UK','address 5');
CREATE TABLE IF NOT EXISTS "Order" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "state" TEXT NOT NULL,
  "date_created" DATETIME NOT NULL,
  "date_shipped" DATETIME,
  "date_delivered" DATETIME,
  "total_price" DECIMAL(12, 2) NOT NULL,
  "customer" INTEGER NOT NULL REFERENCES "Customer" ("id") ON DELETE CASCADE
);
INSERT INTO "Order" VALUES(1,'DELIVERED','2012-10-20 15:22:00.000000','2012-10-21 11:34:00.000000','2012-10-26 17:23:00.000000',292,1);
INSERT INTO "Order" VALUES(2,'DELIVERED','2013-01-10 09:40:00.000000','2013-01-10 14:03:00.000000','2013-01-13 11:57:00.000000',478.49999999999999998,1);
INSERT INTO "Order" VALUES(3,'DELIVERED','2012-11-03 12:10:00.000000','2012-11-04 11:47:00.000000','2012-11-07 18:55:00.000000',680.49999999999999998,2);
INSERT INTO "Order" VALUES(4,'SHIPPED','2013-03-11 19:33:00.000000','2013-03-12 09:40:00.000000',NULL,99.799999999999997159,3);
INSERT INTO "Order" VALUES(5,'CREATED','2013-03-15 23:15:00.000000',NULL,NULL,722,4);
CREATE TABLE IF NOT EXISTS "Product" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "name" TEXT NOT NULL,
  "description" TEXT NOT NULL,
  "picture" BLOB,
  "price" DECIMAL(12, 2) NOT NULL,
  "quantity" INTEGER NOT NULL
);
INSERT INTO Product VALUES(1,'Kindle Fire HD','Amazon tablet for web, movies, music, apps, games, reading and more',NULL,284,120);
INSERT INTO Product VALUES(2,'Apple iPad with Retina Display MD513LL/A (16GB, Wi-Fi, White)','iPad with Retina display now features an A6X chip, FaceTime HD camera, and faster Wi-Fi',NULL,478.49999999999999998,180);
INSERT INTO Product VALUES(3,'SanDisk Cruzer 16 GB USB Flash Drive','Take it all with you on reliable SanDisk USB flash drive',NULL,9.9900000000000002131,400);
INSERT INTO Product VALUES(4,'Kingston Digital DataTraveler SE9 16GB USB 2.0','Convenient - small, capless and pocket-sized for easy transportability',NULL,9.9800000000000004263,350);
INSERT INTO Product VALUES(5,'Samsung 840 Series 2.5 inch 120GB SATA III SSD','Enables you to boot up your computer in as little as 15 seconds',NULL,98.95000000000000284,0);
INSERT INTO Product VALUES(6,'Crucial m4 256GB 2.5-Inch SSD SATA 6Gb/s CT256M4SSD2','The award-winning SSD delivers powerful performance gains for SATA 6Gb/s systems',NULL,188.66999999999998749,60);
CREATE TABLE IF NOT EXISTS "CartItem" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "quantity" INTEGER NOT NULL,
  "customer" INTEGER NOT NULL REFERENCES "Customer" ("id") ON DELETE CASCADE,
  "product" INTEGER NOT NULL REFERENCES "Product" ("id") ON DELETE CASCADE
);
INSERT INTO CartItem VALUES(1,1,1,1);
INSERT INTO CartItem VALUES(2,1,1,2);
INSERT INTO CartItem VALUES(3,2,2,5);
CREATE TABLE IF NOT EXISTS "Category_Product" (
  "category" INTEGER NOT NULL REFERENCES "Category" ("id") ON DELETE CASCADE,
  "product" INTEGER NOT NULL REFERENCES "Product" ("id") ON DELETE CASCADE,
  PRIMARY KEY ("category", "product")
);
INSERT INTO Category_Product VALUES(4,6);
INSERT INTO Category_Product VALUES(3,6);
INSERT INTO Category_Product VALUES(3,5);
INSERT INTO Category_Product VALUES(4,3);
INSERT INTO Category_Product VALUES(1,2);
INSERT INTO Category_Product VALUES(4,5);
INSERT INTO Category_Product VALUES(1,1);
INSERT INTO Category_Product VALUES(4,4);
INSERT INTO Category_Product VALUES(2,3);
INSERT INTO Category_Product VALUES(2,4);
CREATE TABLE IF NOT EXISTS "OrderItem" (
  "quantity" INTEGER NOT NULL,
  "price" DECIMAL(12, 2) NOT NULL,
  "order" INTEGER NOT NULL REFERENCES "Order" ("id") ON DELETE CASCADE,
  "product" INTEGER NOT NULL REFERENCES "Product" ("id") ON DELETE CASCADE,
  PRIMARY KEY ("order", "product")
);
INSERT INTO OrderItem VALUES(1,274,1,1);
INSERT INTO OrderItem VALUES(2,9.9800000000000004263,1,4);
INSERT INTO OrderItem VALUES(1,478.49999999999999998,2,2);
INSERT INTO OrderItem VALUES(1,478.49999999999999998,3,2);
INSERT INTO OrderItem VALUES(2,9.9800000000000004263,3,4);
INSERT INTO OrderItem VALUES(1,199,3,6);
INSERT INTO OrderItem VALUES(10,9.9800000000000004263,4,4);
INSERT INTO OrderItem VALUES(1,284,5,1);
INSERT INTO OrderItem VALUES(1,478.49999999999999998,5,2);
DELETE FROM sqlite_sequence;
INSERT INTO sqlite_sequence VALUES('Customer',5);
INSERT INTO sqlite_sequence VALUES('Category',4);
INSERT INTO sqlite_sequence VALUES('Product',6);
INSERT INTO sqlite_sequence VALUES('CartItem',3);
INSERT INTO sqlite_sequence VALUES('Order',5);
CREATE INDEX "idx_order__customer" ON "Order" ("customer");
CREATE INDEX "idx_cartitem__customer" ON "CartItem" ("customer");
CREATE INDEX "idx_cartitem__product" ON "CartItem" ("product");
CREATE INDEX "idx_category_product" ON "Category_Product" ("product");
CREATE INDEX "idx_orderitem__product" ON "OrderItem" ("product");
COMMIT;
