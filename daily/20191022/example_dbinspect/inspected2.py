import sqlalchemy as sa
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()


class Actor(Base):
    __tablename__ = "actor"

    actor_id = sa.Column(
        sa.SMALLINT, primary_key=True, autoincrement=True, nullable=False
    )
    first_name = sa.Column(sa.VARCHAR(45), nullable=False)
    last_name = sa.Column(sa.VARCHAR(45), nullable=False)
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)


class Address(Base):
    __tablename__ = "address"

    address_id = sa.Column(
        sa.SMALLINT, primary_key=True, autoincrement=True, nullable=False
    )
    address = sa.Column(sa.VARCHAR(50), nullable=False)
    address2 = sa.Column(sa.VARCHAR(50), nullable=True)
    district = sa.Column(sa.VARCHAR(20), nullable=False)
    city_id = sa.Column(sa.SMALLINT, sa.ForeignKey("city.city_id"), nullable=False)
    postal_code = sa.Column(sa.VARCHAR(10), nullable=True)
    phone = sa.Column(sa.VARCHAR(20), nullable=False)
    location = sa.Column(sa.NullType, nullable=False)
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)


class Category(Base):
    __tablename__ = "category"

    category_id = sa.Column(
        sa.TINYINT, primary_key=True, autoincrement=True, nullable=False
    )
    name = sa.Column(sa.VARCHAR(25), nullable=False)
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)


class City(Base):
    __tablename__ = "city"

    city_id = sa.Column(
        sa.SMALLINT, primary_key=True, autoincrement=True, nullable=False
    )
    city = sa.Column(sa.VARCHAR(50), nullable=False)
    country_id = sa.Column(
        sa.SMALLINT, sa.ForeignKey("country.country_id"), nullable=False
    )
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)


class Country(Base):
    __tablename__ = "country"

    country_id = sa.Column(
        sa.SMALLINT, primary_key=True, autoincrement=True, nullable=False
    )
    country = sa.Column(sa.VARCHAR(50), nullable=False)
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)


class Customer(Base):
    __tablename__ = "customer"

    customer_id = sa.Column(
        sa.SMALLINT, primary_key=True, autoincrement=True, nullable=False
    )
    store_id = sa.Column(sa.TINYINT, sa.ForeignKey("store.store_id"), nullable=False)
    first_name = sa.Column(sa.VARCHAR(45), nullable=False)
    last_name = sa.Column(sa.VARCHAR(45), nullable=False)
    email = sa.Column(sa.VARCHAR(50), nullable=True)
    address_id = sa.Column(
        sa.SMALLINT, sa.ForeignKey("address.address_id"), nullable=False
    )
    active = sa.Column(sa.TINYINT, nullable=False)
    create_date = sa.Column(sa.DATETIME, nullable=False)
    last_update = sa.Column(sa.TIMESTAMP, nullable=True)


class Film(Base):
    __tablename__ = "film"

    film_id = sa.Column(
        sa.SMALLINT, primary_key=True, autoincrement=True, nullable=False
    )
    title = sa.Column(sa.VARCHAR(255), nullable=False)
    description = sa.Column(sa.TEXT, nullable=True)
    release_year = sa.Column(sa.YEAR, nullable=True)
    language_id = sa.Column(
        sa.TINYINT, sa.ForeignKey("language.language_id"), nullable=False
    )
    original_language_id = sa.Column(
        sa.TINYINT, sa.ForeignKey("language.language_id"), nullable=True
    )
    rental_duration = sa.Column(sa.TINYINT, nullable=False)
    rental_rate = sa.Column(sa.DECIMAL, nullable=False)
    length = sa.Column(sa.SMALLINT, nullable=True)
    replacement_cost = sa.Column(sa.DECIMAL, nullable=False)
    rating = sa.Column(sa.Enum(5), nullable=True)
    special_features = sa.Column(sa.SET(17), nullable=True)
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)


class Film_actor(Base):
    __tablename__ = "film_actor"

    actor_id = sa.Column(
        sa.SMALLINT, sa.ForeignKey("actor.actor_id"), primary_key=True, nullable=False
    )
    film_id = sa.Column(
        sa.SMALLINT, sa.ForeignKey("film.film_id"), primary_key=True, nullable=False
    )
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)


class Film_category(Base):
    __tablename__ = "film_category"

    film_id = sa.Column(
        sa.SMALLINT, sa.ForeignKey("film.film_id"), primary_key=True, nullable=False
    )
    category_id = sa.Column(
        sa.TINYINT,
        sa.ForeignKey("category.category_id"),
        primary_key=True,
        nullable=False,
    )
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)


class Film_text(Base):
    __tablename__ = "film_text"

    film_id = sa.Column(sa.SMALLINT, primary_key=True, nullable=False)
    title = sa.Column(sa.VARCHAR(255), nullable=False)
    description = sa.Column(sa.TEXT, nullable=True)


class Inventory(Base):
    __tablename__ = "inventory"

    inventory_id = sa.Column(
        sa.MEDIUMINT, primary_key=True, autoincrement=True, nullable=False
    )
    film_id = sa.Column(sa.SMALLINT, sa.ForeignKey("film.film_id"), nullable=False)
    store_id = sa.Column(sa.TINYINT, sa.ForeignKey("store.store_id"), nullable=False)
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)


class Language(Base):
    __tablename__ = "language"

    language_id = sa.Column(
        sa.TINYINT, primary_key=True, autoincrement=True, nullable=False
    )
    name = sa.Column(sa.CHAR(20), nullable=False)
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)


class Payment(Base):
    __tablename__ = "payment"

    payment_id = sa.Column(
        sa.SMALLINT, primary_key=True, autoincrement=True, nullable=False
    )
    customer_id = sa.Column(
        sa.SMALLINT, sa.ForeignKey("customer.customer_id"), nullable=False
    )
    staff_id = sa.Column(sa.TINYINT, sa.ForeignKey("staff.staff_id"), nullable=False)
    rental_id = sa.Column(sa.INTEGER, sa.ForeignKey("rental.rental_id"), nullable=True)
    amount = sa.Column(sa.DECIMAL, nullable=False)
    payment_date = sa.Column(sa.DATETIME, nullable=False)
    last_update = sa.Column(sa.TIMESTAMP, nullable=True)


class Rental(Base):
    __tablename__ = "rental"

    rental_id = sa.Column(
        sa.INTEGER, primary_key=True, autoincrement=True, nullable=False
    )
    rental_date = sa.Column(sa.DATETIME, nullable=False)
    inventory_id = sa.Column(
        sa.MEDIUMINT, sa.ForeignKey("inventory.inventory_id"), nullable=False
    )
    customer_id = sa.Column(
        sa.SMALLINT, sa.ForeignKey("customer.customer_id"), nullable=False
    )
    return_date = sa.Column(sa.DATETIME, nullable=True)
    staff_id = sa.Column(sa.TINYINT, sa.ForeignKey("staff.staff_id"), nullable=False)
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)


class Staff(Base):
    __tablename__ = "staff"

    staff_id = sa.Column(
        sa.TINYINT, primary_key=True, autoincrement=True, nullable=False
    )
    first_name = sa.Column(sa.VARCHAR(45), nullable=False)
    last_name = sa.Column(sa.VARCHAR(45), nullable=False)
    address_id = sa.Column(
        sa.SMALLINT, sa.ForeignKey("address.address_id"), nullable=False
    )
    picture = sa.Column(sa.BLOB, nullable=True)
    email = sa.Column(sa.VARCHAR(50), nullable=True)
    store_id = sa.Column(sa.TINYINT, sa.ForeignKey("store.store_id"), nullable=False)
    active = sa.Column(sa.TINYINT, nullable=False)
    username = sa.Column(sa.VARCHAR(16), nullable=False)
    password = sa.Column(sa.VARCHAR(40), nullable=True)
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)


class Store(Base):
    __tablename__ = "store"

    store_id = sa.Column(
        sa.TINYINT, primary_key=True, autoincrement=True, nullable=False
    )
    manager_staff_id = sa.Column(
        sa.TINYINT, sa.ForeignKey("staff.staff_id"), nullable=False
    )
    address_id = sa.Column(
        sa.SMALLINT, sa.ForeignKey("address.address_id"), nullable=False
    )
    last_update = sa.Column(sa.TIMESTAMP, nullable=False)
