import logging
import sqlalchemy


logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO)

# mysql -h 127.0.0.1 --port 43306 -uroot -D sakila
echo = False  # True
engine = sqlalchemy.create_engine(
    "mysql+pymysql://root@127.0.0.1:43306/sakila", echo=echo
)
metadata = sqlalchemy.MetaData(bind=engine)
metadata.reflect()

for table in metadata.sorted_tables:
    print(table)
