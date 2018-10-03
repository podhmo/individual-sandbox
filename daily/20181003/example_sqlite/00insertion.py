import sqlite3

sqlite3.enable_callback_tracebacks(True)
with sqlite3.connect(":memory:") as conn:
    conn.set_trace_callback(print)
    c = conn.cursor()

    c.execute(
        '''CREATE TABLE stocks
                 (date text, trans text, symbol text, qty real, price real)'''
    )
    c.execute("INSERT INTO stocks VALUES ('2006-01-05','BUY','RHAT',100,35.14)")
    conn.commit()
