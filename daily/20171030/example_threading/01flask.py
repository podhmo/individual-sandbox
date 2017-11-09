import threading
import flask

def run_app():
    app = flask.Flask(__name__)
    return app.run()

t = threading.Thread(target=run_app)
t.start()
print("hoi")
t.join()
print("hoi@")
