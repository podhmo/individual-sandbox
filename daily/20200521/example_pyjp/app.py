from flask import Flask, request, render_template


app = Flask(__name__)


@app.route("/")
def index():
    return render_template("index.html", id="home")


@app.route("/menu")
def menu():
    return render_template("menu.html", id="menu")


@app.route("/news")
def news():
    return render_template("news.html", id="news")


@app.route("/contact")
def contact():
    return render_template("contact.html", id="contact")


@app.route("/review", methods=["GET"])
def review():
    return render_template("review.html", id="review")


@app.route("/review", methods=["POST"])
def view_board():
    if request.form["username"] and request.form["star"] and request.form["review"]:
        return render_template(
            "review.html",
            username=request.form["username"],
            rating=request.form["star"],
            review=request.form["review"],
        )


if __name__ == "__main__":
    app.debug = True
    app.run()
