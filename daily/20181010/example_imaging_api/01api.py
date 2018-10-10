from sanic import Sanic
from sanic import response
from io import BytesIO
import pandas as pd
import seaborn as sns

app = Sanic()


@app.route("/graph", methods=["POST"])
async def graph(request):
    df = pd.DataFrame({"y": request.json["ys"], "x": request.json["xs"]})
    ax = sns.lmplot(x="x", y="y", data=df, ci=None, height=4, scatter_kws={"s": 50, "alpha": 1})

    o = BytesIO()
    ax.savefig(o, format="svg")
    body = o.getvalue()
    return response.HTTPResponse(
        body_bytes=body, status=200, content_type="text/xml; charset=utf-8"
    )


@app.route("/")
async def index(request):
    with open("./index.html") as rf:
        return response.html(rf.read())


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8000)
