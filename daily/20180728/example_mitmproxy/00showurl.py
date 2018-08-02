import mitmproxy


def response(flow):
    print("@@", flow.request.pretty_url)
