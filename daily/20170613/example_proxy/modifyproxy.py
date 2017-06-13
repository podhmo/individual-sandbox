import json
from mitmproxy import ctx
from mitmproxy.script import concurrent

DEFAULTS = [
    {
        "channelGrouping": "Organic Search",
        "category": "organicSearch"
    },
    {
        "channelGrouping": "Paid Search",
        "category": "paidSearch"
    },
    {
        "channelGrouping": "Display",
        "category": "advertising"
    },
    {
        "channelGrouping": "Other Advertising",
        "category": "advertising"
    },
    {
        "channelGrouping": "Referral",
        "category": "referral"
    },
    {
        "channelGrouping": "Social",
        "category": "social"
    },
    {
        "channelGrouping": "Email",
        "category": "email"
    },
    {
        "channelGrouping": "Direct",
        "category": "other"
    },
]


@concurrent
def request(flow):
    if flow.request.method == "POST" and flow.request.path.endswith("/tuning-workspaces"):
        ctx.log.info("@@before {} {}".format(flow.request.method, flow.request.path))

        data = json.loads(flow.request.content.decode("utf-8"))
        setting = data["analysisSetting"]
        if "channelGroupingCategoryMappings" not in setting:
            setting["channelGroupingCategoryMappings"] = []
        mappings = setting["channelGroupingCategoryMappings"]
        for c in DEFAULTS:
            if not any(c0["channelGrouping"] == c["channelGrouping"] for c0 in mappings):
                mappings.append(c)
        flow.request.content = json.dumps(data).encode("utf-8")
        ctx.log.info(
            "@@after {} {} {}".format(flow.request.method, flow.request.path, len(mappings))
        )
