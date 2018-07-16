# more about graphite events at http://obfuscurity.com/2014/01/Graphite-Tip-A-Better-Way-to-Store-Events

import json
import requests


def main(event_description, tags):
    required_data = "Graphite needs SSD!"  # I tried withouth data attribute and it didn't work
    tags_string = " ".join(
        str(x) for x in tags
    )  # Since you will pass an array but graphite expects multi tags like "a,b,c" or "a b c"
    event = {"what": event_description, "tags": tags_string, "data": required_data}
    try:
        requests.post("http://localhost/events/", data=json.dumps(event), timeout=1)
    except Exception as e:
        print('Error while creating graphite event:', e)


main("deploy", ["v0.1.2", "angular_introduced", "profile_microservice"])
