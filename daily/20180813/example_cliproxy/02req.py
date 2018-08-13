import requests
print(requests.get("https://api.github.com/users/podhmo/repos").text)
