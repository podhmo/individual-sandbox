## python handofcats

真面目に記録する側を主体にしてみたほうが楽かも？

## regex

awesome

- https://github.com/aloisdg/awesome-regex
- https://github.com/Varunram/Awesome-Regex-Resources

`regex -> [text]`

- https://github.com/lucasjones/reggen
- https://github.com/audreyt/regex-genex
- https://github.com/fent/randexp.js

`[text] -> regex`

- js https://github.com/devongovett/regexgen
- c https://github.com/rgxg/rgxg
- ?? https://github.com/MaLeLabTs/RegexGenerator
- js https://www.wimpyprogrammer.com/strings-to-regex/
- clojure https://github.com/noprompt/frak
- python https://pypi.org/project/triegex/
- ruby https://github.com/gfx/ruby-regexp_trie
- js https://github.com/alexeld/regex-trie

自作

- https://github.com/nasciiboy/regexp4

alternatives

- https://github.com/vi3k6i5/flashtext
- https://hal.archives-ouvertes.fr/hal-01788827v2/document

shortest

- https://stackoverflow.com/questions/7432830/generating-the-shortest-regex-to-match-an-arbitrary-word-list

visualize

- https://regex101.com/

### hmm

go, regex -> string list

- https://github.com/lucasjones/reggen

string list -> regex

- https://www.wimpyprogrammer.com/strings-to-regex/

###

- https://github.com/gfx/ruby-regexp_trie/blob/master/lib/regexp_trie.rb
- https://github.com/noprompt/frak/blob/master/src/cljc/frak.cljc


## python google_auth_oauthlib

```python
import google_auth_oauthlib

# TODO: Create a client ID for your project.
client_id = "YOUR-CLIENT-ID.apps.googleusercontent.com"
client_secret = "abc_ThIsIsAsEcReT"

# TODO: Choose the needed scopes for your applications.
scopes = ["https://www.googleapis.com/auth/cloud-platform"]

credentials = google_auth_oauthlib.get_user_credentials(
    scopes, client_id, client_secret
)

# 1. Open the link.
# 2. Authorize the application to have access to your account.
# 3. Copy and paste the authorization code to the prompt.

# Use the credentials to construct a client for Google APIs.
from google.cloud import bigquery

bigquery_client = bigquery.Client(
    credentials=credentials, project="your-project-id"
)
print(list(bigquery_client.query("SELECT 1").result()))
```
