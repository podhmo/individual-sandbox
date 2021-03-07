## aws policy

- https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_testing-policies.html#policies_policy-simulator-using
- https://docs.aws.amazon.com/ja_jp/IAM/latest/UserGuide/access_policies_testing-policies.html
- https://docs.aws.amazon.com/cli/latest/reference/iam/list-policies.html
- https://docs.aws.amazon.com/cli/latest/reference/iam/simulate-custom-policy.html
- https://docs.aws.amazon.com/cli/latest/reference/iam/get-context-keys-for-custom-policy.html

### aws action list?

- https://docs.aws.amazon.com/ja_jp/IAM/latest/UserGuide/access_policies_understand-policy-summary.html
- https://docs.aws.amazon.com/ja_jp/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html

あー、知りたかったのはこの辺だ。。

actionのlistとresource type。

- https://docs.aws.amazon.com/ja_jp/service-authorization/latest/reference/list_amazons3.html

### 追記

```
aws cli iam list-policies
```

これ以上取ってこれないらしい。。

- 各サービスのactions, resourcesの一覧はこれ https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html
- https://policysim.aws.amazon.com/home/index.jsp

div.table-container:nth-child(12) > div:nth-child(1)

```
fetch("https://policysim.aws.amazon.com/home/data/action?serviceName=AWS+Accounts&servicePrefix=account", {
  "headers": {
    "accept": "application/json, text/javascript, */*; q=0.01",
    "accept-language": "ja,en-US;q=0.9,en;q=0.8",
    "cache-control": "no-cache",
    "pragma": "no-cache",
    "sec-fetch-dest": "empty",
    "sec-fetch-mode": "cors",
    "sec-fetch-site": "same-origin",
    "x-csrf-token": "37896036",
    "x-requested-with": "XMLHttpRequest"
  },
  "referrer": "https://policysim.aws.amazon.com/home/index.jsp?",
  "referrerPolicy": "strict-origin-when-cross-origin",
  "body": null,
  "method": "GET",
  "mode": "cors",
  "credentials": "include"
});
```
