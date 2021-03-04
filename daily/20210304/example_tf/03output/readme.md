```console
$ make
terraform apply -auto-approve

Apply complete! Resources: 0 added, 0 changed, 0 destroyed.

Outputs:

twice = "** us-west-1a, us-west-1a **"
weeks = {
  "fr" = "金"
  "mo" = "月"
  "sa" = "土"
  "su" = "日"
  "th" = "木"
  "tu" = "火"
  "we" = "水"
}
terraform show

Outputs:

twice = "** us-west-1a, us-west-1a **"
weeks = {
    fr = "金"
    mo = "月"
    sa = "土"
    su = "日"
    th = "木"
    tu = "火"
    we = "水"
}
```
