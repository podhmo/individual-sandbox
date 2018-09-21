## benchmark

そういえば、boomとlocustとfunkloadと何が良いんだろう？

locustが一番っぽい？

利用したいのはこっちでは？

- https://github.com/wg/wrk
- https://github.com/tsenart/vegeta
  
### vegeta

go get -u github.com/tsenart/vegeta

```
echo 'GET http://localhost:4444/' | vegeta attack -rate 5000 -duration 5s | tee /tmp/xxx | vegeta report
```
