## go errorの種類

- multi error
- map error

### hmm?

- https://github.com/ztrue/tracerr
- https://github.com/srvc/fail

## python openCensus, openTracing, openTelemetry

- https://github.com/census-instrumentation/opencensus-python

参考

- https://github.com/tiangolo/fastapi/issues/433
- https://heartbeats.jp/hbblog/2018/09/opencensus.html
- https://tech.mfkessai.co.jp/2019/04/opencensus-meetup-report/
- https://ponde-m.hatenablog.com/entry/2019/04/05/002644


### openTelemetryはまだ仕様だけ？

- https://www.cncf.io/blog/2019/05/21/a-brief-history-of-opentelemetry-so-far/

openTracingとopenCencusがマージされてopenTelemetryになった。

### exporterは?

- awsならx-ray
- gcpならstackdriver

というイメージ。

- https://opencensus.io/exporters/supported-exporters/go/xray/
- https://opencensus.io/exporters/supported-exporters/go/stackdriver/

でもstatsの方はxrayではとれなそう。

## go functional optionsでinterfaceを使う価値
