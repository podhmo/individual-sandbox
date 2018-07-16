{
  "graphiteHost": "127.0.0.1",
  "graphitePort": 2003,
  "port": 8125,
  "flushInterval": 10000,
  "debug": true,
  "log": {"level": "LOG_DEBUG"},
  "servers": [
    { server: "./servers/udp", address: "0.0.0.0", port: 8125 }
  ]
}
