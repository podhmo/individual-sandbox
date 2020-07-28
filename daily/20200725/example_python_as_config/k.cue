package kubernetes

apiVersion: "apps/v1" // apiVersionを指定
spec: {
    replicas: <3 & int // 3未満を指定
    template spec containers: [{image: =~"^[a-z][a-zA-Z0-9]*:[a-zA-Z0-9!-/:-@¥[-`{-~]+"}] // tagの明記を指定
}