module m

go 1.13

replace m/model => ../model

require (
	github.com/gin-gonic/gin v1.6.3
	github.com/go-playground/validator v9.31.0+incompatible // indirect
	github.com/golang/protobuf v1.4.1 // indirect
	github.com/labstack/echo/v4 v4.1.16 // indirect
	golang.org/x/net v0.0.0-20200506145744-7e3656a0809f // indirect
	golang.org/x/sys v0.0.0-20200509044756-6aff5f38e54f // indirect
	google.golang.org/genproto v0.0.0-20200507105951-43844f6eee31 // indirect
	google.golang.org/grpc v1.29.1 // indirect
)
