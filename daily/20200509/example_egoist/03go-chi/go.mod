module m

go 1.13

replace m/model => ../model

require (
	github.com/go-chi/chi v4.1.1+incompatible
	github.com/go-chi/render v1.0.1
	github.com/golangci/golangci-lint v1.26.0 // indirect
	github.com/podhmo/maperr v0.2.4 // indirect
	golang.org/x/net v0.0.0-20200506145744-7e3656a0809f // indirect
	m/model v0.0.0-00010101000000-000000000000
)
