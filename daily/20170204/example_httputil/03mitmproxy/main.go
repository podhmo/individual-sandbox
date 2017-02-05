package main

import (
	"crypto/tls"
	"log"
	"net/http"
	"os"
	"regexp"
	"strings"

	"github.com/elazarl/goproxy"
)

// var AlwaysMitm FuncHttpsHandler = func(host string, ctx *ProxyCtx) (*ConnectAction, string) {
//     return MitmConnect, host
// }
// MitmConnect     = &ConnectAction{Action: ConnectMitm, TLSConfig: TLSConfigFromCA(&GoproxyCa)}
// var GoproxyCa, goproxyCaErr = tls.X509KeyPair(CA_CERT, CA_KEY)

func main() {
	proxy := goproxy.NewProxyHttpServer()
	proxy.Verbose = true

	ca, err := tls.LoadX509KeyPair(
		strings.Replace("~/vboxshare/venvs/my/proxy2/ca.crt", "~", os.Getenv("HOME"), 1),
		strings.Replace("~/vboxshare/venvs/my/proxy2/ca.key", "~", os.Getenv("HOME"), 1),
	)
	if err != nil {
		log.Fatal(err)
	}
	mitmConnect := &goproxy.ConnectAction{
		Action:    goproxy.ConnectMitm,
		TLSConfig: goproxy.TLSConfigFromCA(&ca),
	}

	proxy.OnRequest(goproxy.ReqHostMatches(regexp.MustCompile("^.*$"))).
		HandleConnect(goproxy.FuncHttpsHandler(func(host string, ctx *goproxy.ProxyCtx) (*goproxy.ConnectAction, string) {
			return mitmConnect, host
		}))

	// proxy.OnRequest(goproxy.ReqHostMatches(regexp.MustCompile("^.*$"))).
	// 	HijackConnect(func(req *http.Request, client net.Conn, ctx *goproxy.ProxyCtx) {
	// 		defer func() {
	// 			if e := recover(); e != nil {
	// 				ctx.Logf("error connecting to remote: %v", e)
	// 				client.Write([]byte("HTTP/1.1 500 Cannot reach destination\r\n\r\n"))
	// 			}
	// 			client.Close()
	// 		}()

	// 		includeBody := false
	// 		b, err := httputil.DumpRequestOut(req, includeBody)
	// 		if err != nil {
	// 			panic(err)
	// 		}
	// 		fmt.Fprintln(os.Stderr, b)

	// 		clientBuf := bufio.NewReadWriter(bufio.NewReader(client), bufio.NewWriter(client))
	// 		remote, err := net.Dial("tcp", req.URL.Host)
	// 		remoteBuf := bufio.NewReadWriter(bufio.NewReader(remote), bufio.NewWriter(remote))
	// 		for {
	// 			req, err := http.ReadRequest(clientBuf.Reader)
	// 			if err != nil {
	// 				panic(err)
	// 			}
	// 			if err := req.Write(remoteBuf); err != nil {
	// 				panic(err)
	// 			}
	// 			if err := remoteBuf.Flush(); err != nil {
	// 				panic(err)
	// 			}
	// 			resp, err := http.ReadResponse(remoteBuf.Reader, req)
	// 			if err != nil {
	// 				panic(err)
	// 			}
	// 			if err := resp.Write(clientBuf.Writer); err != nil {
	// 				panic(err)
	// 			}
	// 			if err := clientBuf.Flush(); err != nil {
	// 				panic(err)
	// 			}
	// 		}
	// 	})
	log.Fatal(http.ListenAndServe(":8080", proxy))
}
