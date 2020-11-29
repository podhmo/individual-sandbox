package main

import (
	"context"
	"fmt"

	"github.com/aws/aws-lambda-go/lambda"
	"github.com/k0kubun/pp"
)

type MyEvent struct {
	Name string `json:"name"`
}

func HandleRequest(ctx context.Context, name MyEvent) (string, error) {
	fmt.Println("----------------------------------------")
	// https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/golang-context.html
	pp.Println(ctx)
	fmt.Println("----------------------------------------")
	return fmt.Sprintf("Hello %s!", name.Name), nil
}

func main() {
	lambda.Start(HandleRequest)
}

// ----------------------------------------
// &context.valueCtx {
// 	Context: &context.valueCtx {
// 		Context: &context.timerCtx {
// 			cancelCtx: context.cancelCtx {
// 				Context: &0,
// 				mu:      sync.Mutex {
// 					state: 0,
// 					sema:  0x00000000,
// 				},
// 				done:     (chan struct 
// 					 {}
// 				)(0x0),
// 				children: map[context.canceler]struct 
// 				 {}

// 				 {}
// 				,
// 				err:     nil,
// 			},
// 			timer: &time.Timer {
// 				C: (<-chan time.Time)(0x0),
// 				r: time.runtimeTimer {
// 					pp:       0xc000022000,
// 					when:     671067653751,
// 					period:   0,
// 					f:        func(interface 
// 						 {}
// 						, uintptr)  {...},
// 					arg:      func()  {...},
// 					seq:      0x0,
// 					nextwhen: 0,
// 					status:   0x00000001,
// 				},
// 			},
// 			deadline: (unexported time.Time),
// 		},
// 		key: &lambdacontext.key
// 		 {}
// 		,
// 		val: &lambdacontext.LambdaContext {
// 			AwsRequestID:       "966de8bf-ea7f-42d3-ad0d-d97206fb2697",
// 			InvokedFunctionArn: "arn:aws:lambda:ap-northeast-1:784330574880:function:pp",
// 			Identity:           lambdacontext.CognitoIdentity {
// 				CognitoIdentityID:     "",
// 				CognitoIdentityPoolID: "",
// 			},
// 			ClientContext: lambdacontext.ClientContext {
// 				Client: lambdacontext.ClientApplication {
// 					InstallationID: "",
// 					AppTitle:       "",
// 					AppVersionCode: "",
// 					AppPackageName: "",
// 				},
// 				Env:    map[string]string
// 				 {}
// 				,
// 				Custom: map[string]string
// 				 {}
// 				,
// 			},
// 		},
// 	},
// 	key: "x-amzn-trace-id",
// 	val: "Root=1-5fc3fd29-66038dbf4d85457d0f7503c6;Parent=310f5f1f1195e952;Sampled=0",
// }
// ----------------------------------------
