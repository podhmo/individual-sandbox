package jsend

// https://github.com/omniti-labs/jsend

type Status string

const (
	StatusSuccess Status = "success"
	StatusFail           = "fail"
	StatusError          = "error"
)

type Message string

type Response struct {
	Status  Status      `json:"status"`
	Data    interface{} `json:"data,omitempty"`
	Message Message     `json:"message,omitempty"`
}

func Success(data interface{}) Response {
	return Response{
		Status: StatusSuccess,
		Data:   data,
	}
}
func Fail(data interface{}, message Message) Response {
	return Response{
		Status:  StatusFail,
		Data:    data,
		Message: message, // according to spec, this is not needed, but
	}
}
func Error(message Message) Response {
	return Response{
		Status:  StatusError,
		Message: message,
	}
}
