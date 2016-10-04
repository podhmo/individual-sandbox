package deps

import (
	"log"
	"path"
)

/**

API examples

- POST /skeleton -- 外部serviceの初期データ生成
- GET /status/{jobId} -- 現在のjobの状態を返す

**/

// SkeletonResponse is x
type SkeletonResponse interface{}

// StatusResponse is x
type StatusResponse interface{}

// ExternalServiceAccessor is a manager that accessing external service (e.g. taskA0)
type ExternalServiceAccessor interface {
	CreateSkeleton(basePath string, jobID JobID, params map[string]string) (SkeletonResponse, error)
	CreateSkeletonByURL(url string, params map[string]string) (SkeletonResponse, error)
	GetStatus(basePath string, jobID JobID) (StatusResponse, error)
	GetStatusByURL(url string) (StatusResponse, error)
}

// DummyAccessor is x
type DummyAccessor struct {
}

// CreateSkeleton is x
func (d *DummyAccessor) CreateSkeleton(basePath string, jobID JobID, params map[string]string) (SkeletonResponse, error) {
	url := path.Join(basePath, "skeleton")
	params["jobId"] = string(jobID)
	return d.CreateSkeletonByURL(url, params)
}

// CreateSkeletonByURL is x
func (d *DummyAccessor) CreateSkeletonByURL(url string, params map[string]string) (SkeletonResponse, error) {
	log.Printf("POST: %s params=%v", url, params)
	var response SkeletonResponse // TODO implement
	return response, nil
}

// GetStatus is x
func (d *DummyAccessor) GetStatus(basePath string, jobID JobID) (StatusResponse, error) {
	url := path.Join(basePath, "status", string(jobID))
	return d.GetStatusByURL(url)
}

// GetStatusByURL is x
func (d *DummyAccessor) GetStatusByURL(url string) (StatusResponse, error) {
	log.Printf("Get: %s", url)
	var response StatusResponse // TODO implement
	return response, nil
}
