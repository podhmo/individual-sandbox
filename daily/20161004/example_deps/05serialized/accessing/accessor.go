package accessing

import (
	"../idgen"
	"../status"
	"log"
	"path"
)

// JobID is x
type JobID idgen.ObjectID

/**

API examples

- POST /skeleton -- 外部serviceの初期データ生成
- GET /status/{jobId} -- 現在のjobの状態を返す

**/

// SkeletonResponse is x
type SkeletonResponse interface{}

// StatusResponse is x
type StatusResponse struct {
	Status status.Status
}

// ExternalServiceAccessor is a manager that accessing external service (e.g. taskA0)
type ExternalServiceAccessor interface {
	CreateSkeleton(basePath string, jobID JobID, params map[string]string) (SkeletonResponse, error)
	CreateSkeletonByURL(url string, params map[string]string) (SkeletonResponse, error)
	GetStatus(basePath string, jobID JobID) (StatusResponse, error)
	GetStatusByURL(url string) (StatusResponse, error)
	BuildGetStatusURL(basePath string, jobID JobID) string
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
	url := d.BuildGetStatusURL(basePath, jobID)
	return d.GetStatusByURL(url)
}

// GetStatusByURL is x
func (d *DummyAccessor) GetStatusByURL(url string) (StatusResponse, error) {
	log.Printf("Get: %s", url)
	response := StatusResponse{Status: status.Done}
	return response, nil
}

// BuildGetStatusURL is x
func (d *DummyAccessor) BuildGetStatusURL(basePath string, jobID JobID) string {
	return path.Join(basePath, "status", string(jobID))
}
