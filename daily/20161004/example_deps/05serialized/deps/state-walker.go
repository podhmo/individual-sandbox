package deps

import (
	"../accessing"
	"../idgen"
	"fmt"
	"log"
	"path"
	"reflect"
)

// StateWalker is x
type StateWalker struct {
	gen      idgen.ObjectIDGenerator
	accessor accessing.ExternalServiceAccessor
	Settings *ServiceSettings
}

// GetSpec is x
func (sw *StateWalker) GetSpec(key string) (ServiceSpec, bool) {
	m := (map[string]ServiceSpec)(*sw.Settings)
	spec, ok := m[key]
	return spec, ok
}

// WalkSpec is x
func (sw *StateWalker) WalkSpec(serviceName string, spec ServiceSpec, ws *WholeState) (accessing.JobID, error) {
	state, ok := ws.States[serviceName]
	if ok {
		return state.JobID, nil
	}
	be := newBufError()
	v := reflect.ValueOf(ws.Analysis).Elem()

	params := make(map[string]string)

	// item params
	for _, name := range spec.ItemDependencies {
		field := v.FieldByName(name)
		if !field.IsValid() {
			be.WriteString(fmt.Sprintf("%s -> item %s is not found", serviceName, name))
			continue
		}
		params[name] = field.String()
	}

	// service params
	for _, name := range spec.ServiceDependencies {
		spec, ok := sw.GetSpec(name)
		if !ok {
			be.WriteString(fmt.Sprintf("%s -> service %s is not found.\n", serviceName, name))
			continue
		}
		jobID, err := sw.WalkSpec(name, spec, ws)
		if err != nil {
			be.WriteString(fmt.Sprintf("%s -> {%v}\n", serviceName, err))
		}
		params[name] = string(jobID)
	}

	jobID := accessing.JobID(sw.gen.ObjectID(serviceName))

	// post data to external service
	response, err := sw.accessor.CreateSkeleton(spec.BasePath, jobID, params)
	_ = response
	if err != nil {
		be.WriteString(fmt.Sprintf("%s -> call api: %v", serviceName, err))
	}
	state = State{
		JobID:         jobID,
		Status:        Waiting,
		Params:        params,
		EndNode:       spec.EndNode,
		StatusAPIPath: Endpoint(path.Join(spec.BasePath, "status", string(jobID))),
	}
	ws.States[serviceName] = state
	return jobID, be.Error()
}

// WalkByNames is x
func (sw *StateWalker) WalkByNames(ws *WholeState, required []string) error {
	be := newBufError()
	for _, name := range required {
		spec, ok := sw.GetSpec(name)
		if !ok {
			log.Printf("%s service is not found.\n", name)
			continue
		}
		_, err := sw.WalkSpec(name, spec, ws)
		if err != nil {
			be.WriteString(fmt.Sprintf("%v\n", err))
		}
	}
	ws.sentinel = NewSentinelFromStates(ws.States)
	return be.Error()
}
