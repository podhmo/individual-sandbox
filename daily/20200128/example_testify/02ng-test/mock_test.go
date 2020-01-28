package yours

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/mock"
)

type MyObject interface {
	DoSomething(int) (bool, error)
}

func targetFuncThatDoesSomethingWithObj(testObj MyObject) error {
	if ok, err := testObj.DoSomething(122); !ok || err != nil {
		return errors.New("hmm")
	}
	return nil
}

// TestSomething is an example of how to use our test object to
// make assertions about some target code we are testing.
func TestSomething(t *testing.T) {
	// create an instance of our test object
	testObj := new(MyMockedObject)

	// setup expectations
	testObj.On("DoSomething", 123).Return(true, nil)

	// call the code we are testing
	testObj.Test(t)

	targetFuncThatDoesSomethingWithObj(testObj)

	// assert that the expectations were met
	testObj.AssertExpectations(t)

}

// TestSomethingElse is a second example of how to use our test object to
// make assertions about some target code we are testing.
// This time using a placeholder. Placeholders might be used when the
// data being passed in is normally dynamically generated and cannot be
// predicted beforehand (eg. containing hashes that are time sensitive)
func TestSomethingElse(t *testing.T) {
	// create an instance of our test object
	testObj := new(MyMockedObject)

	// setup expectations with a placeholder in the argument list
	testObj.On("DoSomething", mock.Anything).Return(true, nil)

	// call the code we are testing
	targetFuncThatDoesSomethingWithObj(testObj)

	// assert that the expectations were met
	testObj.AssertExpectations(t)

}
