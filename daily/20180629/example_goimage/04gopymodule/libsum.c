#define Py_LIMITED_API
#include <Python.h>

// Workaround missing variadic function support
// https://github.com/golang/go/issues/975
int PyArg_ParseTuple_LL(PyObject * args, long long * a, long long * b) {  
    return PyArg_ParseTuple(args, "LL", a, b);
}

static PyMethodDef LibsumMethods[] = {  
    {"sum", sum, METH_VARARGS, "Add two numbers."},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef libsummodule = {  
   PyModuleDef_HEAD_INIT, "libsum", NULL, -1, LibsumMethods
};

PyMODINIT_FUNC  
PyInit_libsum(void)  
{
    return PyModule_Create(&libsummodule);
}
