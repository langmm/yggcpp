#include <Python.h>

#include <cassert>
#include <iostream>

// sample enum in C/C++
enum MQ_HANDSHAKE {
  MQ_HANDSHAKE_START,
  MQ_HANDSHAKE_OK,
  MQ_HANDSHAKE_ERROR
};

namespace Py {

namespace MQ {

// make Python binding for MQ_HANDSHAKE

namespace HANDSHAKE {

static struct PyModuleDef module = {
  PyModuleDef_HEAD_INIT,
  "mq.Handshake", // name of module
  nullptr,        // module documentation, may be NULL
  -1,             /* size of per-interpreter state of the module,
                   * or -1 if the module keeps state in global variables.
                   */
  nullptr         // function table (no functions)
};

static PyObject* init()
{
  static PyObject *pSelf = nullptr;
  if (!pSelf) {
    pSelf = PyModule_Create(&module);
    PyModule_AddObject(pSelf, "START", PyLong_FromLong(MQ_HANDSHAKE_START));
    PyModule_AddObject(pSelf, "OK", PyLong_FromLong(MQ_HANDSHAKE_OK));
    PyModule_AddObject(pSelf, "ERROR", PyLong_FromLong(MQ_HANDSHAKE_ERROR));
  }
  return pSelf;
}

} // namespace HANDSHAKE

// make module MQ

static struct PyModuleDef module = {
  PyModuleDef_HEAD_INIT,
  "mq",     // name of module
  nullptr,  // module documentation, may be NULL
  -1,       /* size of per-interpreter state of the module,
             * or -1 if the module keeps state in global variables.
             */
  nullptr   // function table (no functions)
};

// initializes module mq
static PyObject* init()
{
  static PyObject *pSelf = nullptr;
  if (!pSelf) {
    pSelf = PyModule_Create(&module);
    PyModule_AddObject(pSelf, "Handshake", HANDSHAKE::init());

  }
  return pSelf;
}

// adds module mq to Python modules table.
void append()
{
  assert(!Py_IsInitialized());
  PyImport_AppendInittab("mq", &init);
}

} // namespace MQ

} // namespace Py

// test program
int main()
{
  // initialize Python extension mq
  Py::MQ::append();
  // initialize Python interpreter
  Py_Initialize();
  // sample Python program
  static const char *const pyProgram
    = "print(\"Hello world (from Python).\")\n"
      "\n"
      "# import Python extension mq\n"
      "import mq\n"
      "\n"
      "# test whether it works\n"
      "def printHandshake(value):\n"
      "  if value == mq.Handshake.START:\n"
      "    print(\"MQ_HANDSHAKE_START\")\n"
      "  elif value == mq.Handshake.OK:\n"
      "    print(\"MQ_HANDSHAKE_OK\")\n"
      "  elif value == mq.Handshake.ERROR:\n"
      "    print(\"MQ_HANDSHAKE_ERROR\")\n"
      "  else:\n"
      "    print(\"Illegal MQ_HANDSHAKE value!\")\n"
      "\n"
      "printHandshake(mq.Handshake.START)\n"
      "printHandshake(mq.Handshake.OK)\n"
      "printHandshake(mq.Handshake.ERROR)\n"
      "printHandshake(0)\n"
      "printHandshake(1)\n"
      "printHandshake(2)\n"
      "printHandshake(42)\n";
  // run Python interpreter
  const int ret = PyRun_SimpleString(pyProgram);
  if (ret) {
    std::cerr << "Execution in PyRun_SimpleString() failed!\n";
  }
  // done
  return ret;
}