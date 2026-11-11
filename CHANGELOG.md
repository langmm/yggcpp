# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

<!-- git-cliff: end of header -->

## 0.1.0 - 2026-11-XX Pre-release

### TODO

- Finalize conda recipe dependencies by determining which dependencies should remain in build/host/run
- Verify all tests are passing
- Throw error on creation of an invalid communicator in C++?
- Set DOXYGEN_CHECK_MISSING to ON in doc tests

### Features

- APIs in C++, C, Fortran, & Python
- Unified cross-language communication mechanisms using multiple methods including SysV IPC queues, ZeroMQ sockets, MPI communicators, RabbitMQ brokered connections, CURL based REST API communication, filesystem
- Multiple supported communication patterns (one-to-one, many-to-one, one-to-many, client-server)
- Support for use in parallel/concurrent architectures via MPI, OpenMP, C++ threading
- Embedded calls to functions from other supported languages
- Serialization and unit support via YggdrasilRapidJSON

### Miscellaneous Tasks

- CMake based build system with packaging
- pixi environment/task management
- conda recipe for each supported language
- CI via GHA workflows to run tests, build/publish docs, build conda recipe & publish packages
- Tests for each supported language
