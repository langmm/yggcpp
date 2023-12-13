#pragma once

#include <vector>
#include "utils/enums.hpp"
#include "utils/Address.hpp"
#include "utils/logging.hpp"

namespace YggInterface {
namespace communicator {
// Forward declare
class Comm_t;

/**
 * @brief Class for workers
 */
class Worker {
public:
    Worker(const Worker& rhs) = delete;
    Worker& operator=(const Worker& rhs) = delete;
    /**
     * @brief Constructor
     * @param[in] parent The parent communicator for this worker
     * @param[in] dir The communications direction to use
     * @param[in] adr The address to use
     * @see utils::Address
     */
    Worker(Comm_t* parent, DIRECTION dir, utils::Address& adr);
    /**
     * @brief Copy constructor
     * @param[in] rhs The Worker to copy
     */
    Worker(Worker&& rhs) noexcept ;
    /**
     * @brief Copy operator
     * @param[in] rhs The Worker to copy
     * @return This instance with the contents of the given Worker
     */
    Worker& operator=(Worker&& rhs) noexcept ;
    /**
     * @brief Whether or not the given direction and address match that of this worker
     * @param[in] dir The communications direction
     * @param[in] adr The address to check
     * @return True if they match
     * @see utils::Address
     */
    bool matches(DIRECTION dir, utils::Address& adr) const;
    /**
     * @brief Destructor
     */
    ~Worker();

    Comm_t* comm;          //!< The communicator for this worker
    std::string request;   //!<
};

/**
 * @brief Class for a list of workers
 */
class WorkerList {
public:
    WorkerList(const WorkerList& rhs) = delete;
    WorkerList& operator=(const WorkerList& rhs) = delete;
    /**
     * @brief Constructor
     */
    WorkerList() : workers() {}
    /**
     * @brief Destructor
     */
    ~WorkerList();
    /**
     * @brief Add a new Worker to the list, using the given information
     * @param[in] parent The parent communicator
     * @param[in] dir The communications direction
     * @param[in] adr The address to use
     * @return The newly created worker
     * @see utils::Address
     */
    Comm_t* add_worker(Comm_t* parent, DIRECTION dir,
                       utils::Address& adr);
    /**
     * @brief Remove and delete the given worker from the list
     * @param[in] worker The Worker instance to remove
     */
    void remove_worker(Comm_t*& worker);
    /**
     * @brief Find the Communicator that matches the given inputs
     * @param[in] dir The communications direction
     * @param[in] adr The address
     * @param[in] idx The index of the worker
     * @return The matching Communicator, nullptr if none matched
     * @see utils::Address
     */
    Comm_t* find_worker(DIRECTION dir, utils::Address& adr,
                        size_t* idx = nullptr);
    /**
     * @brief Find the index of the given worker
     * @param[in] worker The worker to find
     * @return The index of the worker or -1 if it was not found
     */
    int find_worker(Comm_t* worker);
    /**
     * @brief Get the Communicator for the Worker with the given direction and address. If there is no match
     * create a new Worker using the given communicator.
     * @param[in] parent The communicator to use
     * @param[in] dir The communications direction
     * @param[in] adr The communications address
     * @return The matching or new Worker
     * @see utils::Address
     */
    Comm_t* get(Comm_t* parent, DIRECTION dir,
                utils::Address& adr);
    /**
     * @brief Get the Communicator for the Worker with the given direction and no address. If there is no match
     * create a new Worker using the given communicator.
     * @param[in] parent The communicator to use
     * @param[in] dir The communications direction
     * @return The matching or new Worker
     */
    Comm_t* get(Comm_t* parent, DIRECTION dir);
    /**
     * @brief Get the Communicator for the Worker with the given direction and address. If there is no match
     * create a new Worker using the given communicator.
     * @param[in] parent The communicator to use
     * @param[in] dir The communications direction
     * @param[in] adr The communications address
     * @return The matching or new Worker
     */
    Comm_t* get(Comm_t* parent, DIRECTION dir, const std::string& adr);
    /**
     * @brief Give the given reguest to the specified worker.
     * @param[in] worker The worker to use
     * @param[in] request The request to give the worker
     * @return True if successful, false if the Worker is not in the list
     */
    bool setRequest(Comm_t* worker, const std::string& request);
    /**
     * @brief Check whether a Worker has the given request
     * @param[in] request The request to check for
     * @return True if it was found
     */
    bool setResponse(const std::string& request);
    std::vector<Worker> workers;
};

}
}
