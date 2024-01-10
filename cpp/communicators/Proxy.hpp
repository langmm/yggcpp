#pragma once

#include "AsyncComm.hpp"
#ifdef THREADSINSTALLED
#include <atomic>
#endif // THREADSINSTALLED

namespace YggInterface {
  namespace communicator {

    /**
     * @brief Create a proxy to asynchronously move messages from one
     *   communicator to another.
     */
    class Proxy : public AsyncStatus {
    private:
      Proxy(const Proxy&) = delete;
      Proxy& operator=(const Proxy&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] iname Name of input communicator
       * @param[in] oname Name of output communicator
       * @param[in] iflgs Bitwise flags for input communicator
       * @param[in] oflgs Bitwise flags for output communicator
       * @param[in] itype Enumerated type of input communicator
       * @param[in] otype Enumerated type of output communicator
       * @param[in] fltrs Filters to apply to messages between communicators
       * @param[in] tforms Transforms to apply to messages between communicators
       */
      Proxy(const std::string iname, const std::string oname,
	    int iflgs = 0, int oflgs = 0,
	    const COMM_TYPE itype = DEFAULT_COMM,
	    const COMM_TYPE otype = DEFAULT_COMM,
	    std::vector<YggInterface::utils::filterFunc> fltrs=std::vector<YggInterface::utils::filterFunc>(),
	    std::vector<YggInterface::utils::transformFunc> tforms=std::vector<YggInterface::utils::transformFunc>());
      /** @brief Destructor */
      ~Proxy();
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "Proxy"; }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override;
#ifdef THREADSINSTALLED
      /**
       * @brief Get the address for either of the proxy's communicators
       * @param[in] dir Direction of comm to get the address for
       * @return Address
       */
      std::string getAddress(DIRECTION dir);
    private:
      /**
       * @brief Function to run in a thread which sends/receives messages
       * @param[in] iname Name of input communicator
       * @param[in] oname Name of output communicator
       * @param[in] iflgs Bitwise flags for input communicator
       * @param[in] oflgs Bitwise flags for output communicator
       * @param[in] itype Enumerated type of input communicator
       * @param[in] otype Enumerated type of output communicator
       */
      void on_thread(const std::string iname, const std::string oname,
		     int iflgs, int oflgs,
		     const COMM_TYPE itype, const COMM_TYPE otype);
      /**
       * @brief Receive/send one message
       * @return Result of receive/send. Negative values indicate an error
       */
      long on_message();
      Comm_t* icomm; //! Input communicator
      Comm_t* ocomm; //! Output communicator
      std::vector<YggInterface::utils::filterFunc> filters; //! Filters
      std::vector<YggInterface::utils::transformFunc> transforms; //! Transforms
#endif // THREADSINSTALLED
    };

  }
}
