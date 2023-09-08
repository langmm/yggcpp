#ifndef YGGCONSTANTS_H_
#define YGGCONSTANTS_H_

/*! @brief Maximum message size. */
#ifdef IPCDEF
#define YGG_MSG_MAX 2048
#else
#define YGG_MSG_MAX 1048576
#endif
/*! @brief End of file message. */
#define YGG_MSG_EOF "EOF!!!"
#define YGG_MSG_EOF_LEN 6
/*! @brief End of client message. */
#define YGG_CLIENT_EOF "YGG_END_CLIENT"
/*! @brief Client signing on. */
#define YGG_CLIENT_SIGNON "CLIENT_SIGNON::"
#define YGG_CLIENT_SIGNON_LEN 15
/*! @brief Server signing on. */
#define YGG_SERVER_SIGNON "SERVER_SIGNON::"
#define YGG_SERVER_SIGNON_LEN 15
/*! @brief Resonable size for buffer. */
#define YGG_MSG_BUF 2048
/*! @brief Sleep time in micro-seconds */
// #define YGG_SLEEP_TIME ((int)250000)
#define YGG_SLEEP_TIME ((int)250)
/*! @brief Maximum time to wait for any operation in micro-seconds */
#define YGG_MAX_TIME ((int)54000000000) // 15 minutes
/*! @brief Size for buffers to contain names of Python objects. */
#define PYTHON_NAME_SIZE 1000

/*! @brief Define old style names for compatibility. */
#define PSI_MSG_MAX YGG_MSG_MAX
#define PSI_MSG_BUF YGG_MSG_BUF
#define PSI_MSG_EOF YGG_MSG_EOF

#define MSG_HEAD_SEP "YGG_MSG_HEAD"
/*! @brief Size of COMM buffer. */
#define COMMBUFFSIZ 2000
#define FMT_LEN 100
#define MAX_KEYS_ALLOWED 256

#endif // YGGCONSTANTS_H_
