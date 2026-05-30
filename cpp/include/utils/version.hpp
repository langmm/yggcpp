///////////////////////////////////////////////////////////////////////////////
// YGGINTERFACE_VERSION_STRING
//
#ifndef YGGINTERFACE_VERSION_H_
#define YGGINTERFACE_VERSION_H_

//!@cond YGGINTERFACE_HIDDEN_FROM_DOXYGEN
// token stringification
#define YGGINTERFACE_STRINGIFY(x) YGGINTERFACE_DO_STRINGIFY(x)
#define YGGINTERFACE_DO_STRINGIFY(x) #x
//!@endcond

/*! \def YGGINTERFACE_MAJOR_VERSION
    \ingroup YGGINTERFACE_CONFIG
    \brief Major version of YggInterface
*/
/*! \def YGGINTERFACE_MINOR_VERSION
    \ingroup YGGINTERFACE_CONFIG
    \brief Minor version of YggInterface
*/
/*! \def YGGINTERFACE_PATCH_VERSION
    \ingroup YGGINTERFACE_CONFIG
    \brief Patch version of YggInterface
    that this version of YggdrasilRapidJSON is based on.
*/
/*! \def YGGINTERFACE_VERSION_STRING
    \ingroup YGGINTERFACE_CONFIG
    \brief Version of YggInterface in "<major>.<minor>.<patch>" string format.
*/
#define YGGINTERFACE_MAJOR_VERSION 0
#define YGGINTERFACE_MINOR_VERSION 1
#define YGGINTERFACE_PATCH_VERSION 0
#define YGGINTERFACE_VERSION_STRING \
  YGGINTERFACE_STRINGIFY(YGGINTERFACE_MAJOR_VERSION.YGGINTERFACE_MINOR_VERSION.YGGINTERFACE_PATCH_VERSION)

#endif // YGGINTERFACE_VERSION_H_
