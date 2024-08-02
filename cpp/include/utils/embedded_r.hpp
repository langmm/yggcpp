#pragma once
#include "embedded_languages.hpp"
#ifdef YGG_EMBED_R
#include <RInside.h>
#else
#define SEXP void
#endif
#define YGG_EMBED_R_TYPE SEXP*

namespace YggInterface {
  namespace utils {
    /** @brief Class to handling embedding R. */
    class EmbeddedR : public EmbeddedLanguageBase {
      EMBEDED_LANGUAGE_DECL(EmbeddedR, YGG_EMBED_R_TYPE);
      void* R_; /**< Embedded R */
#ifdef YGG_EMBED_R
      /**
       * @brief Get the embedded R interpreter.
       * @returns R interpreter.
       */
      Rinside& R() {
	if (!R_)
	  throw_error("RInside embedded R not initialized.");
	return ((RInside*)R_)[0];
      }
#endif
    };
  }
}
