#pragma once
#include "embedded_languages.hpp"
#include "utils/tools.hpp"
#ifdef YGG_EMBED_PYTHON
// TODO
#else
// TODO
#endif

namespace YggInterface {
  namespace utils {
    /** @brief Class to handling embedding Python. */
    class EmbeddedPython : public EmbeddedLanguageBase {
      EMBEDED_LANGUAGE_DECL(EmbeddedPython, PyObject*);
      /** \copydoc YggInterface::utils:EmbeddedLanguageBase::commFlags */
      YGG_API int64_t commFlags() const override;
    };
  }
}
