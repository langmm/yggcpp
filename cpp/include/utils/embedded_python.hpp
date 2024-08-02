#pragma once
#include "embedded_languages.hpp"
#include "utils/tools.hpp"

namespace YggInterface {
  namespace utils {
    /** @brief Class to handling embedding Python. */
    class EmbeddedPython : public EmbeddedLanguageBase {
      EMBEDED_LANGUAGE_DECL(EmbeddedPython, PyObject*);
#ifdef YGG_EMBED_PYTHON
      /** \copydoc YggInterface::utils:EmbeddedLanguageBase::commFlags */
      YGG_API int64_t commFlags() const override;
#endif // YGG_EMBED_PYTHON
    };
  }
}
