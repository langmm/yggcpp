#pragma once
#include "embedded_languages.hpp"
#ifdef YGG_EMBED_JULIA
#include <julia.h>
#include <julia/julia.h>
#else
#define jl_value_t void
#endif
#define YGG_EMBED_JULIA_TYPE jl_value_t*

namespace YggInterface {
  namespace utils {
    /** @brief Class to handling embedding Julia. */
    class EmbeddedJulia : public EmbeddedLanguageBase {
      EMBEDED_LANGUAGE_DECL(EmbeddedJulia, YGG_EMBED_JULIA_TYPE);
      void* refs; /**< References to variables to prevent garbage collection in julia */
#ifdef YGG_EMBED_JULIA
      /**
       * @brief Dereference a variable shored as Base.RefValue.
       * @param[in] x Base.RefValue variable.
       * @returns Dereferenced value.
       */
      jl_value_t* deref_(jl_value_t* x) const;
      /** \copydoc YggInterface::utils:EmbeddedLanguageBase::initialize_thread */
      YGG_API bool initialize_thread() override;
      /** \copydoc YggInterface::utils:EmbeddedLanguageBase::finalize_thread */
      YGG_API bool finalize_thread() override;
#endif
    };
  }
}
