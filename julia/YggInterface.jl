
# LINES AFTER THIS WERE GENERATED AND SHOULD NOT BE MODIFIED DIRECTLY
#====================================================================
module communicator
  using CxxWrap
  @wrapmodule(() -> "libYggInterface_julia.dylib")
  function __init__()
    @initcxx
  end
end