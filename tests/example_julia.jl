function example_model_function(data_send)
  if typeof(data_send) != String
    error("Test error")
  end
  return length(data_send)
end