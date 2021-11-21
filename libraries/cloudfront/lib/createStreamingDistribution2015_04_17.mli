open Types
type input = CreateStreamingDistributionRequest.t
type output = CreateStreamingDistributionResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error