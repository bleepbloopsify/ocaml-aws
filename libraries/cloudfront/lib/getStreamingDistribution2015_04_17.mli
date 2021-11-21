open Types
type input = GetStreamingDistributionRequest.t
type output = GetStreamingDistributionResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error