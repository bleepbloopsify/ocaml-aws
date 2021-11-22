open Types
type input = UpdateStreamingDistributionRequest.t
type output = UpdateStreamingDistributionResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error