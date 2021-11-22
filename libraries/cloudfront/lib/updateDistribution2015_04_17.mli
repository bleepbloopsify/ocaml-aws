open Types
type input = UpdateDistributionRequest.t
type output = UpdateDistributionResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error