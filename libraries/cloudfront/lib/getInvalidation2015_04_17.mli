open Types
type input = GetInvalidationRequest.t
type output = GetInvalidationResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error