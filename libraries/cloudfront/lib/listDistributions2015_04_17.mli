open Types
type input = ListDistributionsRequest.t
type output = ListDistributionsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error