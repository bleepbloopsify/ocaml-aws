open Types
type input = UpdateCloudFrontOriginAccessIdentityRequest.t
type output = UpdateCloudFrontOriginAccessIdentityResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error