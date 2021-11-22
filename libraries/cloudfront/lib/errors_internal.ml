type t =
  | AccessDenied 
  | AuthFailure 
  | BatchTooLarge 
  | Blocked 
  | CNAMEAlreadyExists 
  | CloudFrontOriginAccessIdentityAlreadyExists 
  | CloudFrontOriginAccessIdentityInUse 
  | DistributionAlreadyExists 
  | DistributionNotDisabled 
  | DryRunOperation 
  | IdempotentParameterMismatch 
  | IllegalUpdate 
  | IncompleteSignature 
  | InconsistentQuantities 
  | InternalFailure 
  | InvalidAction 
  | InvalidArgument 
  | InvalidClientTokenId 
  | InvalidDefaultRootObject 
  | InvalidErrorCode 
  | InvalidForwardCookies 
  | InvalidGeoRestrictionParameter 
  | InvalidHeadersForS3Origin 
  | InvalidIfMatchVersion 
  | InvalidLocationCode 
  | InvalidMinimumProtocolVersion 
  | InvalidOrigin 
  | InvalidOriginAccessIdentity 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterValue 
  | InvalidProtocolSettings 
  | InvalidQueryParameter 
  | InvalidRelativePath 
  | InvalidRequiredProtocol 
  | InvalidResponseCode 
  | InvalidTTLOrder 
  | InvalidViewerCertificate 
  | MalformedQueryString 
  | MissingAction 
  | MissingAuthenticationToken 
  | MissingBody 
  | MissingParameter 
  | NoSuchCloudFrontOriginAccessIdentity 
  | NoSuchDistribution 
  | NoSuchInvalidation 
  | NoSuchOrigin 
  | NoSuchStreamingDistribution 
  | OptInRequired 
  | PendingVerification 
  | PreconditionFailed 
  | RequestExpired 
  | RequestLimitExceeded 
  | ServiceUnavailable 
  | StreamingDistributionAlreadyExists 
  | StreamingDistributionNotDisabled 
  | Throttling 
  | TooManyCacheBehaviors 
  | TooManyCertificates 
  | TooManyCloudFrontOriginAccessIdentities 
  | TooManyCookieNamesInWhiteList 
  | TooManyDistributionCNAMEs 
  | TooManyDistributions 
  | TooManyHeadersInForwardedValues 
  | TooManyInvalidationsInProgress 
  | TooManyOrigins 
  | TooManyStreamingDistributionCNAMEs 
  | TooManyStreamingDistributions 
  | TooManyTrustedSigners 
  | TrustedSignerDoesNotExist 
  | UnauthorizedOperation 
  | UnknownParameter 
  | UnsupportedProtocol 
  | ValidationError 
  | Uninhabited 
let common =
  [UnsupportedProtocol;
  UnknownParameter;
  UnauthorizedOperation;
  RequestLimitExceeded;
  PendingVerification;
  InvalidParameter;
  IdempotentParameterMismatch;
  DryRunOperation;
  Blocked;
  AuthFailure;
  ValidationError;
  Throttling;
  ServiceUnavailable;
  RequestExpired;
  OptInRequired;
  MissingParameter;
  MissingAuthenticationToken;
  MissingAction;
  MalformedQueryString;
  InvalidQueryParameter;
  InvalidParameterValue;
  InvalidParameterCombination;
  InvalidClientTokenId;
  InvalidAction;
  InternalFailure;
  IncompleteSignature]
let to_http_code e =
  match e with
  | AccessDenied -> Some 403
  | AuthFailure -> None
  | BatchTooLarge -> Some 413
  | Blocked -> None
  | CNAMEAlreadyExists -> Some 409
  | CloudFrontOriginAccessIdentityAlreadyExists -> Some 409
  | CloudFrontOriginAccessIdentityInUse -> Some 409
  | DistributionAlreadyExists -> Some 409
  | DistributionNotDisabled -> Some 409
  | DryRunOperation -> None
  | IdempotentParameterMismatch -> None
  | IllegalUpdate -> Some 400
  | IncompleteSignature -> Some 400
  | InconsistentQuantities -> Some 400
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidArgument -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidDefaultRootObject -> Some 400
  | InvalidErrorCode -> Some 400
  | InvalidForwardCookies -> Some 400
  | InvalidGeoRestrictionParameter -> Some 400
  | InvalidHeadersForS3Origin -> Some 400
  | InvalidIfMatchVersion -> Some 400
  | InvalidLocationCode -> Some 400
  | InvalidMinimumProtocolVersion -> Some 400
  | InvalidOrigin -> Some 400
  | InvalidOriginAccessIdentity -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidProtocolSettings -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidRelativePath -> Some 400
  | InvalidRequiredProtocol -> Some 400
  | InvalidResponseCode -> Some 400
  | InvalidTTLOrder -> Some 400
  | InvalidViewerCertificate -> Some 400
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingBody -> Some 400
  | MissingParameter -> Some 400
  | NoSuchCloudFrontOriginAccessIdentity -> Some 404
  | NoSuchDistribution -> Some 404
  | NoSuchInvalidation -> Some 404
  | NoSuchOrigin -> Some 404
  | NoSuchStreamingDistribution -> Some 404
  | OptInRequired -> Some 403
  | PendingVerification -> None
  | PreconditionFailed -> Some 412
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ServiceUnavailable -> Some 503
  | StreamingDistributionAlreadyExists -> Some 409
  | StreamingDistributionNotDisabled -> Some 409
  | Throttling -> Some 400
  | TooManyCacheBehaviors -> Some 400
  | TooManyCertificates -> Some 400
  | TooManyCloudFrontOriginAccessIdentities -> Some 400
  | TooManyCookieNamesInWhiteList -> Some 400
  | TooManyDistributionCNAMEs -> Some 400
  | TooManyDistributions -> Some 400
  | TooManyHeadersInForwardedValues -> Some 400
  | TooManyInvalidationsInProgress -> Some 400
  | TooManyOrigins -> Some 400
  | TooManyStreamingDistributionCNAMEs -> Some 400
  | TooManyStreamingDistributions -> Some 400
  | TooManyTrustedSigners -> Some 400
  | TrustedSignerDoesNotExist -> Some 400
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None
let to_string e =
  match e with
  | AccessDenied -> "AccessDenied"
  | AuthFailure -> "AuthFailure"
  | BatchTooLarge -> "BatchTooLarge"
  | Blocked -> "Blocked"
  | CNAMEAlreadyExists -> "CNAMEAlreadyExists"
  | CloudFrontOriginAccessIdentityAlreadyExists ->
      "CloudFrontOriginAccessIdentityAlreadyExists"
  | CloudFrontOriginAccessIdentityInUse ->
      "CloudFrontOriginAccessIdentityInUse"
  | DistributionAlreadyExists -> "DistributionAlreadyExists"
  | DistributionNotDisabled -> "DistributionNotDisabled"
  | DryRunOperation -> "DryRunOperation"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IllegalUpdate -> "IllegalUpdate"
  | IncompleteSignature -> "IncompleteSignature"
  | InconsistentQuantities -> "InconsistentQuantities"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidArgument -> "InvalidArgument"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidDefaultRootObject -> "InvalidDefaultRootObject"
  | InvalidErrorCode -> "InvalidErrorCode"
  | InvalidForwardCookies -> "InvalidForwardCookies"
  | InvalidGeoRestrictionParameter -> "InvalidGeoRestrictionParameter"
  | InvalidHeadersForS3Origin -> "InvalidHeadersForS3Origin"
  | InvalidIfMatchVersion -> "InvalidIfMatchVersion"
  | InvalidLocationCode -> "InvalidLocationCode"
  | InvalidMinimumProtocolVersion -> "InvalidMinimumProtocolVersion"
  | InvalidOrigin -> "InvalidOrigin"
  | InvalidOriginAccessIdentity -> "InvalidOriginAccessIdentity"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidProtocolSettings -> "InvalidProtocolSettings"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidRelativePath -> "InvalidRelativePath"
  | InvalidRequiredProtocol -> "InvalidRequiredProtocol"
  | InvalidResponseCode -> "InvalidResponseCode"
  | InvalidTTLOrder -> "InvalidTTLOrder"
  | InvalidViewerCertificate -> "InvalidViewerCertificate"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingBody -> "MissingBody"
  | MissingParameter -> "MissingParameter"
  | NoSuchCloudFrontOriginAccessIdentity ->
      "NoSuchCloudFrontOriginAccessIdentity"
  | NoSuchDistribution -> "NoSuchDistribution"
  | NoSuchInvalidation -> "NoSuchInvalidation"
  | NoSuchOrigin -> "NoSuchOrigin"
  | NoSuchStreamingDistribution -> "NoSuchStreamingDistribution"
  | OptInRequired -> "OptInRequired"
  | PendingVerification -> "PendingVerification"
  | PreconditionFailed -> "PreconditionFailed"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ServiceUnavailable -> "ServiceUnavailable"
  | StreamingDistributionAlreadyExists ->
      "StreamingDistributionAlreadyExists"
  | StreamingDistributionNotDisabled -> "StreamingDistributionNotDisabled"
  | Throttling -> "Throttling"
  | TooManyCacheBehaviors -> "TooManyCacheBehaviors"
  | TooManyCertificates -> "TooManyCertificates"
  | TooManyCloudFrontOriginAccessIdentities ->
      "TooManyCloudFrontOriginAccessIdentities"
  | TooManyCookieNamesInWhiteList -> "TooManyCookieNamesInWhiteList"
  | TooManyDistributionCNAMEs -> "TooManyDistributionCNAMEs"
  | TooManyDistributions -> "TooManyDistributions"
  | TooManyHeadersInForwardedValues -> "TooManyHeadersInForwardedValues"
  | TooManyInvalidationsInProgress -> "TooManyInvalidationsInProgress"
  | TooManyOrigins -> "TooManyOrigins"
  | TooManyStreamingDistributionCNAMEs ->
      "TooManyStreamingDistributionCNAMEs"
  | TooManyStreamingDistributions -> "TooManyStreamingDistributions"
  | TooManyTrustedSigners -> "TooManyTrustedSigners"
  | TrustedSignerDoesNotExist -> "TrustedSignerDoesNotExist"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"
let of_string e =
  match e with
  | "AccessDenied" -> Some AccessDenied
  | "AuthFailure" -> Some AuthFailure
  | "BatchTooLarge" -> Some BatchTooLarge
  | "Blocked" -> Some Blocked
  | "CNAMEAlreadyExists" -> Some CNAMEAlreadyExists
  | "CloudFrontOriginAccessIdentityAlreadyExists" ->
      Some CloudFrontOriginAccessIdentityAlreadyExists
  | "CloudFrontOriginAccessIdentityInUse" ->
      Some CloudFrontOriginAccessIdentityInUse
  | "DistributionAlreadyExists" -> Some DistributionAlreadyExists
  | "DistributionNotDisabled" -> Some DistributionNotDisabled
  | "DryRunOperation" -> Some DryRunOperation
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IllegalUpdate" -> Some IllegalUpdate
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InconsistentQuantities" -> Some InconsistentQuantities
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidArgument" -> Some InvalidArgument
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidDefaultRootObject" -> Some InvalidDefaultRootObject
  | "InvalidErrorCode" -> Some InvalidErrorCode
  | "InvalidForwardCookies" -> Some InvalidForwardCookies
  | "InvalidGeoRestrictionParameter" -> Some InvalidGeoRestrictionParameter
  | "InvalidHeadersForS3Origin" -> Some InvalidHeadersForS3Origin
  | "InvalidIfMatchVersion" -> Some InvalidIfMatchVersion
  | "InvalidLocationCode" -> Some InvalidLocationCode
  | "InvalidMinimumProtocolVersion" -> Some InvalidMinimumProtocolVersion
  | "InvalidOrigin" -> Some InvalidOrigin
  | "InvalidOriginAccessIdentity" -> Some InvalidOriginAccessIdentity
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidProtocolSettings" -> Some InvalidProtocolSettings
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidRelativePath" -> Some InvalidRelativePath
  | "InvalidRequiredProtocol" -> Some InvalidRequiredProtocol
  | "InvalidResponseCode" -> Some InvalidResponseCode
  | "InvalidTTLOrder" -> Some InvalidTTLOrder
  | "InvalidViewerCertificate" -> Some InvalidViewerCertificate
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingBody" -> Some MissingBody
  | "MissingParameter" -> Some MissingParameter
  | "NoSuchCloudFrontOriginAccessIdentity" ->
      Some NoSuchCloudFrontOriginAccessIdentity
  | "NoSuchDistribution" -> Some NoSuchDistribution
  | "NoSuchInvalidation" -> Some NoSuchInvalidation
  | "NoSuchOrigin" -> Some NoSuchOrigin
  | "NoSuchStreamingDistribution" -> Some NoSuchStreamingDistribution
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "PreconditionFailed" -> Some PreconditionFailed
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "StreamingDistributionAlreadyExists" ->
      Some StreamingDistributionAlreadyExists
  | "StreamingDistributionNotDisabled" ->
      Some StreamingDistributionNotDisabled
  | "Throttling" -> Some Throttling
  | "TooManyCacheBehaviors" -> Some TooManyCacheBehaviors
  | "TooManyCertificates" -> Some TooManyCertificates
  | "TooManyCloudFrontOriginAccessIdentities" ->
      Some TooManyCloudFrontOriginAccessIdentities
  | "TooManyCookieNamesInWhiteList" -> Some TooManyCookieNamesInWhiteList
  | "TooManyDistributionCNAMEs" -> Some TooManyDistributionCNAMEs
  | "TooManyDistributions" -> Some TooManyDistributions
  | "TooManyHeadersInForwardedValues" -> Some TooManyHeadersInForwardedValues
  | "TooManyInvalidationsInProgress" -> Some TooManyInvalidationsInProgress
  | "TooManyOrigins" -> Some TooManyOrigins
  | "TooManyStreamingDistributionCNAMEs" ->
      Some TooManyStreamingDistributionCNAMEs
  | "TooManyStreamingDistributions" -> Some TooManyStreamingDistributions
  | "TooManyTrustedSigners" -> Some TooManyTrustedSigners
  | "TrustedSignerDoesNotExist" -> Some TrustedSignerDoesNotExist
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None