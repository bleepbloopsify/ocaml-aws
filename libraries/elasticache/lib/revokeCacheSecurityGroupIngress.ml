open Types_internal
open Aws
type input = RevokeCacheSecurityGroupIngressMessage.t
type output = RevokeCacheSecurityGroupIngressResult.t
type error = Errors_internal.t
let service = "elasticache"
let to_http req =
  let uri =
    Uri.add_query_params (Uri.of_string "https://elasticache.amazonaws.com")
      (List.append
         [("Version", ["2015-02-02"]);
         ("Action", ["RevokeCacheSecurityGroupIngress"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render
                  (RevokeCacheSecurityGroupIngressMessage.to_query req))))) in
  (`POST, uri, [])
let of_http body =
  try
    let xml = Ezxmlm.from_string body in
    let resp =
      Util.option_bind
        (Xml.member "RevokeCacheSecurityGroupIngressResponse" (snd xml))
        (Xml.member "RevokeCacheSecurityGroupIngressResult") in
    try
      Util.or_error
        (Util.option_bind resp RevokeCacheSecurityGroupIngressResult.parse)
        (let open Error in
           BadResponse
             {
               body;
               message =
                 "Could not find well formed RevokeCacheSecurityGroupIngressResult."
             })
    with
    | Xml.RequiredFieldMissing msg ->
        let open Error in
          `Error
            (BadResponse
               {
                 body;
                 message =
                   ("Error parsing RevokeCacheSecurityGroupIngressResult - missing field in body or children: "
                      ^ msg)
               })
  with
  | Failure msg ->
      `Error
        (let open Error in
           BadResponse { body; message = ("Error parsing xml: " ^ msg) })
let parse_error code err =
  let errors =
    [Errors_internal.InvalidParameterCombination;
    Errors_internal.InvalidParameterValue;
    Errors_internal.InvalidCacheSecurityGroupState;
    Errors_internal.AuthorizationNotFound;
    Errors_internal.CacheSecurityGroupNotFound] @ Errors_internal.common in
  match Errors_internal.of_string err with
  | Some var ->
      if
        (List.mem var errors) &&
          ((match Errors_internal.to_http_code var with
            | Some var -> var = code
            | None -> true))
      then Some var
      else None
  | None -> None