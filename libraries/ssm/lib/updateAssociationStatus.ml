open Types
open Aws

type input = UpdateAssociationStatusRequest.t

type output = UpdateAssociationStatusResult.t

type error = Errors_internal.t

let service = "ssm"

let signature_version = Request.V4

let to_http service region req =
  let uri =
    Uri.add_query_params
      (Uri.of_string (Aws.Util.of_option_exn (Endpoints.url_of service region)))
      (List.append
         [ "Version", [ "2014-11-06" ]; "Action", [ "UpdateAssociationStatus" ] ]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (UpdateAssociationStatusRequest.to_query req)))))
  in
  `POST, uri, []

let of_http body =
  try
    let xml = Ezxmlm.from_string body in
    let resp = Xml.member "UpdateAssociationStatusResponse" (snd xml) in
    try
      Util.or_error
        (Util.option_bind resp UpdateAssociationStatusResult.parse)
        (let open Error in
        BadResponse
          { body; message = "Could not find well formed UpdateAssociationStatusResult." })
    with Xml.RequiredFieldMissing msg ->
      let open Error in
      `Error
        (BadResponse
           { body
           ; message =
               "Error parsing UpdateAssociationStatusResult - missing field in body or \
                children: "
               ^ msg
           })
  with Failure msg ->
    `Error
      (let open Error in
      BadResponse { body; message = "Error parsing xml: " ^ msg })

let parse_error code err =
  let errors =
    [ Errors_internal.TooManyUpdates
    ; Errors_internal.StatusUnchanged
    ; Errors_internal.AssociationDoesNotExist
    ; Errors_internal.InvalidDocument
    ; Errors_internal.InvalidInstanceId
    ; Errors_internal.InternalServerError
    ]
    @ Errors_internal.common
  in
  match Errors_internal.of_string err with
  | Some var ->
      if List.mem var errors
         &&
         match Errors_internal.to_http_code var with
         | Some var -> var = code
         | None -> true
      then Some var
      else None
  | None -> None
