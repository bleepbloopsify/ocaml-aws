open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module Method =
  struct
    type t =
      | GET 
      | HEAD 
      | POST 
      | PUT 
      | PATCH 
      | OPTIONS 
      | DELETE 
    let str_to_t =
      [("DELETE", DELETE);
      ("OPTIONS", OPTIONS);
      ("PATCH", PATCH);
      ("PUT", PUT);
      ("POST", POST);
      ("HEAD", HEAD);
      ("GET", GET)]
    let t_to_str =
      [(DELETE, "DELETE");
      (OPTIONS, "OPTIONS");
      (PATCH, "PATCH");
      (PUT, "PUT");
      (POST, "POST");
      (HEAD, "HEAD");
      (GET, "GET")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module CookieNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "Name" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module MethodsList =
  struct
    type t = Method.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Method.parse (Xml.members "Method" xml))
    let to_query v = Query.to_query_list Method.to_query v
    let to_json v = `List (List.map Method.to_json v)
    let of_json j = Json.to_list Method.of_json j
  end
module CookieNames =
  struct
    type t = {
      quantity: Integer.t ;
      items: CookieNameList.t }
    let make ~quantity  ?(items= [])  () = { quantity; items }
    let parse xml =
      Some
        {
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml)
                  CookieNameList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Items.member", (CookieNameList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (CookieNameList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity))])
    let of_json j =
      {
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (CookieNameList.of_json
             (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module ItemSelection =
  struct
    type t =
      | None 
      | Whitelist 
      | All 
    let str_to_t = [("all", All); ("whitelist", Whitelist); ("none", None)]
    let t_to_str = [(All, "all"); (Whitelist, "whitelist"); (None, "none")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module HeaderList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "Name" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module CachedMethods =
  struct
    type t = {
      quantity: Integer.t ;
      items: MethodsList.t }
    let make ~quantity  ~items  () = { quantity; items }
    let parse xml =
      Some
        {
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Xml.required "Items"
               (Util.option_bind (Xml.member "Items" xml) MethodsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Items.member", (MethodsList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (MethodsList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity))])
    let of_json j =
      {
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (MethodsList.of_json (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module CookiePreference =
  struct
    type t =
      {
      forward: ItemSelection.t ;
      whitelisted_names: CookieNames.t option }
    let make ~forward  ?whitelisted_names  () =
      { forward; whitelisted_names }
    let parse xml =
      Some
        {
          forward =
            (Xml.required "Forward"
               (Util.option_bind (Xml.member "Forward" xml)
                  ItemSelection.parse));
          whitelisted_names =
            (Util.option_bind (Xml.member "WhitelistedNames" xml)
               CookieNames.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.whitelisted_names
              (fun f ->
                 Query.Pair ("WhitelistedNames", (CookieNames.to_query f)));
           Some (Query.Pair ("Forward", (ItemSelection.to_query v.forward)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.whitelisted_names
              (fun f -> ("whitelisted_names", (CookieNames.to_json f)));
           Some ("forward", (ItemSelection.to_json v.forward))])
    let of_json j =
      {
        forward =
          (ItemSelection.of_json
             (Util.of_option_exn (Json.lookup j "forward")));
        whitelisted_names =
          (Util.option_map (Json.lookup j "whitelisted_names")
             CookieNames.of_json)
      }
  end
module Headers =
  struct
    type t = {
      quantity: Integer.t ;
      items: HeaderList.t }
    let make ~quantity  ?(items= [])  () = { quantity; items }
    let parse xml =
      Some
        {
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml) HeaderList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Items.member", (HeaderList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (HeaderList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity))])
    let of_json j =
      {
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (HeaderList.of_json (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module AwsAccountNumberList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "AwsAccountNumber" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module OriginProtocolPolicy =
  struct
    type t =
      | Http_only 
      | Match_viewer 
    let str_to_t = [("match-viewer", Match_viewer); ("http-only", Http_only)]
    let t_to_str = [(Match_viewer, "match-viewer"); (Http_only, "http-only")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module AllowedMethods =
  struct
    type t =
      {
      quantity: Integer.t ;
      items: MethodsList.t ;
      cached_methods: CachedMethods.t option }
    let make ~quantity  ~items  ?cached_methods  () =
      { quantity; items; cached_methods }
    let parse xml =
      Some
        {
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Xml.required "Items"
               (Util.option_bind (Xml.member "Items" xml) MethodsList.parse));
          cached_methods =
            (Util.option_bind (Xml.member "CachedMethods" xml)
               CachedMethods.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cached_methods
              (fun f ->
                 Query.Pair ("CachedMethods", (CachedMethods.to_query f)));
           Some (Query.Pair ("Items.member", (MethodsList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cached_methods
              (fun f -> ("cached_methods", (CachedMethods.to_json f)));
           Some ("items", (MethodsList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity))])
    let of_json j =
      {
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (MethodsList.of_json (Util.of_option_exn (Json.lookup j "items")));
        cached_methods =
          (Util.option_map (Json.lookup j "cached_methods")
             CachedMethods.of_json)
      }
  end
module ForwardedValues =
  struct
    type t =
      {
      query_string: Boolean.t ;
      cookies: CookiePreference.t ;
      headers: Headers.t option }
    let make ~query_string  ~cookies  ?headers  () =
      { query_string; cookies; headers }
    let parse xml =
      Some
        {
          query_string =
            (Xml.required "QueryString"
               (Util.option_bind (Xml.member "QueryString" xml) Boolean.parse));
          cookies =
            (Xml.required "Cookies"
               (Util.option_bind (Xml.member "Cookies" xml)
                  CookiePreference.parse));
          headers =
            (Util.option_bind (Xml.member "Headers" xml) Headers.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.headers
              (fun f -> Query.Pair ("Headers", (Headers.to_query f)));
           Some
             (Query.Pair ("Cookies", (CookiePreference.to_query v.cookies)));
           Some
             (Query.Pair ("QueryString", (Boolean.to_query v.query_string)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.headers
              (fun f -> ("headers", (Headers.to_json f)));
           Some ("cookies", (CookiePreference.to_json v.cookies));
           Some ("query_string", (Boolean.to_json v.query_string))])
    let of_json j =
      {
        query_string =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "query_string")));
        cookies =
          (CookiePreference.of_json
             (Util.of_option_exn (Json.lookup j "cookies")));
        headers = (Util.option_map (Json.lookup j "headers") Headers.of_json)
      }
  end
module TrustedSigners =
  struct
    type t =
      {
      enabled: Boolean.t ;
      quantity: Integer.t ;
      items: AwsAccountNumberList.t }
    let make ~enabled  ~quantity  ?(items= [])  () =
      { enabled; quantity; items }
    let parse xml =
      Some
        {
          enabled =
            (Xml.required "Enabled"
               (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse));
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml)
                  AwsAccountNumberList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Items.member", (AwsAccountNumberList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)));
           Some (Query.Pair ("Enabled", (Boolean.to_query v.enabled)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (AwsAccountNumberList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity));
           Some ("enabled", (Boolean.to_json v.enabled))])
    let of_json j =
      {
        enabled =
          (Boolean.of_json (Util.of_option_exn (Json.lookup j "enabled")));
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (AwsAccountNumberList.of_json
             (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module ViewerProtocolPolicy =
  struct
    type t =
      | Allow_all 
      | Https_only 
      | Redirect_to_https 
    let str_to_t =
      [("redirect-to-https", Redirect_to_https);
      ("https-only", Https_only);
      ("allow-all", Allow_all)]
    let t_to_str =
      [(Redirect_to_https, "redirect-to-https");
      (Https_only, "https-only");
      (Allow_all, "allow-all")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module CustomOriginConfig =
  struct
    type t =
      {
      h_t_t_p_port: Integer.t ;
      h_t_t_p_s_port: Integer.t ;
      origin_protocol_policy: OriginProtocolPolicy.t }
    let make ~h_t_t_p_port  ~h_t_t_p_s_port  ~origin_protocol_policy  () =
      { h_t_t_p_port; h_t_t_p_s_port; origin_protocol_policy }
    let parse xml =
      Some
        {
          h_t_t_p_port =
            (Xml.required "HTTPPort"
               (Util.option_bind (Xml.member "HTTPPort" xml) Integer.parse));
          h_t_t_p_s_port =
            (Xml.required "HTTPSPort"
               (Util.option_bind (Xml.member "HTTPSPort" xml) Integer.parse));
          origin_protocol_policy =
            (Xml.required "OriginProtocolPolicy"
               (Util.option_bind (Xml.member "OriginProtocolPolicy" xml)
                  OriginProtocolPolicy.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("OriginProtocolPolicy",
                   (OriginProtocolPolicy.to_query v.origin_protocol_policy)));
           Some
             (Query.Pair ("HTTPSPort", (Integer.to_query v.h_t_t_p_s_port)));
           Some (Query.Pair ("HTTPPort", (Integer.to_query v.h_t_t_p_port)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("origin_protocol_policy",
                (OriginProtocolPolicy.to_json v.origin_protocol_policy));
           Some ("h_t_t_p_s_port", (Integer.to_json v.h_t_t_p_s_port));
           Some ("h_t_t_p_port", (Integer.to_json v.h_t_t_p_port))])
    let of_json j =
      {
        h_t_t_p_port =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "h_t_t_p_port")));
        h_t_t_p_s_port =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "h_t_t_p_s_port")));
        origin_protocol_policy =
          (OriginProtocolPolicy.of_json
             (Util.of_option_exn (Json.lookup j "origin_protocol_policy")))
      }
  end
module S3OriginConfig =
  struct
    type t = {
      origin_access_identity: String.t }
    let make ~origin_access_identity  () = { origin_access_identity }
    let parse xml =
      Some
        {
          origin_access_identity =
            (Xml.required "OriginAccessIdentity"
               (Util.option_bind (Xml.member "OriginAccessIdentity" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("OriginAccessIdentity",
                   (String.to_query v.origin_access_identity)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("origin_access_identity",
                (String.to_json v.origin_access_identity))])
    let of_json j =
      {
        origin_access_identity =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "origin_access_identity")))
      }
  end
module CacheBehavior =
  struct
    type t =
      {
      path_pattern: String.t ;
      target_origin_id: String.t ;
      forwarded_values: ForwardedValues.t ;
      trusted_signers: TrustedSigners.t ;
      viewer_protocol_policy: ViewerProtocolPolicy.t ;
      min_t_t_l: Long.t ;
      allowed_methods: AllowedMethods.t option ;
      smooth_streaming: Boolean.t option ;
      default_t_t_l: Long.t option ;
      max_t_t_l: Long.t option }
    let make ~path_pattern  ~target_origin_id  ~forwarded_values 
      ~trusted_signers  ~viewer_protocol_policy  ~min_t_t_l  ?allowed_methods
       ?smooth_streaming  ?default_t_t_l  ?max_t_t_l  () =
      {
        path_pattern;
        target_origin_id;
        forwarded_values;
        trusted_signers;
        viewer_protocol_policy;
        min_t_t_l;
        allowed_methods;
        smooth_streaming;
        default_t_t_l;
        max_t_t_l
      }
    let parse xml =
      Some
        {
          path_pattern =
            (Xml.required "PathPattern"
               (Util.option_bind (Xml.member "PathPattern" xml) String.parse));
          target_origin_id =
            (Xml.required "TargetOriginId"
               (Util.option_bind (Xml.member "TargetOriginId" xml)
                  String.parse));
          forwarded_values =
            (Xml.required "ForwardedValues"
               (Util.option_bind (Xml.member "ForwardedValues" xml)
                  ForwardedValues.parse));
          trusted_signers =
            (Xml.required "TrustedSigners"
               (Util.option_bind (Xml.member "TrustedSigners" xml)
                  TrustedSigners.parse));
          viewer_protocol_policy =
            (Xml.required "ViewerProtocolPolicy"
               (Util.option_bind (Xml.member "ViewerProtocolPolicy" xml)
                  ViewerProtocolPolicy.parse));
          min_t_t_l =
            (Xml.required "MinTTL"
               (Util.option_bind (Xml.member "MinTTL" xml) Long.parse));
          allowed_methods =
            (Util.option_bind (Xml.member "AllowedMethods" xml)
               AllowedMethods.parse);
          smooth_streaming =
            (Util.option_bind (Xml.member "SmoothStreaming" xml)
               Boolean.parse);
          default_t_t_l =
            (Util.option_bind (Xml.member "DefaultTTL" xml) Long.parse);
          max_t_t_l = (Util.option_bind (Xml.member "MaxTTL" xml) Long.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_t_t_l
              (fun f -> Query.Pair ("MaxTTL", (Long.to_query f)));
           Util.option_map v.default_t_t_l
             (fun f -> Query.Pair ("DefaultTTL", (Long.to_query f)));
           Util.option_map v.smooth_streaming
             (fun f -> Query.Pair ("SmoothStreaming", (Boolean.to_query f)));
           Util.option_map v.allowed_methods
             (fun f ->
                Query.Pair ("AllowedMethods", (AllowedMethods.to_query f)));
           Some (Query.Pair ("MinTTL", (Long.to_query v.min_t_t_l)));
           Some
             (Query.Pair
                ("ViewerProtocolPolicy",
                  (ViewerProtocolPolicy.to_query v.viewer_protocol_policy)));
           Some
             (Query.Pair
                ("TrustedSigners",
                  (TrustedSigners.to_query v.trusted_signers)));
           Some
             (Query.Pair
                ("ForwardedValues",
                  (ForwardedValues.to_query v.forwarded_values)));
           Some
             (Query.Pair
                ("TargetOriginId", (String.to_query v.target_origin_id)));
           Some
             (Query.Pair ("PathPattern", (String.to_query v.path_pattern)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_t_t_l
              (fun f -> ("max_t_t_l", (Long.to_json f)));
           Util.option_map v.default_t_t_l
             (fun f -> ("default_t_t_l", (Long.to_json f)));
           Util.option_map v.smooth_streaming
             (fun f -> ("smooth_streaming", (Boolean.to_json f)));
           Util.option_map v.allowed_methods
             (fun f -> ("allowed_methods", (AllowedMethods.to_json f)));
           Some ("min_t_t_l", (Long.to_json v.min_t_t_l));
           Some
             ("viewer_protocol_policy",
               (ViewerProtocolPolicy.to_json v.viewer_protocol_policy));
           Some
             ("trusted_signers", (TrustedSigners.to_json v.trusted_signers));
           Some
             ("forwarded_values",
               (ForwardedValues.to_json v.forwarded_values));
           Some ("target_origin_id", (String.to_json v.target_origin_id));
           Some ("path_pattern", (String.to_json v.path_pattern))])
    let of_json j =
      {
        path_pattern =
          (String.of_json (Util.of_option_exn (Json.lookup j "path_pattern")));
        target_origin_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "target_origin_id")));
        forwarded_values =
          (ForwardedValues.of_json
             (Util.of_option_exn (Json.lookup j "forwarded_values")));
        trusted_signers =
          (TrustedSigners.of_json
             (Util.of_option_exn (Json.lookup j "trusted_signers")));
        viewer_protocol_policy =
          (ViewerProtocolPolicy.of_json
             (Util.of_option_exn (Json.lookup j "viewer_protocol_policy")));
        min_t_t_l =
          (Long.of_json (Util.of_option_exn (Json.lookup j "min_t_t_l")));
        allowed_methods =
          (Util.option_map (Json.lookup j "allowed_methods")
             AllowedMethods.of_json);
        smooth_streaming =
          (Util.option_map (Json.lookup j "smooth_streaming") Boolean.of_json);
        default_t_t_l =
          (Util.option_map (Json.lookup j "default_t_t_l") Long.of_json);
        max_t_t_l =
          (Util.option_map (Json.lookup j "max_t_t_l") Long.of_json)
      }
  end
module CustomErrorResponse =
  struct
    type t =
      {
      error_code: Integer.t ;
      response_page_path: String.t option ;
      response_code: String.t option ;
      error_caching_min_t_t_l: Long.t option }
    let make ~error_code  ?response_page_path  ?response_code 
      ?error_caching_min_t_t_l  () =
      {
        error_code;
        response_page_path;
        response_code;
        error_caching_min_t_t_l
      }
    let parse xml =
      Some
        {
          error_code =
            (Xml.required "ErrorCode"
               (Util.option_bind (Xml.member "ErrorCode" xml) Integer.parse));
          response_page_path =
            (Util.option_bind (Xml.member "ResponsePagePath" xml)
               String.parse);
          response_code =
            (Util.option_bind (Xml.member "ResponseCode" xml) String.parse);
          error_caching_min_t_t_l =
            (Util.option_bind (Xml.member "ErrorCachingMinTTL" xml)
               Long.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.error_caching_min_t_t_l
              (fun f -> Query.Pair ("ErrorCachingMinTTL", (Long.to_query f)));
           Util.option_map v.response_code
             (fun f -> Query.Pair ("ResponseCode", (String.to_query f)));
           Util.option_map v.response_page_path
             (fun f -> Query.Pair ("ResponsePagePath", (String.to_query f)));
           Some (Query.Pair ("ErrorCode", (Integer.to_query v.error_code)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.error_caching_min_t_t_l
              (fun f -> ("error_caching_min_t_t_l", (Long.to_json f)));
           Util.option_map v.response_code
             (fun f -> ("response_code", (String.to_json f)));
           Util.option_map v.response_page_path
             (fun f -> ("response_page_path", (String.to_json f)));
           Some ("error_code", (Integer.to_json v.error_code))])
    let of_json j =
      {
        error_code =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "error_code")));
        response_page_path =
          (Util.option_map (Json.lookup j "response_page_path")
             String.of_json);
        response_code =
          (Util.option_map (Json.lookup j "response_code") String.of_json);
        error_caching_min_t_t_l =
          (Util.option_map (Json.lookup j "error_caching_min_t_t_l")
             Long.of_json)
      }
  end
module Origin =
  struct
    type t =
      {
      id: String.t ;
      domain_name: String.t ;
      origin_path: String.t option ;
      s3_origin_config: S3OriginConfig.t option ;
      custom_origin_config: CustomOriginConfig.t option }
    let make ~id  ~domain_name  ?origin_path  ?s3_origin_config 
      ?custom_origin_config  () =
      { id; domain_name; origin_path; s3_origin_config; custom_origin_config
      }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse));
          origin_path =
            (Util.option_bind (Xml.member "OriginPath" xml) String.parse);
          s3_origin_config =
            (Util.option_bind (Xml.member "S3OriginConfig" xml)
               S3OriginConfig.parse);
          custom_origin_config =
            (Util.option_bind (Xml.member "CustomOriginConfig" xml)
               CustomOriginConfig.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.custom_origin_config
              (fun f ->
                 Query.Pair
                   ("CustomOriginConfig", (CustomOriginConfig.to_query f)));
           Util.option_map v.s3_origin_config
             (fun f ->
                Query.Pair ("S3OriginConfig", (S3OriginConfig.to_query f)));
           Util.option_map v.origin_path
             (fun f -> Query.Pair ("OriginPath", (String.to_query f)));
           Some (Query.Pair ("DomainName", (String.to_query v.domain_name)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.custom_origin_config
              (fun f ->
                 ("custom_origin_config", (CustomOriginConfig.to_json f)));
           Util.option_map v.s3_origin_config
             (fun f -> ("s3_origin_config", (S3OriginConfig.to_json f)));
           Util.option_map v.origin_path
             (fun f -> ("origin_path", (String.to_json f)));
           Some ("domain_name", (String.to_json v.domain_name));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")));
        origin_path =
          (Util.option_map (Json.lookup j "origin_path") String.of_json);
        s3_origin_config =
          (Util.option_map (Json.lookup j "s3_origin_config")
             S3OriginConfig.of_json);
        custom_origin_config =
          (Util.option_map (Json.lookup j "custom_origin_config")
             CustomOriginConfig.of_json)
      }
  end
module GeoRestrictionType =
  struct
    type t =
      | Blacklist 
      | Whitelist 
      | None 
    let str_to_t =
      [("none", None); ("whitelist", Whitelist); ("blacklist", Blacklist)]
    let t_to_str =
      [(None, "none"); (Whitelist, "whitelist"); (Blacklist, "blacklist")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module LocationList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "Location" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module KeyPairIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "KeyPairId" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module AliasList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "CNAME" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module CacheBehaviorList =
  struct
    type t = CacheBehavior.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CacheBehavior.parse (Xml.members "CacheBehavior" xml))
    let to_query v = Query.to_query_list CacheBehavior.to_query v
    let to_json v = `List (List.map CacheBehavior.to_json v)
    let of_json j = Json.to_list CacheBehavior.of_json j
  end
module CustomErrorResponseList =
  struct
    type t = CustomErrorResponse.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CustomErrorResponse.parse
           (Xml.members "CustomErrorResponse" xml))
    let to_query v = Query.to_query_list CustomErrorResponse.to_query v
    let to_json v = `List (List.map CustomErrorResponse.to_json v)
    let of_json j = Json.to_list CustomErrorResponse.of_json j
  end
module OriginList =
  struct
    type t = Origin.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Origin.parse (Xml.members "Origin" xml))
    let to_query v = Query.to_query_list Origin.to_query v
    let to_json v = `List (List.map Origin.to_json v)
    let of_json j = Json.to_list Origin.of_json j
  end
module GeoRestriction =
  struct
    type t =
      {
      restriction_type: GeoRestrictionType.t ;
      quantity: Integer.t ;
      items: LocationList.t }
    let make ~restriction_type  ~quantity  ?(items= [])  () =
      { restriction_type; quantity; items }
    let parse xml =
      Some
        {
          restriction_type =
            (Xml.required "RestrictionType"
               (Util.option_bind (Xml.member "RestrictionType" xml)
                  GeoRestrictionType.parse));
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml) LocationList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Items.member", (LocationList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)));
           Some
             (Query.Pair
                ("RestrictionType",
                  (GeoRestrictionType.to_query v.restriction_type)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (LocationList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity));
           Some
             ("restriction_type",
               (GeoRestrictionType.to_json v.restriction_type))])
    let of_json j =
      {
        restriction_type =
          (GeoRestrictionType.of_json
             (Util.of_option_exn (Json.lookup j "restriction_type")));
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (LocationList.of_json (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module MinimumProtocolVersion =
  struct
    type t =
      | SSLv3 
      | TLSv1 
    let str_to_t = [("TLSv1", TLSv1); ("SSLv3", SSLv3)]
    let t_to_str = [(TLSv1, "TLSv1"); (SSLv3, "SSLv3")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module SSLSupportMethod =
  struct
    type t =
      | Sni_only 
      | Vip 
    let str_to_t = [("vip", Vip); ("sni-only", Sni_only)]
    let t_to_str = [(Vip, "vip"); (Sni_only, "sni-only")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module KeyPairIds =
  struct
    type t = {
      quantity: Integer.t ;
      items: KeyPairIdList.t }
    let make ~quantity  ?(items= [])  () = { quantity; items }
    let parse xml =
      Some
        {
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml) KeyPairIdList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Items.member", (KeyPairIdList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (KeyPairIdList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity))])
    let of_json j =
      {
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (KeyPairIdList.of_json (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module PathList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "Path" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module Aliases =
  struct
    type t = {
      quantity: Integer.t ;
      items: AliasList.t }
    let make ~quantity  ?(items= [])  () = { quantity; items }
    let parse xml =
      Some
        {
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml) AliasList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Items.member", (AliasList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (AliasList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity))])
    let of_json j =
      {
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (AliasList.of_json (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module CacheBehaviors =
  struct
    type t = {
      quantity: Integer.t ;
      items: CacheBehaviorList.t }
    let make ~quantity  ?(items= [])  () = { quantity; items }
    let parse xml =
      Some
        {
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml)
                  CacheBehaviorList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Items.member", (CacheBehaviorList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (CacheBehaviorList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity))])
    let of_json j =
      {
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (CacheBehaviorList.of_json
             (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module CustomErrorResponses =
  struct
    type t = {
      quantity: Integer.t ;
      items: CustomErrorResponseList.t }
    let make ~quantity  ?(items= [])  () = { quantity; items }
    let parse xml =
      Some
        {
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml)
                  CustomErrorResponseList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Items.member", (CustomErrorResponseList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (CustomErrorResponseList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity))])
    let of_json j =
      {
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (CustomErrorResponseList.of_json
             (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module DefaultCacheBehavior =
  struct
    type t =
      {
      target_origin_id: String.t ;
      forwarded_values: ForwardedValues.t ;
      trusted_signers: TrustedSigners.t ;
      viewer_protocol_policy: ViewerProtocolPolicy.t ;
      min_t_t_l: Long.t ;
      allowed_methods: AllowedMethods.t option ;
      smooth_streaming: Boolean.t option ;
      default_t_t_l: Long.t option ;
      max_t_t_l: Long.t option }
    let make ~target_origin_id  ~forwarded_values  ~trusted_signers 
      ~viewer_protocol_policy  ~min_t_t_l  ?allowed_methods 
      ?smooth_streaming  ?default_t_t_l  ?max_t_t_l  () =
      {
        target_origin_id;
        forwarded_values;
        trusted_signers;
        viewer_protocol_policy;
        min_t_t_l;
        allowed_methods;
        smooth_streaming;
        default_t_t_l;
        max_t_t_l
      }
    let parse xml =
      Some
        {
          target_origin_id =
            (Xml.required "TargetOriginId"
               (Util.option_bind (Xml.member "TargetOriginId" xml)
                  String.parse));
          forwarded_values =
            (Xml.required "ForwardedValues"
               (Util.option_bind (Xml.member "ForwardedValues" xml)
                  ForwardedValues.parse));
          trusted_signers =
            (Xml.required "TrustedSigners"
               (Util.option_bind (Xml.member "TrustedSigners" xml)
                  TrustedSigners.parse));
          viewer_protocol_policy =
            (Xml.required "ViewerProtocolPolicy"
               (Util.option_bind (Xml.member "ViewerProtocolPolicy" xml)
                  ViewerProtocolPolicy.parse));
          min_t_t_l =
            (Xml.required "MinTTL"
               (Util.option_bind (Xml.member "MinTTL" xml) Long.parse));
          allowed_methods =
            (Util.option_bind (Xml.member "AllowedMethods" xml)
               AllowedMethods.parse);
          smooth_streaming =
            (Util.option_bind (Xml.member "SmoothStreaming" xml)
               Boolean.parse);
          default_t_t_l =
            (Util.option_bind (Xml.member "DefaultTTL" xml) Long.parse);
          max_t_t_l = (Util.option_bind (Xml.member "MaxTTL" xml) Long.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_t_t_l
              (fun f -> Query.Pair ("MaxTTL", (Long.to_query f)));
           Util.option_map v.default_t_t_l
             (fun f -> Query.Pair ("DefaultTTL", (Long.to_query f)));
           Util.option_map v.smooth_streaming
             (fun f -> Query.Pair ("SmoothStreaming", (Boolean.to_query f)));
           Util.option_map v.allowed_methods
             (fun f ->
                Query.Pair ("AllowedMethods", (AllowedMethods.to_query f)));
           Some (Query.Pair ("MinTTL", (Long.to_query v.min_t_t_l)));
           Some
             (Query.Pair
                ("ViewerProtocolPolicy",
                  (ViewerProtocolPolicy.to_query v.viewer_protocol_policy)));
           Some
             (Query.Pair
                ("TrustedSigners",
                  (TrustedSigners.to_query v.trusted_signers)));
           Some
             (Query.Pair
                ("ForwardedValues",
                  (ForwardedValues.to_query v.forwarded_values)));
           Some
             (Query.Pair
                ("TargetOriginId", (String.to_query v.target_origin_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_t_t_l
              (fun f -> ("max_t_t_l", (Long.to_json f)));
           Util.option_map v.default_t_t_l
             (fun f -> ("default_t_t_l", (Long.to_json f)));
           Util.option_map v.smooth_streaming
             (fun f -> ("smooth_streaming", (Boolean.to_json f)));
           Util.option_map v.allowed_methods
             (fun f -> ("allowed_methods", (AllowedMethods.to_json f)));
           Some ("min_t_t_l", (Long.to_json v.min_t_t_l));
           Some
             ("viewer_protocol_policy",
               (ViewerProtocolPolicy.to_json v.viewer_protocol_policy));
           Some
             ("trusted_signers", (TrustedSigners.to_json v.trusted_signers));
           Some
             ("forwarded_values",
               (ForwardedValues.to_json v.forwarded_values));
           Some ("target_origin_id", (String.to_json v.target_origin_id))])
    let of_json j =
      {
        target_origin_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "target_origin_id")));
        forwarded_values =
          (ForwardedValues.of_json
             (Util.of_option_exn (Json.lookup j "forwarded_values")));
        trusted_signers =
          (TrustedSigners.of_json
             (Util.of_option_exn (Json.lookup j "trusted_signers")));
        viewer_protocol_policy =
          (ViewerProtocolPolicy.of_json
             (Util.of_option_exn (Json.lookup j "viewer_protocol_policy")));
        min_t_t_l =
          (Long.of_json (Util.of_option_exn (Json.lookup j "min_t_t_l")));
        allowed_methods =
          (Util.option_map (Json.lookup j "allowed_methods")
             AllowedMethods.of_json);
        smooth_streaming =
          (Util.option_map (Json.lookup j "smooth_streaming") Boolean.of_json);
        default_t_t_l =
          (Util.option_map (Json.lookup j "default_t_t_l") Long.of_json);
        max_t_t_l =
          (Util.option_map (Json.lookup j "max_t_t_l") Long.of_json)
      }
  end
module Origins =
  struct
    type t = {
      quantity: Integer.t ;
      items: OriginList.t }
    let make ~quantity  ?(items= [])  () = { quantity; items }
    let parse xml =
      Some
        {
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml) OriginList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Items.member", (OriginList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (OriginList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity))])
    let of_json j =
      {
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (OriginList.of_json (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module PriceClass =
  struct
    type t =
      | PriceClass_100 
      | PriceClass_200 
      | PriceClass_All 
    let str_to_t =
      [("PriceClass_All", PriceClass_All);
      ("PriceClass_200", PriceClass_200);
      ("PriceClass_100", PriceClass_100)]
    let t_to_str =
      [(PriceClass_All, "PriceClass_All");
      (PriceClass_200, "PriceClass_200");
      (PriceClass_100, "PriceClass_100")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module Restrictions =
  struct
    type t = {
      geo_restriction: GeoRestriction.t }
    let make ~geo_restriction  () = { geo_restriction }
    let parse xml =
      Some
        {
          geo_restriction =
            (Xml.required "GeoRestriction"
               (Util.option_bind (Xml.member "GeoRestriction" xml)
                  GeoRestriction.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("GeoRestriction",
                   (GeoRestriction.to_query v.geo_restriction)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("geo_restriction", (GeoRestriction.to_json v.geo_restriction))])
    let of_json j =
      {
        geo_restriction =
          (GeoRestriction.of_json
             (Util.of_option_exn (Json.lookup j "geo_restriction")))
      }
  end
module ViewerCertificate =
  struct
    type t =
      {
      i_a_m_certificate_id: String.t option ;
      cloud_front_default_certificate: Boolean.t option ;
      s_s_l_support_method: SSLSupportMethod.t option ;
      minimum_protocol_version: MinimumProtocolVersion.t option }
    let make ?i_a_m_certificate_id  ?cloud_front_default_certificate 
      ?s_s_l_support_method  ?minimum_protocol_version  () =
      {
        i_a_m_certificate_id;
        cloud_front_default_certificate;
        s_s_l_support_method;
        minimum_protocol_version
      }
    let parse xml =
      Some
        {
          i_a_m_certificate_id =
            (Util.option_bind (Xml.member "IAMCertificateId" xml)
               String.parse);
          cloud_front_default_certificate =
            (Util.option_bind (Xml.member "CloudFrontDefaultCertificate" xml)
               Boolean.parse);
          s_s_l_support_method =
            (Util.option_bind (Xml.member "SSLSupportMethod" xml)
               SSLSupportMethod.parse);
          minimum_protocol_version =
            (Util.option_bind (Xml.member "MinimumProtocolVersion" xml)
               MinimumProtocolVersion.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.minimum_protocol_version
              (fun f ->
                 Query.Pair
                   ("MinimumProtocolVersion",
                     (MinimumProtocolVersion.to_query f)));
           Util.option_map v.s_s_l_support_method
             (fun f ->
                Query.Pair
                  ("SSLSupportMethod", (SSLSupportMethod.to_query f)));
           Util.option_map v.cloud_front_default_certificate
             (fun f ->
                Query.Pair
                  ("CloudFrontDefaultCertificate", (Boolean.to_query f)));
           Util.option_map v.i_a_m_certificate_id
             (fun f -> Query.Pair ("IAMCertificateId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.minimum_protocol_version
              (fun f ->
                 ("minimum_protocol_version",
                   (MinimumProtocolVersion.to_json f)));
           Util.option_map v.s_s_l_support_method
             (fun f -> ("s_s_l_support_method", (SSLSupportMethod.to_json f)));
           Util.option_map v.cloud_front_default_certificate
             (fun f ->
                ("cloud_front_default_certificate", (Boolean.to_json f)));
           Util.option_map v.i_a_m_certificate_id
             (fun f -> ("i_a_m_certificate_id", (String.to_json f)))])
    let of_json j =
      {
        i_a_m_certificate_id =
          (Util.option_map (Json.lookup j "i_a_m_certificate_id")
             String.of_json);
        cloud_front_default_certificate =
          (Util.option_map (Json.lookup j "cloud_front_default_certificate")
             Boolean.of_json);
        s_s_l_support_method =
          (Util.option_map (Json.lookup j "s_s_l_support_method")
             SSLSupportMethod.of_json);
        minimum_protocol_version =
          (Util.option_map (Json.lookup j "minimum_protocol_version")
             MinimumProtocolVersion.of_json)
      }
  end
module Signer =
  struct
    type t =
      {
      aws_account_number: String.t option ;
      key_pair_ids: KeyPairIds.t option }
    let make ?aws_account_number  ?key_pair_ids  () =
      { aws_account_number; key_pair_ids }
    let parse xml =
      Some
        {
          aws_account_number =
            (Util.option_bind (Xml.member "AwsAccountNumber" xml)
               String.parse);
          key_pair_ids =
            (Util.option_bind (Xml.member "KeyPairIds" xml) KeyPairIds.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key_pair_ids
              (fun f -> Query.Pair ("KeyPairIds", (KeyPairIds.to_query f)));
           Util.option_map v.aws_account_number
             (fun f -> Query.Pair ("AwsAccountNumber", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key_pair_ids
              (fun f -> ("key_pair_ids", (KeyPairIds.to_json f)));
           Util.option_map v.aws_account_number
             (fun f -> ("aws_account_number", (String.to_json f)))])
    let of_json j =
      {
        aws_account_number =
          (Util.option_map (Json.lookup j "aws_account_number")
             String.of_json);
        key_pair_ids =
          (Util.option_map (Json.lookup j "key_pair_ids") KeyPairIds.of_json)
      }
  end
module S3Origin =
  struct
    type t = {
      domain_name: String.t ;
      origin_access_identity: String.t }
    let make ~domain_name  ~origin_access_identity  () =
      { domain_name; origin_access_identity }
    let parse xml =
      Some
        {
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse));
          origin_access_identity =
            (Xml.required "OriginAccessIdentity"
               (Util.option_bind (Xml.member "OriginAccessIdentity" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("OriginAccessIdentity",
                   (String.to_query v.origin_access_identity)));
           Some (Query.Pair ("DomainName", (String.to_query v.domain_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("origin_access_identity",
                (String.to_json v.origin_access_identity));
           Some ("domain_name", (String.to_json v.domain_name))])
    let of_json j =
      {
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")));
        origin_access_identity =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "origin_access_identity")))
      }
  end
module Paths =
  struct
    type t = {
      quantity: Integer.t ;
      items: PathList.t }
    let make ~quantity  ?(items= [])  () = { quantity; items }
    let parse xml =
      Some
        {
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml) PathList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Items.member", (PathList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (PathList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity))])
    let of_json j =
      {
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (PathList.of_json (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module DistributionSummary =
  struct
    type t =
      {
      id: String.t ;
      status: String.t ;
      last_modified_time: DateTime.t ;
      domain_name: String.t ;
      aliases: Aliases.t ;
      origins: Origins.t ;
      default_cache_behavior: DefaultCacheBehavior.t ;
      cache_behaviors: CacheBehaviors.t ;
      custom_error_responses: CustomErrorResponses.t ;
      comment: String.t ;
      price_class: PriceClass.t ;
      enabled: Boolean.t ;
      viewer_certificate: ViewerCertificate.t ;
      restrictions: Restrictions.t }
    let make ~id  ~status  ~last_modified_time  ~domain_name  ~aliases 
      ~origins  ~default_cache_behavior  ~cache_behaviors 
      ~custom_error_responses  ~comment  ~price_class  ~enabled 
      ~viewer_certificate  ~restrictions  () =
      {
        id;
        status;
        last_modified_time;
        domain_name;
        aliases;
        origins;
        default_cache_behavior;
        cache_behaviors;
        custom_error_responses;
        comment;
        price_class;
        enabled;
        viewer_certificate;
        restrictions
      }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          status =
            (Xml.required "Status"
               (Util.option_bind (Xml.member "Status" xml) String.parse));
          last_modified_time =
            (Xml.required "LastModifiedTime"
               (Util.option_bind (Xml.member "LastModifiedTime" xml)
                  DateTime.parse));
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse));
          aliases =
            (Xml.required "Aliases"
               (Util.option_bind (Xml.member "Aliases" xml) Aliases.parse));
          origins =
            (Xml.required "Origins"
               (Util.option_bind (Xml.member "Origins" xml) Origins.parse));
          default_cache_behavior =
            (Xml.required "DefaultCacheBehavior"
               (Util.option_bind (Xml.member "DefaultCacheBehavior" xml)
                  DefaultCacheBehavior.parse));
          cache_behaviors =
            (Xml.required "CacheBehaviors"
               (Util.option_bind (Xml.member "CacheBehaviors" xml)
                  CacheBehaviors.parse));
          custom_error_responses =
            (Xml.required "CustomErrorResponses"
               (Util.option_bind (Xml.member "CustomErrorResponses" xml)
                  CustomErrorResponses.parse));
          comment =
            (Xml.required "Comment"
               (Util.option_bind (Xml.member "Comment" xml) String.parse));
          price_class =
            (Xml.required "PriceClass"
               (Util.option_bind (Xml.member "PriceClass" xml)
                  PriceClass.parse));
          enabled =
            (Xml.required "Enabled"
               (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse));
          viewer_certificate =
            (Xml.required "ViewerCertificate"
               (Util.option_bind (Xml.member "ViewerCertificate" xml)
                  ViewerCertificate.parse));
          restrictions =
            (Xml.required "Restrictions"
               (Util.option_bind (Xml.member "Restrictions" xml)
                  Restrictions.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Restrictions", (Restrictions.to_query v.restrictions)));
           Some
             (Query.Pair
                ("ViewerCertificate",
                  (ViewerCertificate.to_query v.viewer_certificate)));
           Some (Query.Pair ("Enabled", (Boolean.to_query v.enabled)));
           Some
             (Query.Pair ("PriceClass", (PriceClass.to_query v.price_class)));
           Some (Query.Pair ("Comment", (String.to_query v.comment)));
           Some
             (Query.Pair
                ("CustomErrorResponses",
                  (CustomErrorResponses.to_query v.custom_error_responses)));
           Some
             (Query.Pair
                ("CacheBehaviors",
                  (CacheBehaviors.to_query v.cache_behaviors)));
           Some
             (Query.Pair
                ("DefaultCacheBehavior",
                  (DefaultCacheBehavior.to_query v.default_cache_behavior)));
           Some (Query.Pair ("Origins", (Origins.to_query v.origins)));
           Some (Query.Pair ("Aliases", (Aliases.to_query v.aliases)));
           Some (Query.Pair ("DomainName", (String.to_query v.domain_name)));
           Some
             (Query.Pair
                ("LastModifiedTime",
                  (DateTime.to_query v.last_modified_time)));
           Some (Query.Pair ("Status", (String.to_query v.status)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("restrictions", (Restrictions.to_json v.restrictions));
           Some
             ("viewer_certificate",
               (ViewerCertificate.to_json v.viewer_certificate));
           Some ("enabled", (Boolean.to_json v.enabled));
           Some ("price_class", (PriceClass.to_json v.price_class));
           Some ("comment", (String.to_json v.comment));
           Some
             ("custom_error_responses",
               (CustomErrorResponses.to_json v.custom_error_responses));
           Some
             ("cache_behaviors", (CacheBehaviors.to_json v.cache_behaviors));
           Some
             ("default_cache_behavior",
               (DefaultCacheBehavior.to_json v.default_cache_behavior));
           Some ("origins", (Origins.to_json v.origins));
           Some ("aliases", (Aliases.to_json v.aliases));
           Some ("domain_name", (String.to_json v.domain_name));
           Some
             ("last_modified_time", (DateTime.to_json v.last_modified_time));
           Some ("status", (String.to_json v.status));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        status =
          (String.of_json (Util.of_option_exn (Json.lookup j "status")));
        last_modified_time =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "last_modified_time")));
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")));
        aliases =
          (Aliases.of_json (Util.of_option_exn (Json.lookup j "aliases")));
        origins =
          (Origins.of_json (Util.of_option_exn (Json.lookup j "origins")));
        default_cache_behavior =
          (DefaultCacheBehavior.of_json
             (Util.of_option_exn (Json.lookup j "default_cache_behavior")));
        cache_behaviors =
          (CacheBehaviors.of_json
             (Util.of_option_exn (Json.lookup j "cache_behaviors")));
        custom_error_responses =
          (CustomErrorResponses.of_json
             (Util.of_option_exn (Json.lookup j "custom_error_responses")));
        comment =
          (String.of_json (Util.of_option_exn (Json.lookup j "comment")));
        price_class =
          (PriceClass.of_json
             (Util.of_option_exn (Json.lookup j "price_class")));
        enabled =
          (Boolean.of_json (Util.of_option_exn (Json.lookup j "enabled")));
        viewer_certificate =
          (ViewerCertificate.of_json
             (Util.of_option_exn (Json.lookup j "viewer_certificate")));
        restrictions =
          (Restrictions.of_json
             (Util.of_option_exn (Json.lookup j "restrictions")))
      }
  end
module SignerList =
  struct
    type t = Signer.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Signer.parse (Xml.members "Signer" xml))
    let to_query v = Query.to_query_list Signer.to_query v
    let to_json v = `List (List.map Signer.to_json v)
    let of_json j = Json.to_list Signer.of_json j
  end
module StreamingLoggingConfig =
  struct
    type t = {
      enabled: Boolean.t ;
      bucket: String.t ;
      prefix: String.t }
    let make ~enabled  ~bucket  ~prefix  () = { enabled; bucket; prefix }
    let parse xml =
      Some
        {
          enabled =
            (Xml.required "Enabled"
               (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse));
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          prefix =
            (Xml.required "Prefix"
               (Util.option_bind (Xml.member "Prefix" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Prefix", (String.to_query v.prefix)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)));
           Some (Query.Pair ("Enabled", (Boolean.to_query v.enabled)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("prefix", (String.to_json v.prefix));
           Some ("bucket", (String.to_json v.bucket));
           Some ("enabled", (Boolean.to_json v.enabled))])
    let of_json j =
      {
        enabled =
          (Boolean.of_json (Util.of_option_exn (Json.lookup j "enabled")));
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        prefix =
          (String.of_json (Util.of_option_exn (Json.lookup j "prefix")))
      }
  end
module LoggingConfig =
  struct
    type t =
      {
      enabled: Boolean.t ;
      include_cookies: Boolean.t ;
      bucket: String.t ;
      prefix: String.t }
    let make ~enabled  ~include_cookies  ~bucket  ~prefix  () =
      { enabled; include_cookies; bucket; prefix }
    let parse xml =
      Some
        {
          enabled =
            (Xml.required "Enabled"
               (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse));
          include_cookies =
            (Xml.required "IncludeCookies"
               (Util.option_bind (Xml.member "IncludeCookies" xml)
                  Boolean.parse));
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          prefix =
            (Xml.required "Prefix"
               (Util.option_bind (Xml.member "Prefix" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Prefix", (String.to_query v.prefix)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)));
           Some
             (Query.Pair
                ("IncludeCookies", (Boolean.to_query v.include_cookies)));
           Some (Query.Pair ("Enabled", (Boolean.to_query v.enabled)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("prefix", (String.to_json v.prefix));
           Some ("bucket", (String.to_json v.bucket));
           Some ("include_cookies", (Boolean.to_json v.include_cookies));
           Some ("enabled", (Boolean.to_json v.enabled))])
    let of_json j =
      {
        enabled =
          (Boolean.of_json (Util.of_option_exn (Json.lookup j "enabled")));
        include_cookies =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "include_cookies")));
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        prefix =
          (String.of_json (Util.of_option_exn (Json.lookup j "prefix")))
      }
  end
module CloudFrontOriginAccessIdentitySummary =
  struct
    type t =
      {
      id: String.t ;
      s3_canonical_user_id: String.t ;
      comment: String.t }
    let make ~id  ~s3_canonical_user_id  ~comment  () =
      { id; s3_canonical_user_id; comment }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          s3_canonical_user_id =
            (Xml.required "S3CanonicalUserId"
               (Util.option_bind (Xml.member "S3CanonicalUserId" xml)
                  String.parse));
          comment =
            (Xml.required "Comment"
               (Util.option_bind (Xml.member "Comment" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Comment", (String.to_query v.comment)));
           Some
             (Query.Pair
                ("S3CanonicalUserId",
                  (String.to_query v.s3_canonical_user_id)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("comment", (String.to_json v.comment));
           Some
             ("s3_canonical_user_id",
               (String.to_json v.s3_canonical_user_id));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        s3_canonical_user_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "s3_canonical_user_id")));
        comment =
          (String.of_json (Util.of_option_exn (Json.lookup j "comment")))
      }
  end
module InvalidationSummary =
  struct
    type t = {
      id: String.t ;
      create_time: DateTime.t ;
      status: String.t }
    let make ~id  ~create_time  ~status  () = { id; create_time; status }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          create_time =
            (Xml.required "CreateTime"
               (Util.option_bind (Xml.member "CreateTime" xml) DateTime.parse));
          status =
            (Xml.required "Status"
               (Util.option_bind (Xml.member "Status" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Status", (String.to_query v.status)));
           Some
             (Query.Pair ("CreateTime", (DateTime.to_query v.create_time)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("status", (String.to_json v.status));
           Some ("create_time", (DateTime.to_json v.create_time));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        create_time =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "create_time")));
        status =
          (String.of_json (Util.of_option_exn (Json.lookup j "status")))
      }
  end
module StreamingDistributionSummary =
  struct
    type t =
      {
      id: String.t ;
      status: String.t ;
      last_modified_time: DateTime.t ;
      domain_name: String.t ;
      s3_origin: S3Origin.t ;
      aliases: Aliases.t ;
      trusted_signers: TrustedSigners.t ;
      comment: String.t ;
      price_class: PriceClass.t ;
      enabled: Boolean.t }
    let make ~id  ~status  ~last_modified_time  ~domain_name  ~s3_origin 
      ~aliases  ~trusted_signers  ~comment  ~price_class  ~enabled  () =
      {
        id;
        status;
        last_modified_time;
        domain_name;
        s3_origin;
        aliases;
        trusted_signers;
        comment;
        price_class;
        enabled
      }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          status =
            (Xml.required "Status"
               (Util.option_bind (Xml.member "Status" xml) String.parse));
          last_modified_time =
            (Xml.required "LastModifiedTime"
               (Util.option_bind (Xml.member "LastModifiedTime" xml)
                  DateTime.parse));
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse));
          s3_origin =
            (Xml.required "S3Origin"
               (Util.option_bind (Xml.member "S3Origin" xml) S3Origin.parse));
          aliases =
            (Xml.required "Aliases"
               (Util.option_bind (Xml.member "Aliases" xml) Aliases.parse));
          trusted_signers =
            (Xml.required "TrustedSigners"
               (Util.option_bind (Xml.member "TrustedSigners" xml)
                  TrustedSigners.parse));
          comment =
            (Xml.required "Comment"
               (Util.option_bind (Xml.member "Comment" xml) String.parse));
          price_class =
            (Xml.required "PriceClass"
               (Util.option_bind (Xml.member "PriceClass" xml)
                  PriceClass.parse));
          enabled =
            (Xml.required "Enabled"
               (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Enabled", (Boolean.to_query v.enabled)));
           Some
             (Query.Pair ("PriceClass", (PriceClass.to_query v.price_class)));
           Some (Query.Pair ("Comment", (String.to_query v.comment)));
           Some
             (Query.Pair
                ("TrustedSigners",
                  (TrustedSigners.to_query v.trusted_signers)));
           Some (Query.Pair ("Aliases", (Aliases.to_query v.aliases)));
           Some (Query.Pair ("S3Origin", (S3Origin.to_query v.s3_origin)));
           Some (Query.Pair ("DomainName", (String.to_query v.domain_name)));
           Some
             (Query.Pair
                ("LastModifiedTime",
                  (DateTime.to_query v.last_modified_time)));
           Some (Query.Pair ("Status", (String.to_query v.status)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("enabled", (Boolean.to_json v.enabled));
           Some ("price_class", (PriceClass.to_json v.price_class));
           Some ("comment", (String.to_json v.comment));
           Some
             ("trusted_signers", (TrustedSigners.to_json v.trusted_signers));
           Some ("aliases", (Aliases.to_json v.aliases));
           Some ("s3_origin", (S3Origin.to_json v.s3_origin));
           Some ("domain_name", (String.to_json v.domain_name));
           Some
             ("last_modified_time", (DateTime.to_json v.last_modified_time));
           Some ("status", (String.to_json v.status));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        status =
          (String.of_json (Util.of_option_exn (Json.lookup j "status")));
        last_modified_time =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "last_modified_time")));
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")));
        s3_origin =
          (S3Origin.of_json (Util.of_option_exn (Json.lookup j "s3_origin")));
        aliases =
          (Aliases.of_json (Util.of_option_exn (Json.lookup j "aliases")));
        trusted_signers =
          (TrustedSigners.of_json
             (Util.of_option_exn (Json.lookup j "trusted_signers")));
        comment =
          (String.of_json (Util.of_option_exn (Json.lookup j "comment")));
        price_class =
          (PriceClass.of_json
             (Util.of_option_exn (Json.lookup j "price_class")));
        enabled =
          (Boolean.of_json (Util.of_option_exn (Json.lookup j "enabled")))
      }
  end
module CloudFrontOriginAccessIdentityConfig =
  struct
    type t = {
      caller_reference: String.t ;
      comment: String.t }
    let make ~caller_reference  ~comment  () = { caller_reference; comment }
    let parse xml =
      Some
        {
          caller_reference =
            (Xml.required "CallerReference"
               (Util.option_bind (Xml.member "CallerReference" xml)
                  String.parse));
          comment =
            (Xml.required "Comment"
               (Util.option_bind (Xml.member "Comment" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Comment", (String.to_query v.comment)));
           Some
             (Query.Pair
                ("CallerReference", (String.to_query v.caller_reference)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("comment", (String.to_json v.comment));
           Some ("caller_reference", (String.to_json v.caller_reference))])
    let of_json j =
      {
        caller_reference =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "caller_reference")));
        comment =
          (String.of_json (Util.of_option_exn (Json.lookup j "comment")))
      }
  end
module InvalidationBatch =
  struct
    type t = {
      paths: Paths.t ;
      caller_reference: String.t }
    let make ~paths  ~caller_reference  () = { paths; caller_reference }
    let parse xml =
      Some
        {
          paths =
            (Xml.required "Paths"
               (Util.option_bind (Xml.member "Paths" xml) Paths.parse));
          caller_reference =
            (Xml.required "CallerReference"
               (Util.option_bind (Xml.member "CallerReference" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CallerReference", (String.to_query v.caller_reference)));
           Some (Query.Pair ("Paths", (Paths.to_query v.paths)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("caller_reference", (String.to_json v.caller_reference));
           Some ("paths", (Paths.to_json v.paths))])
    let of_json j =
      {
        paths = (Paths.of_json (Util.of_option_exn (Json.lookup j "paths")));
        caller_reference =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "caller_reference")))
      }
  end
module DistributionSummaryList =
  struct
    type t = DistributionSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DistributionSummary.parse
           (Xml.members "DistributionSummary" xml))
    let to_query v = Query.to_query_list DistributionSummary.to_query v
    let to_json v = `List (List.map DistributionSummary.to_json v)
    let of_json j = Json.to_list DistributionSummary.of_json j
  end
module ActiveTrustedSigners =
  struct
    type t = {
      enabled: Boolean.t ;
      quantity: Integer.t ;
      items: SignerList.t }
    let make ~enabled  ~quantity  ?(items= [])  () =
      { enabled; quantity; items }
    let parse xml =
      Some
        {
          enabled =
            (Xml.required "Enabled"
               (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse));
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml) SignerList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Items.member", (SignerList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)));
           Some (Query.Pair ("Enabled", (Boolean.to_query v.enabled)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (SignerList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity));
           Some ("enabled", (Boolean.to_json v.enabled))])
    let of_json j =
      {
        enabled =
          (Boolean.of_json (Util.of_option_exn (Json.lookup j "enabled")));
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (SignerList.of_json (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module StreamingDistributionConfig =
  struct
    type t =
      {
      caller_reference: String.t ;
      s3_origin: S3Origin.t ;
      aliases: Aliases.t option ;
      comment: String.t ;
      logging: StreamingLoggingConfig.t option ;
      trusted_signers: TrustedSigners.t ;
      price_class: PriceClass.t option ;
      enabled: Boolean.t }
    let make ~caller_reference  ~s3_origin  ?aliases  ~comment  ?logging 
      ~trusted_signers  ?price_class  ~enabled  () =
      {
        caller_reference;
        s3_origin;
        aliases;
        comment;
        logging;
        trusted_signers;
        price_class;
        enabled
      }
    let parse xml =
      Some
        {
          caller_reference =
            (Xml.required "CallerReference"
               (Util.option_bind (Xml.member "CallerReference" xml)
                  String.parse));
          s3_origin =
            (Xml.required "S3Origin"
               (Util.option_bind (Xml.member "S3Origin" xml) S3Origin.parse));
          aliases =
            (Util.option_bind (Xml.member "Aliases" xml) Aliases.parse);
          comment =
            (Xml.required "Comment"
               (Util.option_bind (Xml.member "Comment" xml) String.parse));
          logging =
            (Util.option_bind (Xml.member "Logging" xml)
               StreamingLoggingConfig.parse);
          trusted_signers =
            (Xml.required "TrustedSigners"
               (Util.option_bind (Xml.member "TrustedSigners" xml)
                  TrustedSigners.parse));
          price_class =
            (Util.option_bind (Xml.member "PriceClass" xml) PriceClass.parse);
          enabled =
            (Xml.required "Enabled"
               (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Enabled", (Boolean.to_query v.enabled)));
           Util.option_map v.price_class
             (fun f -> Query.Pair ("PriceClass", (PriceClass.to_query f)));
           Some
             (Query.Pair
                ("TrustedSigners",
                  (TrustedSigners.to_query v.trusted_signers)));
           Util.option_map v.logging
             (fun f ->
                Query.Pair ("Logging", (StreamingLoggingConfig.to_query f)));
           Some (Query.Pair ("Comment", (String.to_query v.comment)));
           Util.option_map v.aliases
             (fun f -> Query.Pair ("Aliases", (Aliases.to_query f)));
           Some (Query.Pair ("S3Origin", (S3Origin.to_query v.s3_origin)));
           Some
             (Query.Pair
                ("CallerReference", (String.to_query v.caller_reference)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("enabled", (Boolean.to_json v.enabled));
           Util.option_map v.price_class
             (fun f -> ("price_class", (PriceClass.to_json f)));
           Some
             ("trusted_signers", (TrustedSigners.to_json v.trusted_signers));
           Util.option_map v.logging
             (fun f -> ("logging", (StreamingLoggingConfig.to_json f)));
           Some ("comment", (String.to_json v.comment));
           Util.option_map v.aliases
             (fun f -> ("aliases", (Aliases.to_json f)));
           Some ("s3_origin", (S3Origin.to_json v.s3_origin));
           Some ("caller_reference", (String.to_json v.caller_reference))])
    let of_json j =
      {
        caller_reference =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "caller_reference")));
        s3_origin =
          (S3Origin.of_json (Util.of_option_exn (Json.lookup j "s3_origin")));
        aliases = (Util.option_map (Json.lookup j "aliases") Aliases.of_json);
        comment =
          (String.of_json (Util.of_option_exn (Json.lookup j "comment")));
        logging =
          (Util.option_map (Json.lookup j "logging")
             StreamingLoggingConfig.of_json);
        trusted_signers =
          (TrustedSigners.of_json
             (Util.of_option_exn (Json.lookup j "trusted_signers")));
        price_class =
          (Util.option_map (Json.lookup j "price_class") PriceClass.of_json);
        enabled =
          (Boolean.of_json (Util.of_option_exn (Json.lookup j "enabled")))
      }
  end
module DistributionConfig =
  struct
    type t =
      {
      caller_reference: String.t ;
      aliases: Aliases.t option ;
      default_root_object: String.t option ;
      origins: Origins.t ;
      default_cache_behavior: DefaultCacheBehavior.t ;
      cache_behaviors: CacheBehaviors.t option ;
      custom_error_responses: CustomErrorResponses.t option ;
      comment: String.t ;
      logging: LoggingConfig.t option ;
      price_class: PriceClass.t option ;
      enabled: Boolean.t ;
      viewer_certificate: ViewerCertificate.t option ;
      restrictions: Restrictions.t option }
    let make ~caller_reference  ?aliases  ?default_root_object  ~origins 
      ~default_cache_behavior  ?cache_behaviors  ?custom_error_responses 
      ~comment  ?logging  ?price_class  ~enabled  ?viewer_certificate 
      ?restrictions  () =
      {
        caller_reference;
        aliases;
        default_root_object;
        origins;
        default_cache_behavior;
        cache_behaviors;
        custom_error_responses;
        comment;
        logging;
        price_class;
        enabled;
        viewer_certificate;
        restrictions
      }
    let parse xml =
      Some
        {
          caller_reference =
            (Xml.required "CallerReference"
               (Util.option_bind (Xml.member "CallerReference" xml)
                  String.parse));
          aliases =
            (Util.option_bind (Xml.member "Aliases" xml) Aliases.parse);
          default_root_object =
            (Util.option_bind (Xml.member "DefaultRootObject" xml)
               String.parse);
          origins =
            (Xml.required "Origins"
               (Util.option_bind (Xml.member "Origins" xml) Origins.parse));
          default_cache_behavior =
            (Xml.required "DefaultCacheBehavior"
               (Util.option_bind (Xml.member "DefaultCacheBehavior" xml)
                  DefaultCacheBehavior.parse));
          cache_behaviors =
            (Util.option_bind (Xml.member "CacheBehaviors" xml)
               CacheBehaviors.parse);
          custom_error_responses =
            (Util.option_bind (Xml.member "CustomErrorResponses" xml)
               CustomErrorResponses.parse);
          comment =
            (Xml.required "Comment"
               (Util.option_bind (Xml.member "Comment" xml) String.parse));
          logging =
            (Util.option_bind (Xml.member "Logging" xml) LoggingConfig.parse);
          price_class =
            (Util.option_bind (Xml.member "PriceClass" xml) PriceClass.parse);
          enabled =
            (Xml.required "Enabled"
               (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse));
          viewer_certificate =
            (Util.option_bind (Xml.member "ViewerCertificate" xml)
               ViewerCertificate.parse);
          restrictions =
            (Util.option_bind (Xml.member "Restrictions" xml)
               Restrictions.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.restrictions
              (fun f ->
                 Query.Pair ("Restrictions", (Restrictions.to_query f)));
           Util.option_map v.viewer_certificate
             (fun f ->
                Query.Pair
                  ("ViewerCertificate", (ViewerCertificate.to_query f)));
           Some (Query.Pair ("Enabled", (Boolean.to_query v.enabled)));
           Util.option_map v.price_class
             (fun f -> Query.Pair ("PriceClass", (PriceClass.to_query f)));
           Util.option_map v.logging
             (fun f -> Query.Pair ("Logging", (LoggingConfig.to_query f)));
           Some (Query.Pair ("Comment", (String.to_query v.comment)));
           Util.option_map v.custom_error_responses
             (fun f ->
                Query.Pair
                  ("CustomErrorResponses", (CustomErrorResponses.to_query f)));
           Util.option_map v.cache_behaviors
             (fun f ->
                Query.Pair ("CacheBehaviors", (CacheBehaviors.to_query f)));
           Some
             (Query.Pair
                ("DefaultCacheBehavior",
                  (DefaultCacheBehavior.to_query v.default_cache_behavior)));
           Some (Query.Pair ("Origins", (Origins.to_query v.origins)));
           Util.option_map v.default_root_object
             (fun f -> Query.Pair ("DefaultRootObject", (String.to_query f)));
           Util.option_map v.aliases
             (fun f -> Query.Pair ("Aliases", (Aliases.to_query f)));
           Some
             (Query.Pair
                ("CallerReference", (String.to_query v.caller_reference)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.restrictions
              (fun f -> ("restrictions", (Restrictions.to_json f)));
           Util.option_map v.viewer_certificate
             (fun f -> ("viewer_certificate", (ViewerCertificate.to_json f)));
           Some ("enabled", (Boolean.to_json v.enabled));
           Util.option_map v.price_class
             (fun f -> ("price_class", (PriceClass.to_json f)));
           Util.option_map v.logging
             (fun f -> ("logging", (LoggingConfig.to_json f)));
           Some ("comment", (String.to_json v.comment));
           Util.option_map v.custom_error_responses
             (fun f ->
                ("custom_error_responses", (CustomErrorResponses.to_json f)));
           Util.option_map v.cache_behaviors
             (fun f -> ("cache_behaviors", (CacheBehaviors.to_json f)));
           Some
             ("default_cache_behavior",
               (DefaultCacheBehavior.to_json v.default_cache_behavior));
           Some ("origins", (Origins.to_json v.origins));
           Util.option_map v.default_root_object
             (fun f -> ("default_root_object", (String.to_json f)));
           Util.option_map v.aliases
             (fun f -> ("aliases", (Aliases.to_json f)));
           Some ("caller_reference", (String.to_json v.caller_reference))])
    let of_json j =
      {
        caller_reference =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "caller_reference")));
        aliases = (Util.option_map (Json.lookup j "aliases") Aliases.of_json);
        default_root_object =
          (Util.option_map (Json.lookup j "default_root_object")
             String.of_json);
        origins =
          (Origins.of_json (Util.of_option_exn (Json.lookup j "origins")));
        default_cache_behavior =
          (DefaultCacheBehavior.of_json
             (Util.of_option_exn (Json.lookup j "default_cache_behavior")));
        cache_behaviors =
          (Util.option_map (Json.lookup j "cache_behaviors")
             CacheBehaviors.of_json);
        custom_error_responses =
          (Util.option_map (Json.lookup j "custom_error_responses")
             CustomErrorResponses.of_json);
        comment =
          (String.of_json (Util.of_option_exn (Json.lookup j "comment")));
        logging =
          (Util.option_map (Json.lookup j "logging") LoggingConfig.of_json);
        price_class =
          (Util.option_map (Json.lookup j "price_class") PriceClass.of_json);
        enabled =
          (Boolean.of_json (Util.of_option_exn (Json.lookup j "enabled")));
        viewer_certificate =
          (Util.option_map (Json.lookup j "viewer_certificate")
             ViewerCertificate.of_json);
        restrictions =
          (Util.option_map (Json.lookup j "restrictions")
             Restrictions.of_json)
      }
  end
module CloudFrontOriginAccessIdentitySummaryList =
  struct
    type t = CloudFrontOriginAccessIdentitySummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CloudFrontOriginAccessIdentitySummary.parse
           (Xml.members "CloudFrontOriginAccessIdentitySummary" xml))
    let to_query v =
      Query.to_query_list CloudFrontOriginAccessIdentitySummary.to_query v
    let to_json v =
      `List (List.map CloudFrontOriginAccessIdentitySummary.to_json v)
    let of_json j =
      Json.to_list CloudFrontOriginAccessIdentitySummary.of_json j
  end
module InvalidationSummaryList =
  struct
    type t = InvalidationSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map InvalidationSummary.parse
           (Xml.members "InvalidationSummary" xml))
    let to_query v = Query.to_query_list InvalidationSummary.to_query v
    let to_json v = `List (List.map InvalidationSummary.to_json v)
    let of_json j = Json.to_list InvalidationSummary.of_json j
  end
module StreamingDistributionSummaryList =
  struct
    type t = StreamingDistributionSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map StreamingDistributionSummary.parse
           (Xml.members "StreamingDistributionSummary" xml))
    let to_query v =
      Query.to_query_list StreamingDistributionSummary.to_query v
    let to_json v = `List (List.map StreamingDistributionSummary.to_json v)
    let of_json j = Json.to_list StreamingDistributionSummary.of_json j
  end
module CloudFrontOriginAccessIdentity =
  struct
    type t =
      {
      id: String.t ;
      s3_canonical_user_id: String.t ;
      cloud_front_origin_access_identity_config:
        CloudFrontOriginAccessIdentityConfig.t option }
    let make ~id  ~s3_canonical_user_id 
      ?cloud_front_origin_access_identity_config  () =
      { id; s3_canonical_user_id; cloud_front_origin_access_identity_config }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          s3_canonical_user_id =
            (Xml.required "S3CanonicalUserId"
               (Util.option_bind (Xml.member "S3CanonicalUserId" xml)
                  String.parse));
          cloud_front_origin_access_identity_config =
            (Util.option_bind
               (Xml.member "CloudFrontOriginAccessIdentityConfig" xml)
               CloudFrontOriginAccessIdentityConfig.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cloud_front_origin_access_identity_config
              (fun f ->
                 Query.Pair
                   ("CloudFrontOriginAccessIdentityConfig",
                     (CloudFrontOriginAccessIdentityConfig.to_query f)));
           Some
             (Query.Pair
                ("S3CanonicalUserId",
                  (String.to_query v.s3_canonical_user_id)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cloud_front_origin_access_identity_config
              (fun f ->
                 ("cloud_front_origin_access_identity_config",
                   (CloudFrontOriginAccessIdentityConfig.to_json f)));
           Some
             ("s3_canonical_user_id",
               (String.to_json v.s3_canonical_user_id));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        s3_canonical_user_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "s3_canonical_user_id")));
        cloud_front_origin_access_identity_config =
          (Util.option_map
             (Json.lookup j "cloud_front_origin_access_identity_config")
             CloudFrontOriginAccessIdentityConfig.of_json)
      }
  end
module Invalidation =
  struct
    type t =
      {
      id: String.t ;
      status: String.t ;
      create_time: DateTime.t ;
      invalidation_batch: InvalidationBatch.t }
    let make ~id  ~status  ~create_time  ~invalidation_batch  () =
      { id; status; create_time; invalidation_batch }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          status =
            (Xml.required "Status"
               (Util.option_bind (Xml.member "Status" xml) String.parse));
          create_time =
            (Xml.required "CreateTime"
               (Util.option_bind (Xml.member "CreateTime" xml) DateTime.parse));
          invalidation_batch =
            (Xml.required "InvalidationBatch"
               (Util.option_bind (Xml.member "InvalidationBatch" xml)
                  InvalidationBatch.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("InvalidationBatch",
                   (InvalidationBatch.to_query v.invalidation_batch)));
           Some
             (Query.Pair ("CreateTime", (DateTime.to_query v.create_time)));
           Some (Query.Pair ("Status", (String.to_query v.status)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("invalidation_batch",
                (InvalidationBatch.to_json v.invalidation_batch));
           Some ("create_time", (DateTime.to_json v.create_time));
           Some ("status", (String.to_json v.status));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        status =
          (String.of_json (Util.of_option_exn (Json.lookup j "status")));
        create_time =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "create_time")));
        invalidation_batch =
          (InvalidationBatch.of_json
             (Util.of_option_exn (Json.lookup j "invalidation_batch")))
      }
  end
module DistributionList =
  struct
    type t =
      {
      marker: String.t ;
      next_marker: String.t option ;
      max_items: Integer.t ;
      is_truncated: Boolean.t ;
      quantity: Integer.t ;
      items: DistributionSummaryList.t }
    let make ~marker  ?next_marker  ~max_items  ~is_truncated  ~quantity 
      ?(items= [])  () =
      { marker; next_marker; max_items; is_truncated; quantity; items }
    let parse xml =
      Some
        {
          marker =
            (Xml.required "Marker"
               (Util.option_bind (Xml.member "Marker" xml) String.parse));
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) Integer.parse));
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml)
                  DistributionSummaryList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Items.member", (DistributionSummaryList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Some (Query.Pair ("MaxItems", (Integer.to_query v.max_items)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)));
           Some (Query.Pair ("Marker", (String.to_query v.marker)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (DistributionSummaryList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Some ("max_items", (Integer.to_json v.max_items));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)));
           Some ("marker", (String.to_json v.marker))])
    let of_json j =
      {
        marker =
          (String.of_json (Util.of_option_exn (Json.lookup j "marker")));
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        max_items =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "max_items")));
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (DistributionSummaryList.of_json
             (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module StreamingDistribution =
  struct
    type t =
      {
      id: String.t ;
      status: String.t ;
      last_modified_time: DateTime.t option ;
      domain_name: String.t ;
      active_trusted_signers: ActiveTrustedSigners.t ;
      streaming_distribution_config: StreamingDistributionConfig.t }
    let make ~id  ~status  ?last_modified_time  ~domain_name 
      ~active_trusted_signers  ~streaming_distribution_config  () =
      {
        id;
        status;
        last_modified_time;
        domain_name;
        active_trusted_signers;
        streaming_distribution_config
      }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          status =
            (Xml.required "Status"
               (Util.option_bind (Xml.member "Status" xml) String.parse));
          last_modified_time =
            (Util.option_bind (Xml.member "LastModifiedTime" xml)
               DateTime.parse);
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse));
          active_trusted_signers =
            (Xml.required "ActiveTrustedSigners"
               (Util.option_bind (Xml.member "ActiveTrustedSigners" xml)
                  ActiveTrustedSigners.parse));
          streaming_distribution_config =
            (Xml.required "StreamingDistributionConfig"
               (Util.option_bind
                  (Xml.member "StreamingDistributionConfig" xml)
                  StreamingDistributionConfig.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StreamingDistributionConfig",
                   (StreamingDistributionConfig.to_query
                      v.streaming_distribution_config)));
           Some
             (Query.Pair
                ("ActiveTrustedSigners",
                  (ActiveTrustedSigners.to_query v.active_trusted_signers)));
           Some (Query.Pair ("DomainName", (String.to_query v.domain_name)));
           Util.option_map v.last_modified_time
             (fun f -> Query.Pair ("LastModifiedTime", (DateTime.to_query f)));
           Some (Query.Pair ("Status", (String.to_query v.status)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("streaming_distribution_config",
                (StreamingDistributionConfig.to_json
                   v.streaming_distribution_config));
           Some
             ("active_trusted_signers",
               (ActiveTrustedSigners.to_json v.active_trusted_signers));
           Some ("domain_name", (String.to_json v.domain_name));
           Util.option_map v.last_modified_time
             (fun f -> ("last_modified_time", (DateTime.to_json f)));
           Some ("status", (String.to_json v.status));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        status =
          (String.of_json (Util.of_option_exn (Json.lookup j "status")));
        last_modified_time =
          (Util.option_map (Json.lookup j "last_modified_time")
             DateTime.of_json);
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")));
        active_trusted_signers =
          (ActiveTrustedSigners.of_json
             (Util.of_option_exn (Json.lookup j "active_trusted_signers")));
        streaming_distribution_config =
          (StreamingDistributionConfig.of_json
             (Util.of_option_exn
                (Json.lookup j "streaming_distribution_config")))
      }
  end
module Distribution =
  struct
    type t =
      {
      id: String.t ;
      status: String.t ;
      last_modified_time: DateTime.t ;
      in_progress_invalidation_batches: Integer.t ;
      domain_name: String.t ;
      active_trusted_signers: ActiveTrustedSigners.t ;
      distribution_config: DistributionConfig.t }
    let make ~id  ~status  ~last_modified_time 
      ~in_progress_invalidation_batches  ~domain_name 
      ~active_trusted_signers  ~distribution_config  () =
      {
        id;
        status;
        last_modified_time;
        in_progress_invalidation_batches;
        domain_name;
        active_trusted_signers;
        distribution_config
      }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          status =
            (Xml.required "Status"
               (Util.option_bind (Xml.member "Status" xml) String.parse));
          last_modified_time =
            (Xml.required "LastModifiedTime"
               (Util.option_bind (Xml.member "LastModifiedTime" xml)
                  DateTime.parse));
          in_progress_invalidation_batches =
            (Xml.required "InProgressInvalidationBatches"
               (Util.option_bind
                  (Xml.member "InProgressInvalidationBatches" xml)
                  Integer.parse));
          domain_name =
            (Xml.required "DomainName"
               (Util.option_bind (Xml.member "DomainName" xml) String.parse));
          active_trusted_signers =
            (Xml.required "ActiveTrustedSigners"
               (Util.option_bind (Xml.member "ActiveTrustedSigners" xml)
                  ActiveTrustedSigners.parse));
          distribution_config =
            (Xml.required "DistributionConfig"
               (Util.option_bind (Xml.member "DistributionConfig" xml)
                  DistributionConfig.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DistributionConfig",
                   (DistributionConfig.to_query v.distribution_config)));
           Some
             (Query.Pair
                ("ActiveTrustedSigners",
                  (ActiveTrustedSigners.to_query v.active_trusted_signers)));
           Some (Query.Pair ("DomainName", (String.to_query v.domain_name)));
           Some
             (Query.Pair
                ("InProgressInvalidationBatches",
                  (Integer.to_query v.in_progress_invalidation_batches)));
           Some
             (Query.Pair
                ("LastModifiedTime",
                  (DateTime.to_query v.last_modified_time)));
           Some (Query.Pair ("Status", (String.to_query v.status)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("distribution_config",
                (DistributionConfig.to_json v.distribution_config));
           Some
             ("active_trusted_signers",
               (ActiveTrustedSigners.to_json v.active_trusted_signers));
           Some ("domain_name", (String.to_json v.domain_name));
           Some
             ("in_progress_invalidation_batches",
               (Integer.to_json v.in_progress_invalidation_batches));
           Some
             ("last_modified_time", (DateTime.to_json v.last_modified_time));
           Some ("status", (String.to_json v.status));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        status =
          (String.of_json (Util.of_option_exn (Json.lookup j "status")));
        last_modified_time =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "last_modified_time")));
        in_progress_invalidation_batches =
          (Integer.of_json
             (Util.of_option_exn
                (Json.lookup j "in_progress_invalidation_batches")));
        domain_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "domain_name")));
        active_trusted_signers =
          (ActiveTrustedSigners.of_json
             (Util.of_option_exn (Json.lookup j "active_trusted_signers")));
        distribution_config =
          (DistributionConfig.of_json
             (Util.of_option_exn (Json.lookup j "distribution_config")))
      }
  end
module CloudFrontOriginAccessIdentityList =
  struct
    type t =
      {
      marker: String.t ;
      next_marker: String.t option ;
      max_items: Integer.t ;
      is_truncated: Boolean.t ;
      quantity: Integer.t ;
      items: CloudFrontOriginAccessIdentitySummaryList.t }
    let make ~marker  ?next_marker  ~max_items  ~is_truncated  ~quantity 
      ?(items= [])  () =
      { marker; next_marker; max_items; is_truncated; quantity; items }
    let parse xml =
      Some
        {
          marker =
            (Xml.required "Marker"
               (Util.option_bind (Xml.member "Marker" xml) String.parse));
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) Integer.parse));
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml)
                  CloudFrontOriginAccessIdentitySummaryList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Items.member",
                   (CloudFrontOriginAccessIdentitySummaryList.to_query
                      v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Some (Query.Pair ("MaxItems", (Integer.to_query v.max_items)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)));
           Some (Query.Pair ("Marker", (String.to_query v.marker)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("items",
                (CloudFrontOriginAccessIdentitySummaryList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Some ("max_items", (Integer.to_json v.max_items));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)));
           Some ("marker", (String.to_json v.marker))])
    let of_json j =
      {
        marker =
          (String.of_json (Util.of_option_exn (Json.lookup j "marker")));
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        max_items =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "max_items")));
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (CloudFrontOriginAccessIdentitySummaryList.of_json
             (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module InvalidationList =
  struct
    type t =
      {
      marker: String.t ;
      next_marker: String.t option ;
      max_items: Integer.t ;
      is_truncated: Boolean.t ;
      quantity: Integer.t ;
      items: InvalidationSummaryList.t }
    let make ~marker  ?next_marker  ~max_items  ~is_truncated  ~quantity 
      ?(items= [])  () =
      { marker; next_marker; max_items; is_truncated; quantity; items }
    let parse xml =
      Some
        {
          marker =
            (Xml.required "Marker"
               (Util.option_bind (Xml.member "Marker" xml) String.parse));
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) Integer.parse));
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml)
                  InvalidationSummaryList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Items.member", (InvalidationSummaryList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Some (Query.Pair ("MaxItems", (Integer.to_query v.max_items)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)));
           Some (Query.Pair ("Marker", (String.to_query v.marker)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("items", (InvalidationSummaryList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Some ("max_items", (Integer.to_json v.max_items));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)));
           Some ("marker", (String.to_json v.marker))])
    let of_json j =
      {
        marker =
          (String.of_json (Util.of_option_exn (Json.lookup j "marker")));
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        max_items =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "max_items")));
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (InvalidationSummaryList.of_json
             (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module StreamingDistributionList =
  struct
    type t =
      {
      marker: String.t ;
      next_marker: String.t option ;
      max_items: Integer.t ;
      is_truncated: Boolean.t ;
      quantity: Integer.t ;
      items: StreamingDistributionSummaryList.t }
    let make ~marker  ?next_marker  ~max_items  ~is_truncated  ~quantity 
      ?(items= [])  () =
      { marker; next_marker; max_items; is_truncated; quantity; items }
    let parse xml =
      Some
        {
          marker =
            (Xml.required "Marker"
               (Util.option_bind (Xml.member "Marker" xml) String.parse));
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) Integer.parse));
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          quantity =
            (Xml.required "Quantity"
               (Util.option_bind (Xml.member "Quantity" xml) Integer.parse));
          items =
            (Util.of_option []
               (Util.option_bind (Xml.member "Items" xml)
                  StreamingDistributionSummaryList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Items.member",
                   (StreamingDistributionSummaryList.to_query v.items)));
           Some (Query.Pair ("Quantity", (Integer.to_query v.quantity)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Some (Query.Pair ("MaxItems", (Integer.to_query v.max_items)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)));
           Some (Query.Pair ("Marker", (String.to_query v.marker)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("items", (StreamingDistributionSummaryList.to_json v.items));
           Some ("quantity", (Integer.to_json v.quantity));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Some ("max_items", (Integer.to_json v.max_items));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)));
           Some ("marker", (String.to_json v.marker))])
    let of_json j =
      {
        marker =
          (String.of_json (Util.of_option_exn (Json.lookup j "marker")));
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        max_items =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "max_items")));
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        quantity =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "quantity")));
        items =
          (StreamingDistributionSummaryList.of_json
             (Util.of_option_exn (Json.lookup j "items")))
      }
  end
module GetCloudFrontOriginAccessIdentityRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module ListInvalidationsRequest =
  struct
    type t =
      {
      distribution_id: String.t ;
      marker: String.t option ;
      max_items: String.t option }
    let make ~distribution_id  ?marker  ?max_items  () =
      { distribution_id; marker; max_items }
    let parse xml =
      Some
        {
          distribution_id =
            (Xml.required "DistributionId"
               (Util.option_bind (Xml.member "DistributionId" xml)
                  String.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          max_items =
            (Util.option_bind (Xml.member "MaxItems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("MaxItems", (String.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("DistributionId", (String.to_query v.distribution_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Some ("distribution_id", (String.to_json v.distribution_id))])
    let of_json j =
      {
        distribution_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "distribution_id")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module GetCloudFrontOriginAccessIdentityResult =
  struct
    type t =
      {
      cloud_front_origin_access_identity:
        CloudFrontOriginAccessIdentity.t option ;
      e_tag: String.t option }
    let make ?cloud_front_origin_access_identity  ?e_tag  () =
      { cloud_front_origin_access_identity; e_tag }
    let parse xml =
      Some
        {
          cloud_front_origin_access_identity =
            (Util.option_bind
               (Xml.member "CloudFrontOriginAccessIdentity" xml)
               CloudFrontOriginAccessIdentity.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_tag
              (fun f -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.cloud_front_origin_access_identity
             (fun f ->
                Query.Pair
                  ("CloudFrontOriginAccessIdentity",
                    (CloudFrontOriginAccessIdentity.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_tag (fun f -> ("e_tag", (String.to_json f)));
           Util.option_map v.cloud_front_origin_access_identity
             (fun f ->
                ("cloud_front_origin_access_identity",
                  (CloudFrontOriginAccessIdentity.to_json f)))])
    let of_json j =
      {
        cloud_front_origin_access_identity =
          (Util.option_map
             (Json.lookup j "cloud_front_origin_access_identity")
             CloudFrontOriginAccessIdentity.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json)
      }
  end
module GetInvalidationResult =
  struct
    type t = {
      invalidation: Invalidation.t option }
    let make ?invalidation  () = { invalidation }
    let parse xml =
      Some
        {
          invalidation =
            (Util.option_bind (Xml.member "Invalidation" xml)
               Invalidation.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.invalidation
              (fun f ->
                 Query.Pair ("Invalidation", (Invalidation.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.invalidation
              (fun f -> ("invalidation", (Invalidation.to_json f)))])
    let of_json j =
      {
        invalidation =
          (Util.option_map (Json.lookup j "invalidation")
             Invalidation.of_json)
      }
  end
module TooManyInvalidationsInProgress =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CNAMEAlreadyExists =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidHeadersForS3Origin =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module NoSuchInvalidation =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CloudFrontOriginAccessIdentityInUse =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidMinimumProtocolVersion =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidArgument =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TooManyCloudFrontOriginAccessIdentities =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListDistributionsResult =
  struct
    type t = {
      distribution_list: DistributionList.t }
    let make ~distribution_list  () = { distribution_list }
    let parse xml =
      Some
        {
          distribution_list =
            (Xml.required "DistributionList"
               (Util.option_bind (Xml.member "DistributionList" xml)
                  DistributionList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DistributionList",
                   (DistributionList.to_query v.distribution_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("distribution_list",
                (DistributionList.to_json v.distribution_list))])
    let of_json j =
      {
        distribution_list =
          (DistributionList.of_json
             (Util.of_option_exn (Json.lookup j "distribution_list")))
      }
  end
module TooManyOrigins =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module UpdateStreamingDistributionResult =
  struct
    type t =
      {
      streaming_distribution: StreamingDistribution.t option ;
      e_tag: String.t option }
    let make ?streaming_distribution  ?e_tag  () =
      { streaming_distribution; e_tag }
    let parse xml =
      Some
        {
          streaming_distribution =
            (Util.option_bind (Xml.member "StreamingDistribution" xml)
               StreamingDistribution.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_tag
              (fun f -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.streaming_distribution
             (fun f ->
                Query.Pair
                  ("StreamingDistribution",
                    (StreamingDistribution.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_tag (fun f -> ("e_tag", (String.to_json f)));
           Util.option_map v.streaming_distribution
             (fun f ->
                ("streaming_distribution", (StreamingDistribution.to_json f)))])
    let of_json j =
      {
        streaming_distribution =
          (Util.option_map (Json.lookup j "streaming_distribution")
             StreamingDistribution.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json)
      }
  end
module CreateCloudFrontOriginAccessIdentityResult =
  struct
    type t =
      {
      cloud_front_origin_access_identity:
        CloudFrontOriginAccessIdentity.t option ;
      location: String.t option ;
      e_tag: String.t option }
    let make ?cloud_front_origin_access_identity  ?location  ?e_tag  () =
      { cloud_front_origin_access_identity; location; e_tag }
    let parse xml =
      Some
        {
          cloud_front_origin_access_identity =
            (Util.option_bind
               (Xml.member "CloudFrontOriginAccessIdentity" xml)
               CloudFrontOriginAccessIdentity.parse);
          location =
            (Util.option_bind (Xml.member "Location" xml) String.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_tag
              (fun f -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.location
             (fun f -> Query.Pair ("Location", (String.to_query f)));
           Util.option_map v.cloud_front_origin_access_identity
             (fun f ->
                Query.Pair
                  ("CloudFrontOriginAccessIdentity",
                    (CloudFrontOriginAccessIdentity.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_tag (fun f -> ("e_tag", (String.to_json f)));
           Util.option_map v.location
             (fun f -> ("location", (String.to_json f)));
           Util.option_map v.cloud_front_origin_access_identity
             (fun f ->
                ("cloud_front_origin_access_identity",
                  (CloudFrontOriginAccessIdentity.to_json f)))])
    let of_json j =
      {
        cloud_front_origin_access_identity =
          (Util.option_map
             (Json.lookup j "cloud_front_origin_access_identity")
             CloudFrontOriginAccessIdentity.of_json);
        location =
          (Util.option_map (Json.lookup j "location") String.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json)
      }
  end
module GetStreamingDistributionConfigRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module NoSuchDistribution =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListStreamingDistributionsRequest =
  struct
    type t = {
      marker: String.t option ;
      max_items: String.t option }
    let make ?marker  ?max_items  () = { marker; max_items }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          max_items =
            (Util.option_bind (Xml.member "MaxItems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("MaxItems", (String.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module NoSuchCloudFrontOriginAccessIdentity =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module StreamingDistributionNotDisabled =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TooManyDistributions =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CreateDistributionResult =
  struct
    type t =
      {
      distribution: Distribution.t option ;
      location: String.t option ;
      e_tag: String.t option }
    let make ?distribution  ?location  ?e_tag  () =
      { distribution; location; e_tag }
    let parse xml =
      Some
        {
          distribution =
            (Util.option_bind (Xml.member "Distribution" xml)
               Distribution.parse);
          location =
            (Util.option_bind (Xml.member "Location" xml) String.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_tag
              (fun f -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.location
             (fun f -> Query.Pair ("Location", (String.to_query f)));
           Util.option_map v.distribution
             (fun f -> Query.Pair ("Distribution", (Distribution.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_tag (fun f -> ("e_tag", (String.to_json f)));
           Util.option_map v.location
             (fun f -> ("location", (String.to_json f)));
           Util.option_map v.distribution
             (fun f -> ("distribution", (Distribution.to_json f)))])
    let of_json j =
      {
        distribution =
          (Util.option_map (Json.lookup j "distribution")
             Distribution.of_json);
        location =
          (Util.option_map (Json.lookup j "location") String.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json)
      }
  end
module BatchTooLarge =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidGeoRestrictionParameter =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidErrorCode =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListDistributionsRequest =
  struct
    type t = {
      marker: String.t option ;
      max_items: String.t option }
    let make ?marker  ?max_items  () = { marker; max_items }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          max_items =
            (Util.option_bind (Xml.member "MaxItems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("MaxItems", (String.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module TooManyHeadersInForwardedValues =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InconsistentQuantities =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetCloudFrontOriginAccessIdentityConfigResult =
  struct
    type t =
      {
      cloud_front_origin_access_identity_config:
        CloudFrontOriginAccessIdentityConfig.t option ;
      e_tag: String.t option }
    let make ?cloud_front_origin_access_identity_config  ?e_tag  () =
      { cloud_front_origin_access_identity_config; e_tag }
    let parse xml =
      Some
        {
          cloud_front_origin_access_identity_config =
            (Util.option_bind
               (Xml.member "CloudFrontOriginAccessIdentityConfig" xml)
               CloudFrontOriginAccessIdentityConfig.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_tag
              (fun f -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.cloud_front_origin_access_identity_config
             (fun f ->
                Query.Pair
                  ("CloudFrontOriginAccessIdentityConfig",
                    (CloudFrontOriginAccessIdentityConfig.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_tag (fun f -> ("e_tag", (String.to_json f)));
           Util.option_map v.cloud_front_origin_access_identity_config
             (fun f ->
                ("cloud_front_origin_access_identity_config",
                  (CloudFrontOriginAccessIdentityConfig.to_json f)))])
    let of_json j =
      {
        cloud_front_origin_access_identity_config =
          (Util.option_map
             (Json.lookup j "cloud_front_origin_access_identity_config")
             CloudFrontOriginAccessIdentityConfig.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json)
      }
  end
module CreateInvalidationResult =
  struct
    type t =
      {
      location: String.t option ;
      invalidation: Invalidation.t option }
    let make ?location  ?invalidation  () = { location; invalidation }
    let parse xml =
      Some
        {
          location =
            (Util.option_bind (Xml.member "Location" xml) String.parse);
          invalidation =
            (Util.option_bind (Xml.member "Invalidation" xml)
               Invalidation.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.invalidation
              (fun f ->
                 Query.Pair ("Invalidation", (Invalidation.to_query f)));
           Util.option_map v.location
             (fun f -> Query.Pair ("Location", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.invalidation
              (fun f -> ("invalidation", (Invalidation.to_json f)));
           Util.option_map v.location
             (fun f -> ("location", (String.to_json f)))])
    let of_json j =
      {
        location =
          (Util.option_map (Json.lookup j "location") String.of_json);
        invalidation =
          (Util.option_map (Json.lookup j "invalidation")
             Invalidation.of_json)
      }
  end
module InvalidProtocolSettings =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DeleteStreamingDistributionRequest =
  struct
    type t = {
      id: String.t ;
      if_match: String.t option }
    let make ~id  ?if_match  () = { id; if_match }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          if_match =
            (Util.option_bind (Xml.member "If-Match" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.if_match
              (fun f -> Query.Pair ("If-Match", (String.to_query f)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.if_match
              (fun f -> ("if_match", (String.to_json f)));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        if_match =
          (Util.option_map (Json.lookup j "if_match") String.of_json)
      }
  end
module InvalidIfMatchVersion =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module UpdateCloudFrontOriginAccessIdentityResult =
  struct
    type t =
      {
      cloud_front_origin_access_identity:
        CloudFrontOriginAccessIdentity.t option ;
      e_tag: String.t option }
    let make ?cloud_front_origin_access_identity  ?e_tag  () =
      { cloud_front_origin_access_identity; e_tag }
    let parse xml =
      Some
        {
          cloud_front_origin_access_identity =
            (Util.option_bind
               (Xml.member "CloudFrontOriginAccessIdentity" xml)
               CloudFrontOriginAccessIdentity.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_tag
              (fun f -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.cloud_front_origin_access_identity
             (fun f ->
                Query.Pair
                  ("CloudFrontOriginAccessIdentity",
                    (CloudFrontOriginAccessIdentity.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_tag (fun f -> ("e_tag", (String.to_json f)));
           Util.option_map v.cloud_front_origin_access_identity
             (fun f ->
                ("cloud_front_origin_access_identity",
                  (CloudFrontOriginAccessIdentity.to_json f)))])
    let of_json j =
      {
        cloud_front_origin_access_identity =
          (Util.option_map
             (Json.lookup j "cloud_front_origin_access_identity")
             CloudFrontOriginAccessIdentity.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json)
      }
  end
module ListCloudFrontOriginAccessIdentitiesResult =
  struct
    type t =
      {
      cloud_front_origin_access_identity_list:
        CloudFrontOriginAccessIdentityList.t }
    let make ~cloud_front_origin_access_identity_list  () =
      { cloud_front_origin_access_identity_list }
    let parse xml =
      Some
        {
          cloud_front_origin_access_identity_list =
            (Xml.required "CloudFrontOriginAccessIdentityList"
               (Util.option_bind
                  (Xml.member "CloudFrontOriginAccessIdentityList" xml)
                  CloudFrontOriginAccessIdentityList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CloudFrontOriginAccessIdentityList",
                   (CloudFrontOriginAccessIdentityList.to_query
                      v.cloud_front_origin_access_identity_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cloud_front_origin_access_identity_list",
                (CloudFrontOriginAccessIdentityList.to_json
                   v.cloud_front_origin_access_identity_list))])
    let of_json j =
      {
        cloud_front_origin_access_identity_list =
          (CloudFrontOriginAccessIdentityList.of_json
             (Util.of_option_exn
                (Json.lookup j "cloud_front_origin_access_identity_list")))
      }
  end
module CreateDistributionRequest =
  struct
    type t = {
      distribution_config: DistributionConfig.t }
    let make ~distribution_config  () = { distribution_config }
    let parse xml =
      Some
        {
          distribution_config =
            (Xml.required "DistributionConfig"
               (Util.option_bind (Xml.member "DistributionConfig" xml)
                  DistributionConfig.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DistributionConfig",
                   (DistributionConfig.to_query v.distribution_config)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("distribution_config",
                (DistributionConfig.to_json v.distribution_config))])
    let of_json j =
      {
        distribution_config =
          (DistributionConfig.of_json
             (Util.of_option_exn (Json.lookup j "distribution_config")))
      }
  end
module DistributionNotDisabled =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidViewerCertificate =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TooManyCacheBehaviors =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module PreconditionFailed =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetDistributionConfigRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module GetInvalidationRequest =
  struct
    type t = {
      distribution_id: String.t ;
      id: String.t }
    let make ~distribution_id  ~id  () = { distribution_id; id }
    let parse xml =
      Some
        {
          distribution_id =
            (Xml.required "DistributionId"
               (Util.option_bind (Xml.member "DistributionId" xml)
                  String.parse));
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)));
           Some
             (Query.Pair
                ("DistributionId", (String.to_query v.distribution_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("id", (String.to_json v.id));
           Some ("distribution_id", (String.to_json v.distribution_id))])
    let of_json j =
      {
        distribution_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "distribution_id")));
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")))
      }
  end
module GetDistributionRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module TooManyStreamingDistributionCNAMEs =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidRelativePath =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CreateCloudFrontOriginAccessIdentityRequest =
  struct
    type t =
      {
      cloud_front_origin_access_identity_config:
        CloudFrontOriginAccessIdentityConfig.t }
    let make ~cloud_front_origin_access_identity_config  () =
      { cloud_front_origin_access_identity_config }
    let parse xml =
      Some
        {
          cloud_front_origin_access_identity_config =
            (Xml.required "CloudFrontOriginAccessIdentityConfig"
               (Util.option_bind
                  (Xml.member "CloudFrontOriginAccessIdentityConfig" xml)
                  CloudFrontOriginAccessIdentityConfig.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CloudFrontOriginAccessIdentityConfig",
                   (CloudFrontOriginAccessIdentityConfig.to_query
                      v.cloud_front_origin_access_identity_config)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cloud_front_origin_access_identity_config",
                (CloudFrontOriginAccessIdentityConfig.to_json
                   v.cloud_front_origin_access_identity_config))])
    let of_json j =
      {
        cloud_front_origin_access_identity_config =
          (CloudFrontOriginAccessIdentityConfig.of_json
             (Util.of_option_exn
                (Json.lookup j "cloud_front_origin_access_identity_config")))
      }
  end
module UpdateDistributionResult =
  struct
    type t = {
      distribution: Distribution.t option ;
      e_tag: String.t option }
    let make ?distribution  ?e_tag  () = { distribution; e_tag }
    let parse xml =
      Some
        {
          distribution =
            (Util.option_bind (Xml.member "Distribution" xml)
               Distribution.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_tag
              (fun f -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.distribution
             (fun f -> Query.Pair ("Distribution", (Distribution.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_tag (fun f -> ("e_tag", (String.to_json f)));
           Util.option_map v.distribution
             (fun f -> ("distribution", (Distribution.to_json f)))])
    let of_json j =
      {
        distribution =
          (Util.option_map (Json.lookup j "distribution")
             Distribution.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json)
      }
  end
module InvalidTTLOrder =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DistributionAlreadyExists =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module UpdateStreamingDistributionRequest =
  struct
    type t =
      {
      streaming_distribution_config: StreamingDistributionConfig.t ;
      id: String.t ;
      if_match: String.t option }
    let make ~streaming_distribution_config  ~id  ?if_match  () =
      { streaming_distribution_config; id; if_match }
    let parse xml =
      Some
        {
          streaming_distribution_config =
            (Xml.required "StreamingDistributionConfig"
               (Util.option_bind
                  (Xml.member "StreamingDistributionConfig" xml)
                  StreamingDistributionConfig.parse));
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          if_match =
            (Util.option_bind (Xml.member "If-Match" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.if_match
              (fun f -> Query.Pair ("If-Match", (String.to_query f)));
           Some (Query.Pair ("Id", (String.to_query v.id)));
           Some
             (Query.Pair
                ("StreamingDistributionConfig",
                  (StreamingDistributionConfig.to_query
                     v.streaming_distribution_config)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.if_match
              (fun f -> ("if_match", (String.to_json f)));
           Some ("id", (String.to_json v.id));
           Some
             ("streaming_distribution_config",
               (StreamingDistributionConfig.to_json
                  v.streaming_distribution_config))])
    let of_json j =
      {
        streaming_distribution_config =
          (StreamingDistributionConfig.of_json
             (Util.of_option_exn
                (Json.lookup j "streaming_distribution_config")));
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        if_match =
          (Util.option_map (Json.lookup j "if_match") String.of_json)
      }
  end
module TooManyCertificates =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CloudFrontOriginAccessIdentityAlreadyExists =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidOriginAccessIdentity =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidRequiredProtocol =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidResponseCode =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListInvalidationsResult =
  struct
    type t = {
      invalidation_list: InvalidationList.t }
    let make ~invalidation_list  () = { invalidation_list }
    let parse xml =
      Some
        {
          invalidation_list =
            (Xml.required "InvalidationList"
               (Util.option_bind (Xml.member "InvalidationList" xml)
                  InvalidationList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("InvalidationList",
                   (InvalidationList.to_query v.invalidation_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("invalidation_list",
                (InvalidationList.to_json v.invalidation_list))])
    let of_json j =
      {
        invalidation_list =
          (InvalidationList.of_json
             (Util.of_option_exn (Json.lookup j "invalidation_list")))
      }
  end
module GetStreamingDistributionRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module IllegalUpdate =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module UpdateCloudFrontOriginAccessIdentityRequest =
  struct
    type t =
      {
      cloud_front_origin_access_identity_config:
        CloudFrontOriginAccessIdentityConfig.t ;
      id: String.t ;
      if_match: String.t option }
    let make ~cloud_front_origin_access_identity_config  ~id  ?if_match  () =
      { cloud_front_origin_access_identity_config; id; if_match }
    let parse xml =
      Some
        {
          cloud_front_origin_access_identity_config =
            (Xml.required "CloudFrontOriginAccessIdentityConfig"
               (Util.option_bind
                  (Xml.member "CloudFrontOriginAccessIdentityConfig" xml)
                  CloudFrontOriginAccessIdentityConfig.parse));
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          if_match =
            (Util.option_bind (Xml.member "If-Match" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.if_match
              (fun f -> Query.Pair ("If-Match", (String.to_query f)));
           Some (Query.Pair ("Id", (String.to_query v.id)));
           Some
             (Query.Pair
                ("CloudFrontOriginAccessIdentityConfig",
                  (CloudFrontOriginAccessIdentityConfig.to_query
                     v.cloud_front_origin_access_identity_config)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.if_match
              (fun f -> ("if_match", (String.to_json f)));
           Some ("id", (String.to_json v.id));
           Some
             ("cloud_front_origin_access_identity_config",
               (CloudFrontOriginAccessIdentityConfig.to_json
                  v.cloud_front_origin_access_identity_config))])
    let of_json j =
      {
        cloud_front_origin_access_identity_config =
          (CloudFrontOriginAccessIdentityConfig.of_json
             (Util.of_option_exn
                (Json.lookup j "cloud_front_origin_access_identity_config")));
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        if_match =
          (Util.option_map (Json.lookup j "if_match") String.of_json)
      }
  end
module DeleteCloudFrontOriginAccessIdentityRequest =
  struct
    type t = {
      id: String.t ;
      if_match: String.t option }
    let make ~id  ?if_match  () = { id; if_match }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          if_match =
            (Util.option_bind (Xml.member "If-Match" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.if_match
              (fun f -> Query.Pair ("If-Match", (String.to_query f)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.if_match
              (fun f -> ("if_match", (String.to_json f)));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        if_match =
          (Util.option_map (Json.lookup j "if_match") String.of_json)
      }
  end
module InvalidOrigin =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TrustedSignerDoesNotExist =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetDistributionConfigResult =
  struct
    type t =
      {
      distribution_config: DistributionConfig.t option ;
      e_tag: String.t option }
    let make ?distribution_config  ?e_tag  () =
      { distribution_config; e_tag }
    let parse xml =
      Some
        {
          distribution_config =
            (Util.option_bind (Xml.member "DistributionConfig" xml)
               DistributionConfig.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_tag
              (fun f -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.distribution_config
             (fun f ->
                Query.Pair
                  ("DistributionConfig", (DistributionConfig.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_tag (fun f -> ("e_tag", (String.to_json f)));
           Util.option_map v.distribution_config
             (fun f ->
                ("distribution_config", (DistributionConfig.to_json f)))])
    let of_json j =
      {
        distribution_config =
          (Util.option_map (Json.lookup j "distribution_config")
             DistributionConfig.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json)
      }
  end
module UpdateDistributionRequest =
  struct
    type t =
      {
      distribution_config: DistributionConfig.t ;
      id: String.t ;
      if_match: String.t option }
    let make ~distribution_config  ~id  ?if_match  () =
      { distribution_config; id; if_match }
    let parse xml =
      Some
        {
          distribution_config =
            (Xml.required "DistributionConfig"
               (Util.option_bind (Xml.member "DistributionConfig" xml)
                  DistributionConfig.parse));
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          if_match =
            (Util.option_bind (Xml.member "If-Match" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.if_match
              (fun f -> Query.Pair ("If-Match", (String.to_query f)));
           Some (Query.Pair ("Id", (String.to_query v.id)));
           Some
             (Query.Pair
                ("DistributionConfig",
                  (DistributionConfig.to_query v.distribution_config)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.if_match
              (fun f -> ("if_match", (String.to_json f)));
           Some ("id", (String.to_json v.id));
           Some
             ("distribution_config",
               (DistributionConfig.to_json v.distribution_config))])
    let of_json j =
      {
        distribution_config =
          (DistributionConfig.of_json
             (Util.of_option_exn (Json.lookup j "distribution_config")));
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        if_match =
          (Util.option_map (Json.lookup j "if_match") String.of_json)
      }
  end
module TooManyStreamingDistributions =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CreateStreamingDistributionRequest =
  struct
    type t = {
      streaming_distribution_config: StreamingDistributionConfig.t }
    let make ~streaming_distribution_config  () =
      { streaming_distribution_config }
    let parse xml =
      Some
        {
          streaming_distribution_config =
            (Xml.required "StreamingDistributionConfig"
               (Util.option_bind
                  (Xml.member "StreamingDistributionConfig" xml)
                  StreamingDistributionConfig.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StreamingDistributionConfig",
                   (StreamingDistributionConfig.to_query
                      v.streaming_distribution_config)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("streaming_distribution_config",
                (StreamingDistributionConfig.to_json
                   v.streaming_distribution_config))])
    let of_json j =
      {
        streaming_distribution_config =
          (StreamingDistributionConfig.of_json
             (Util.of_option_exn
                (Json.lookup j "streaming_distribution_config")))
      }
  end
module GetStreamingDistributionResult =
  struct
    type t =
      {
      streaming_distribution: StreamingDistribution.t option ;
      e_tag: String.t option }
    let make ?streaming_distribution  ?e_tag  () =
      { streaming_distribution; e_tag }
    let parse xml =
      Some
        {
          streaming_distribution =
            (Util.option_bind (Xml.member "StreamingDistribution" xml)
               StreamingDistribution.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_tag
              (fun f -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.streaming_distribution
             (fun f ->
                Query.Pair
                  ("StreamingDistribution",
                    (StreamingDistribution.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_tag (fun f -> ("e_tag", (String.to_json f)));
           Util.option_map v.streaming_distribution
             (fun f ->
                ("streaming_distribution", (StreamingDistribution.to_json f)))])
    let of_json j =
      {
        streaming_distribution =
          (Util.option_map (Json.lookup j "streaming_distribution")
             StreamingDistribution.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json)
      }
  end
module InvalidForwardCookies =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetStreamingDistributionConfigResult =
  struct
    type t =
      {
      streaming_distribution_config: StreamingDistributionConfig.t option ;
      e_tag: String.t option }
    let make ?streaming_distribution_config  ?e_tag  () =
      { streaming_distribution_config; e_tag }
    let parse xml =
      Some
        {
          streaming_distribution_config =
            (Util.option_bind (Xml.member "StreamingDistributionConfig" xml)
               StreamingDistributionConfig.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_tag
              (fun f -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.streaming_distribution_config
             (fun f ->
                Query.Pair
                  ("StreamingDistributionConfig",
                    (StreamingDistributionConfig.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_tag (fun f -> ("e_tag", (String.to_json f)));
           Util.option_map v.streaming_distribution_config
             (fun f ->
                ("streaming_distribution_config",
                  (StreamingDistributionConfig.to_json f)))])
    let of_json j =
      {
        streaming_distribution_config =
          (Util.option_map (Json.lookup j "streaming_distribution_config")
             StreamingDistributionConfig.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json)
      }
  end
module InvalidDefaultRootObject =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TooManyTrustedSigners =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetDistributionResult =
  struct
    type t = {
      distribution: Distribution.t option ;
      e_tag: String.t option }
    let make ?distribution  ?e_tag  () = { distribution; e_tag }
    let parse xml =
      Some
        {
          distribution =
            (Util.option_bind (Xml.member "Distribution" xml)
               Distribution.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_tag
              (fun f -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.distribution
             (fun f -> Query.Pair ("Distribution", (Distribution.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_tag (fun f -> ("e_tag", (String.to_json f)));
           Util.option_map v.distribution
             (fun f -> ("distribution", (Distribution.to_json f)))])
    let of_json j =
      {
        distribution =
          (Util.option_map (Json.lookup j "distribution")
             Distribution.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json)
      }
  end
module NoSuchStreamingDistribution =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidLocationCode =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListCloudFrontOriginAccessIdentitiesRequest =
  struct
    type t = {
      marker: String.t option ;
      max_items: String.t option }
    let make ?marker  ?max_items  () = { marker; max_items }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          max_items =
            (Util.option_bind (Xml.member "MaxItems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("MaxItems", (String.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module CreateStreamingDistributionResult =
  struct
    type t =
      {
      streaming_distribution: StreamingDistribution.t option ;
      location: String.t option ;
      e_tag: String.t option }
    let make ?streaming_distribution  ?location  ?e_tag  () =
      { streaming_distribution; location; e_tag }
    let parse xml =
      Some
        {
          streaming_distribution =
            (Util.option_bind (Xml.member "StreamingDistribution" xml)
               StreamingDistribution.parse);
          location =
            (Util.option_bind (Xml.member "Location" xml) String.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_tag
              (fun f -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.location
             (fun f -> Query.Pair ("Location", (String.to_query f)));
           Util.option_map v.streaming_distribution
             (fun f ->
                Query.Pair
                  ("StreamingDistribution",
                    (StreamingDistribution.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_tag (fun f -> ("e_tag", (String.to_json f)));
           Util.option_map v.location
             (fun f -> ("location", (String.to_json f)));
           Util.option_map v.streaming_distribution
             (fun f ->
                ("streaming_distribution", (StreamingDistribution.to_json f)))])
    let of_json j =
      {
        streaming_distribution =
          (Util.option_map (Json.lookup j "streaming_distribution")
             StreamingDistribution.of_json);
        location =
          (Util.option_map (Json.lookup j "location") String.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json)
      }
  end
module GetCloudFrontOriginAccessIdentityConfigRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module ListStreamingDistributionsResult =
  struct
    type t = {
      streaming_distribution_list: StreamingDistributionList.t }
    let make ~streaming_distribution_list  () =
      { streaming_distribution_list }
    let parse xml =
      Some
        {
          streaming_distribution_list =
            (Xml.required "StreamingDistributionList"
               (Util.option_bind (Xml.member "StreamingDistributionList" xml)
                  StreamingDistributionList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StreamingDistributionList",
                   (StreamingDistributionList.to_query
                      v.streaming_distribution_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("streaming_distribution_list",
                (StreamingDistributionList.to_json
                   v.streaming_distribution_list))])
    let of_json j =
      {
        streaming_distribution_list =
          (StreamingDistributionList.of_json
             (Util.of_option_exn
                (Json.lookup j "streaming_distribution_list")))
      }
  end
module NoSuchOrigin =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CreateInvalidationRequest =
  struct
    type t =
      {
      distribution_id: String.t ;
      invalidation_batch: InvalidationBatch.t }
    let make ~distribution_id  ~invalidation_batch  () =
      { distribution_id; invalidation_batch }
    let parse xml =
      Some
        {
          distribution_id =
            (Xml.required "DistributionId"
               (Util.option_bind (Xml.member "DistributionId" xml)
                  String.parse));
          invalidation_batch =
            (Xml.required "InvalidationBatch"
               (Util.option_bind (Xml.member "InvalidationBatch" xml)
                  InvalidationBatch.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("InvalidationBatch",
                   (InvalidationBatch.to_query v.invalidation_batch)));
           Some
             (Query.Pair
                ("DistributionId", (String.to_query v.distribution_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("invalidation_batch",
                (InvalidationBatch.to_json v.invalidation_batch));
           Some ("distribution_id", (String.to_json v.distribution_id))])
    let of_json j =
      {
        distribution_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "distribution_id")));
        invalidation_batch =
          (InvalidationBatch.of_json
             (Util.of_option_exn (Json.lookup j "invalidation_batch")))
      }
  end
module DeleteDistributionRequest =
  struct
    type t = {
      id: String.t ;
      if_match: String.t option }
    let make ~id  ?if_match  () = { id; if_match }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          if_match =
            (Util.option_bind (Xml.member "If-Match" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.if_match
              (fun f -> Query.Pair ("If-Match", (String.to_query f)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.if_match
              (fun f -> ("if_match", (String.to_json f)));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        if_match =
          (Util.option_map (Json.lookup j "if_match") String.of_json)
      }
  end
module AccessDenied =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module StreamingDistributionAlreadyExists =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TooManyCookieNamesInWhiteList =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module MissingBody =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TooManyDistributionCNAMEs =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end