open Aws
open Aws.BaseTypes
open CalendarLib

type calendar = Calendar.t

module MinimumEngineVersionPerAllowedValue = struct
  type t =
    { allowed_value : String.t option
    ; minimum_engine_version : String.t option
    }

  let make ?allowed_value ?minimum_engine_version () =
    { allowed_value; minimum_engine_version }

  let parse xml =
    Some
      { allowed_value = Util.option_bind (Xml.member "AllowedValue" xml) String.parse
      ; minimum_engine_version =
          Util.option_bind (Xml.member "MinimumEngineVersion" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.minimum_engine_version (fun f ->
               Query.Pair ("MinimumEngineVersion", String.to_query f))
         ; Util.option_map v.allowed_value (fun f ->
               Query.Pair ("AllowedValue", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.minimum_engine_version (fun f ->
               "minimum_engine_version", String.to_json f)
         ; Util.option_map v.allowed_value (fun f -> "allowed_value", String.to_json f)
         ])

  let of_json j =
    { allowed_value = Util.option_map (Json.lookup j "allowed_value") String.of_json
    ; minimum_engine_version =
        Util.option_map (Json.lookup j "minimum_engine_version") String.of_json
    }
end

module DBSecurityGroupMembership = struct
  type t =
    { d_b_security_group_name : String.t option
    ; status : String.t option
    }

  let make ?d_b_security_group_name ?status () = { d_b_security_group_name; status }

  let parse xml =
    Some
      { d_b_security_group_name =
          Util.option_bind (Xml.member "DBSecurityGroupName" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.d_b_security_group_name (fun f ->
               Query.Pair ("DBSecurityGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.d_b_security_group_name (fun f ->
               "d_b_security_group_name", String.to_json f)
         ])

  let of_json j =
    { d_b_security_group_name =
        Util.option_map (Json.lookup j "d_b_security_group_name") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    }
end

module OptionSetting = struct
  type t =
    { name : String.t option
    ; value : String.t option
    ; default_value : String.t option
    ; description : String.t option
    ; apply_type : String.t option
    ; data_type : String.t option
    ; allowed_values : String.t option
    ; is_modifiable : Boolean.t option
    ; is_collection : Boolean.t option
    }

  let make
      ?name
      ?value
      ?default_value
      ?description
      ?apply_type
      ?data_type
      ?allowed_values
      ?is_modifiable
      ?is_collection
      () =
    { name
    ; value
    ; default_value
    ; description
    ; apply_type
    ; data_type
    ; allowed_values
    ; is_modifiable
    ; is_collection
    }

  let parse xml =
    Some
      { name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; value = Util.option_bind (Xml.member "Value" xml) String.parse
      ; default_value = Util.option_bind (Xml.member "DefaultValue" xml) String.parse
      ; description = Util.option_bind (Xml.member "Description" xml) String.parse
      ; apply_type = Util.option_bind (Xml.member "ApplyType" xml) String.parse
      ; data_type = Util.option_bind (Xml.member "DataType" xml) String.parse
      ; allowed_values = Util.option_bind (Xml.member "AllowedValues" xml) String.parse
      ; is_modifiable = Util.option_bind (Xml.member "IsModifiable" xml) Boolean.parse
      ; is_collection = Util.option_bind (Xml.member "IsCollection" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.is_collection (fun f ->
               Query.Pair ("IsCollection", Boolean.to_query f))
         ; Util.option_map v.is_modifiable (fun f ->
               Query.Pair ("IsModifiable", Boolean.to_query f))
         ; Util.option_map v.allowed_values (fun f ->
               Query.Pair ("AllowedValues", String.to_query f))
         ; Util.option_map v.data_type (fun f ->
               Query.Pair ("DataType", String.to_query f))
         ; Util.option_map v.apply_type (fun f ->
               Query.Pair ("ApplyType", String.to_query f))
         ; Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ; Util.option_map v.default_value (fun f ->
               Query.Pair ("DefaultValue", String.to_query f))
         ; Util.option_map v.value (fun f -> Query.Pair ("Value", String.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.is_collection (fun f -> "is_collection", Boolean.to_json f)
         ; Util.option_map v.is_modifiable (fun f -> "is_modifiable", Boolean.to_json f)
         ; Util.option_map v.allowed_values (fun f -> "allowed_values", String.to_json f)
         ; Util.option_map v.data_type (fun f -> "data_type", String.to_json f)
         ; Util.option_map v.apply_type (fun f -> "apply_type", String.to_json f)
         ; Util.option_map v.description (fun f -> "description", String.to_json f)
         ; Util.option_map v.default_value (fun f -> "default_value", String.to_json f)
         ; Util.option_map v.value (fun f -> "value", String.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ])

  let of_json j =
    { name = Util.option_map (Json.lookup j "name") String.of_json
    ; value = Util.option_map (Json.lookup j "value") String.of_json
    ; default_value = Util.option_map (Json.lookup j "default_value") String.of_json
    ; description = Util.option_map (Json.lookup j "description") String.of_json
    ; apply_type = Util.option_map (Json.lookup j "apply_type") String.of_json
    ; data_type = Util.option_map (Json.lookup j "data_type") String.of_json
    ; allowed_values = Util.option_map (Json.lookup j "allowed_values") String.of_json
    ; is_modifiable = Util.option_map (Json.lookup j "is_modifiable") Boolean.of_json
    ; is_collection = Util.option_map (Json.lookup j "is_collection") Boolean.of_json
    }
end

module VpcSecurityGroupMembership = struct
  type t =
    { vpc_security_group_id : String.t option
    ; status : String.t option
    }

  let make ?vpc_security_group_id ?status () = { vpc_security_group_id; status }

  let parse xml =
    Some
      { vpc_security_group_id =
          Util.option_bind (Xml.member "VpcSecurityGroupId" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.vpc_security_group_id (fun f ->
               Query.Pair ("VpcSecurityGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.vpc_security_group_id (fun f ->
               "vpc_security_group_id", String.to_json f)
         ])

  let of_json j =
    { vpc_security_group_id =
        Util.option_map (Json.lookup j "vpc_security_group_id") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    }
end

module AvailabilityZone = struct
  type t = { name : String.t option }

  let make ?name () = { name }

  let parse xml = Some { name = Util.option_bind (Xml.member "Name" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.name (fun f -> "name", String.to_json f) ])

  let of_json j = { name = Util.option_map (Json.lookup j "name") String.of_json }
end

module Outpost = struct
  type t = { arn : String.t option }

  let make ?arn () = { arn }

  let parse xml = Some { arn = Util.option_bind (Xml.member "Arn" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.arn (fun f -> Query.Pair ("Arn", String.to_query f)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Util.option_map v.arn (fun f -> "arn", String.to_json f) ])

  let of_json j = { arn = Util.option_map (Json.lookup j "arn") String.of_json }
end

module MinimumEngineVersionPerAllowedValueList = struct
  type t = MinimumEngineVersionPerAllowedValue.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map
         MinimumEngineVersionPerAllowedValue.parse
         (Xml.members "MinimumEngineVersionPerAllowedValue" xml))

  let to_query v = Query.to_query_list MinimumEngineVersionPerAllowedValue.to_query v

  let to_json v = `List (List.map MinimumEngineVersionPerAllowedValue.to_json v)

  let of_json j = Json.to_list MinimumEngineVersionPerAllowedValue.of_json j
end

module DoubleRange = struct
  type t =
    { from : Double.t option
    ; to_ : Double.t option
    }

  let make ?from ?to_ () = { from; to_ }

  let parse xml =
    Some
      { from = Util.option_bind (Xml.member "From" xml) Double.parse
      ; to_ = Util.option_bind (Xml.member "To" xml) Double.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.to_ (fun f -> Query.Pair ("To", Double.to_query f))
         ; Util.option_map v.from (fun f -> Query.Pair ("From", Double.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.to_ (fun f -> "to_", Double.to_json f)
         ; Util.option_map v.from (fun f -> "from", Double.to_json f)
         ])

  let of_json j =
    { from = Util.option_map (Json.lookup j "from") Double.of_json
    ; to_ = Util.option_map (Json.lookup j "to_") Double.of_json
    }
end

module Range = struct
  type t =
    { from : Integer.t option
    ; to_ : Integer.t option
    ; step : Integer.t option
    }

  let make ?from ?to_ ?step () = { from; to_; step }

  let parse xml =
    Some
      { from = Util.option_bind (Xml.member "From" xml) Integer.parse
      ; to_ = Util.option_bind (Xml.member "To" xml) Integer.parse
      ; step = Util.option_bind (Xml.member "Step" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.step (fun f -> Query.Pair ("Step", Integer.to_query f))
         ; Util.option_map v.to_ (fun f -> Query.Pair ("To", Integer.to_query f))
         ; Util.option_map v.from (fun f -> Query.Pair ("From", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.step (fun f -> "step", Integer.to_json f)
         ; Util.option_map v.to_ (fun f -> "to_", Integer.to_json f)
         ; Util.option_map v.from (fun f -> "from", Integer.to_json f)
         ])

  let of_json j =
    { from = Util.option_map (Json.lookup j "from") Integer.of_json
    ; to_ = Util.option_map (Json.lookup j "to_") Integer.of_json
    ; step = Util.option_map (Json.lookup j "step") Integer.of_json
    }
end

module ReadersArnList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module WriteForwardingStatus = struct
  type t =
    | Enabled
    | Disabled
    | Enabling
    | Disabling
    | Unknown

  let str_to_t =
    [ "unknown", Unknown
    ; "disabling", Disabling
    ; "enabling", Enabling
    ; "disabled", Disabled
    ; "enabled", Enabled
    ]

  let t_to_str =
    [ Unknown, "unknown"
    ; Disabling, "disabling"
    ; Enabling, "enabling"
    ; Disabled, "disabled"
    ; Enabled, "enabled"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module DBSecurityGroupMembershipList = struct
  type t = DBSecurityGroupMembership.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map DBSecurityGroupMembership.parse (Xml.members "DBSecurityGroup" xml))

  let to_query v = Query.to_query_list DBSecurityGroupMembership.to_query v

  let to_json v = `List (List.map DBSecurityGroupMembership.to_json v)

  let of_json j = Json.to_list DBSecurityGroupMembership.of_json j
end

module OptionSettingConfigurationList = struct
  type t = OptionSetting.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map OptionSetting.parse (Xml.members "OptionSetting" xml))

  let to_query v = Query.to_query_list OptionSetting.to_query v

  let to_json v = `List (List.map OptionSetting.to_json v)

  let of_json j = Json.to_list OptionSetting.of_json j
end

module VpcSecurityGroupMembershipList = struct
  type t = VpcSecurityGroupMembership.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map
         VpcSecurityGroupMembership.parse
         (Xml.members "VpcSecurityGroupMembership" xml))

  let to_query v = Query.to_query_list VpcSecurityGroupMembership.to_query v

  let to_json v = `List (List.map VpcSecurityGroupMembership.to_json v)

  let of_json j = Json.to_list VpcSecurityGroupMembership.of_json j
end

module AuthScheme = struct
  type t = SECRETS

  let str_to_t = [ "SECRETS", SECRETS ]

  let t_to_str = [ SECRETS, "SECRETS" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module IAMAuthMode = struct
  type t =
    | DISABLED
    | REQUIRED

  let str_to_t = [ "REQUIRED", REQUIRED; "DISABLED", DISABLED ]

  let t_to_str = [ REQUIRED, "REQUIRED"; DISABLED, "DISABLED" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module Subnet = struct
  type t =
    { subnet_identifier : String.t option
    ; subnet_availability_zone : AvailabilityZone.t option
    ; subnet_outpost : Outpost.t option
    ; subnet_status : String.t option
    }

  let make ?subnet_identifier ?subnet_availability_zone ?subnet_outpost ?subnet_status ()
      =
    { subnet_identifier; subnet_availability_zone; subnet_outpost; subnet_status }

  let parse xml =
    Some
      { subnet_identifier =
          Util.option_bind (Xml.member "SubnetIdentifier" xml) String.parse
      ; subnet_availability_zone =
          Util.option_bind
            (Xml.member "SubnetAvailabilityZone" xml)
            AvailabilityZone.parse
      ; subnet_outpost = Util.option_bind (Xml.member "SubnetOutpost" xml) Outpost.parse
      ; subnet_status = Util.option_bind (Xml.member "SubnetStatus" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.subnet_status (fun f ->
               Query.Pair ("SubnetStatus", String.to_query f))
         ; Util.option_map v.subnet_outpost (fun f ->
               Query.Pair ("SubnetOutpost", Outpost.to_query f))
         ; Util.option_map v.subnet_availability_zone (fun f ->
               Query.Pair ("SubnetAvailabilityZone", AvailabilityZone.to_query f))
         ; Util.option_map v.subnet_identifier (fun f ->
               Query.Pair ("SubnetIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.subnet_status (fun f -> "subnet_status", String.to_json f)
         ; Util.option_map v.subnet_outpost (fun f -> "subnet_outpost", Outpost.to_json f)
         ; Util.option_map v.subnet_availability_zone (fun f ->
               "subnet_availability_zone", AvailabilityZone.to_json f)
         ; Util.option_map v.subnet_identifier (fun f ->
               "subnet_identifier", String.to_json f)
         ])

  let of_json j =
    { subnet_identifier =
        Util.option_map (Json.lookup j "subnet_identifier") String.of_json
    ; subnet_availability_zone =
        Util.option_map
          (Json.lookup j "subnet_availability_zone")
          AvailabilityZone.of_json
    ; subnet_outpost = Util.option_map (Json.lookup j "subnet_outpost") Outpost.of_json
    ; subnet_status = Util.option_map (Json.lookup j "subnet_status") String.of_json
    }
end

module LogTypeList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module ProcessorFeature = struct
  type t =
    { name : String.t option
    ; value : String.t option
    }

  let make ?name ?value () = { name; value }

  let parse xml =
    Some
      { name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; value = Util.option_bind (Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> Query.Pair ("Value", String.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> "value", String.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ])

  let of_json j =
    { name = Util.option_map (Json.lookup j "name") String.of_json
    ; value = Util.option_map (Json.lookup j "value") String.of_json
    }
end

module OptionGroupOptionSetting = struct
  type t =
    { setting_name : String.t option
    ; setting_description : String.t option
    ; default_value : String.t option
    ; apply_type : String.t option
    ; allowed_values : String.t option
    ; is_modifiable : Boolean.t option
    ; is_required : Boolean.t option
    ; minimum_engine_version_per_allowed_value : MinimumEngineVersionPerAllowedValueList.t
    }

  let make
      ?setting_name
      ?setting_description
      ?default_value
      ?apply_type
      ?allowed_values
      ?is_modifiable
      ?is_required
      ?(minimum_engine_version_per_allowed_value = [])
      () =
    { setting_name
    ; setting_description
    ; default_value
    ; apply_type
    ; allowed_values
    ; is_modifiable
    ; is_required
    ; minimum_engine_version_per_allowed_value
    }

  let parse xml =
    Some
      { setting_name = Util.option_bind (Xml.member "SettingName" xml) String.parse
      ; setting_description =
          Util.option_bind (Xml.member "SettingDescription" xml) String.parse
      ; default_value = Util.option_bind (Xml.member "DefaultValue" xml) String.parse
      ; apply_type = Util.option_bind (Xml.member "ApplyType" xml) String.parse
      ; allowed_values = Util.option_bind (Xml.member "AllowedValues" xml) String.parse
      ; is_modifiable = Util.option_bind (Xml.member "IsModifiable" xml) Boolean.parse
      ; is_required = Util.option_bind (Xml.member "IsRequired" xml) Boolean.parse
      ; minimum_engine_version_per_allowed_value =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "MinimumEngineVersionPerAllowedValue" xml)
               MinimumEngineVersionPerAllowedValueList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "MinimumEngineVersionPerAllowedValue.member"
                , MinimumEngineVersionPerAllowedValueList.to_query
                    v.minimum_engine_version_per_allowed_value ))
         ; Util.option_map v.is_required (fun f ->
               Query.Pair ("IsRequired", Boolean.to_query f))
         ; Util.option_map v.is_modifiable (fun f ->
               Query.Pair ("IsModifiable", Boolean.to_query f))
         ; Util.option_map v.allowed_values (fun f ->
               Query.Pair ("AllowedValues", String.to_query f))
         ; Util.option_map v.apply_type (fun f ->
               Query.Pair ("ApplyType", String.to_query f))
         ; Util.option_map v.default_value (fun f ->
               Query.Pair ("DefaultValue", String.to_query f))
         ; Util.option_map v.setting_description (fun f ->
               Query.Pair ("SettingDescription", String.to_query f))
         ; Util.option_map v.setting_name (fun f ->
               Query.Pair ("SettingName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "minimum_engine_version_per_allowed_value"
             , MinimumEngineVersionPerAllowedValueList.to_json
                 v.minimum_engine_version_per_allowed_value )
         ; Util.option_map v.is_required (fun f -> "is_required", Boolean.to_json f)
         ; Util.option_map v.is_modifiable (fun f -> "is_modifiable", Boolean.to_json f)
         ; Util.option_map v.allowed_values (fun f -> "allowed_values", String.to_json f)
         ; Util.option_map v.apply_type (fun f -> "apply_type", String.to_json f)
         ; Util.option_map v.default_value (fun f -> "default_value", String.to_json f)
         ; Util.option_map v.setting_description (fun f ->
               "setting_description", String.to_json f)
         ; Util.option_map v.setting_name (fun f -> "setting_name", String.to_json f)
         ])

  let of_json j =
    { setting_name = Util.option_map (Json.lookup j "setting_name") String.of_json
    ; setting_description =
        Util.option_map (Json.lookup j "setting_description") String.of_json
    ; default_value = Util.option_map (Json.lookup j "default_value") String.of_json
    ; apply_type = Util.option_map (Json.lookup j "apply_type") String.of_json
    ; allowed_values = Util.option_map (Json.lookup j "allowed_values") String.of_json
    ; is_modifiable = Util.option_map (Json.lookup j "is_modifiable") Boolean.of_json
    ; is_required = Util.option_map (Json.lookup j "is_required") Boolean.of_json
    ; minimum_engine_version_per_allowed_value =
        MinimumEngineVersionPerAllowedValueList.of_json
          (Util.of_option_exn (Json.lookup j "minimum_engine_version_per_allowed_value"))
    }
end

module OptionVersion = struct
  type t =
    { version : String.t option
    ; is_default : Boolean.t option
    }

  let make ?version ?is_default () = { version; is_default }

  let parse xml =
    Some
      { version = Util.option_bind (Xml.member "Version" xml) String.parse
      ; is_default = Util.option_bind (Xml.member "IsDefault" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.is_default (fun f ->
               Query.Pair ("IsDefault", Boolean.to_query f))
         ; Util.option_map v.version (fun f -> Query.Pair ("Version", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.is_default (fun f -> "is_default", Boolean.to_json f)
         ; Util.option_map v.version (fun f -> "version", String.to_json f)
         ])

  let of_json j =
    { version = Util.option_map (Json.lookup j "version") String.of_json
    ; is_default = Util.option_map (Json.lookup j "is_default") Boolean.of_json
    }
end

module DoubleRangeList = struct
  type t = DoubleRange.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DoubleRange.parse (Xml.members "DoubleRange" xml))

  let to_query v = Query.to_query_list DoubleRange.to_query v

  let to_json v = `List (List.map DoubleRange.to_json v)

  let of_json j = Json.to_list DoubleRange.of_json j
end

module RangeList = struct
  type t = Range.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Range.parse (Xml.members "Range" xml))

  let to_query v = Query.to_query_list Range.to_query v

  let to_json v = `List (List.map Range.to_json v)

  let of_json j = Json.to_list Range.of_json j
end

module ApplyMethod = struct
  type t =
    | Immediate
    | Pending_reboot

  let str_to_t = [ "pending-reboot", Pending_reboot; "immediate", Immediate ]

  let t_to_str = [ Pending_reboot, "pending-reboot"; Immediate, "immediate" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module EngineModeList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module RecurringCharge = struct
  type t =
    { recurring_charge_amount : Double.t option
    ; recurring_charge_frequency : String.t option
    }

  let make ?recurring_charge_amount ?recurring_charge_frequency () =
    { recurring_charge_amount; recurring_charge_frequency }

  let parse xml =
    Some
      { recurring_charge_amount =
          Util.option_bind (Xml.member "RecurringChargeAmount" xml) Double.parse
      ; recurring_charge_frequency =
          Util.option_bind (Xml.member "RecurringChargeFrequency" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.recurring_charge_frequency (fun f ->
               Query.Pair ("RecurringChargeFrequency", String.to_query f))
         ; Util.option_map v.recurring_charge_amount (fun f ->
               Query.Pair ("RecurringChargeAmount", Double.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.recurring_charge_frequency (fun f ->
               "recurring_charge_frequency", String.to_json f)
         ; Util.option_map v.recurring_charge_amount (fun f ->
               "recurring_charge_amount", Double.to_json f)
         ])

  let of_json j =
    { recurring_charge_amount =
        Util.option_map (Json.lookup j "recurring_charge_amount") Double.of_json
    ; recurring_charge_frequency =
        Util.option_map (Json.lookup j "recurring_charge_frequency") String.of_json
    }
end

module AttributeValueList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map String.parse (Xml.members "AttributeValue" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module PendingMaintenanceAction = struct
  type t =
    { action : String.t option
    ; auto_applied_after_date : DateTime.t option
    ; forced_apply_date : DateTime.t option
    ; opt_in_status : String.t option
    ; current_apply_date : DateTime.t option
    ; description : String.t option
    }

  let make
      ?action
      ?auto_applied_after_date
      ?forced_apply_date
      ?opt_in_status
      ?current_apply_date
      ?description
      () =
    { action
    ; auto_applied_after_date
    ; forced_apply_date
    ; opt_in_status
    ; current_apply_date
    ; description
    }

  let parse xml =
    Some
      { action = Util.option_bind (Xml.member "Action" xml) String.parse
      ; auto_applied_after_date =
          Util.option_bind (Xml.member "AutoAppliedAfterDate" xml) DateTime.parse
      ; forced_apply_date =
          Util.option_bind (Xml.member "ForcedApplyDate" xml) DateTime.parse
      ; opt_in_status = Util.option_bind (Xml.member "OptInStatus" xml) String.parse
      ; current_apply_date =
          Util.option_bind (Xml.member "CurrentApplyDate" xml) DateTime.parse
      ; description = Util.option_bind (Xml.member "Description" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ; Util.option_map v.current_apply_date (fun f ->
               Query.Pair ("CurrentApplyDate", DateTime.to_query f))
         ; Util.option_map v.opt_in_status (fun f ->
               Query.Pair ("OptInStatus", String.to_query f))
         ; Util.option_map v.forced_apply_date (fun f ->
               Query.Pair ("ForcedApplyDate", DateTime.to_query f))
         ; Util.option_map v.auto_applied_after_date (fun f ->
               Query.Pair ("AutoAppliedAfterDate", DateTime.to_query f))
         ; Util.option_map v.action (fun f -> Query.Pair ("Action", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.description (fun f -> "description", String.to_json f)
         ; Util.option_map v.current_apply_date (fun f ->
               "current_apply_date", DateTime.to_json f)
         ; Util.option_map v.opt_in_status (fun f -> "opt_in_status", String.to_json f)
         ; Util.option_map v.forced_apply_date (fun f ->
               "forced_apply_date", DateTime.to_json f)
         ; Util.option_map v.auto_applied_after_date (fun f ->
               "auto_applied_after_date", DateTime.to_json f)
         ; Util.option_map v.action (fun f -> "action", String.to_json f)
         ])

  let of_json j =
    { action = Util.option_map (Json.lookup j "action") String.of_json
    ; auto_applied_after_date =
        Util.option_map (Json.lookup j "auto_applied_after_date") DateTime.of_json
    ; forced_apply_date =
        Util.option_map (Json.lookup j "forced_apply_date") DateTime.of_json
    ; opt_in_status = Util.option_map (Json.lookup j "opt_in_status") String.of_json
    ; current_apply_date =
        Util.option_map (Json.lookup j "current_apply_date") DateTime.of_json
    ; description = Util.option_map (Json.lookup j "description") String.of_json
    }
end

module StringList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module TargetHealthReason = struct
  type t =
    | UNREACHABLE
    | CONNECTION_FAILED
    | AUTH_FAILURE
    | PENDING_PROXY_CAPACITY

  let str_to_t =
    [ "PENDING_PROXY_CAPACITY", PENDING_PROXY_CAPACITY
    ; "AUTH_FAILURE", AUTH_FAILURE
    ; "CONNECTION_FAILED", CONNECTION_FAILED
    ; "UNREACHABLE", UNREACHABLE
    ]

  let t_to_str =
    [ PENDING_PROXY_CAPACITY, "PENDING_PROXY_CAPACITY"
    ; AUTH_FAILURE, "AUTH_FAILURE"
    ; CONNECTION_FAILED, "CONNECTION_FAILED"
    ; UNREACHABLE, "UNREACHABLE"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module TargetState = struct
  type t =
    | REGISTERING
    | AVAILABLE
    | UNAVAILABLE

  let str_to_t =
    [ "UNAVAILABLE", UNAVAILABLE; "AVAILABLE", AVAILABLE; "REGISTERING", REGISTERING ]

  let t_to_str =
    [ UNAVAILABLE, "UNAVAILABLE"; AVAILABLE, "AVAILABLE"; REGISTERING, "REGISTERING" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module CharacterSet = struct
  type t =
    { character_set_name : String.t option
    ; character_set_description : String.t option
    }

  let make ?character_set_name ?character_set_description () =
    { character_set_name; character_set_description }

  let parse xml =
    Some
      { character_set_name =
          Util.option_bind (Xml.member "CharacterSetName" xml) String.parse
      ; character_set_description =
          Util.option_bind (Xml.member "CharacterSetDescription" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.character_set_description (fun f ->
               Query.Pair ("CharacterSetDescription", String.to_query f))
         ; Util.option_map v.character_set_name (fun f ->
               Query.Pair ("CharacterSetName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.character_set_description (fun f ->
               "character_set_description", String.to_json f)
         ; Util.option_map v.character_set_name (fun f ->
               "character_set_name", String.to_json f)
         ])

  let of_json j =
    { character_set_name =
        Util.option_map (Json.lookup j "character_set_name") String.of_json
    ; character_set_description =
        Util.option_map (Json.lookup j "character_set_description") String.of_json
    }
end

module Timezone = struct
  type t = { timezone_name : String.t option }

  let make ?timezone_name () = { timezone_name }

  let parse xml =
    Some { timezone_name = Util.option_bind (Xml.member "TimezoneName" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.timezone_name (fun f ->
               Query.Pair ("TimezoneName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.timezone_name (fun f -> "timezone_name", String.to_json f) ])

  let of_json j =
    { timezone_name = Util.option_map (Json.lookup j "timezone_name") String.of_json }
end

module UpgradeTarget = struct
  type t =
    { engine : String.t option
    ; engine_version : String.t option
    ; description : String.t option
    ; auto_upgrade : Boolean.t option
    ; is_major_version_upgrade : Boolean.t option
    }

  let make ?engine ?engine_version ?description ?auto_upgrade ?is_major_version_upgrade ()
      =
    { engine; engine_version; description; auto_upgrade; is_major_version_upgrade }

  let parse xml =
    Some
      { engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; description = Util.option_bind (Xml.member "Description" xml) String.parse
      ; auto_upgrade = Util.option_bind (Xml.member "AutoUpgrade" xml) Boolean.parse
      ; is_major_version_upgrade =
          Util.option_bind (Xml.member "IsMajorVersionUpgrade" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.is_major_version_upgrade (fun f ->
               Query.Pair ("IsMajorVersionUpgrade", Boolean.to_query f))
         ; Util.option_map v.auto_upgrade (fun f ->
               Query.Pair ("AutoUpgrade", Boolean.to_query f))
         ; Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.is_major_version_upgrade (fun f ->
               "is_major_version_upgrade", Boolean.to_json f)
         ; Util.option_map v.auto_upgrade (fun f -> "auto_upgrade", Boolean.to_json f)
         ; Util.option_map v.description (fun f -> "description", String.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ])

  let of_json j =
    { engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; description = Util.option_map (Json.lookup j "description") String.of_json
    ; auto_upgrade = Util.option_map (Json.lookup j "auto_upgrade") Boolean.of_json
    ; is_major_version_upgrade =
        Util.option_map (Json.lookup j "is_major_version_upgrade") Boolean.of_json
    }
end

module GlobalClusterMember = struct
  type t =
    { d_b_cluster_arn : String.t option
    ; readers : ReadersArnList.t
    ; is_writer : Boolean.t option
    ; global_write_forwarding_status : WriteForwardingStatus.t option
    }

  let make ?d_b_cluster_arn ?(readers = []) ?is_writer ?global_write_forwarding_status ()
      =
    { d_b_cluster_arn; readers; is_writer; global_write_forwarding_status }

  let parse xml =
    Some
      { d_b_cluster_arn = Util.option_bind (Xml.member "DBClusterArn" xml) String.parse
      ; readers =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Readers" xml) ReadersArnList.parse)
      ; is_writer = Util.option_bind (Xml.member "IsWriter" xml) Boolean.parse
      ; global_write_forwarding_status =
          Util.option_bind
            (Xml.member "GlobalWriteForwardingStatus" xml)
            WriteForwardingStatus.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.global_write_forwarding_status (fun f ->
               Query.Pair ("GlobalWriteForwardingStatus", WriteForwardingStatus.to_query f))
         ; Util.option_map v.is_writer (fun f ->
               Query.Pair ("IsWriter", Boolean.to_query f))
         ; Some (Query.Pair ("Readers.member", ReadersArnList.to_query v.readers))
         ; Util.option_map v.d_b_cluster_arn (fun f ->
               Query.Pair ("DBClusterArn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.global_write_forwarding_status (fun f ->
               "global_write_forwarding_status", WriteForwardingStatus.to_json f)
         ; Util.option_map v.is_writer (fun f -> "is_writer", Boolean.to_json f)
         ; Some ("readers", ReadersArnList.to_json v.readers)
         ; Util.option_map v.d_b_cluster_arn (fun f ->
               "d_b_cluster_arn", String.to_json f)
         ])

  let of_json j =
    { d_b_cluster_arn = Util.option_map (Json.lookup j "d_b_cluster_arn") String.of_json
    ; readers = ReadersArnList.of_json (Util.of_option_exn (Json.lookup j "readers"))
    ; is_writer = Util.option_map (Json.lookup j "is_writer") Boolean.of_json
    ; global_write_forwarding_status =
        Util.option_map
          (Json.lookup j "global_write_forwarding_status")
          WriteForwardingStatus.of_json
    }
end

module Option = struct
  type t =
    { option_name : String.t option
    ; option_description : String.t option
    ; persistent : Boolean.t option
    ; permanent : Boolean.t option
    ; port : Integer.t option
    ; option_version : String.t option
    ; option_settings : OptionSettingConfigurationList.t
    ; d_b_security_group_memberships : DBSecurityGroupMembershipList.t
    ; vpc_security_group_memberships : VpcSecurityGroupMembershipList.t
    }

  let make
      ?option_name
      ?option_description
      ?persistent
      ?permanent
      ?port
      ?option_version
      ?(option_settings = [])
      ?(d_b_security_group_memberships = [])
      ?(vpc_security_group_memberships = [])
      () =
    { option_name
    ; option_description
    ; persistent
    ; permanent
    ; port
    ; option_version
    ; option_settings
    ; d_b_security_group_memberships
    ; vpc_security_group_memberships
    }

  let parse xml =
    Some
      { option_name = Util.option_bind (Xml.member "OptionName" xml) String.parse
      ; option_description =
          Util.option_bind (Xml.member "OptionDescription" xml) String.parse
      ; persistent = Util.option_bind (Xml.member "Persistent" xml) Boolean.parse
      ; permanent = Util.option_bind (Xml.member "Permanent" xml) Boolean.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; option_version = Util.option_bind (Xml.member "OptionVersion" xml) String.parse
      ; option_settings =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "OptionSettings" xml)
               OptionSettingConfigurationList.parse)
      ; d_b_security_group_memberships =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBSecurityGroupMemberships" xml)
               DBSecurityGroupMembershipList.parse)
      ; vpc_security_group_memberships =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupMemberships" xml)
               VpcSecurityGroupMembershipList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "VpcSecurityGroupMemberships.member"
                , VpcSecurityGroupMembershipList.to_query v.vpc_security_group_memberships
                ))
         ; Some
             (Query.Pair
                ( "DBSecurityGroupMemberships.member"
                , DBSecurityGroupMembershipList.to_query v.d_b_security_group_memberships
                ))
         ; Some
             (Query.Pair
                ( "OptionSettings.member"
                , OptionSettingConfigurationList.to_query v.option_settings ))
         ; Util.option_map v.option_version (fun f ->
               Query.Pair ("OptionVersion", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.permanent (fun f ->
               Query.Pair ("Permanent", Boolean.to_query f))
         ; Util.option_map v.persistent (fun f ->
               Query.Pair ("Persistent", Boolean.to_query f))
         ; Util.option_map v.option_description (fun f ->
               Query.Pair ("OptionDescription", String.to_query f))
         ; Util.option_map v.option_name (fun f ->
               Query.Pair ("OptionName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "vpc_security_group_memberships"
             , VpcSecurityGroupMembershipList.to_json v.vpc_security_group_memberships )
         ; Some
             ( "d_b_security_group_memberships"
             , DBSecurityGroupMembershipList.to_json v.d_b_security_group_memberships )
         ; Some
             ("option_settings", OptionSettingConfigurationList.to_json v.option_settings)
         ; Util.option_map v.option_version (fun f -> "option_version", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.permanent (fun f -> "permanent", Boolean.to_json f)
         ; Util.option_map v.persistent (fun f -> "persistent", Boolean.to_json f)
         ; Util.option_map v.option_description (fun f ->
               "option_description", String.to_json f)
         ; Util.option_map v.option_name (fun f -> "option_name", String.to_json f)
         ])

  let of_json j =
    { option_name = Util.option_map (Json.lookup j "option_name") String.of_json
    ; option_description =
        Util.option_map (Json.lookup j "option_description") String.of_json
    ; persistent = Util.option_map (Json.lookup j "persistent") Boolean.of_json
    ; permanent = Util.option_map (Json.lookup j "permanent") Boolean.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; option_version = Util.option_map (Json.lookup j "option_version") String.of_json
    ; option_settings =
        OptionSettingConfigurationList.of_json
          (Util.of_option_exn (Json.lookup j "option_settings"))
    ; d_b_security_group_memberships =
        DBSecurityGroupMembershipList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_security_group_memberships"))
    ; vpc_security_group_memberships =
        VpcSecurityGroupMembershipList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_memberships"))
    }
end

module Tag = struct
  type t =
    { key : String.t option
    ; value : String.t option
    }

  let make ?key ?value () = { key; value }

  let parse xml =
    Some
      { key = Util.option_bind (Xml.member "Key" xml) String.parse
      ; value = Util.option_bind (Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> Query.Pair ("Value", String.to_query f))
         ; Util.option_map v.key (fun f -> Query.Pair ("Key", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> "value", String.to_json f)
         ; Util.option_map v.key (fun f -> "key", String.to_json f)
         ])

  let of_json j =
    { key = Util.option_map (Json.lookup j "key") String.of_json
    ; value = Util.option_map (Json.lookup j "value") String.of_json
    }
end

module UserAuthConfigInfo = struct
  type t =
    { description : String.t option
    ; user_name : String.t option
    ; auth_scheme : AuthScheme.t option
    ; secret_arn : String.t option
    ; i_a_m_auth : IAMAuthMode.t option
    }

  let make ?description ?user_name ?auth_scheme ?secret_arn ?i_a_m_auth () =
    { description; user_name; auth_scheme; secret_arn; i_a_m_auth }

  let parse xml =
    Some
      { description = Util.option_bind (Xml.member "Description" xml) String.parse
      ; user_name = Util.option_bind (Xml.member "UserName" xml) String.parse
      ; auth_scheme = Util.option_bind (Xml.member "AuthScheme" xml) AuthScheme.parse
      ; secret_arn = Util.option_bind (Xml.member "SecretArn" xml) String.parse
      ; i_a_m_auth = Util.option_bind (Xml.member "IAMAuth" xml) IAMAuthMode.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.i_a_m_auth (fun f ->
               Query.Pair ("IAMAuth", IAMAuthMode.to_query f))
         ; Util.option_map v.secret_arn (fun f ->
               Query.Pair ("SecretArn", String.to_query f))
         ; Util.option_map v.auth_scheme (fun f ->
               Query.Pair ("AuthScheme", AuthScheme.to_query f))
         ; Util.option_map v.user_name (fun f ->
               Query.Pair ("UserName", String.to_query f))
         ; Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.i_a_m_auth (fun f -> "i_a_m_auth", IAMAuthMode.to_json f)
         ; Util.option_map v.secret_arn (fun f -> "secret_arn", String.to_json f)
         ; Util.option_map v.auth_scheme (fun f -> "auth_scheme", AuthScheme.to_json f)
         ; Util.option_map v.user_name (fun f -> "user_name", String.to_json f)
         ; Util.option_map v.description (fun f -> "description", String.to_json f)
         ])

  let of_json j =
    { description = Util.option_map (Json.lookup j "description") String.of_json
    ; user_name = Util.option_map (Json.lookup j "user_name") String.of_json
    ; auth_scheme = Util.option_map (Json.lookup j "auth_scheme") AuthScheme.of_json
    ; secret_arn = Util.option_map (Json.lookup j "secret_arn") String.of_json
    ; i_a_m_auth = Util.option_map (Json.lookup j "i_a_m_auth") IAMAuthMode.of_json
    }
end

module DBInstanceRole = struct
  type t =
    { role_arn : String.t option
    ; feature_name : String.t option
    ; status : String.t option
    }

  let make ?role_arn ?feature_name ?status () = { role_arn; feature_name; status }

  let parse xml =
    Some
      { role_arn = Util.option_bind (Xml.member "RoleArn" xml) String.parse
      ; feature_name = Util.option_bind (Xml.member "FeatureName" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.feature_name (fun f ->
               Query.Pair ("FeatureName", String.to_query f))
         ; Util.option_map v.role_arn (fun f -> Query.Pair ("RoleArn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.feature_name (fun f -> "feature_name", String.to_json f)
         ; Util.option_map v.role_arn (fun f -> "role_arn", String.to_json f)
         ])

  let of_json j =
    { role_arn = Util.option_map (Json.lookup j "role_arn") String.of_json
    ; feature_name = Util.option_map (Json.lookup j "feature_name") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    }
end

module DBInstanceStatusInfo = struct
  type t =
    { status_type : String.t option
    ; normal : Boolean.t option
    ; status : String.t option
    ; message : String.t option
    }

  let make ?status_type ?normal ?status ?message () =
    { status_type; normal; status; message }

  let parse xml =
    Some
      { status_type = Util.option_bind (Xml.member "StatusType" xml) String.parse
      ; normal = Util.option_bind (Xml.member "Normal" xml) Boolean.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; message = Util.option_bind (Xml.member "Message" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("Message", String.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.normal (fun f -> Query.Pair ("Normal", Boolean.to_query f))
         ; Util.option_map v.status_type (fun f ->
               Query.Pair ("StatusType", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.normal (fun f -> "normal", Boolean.to_json f)
         ; Util.option_map v.status_type (fun f -> "status_type", String.to_json f)
         ])

  let of_json j =
    { status_type = Util.option_map (Json.lookup j "status_type") String.of_json
    ; normal = Util.option_map (Json.lookup j "normal") Boolean.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; message = Util.option_map (Json.lookup j "message") String.of_json
    }
end

module DBParameterGroupStatus = struct
  type t =
    { d_b_parameter_group_name : String.t option
    ; parameter_apply_status : String.t option
    }

  let make ?d_b_parameter_group_name ?parameter_apply_status () =
    { d_b_parameter_group_name; parameter_apply_status }

  let parse xml =
    Some
      { d_b_parameter_group_name =
          Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse
      ; parameter_apply_status =
          Util.option_bind (Xml.member "ParameterApplyStatus" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.parameter_apply_status (fun f ->
               Query.Pair ("ParameterApplyStatus", String.to_query f))
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               Query.Pair ("DBParameterGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.parameter_apply_status (fun f ->
               "parameter_apply_status", String.to_json f)
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               "d_b_parameter_group_name", String.to_json f)
         ])

  let of_json j =
    { d_b_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_parameter_group_name") String.of_json
    ; parameter_apply_status =
        Util.option_map (Json.lookup j "parameter_apply_status") String.of_json
    }
end

module SubnetList = struct
  type t = Subnet.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Subnet.parse (Xml.members "Subnet" xml))

  let to_query v = Query.to_query_list Subnet.to_query v

  let to_json v = `List (List.map Subnet.to_json v)

  let of_json j = Json.to_list Subnet.of_json j
end

module DomainMembership = struct
  type t =
    { domain : String.t option
    ; status : String.t option
    ; f_q_d_n : String.t option
    ; i_a_m_role_name : String.t option
    }

  let make ?domain ?status ?f_q_d_n ?i_a_m_role_name () =
    { domain; status; f_q_d_n; i_a_m_role_name }

  let parse xml =
    Some
      { domain = Util.option_bind (Xml.member "Domain" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; f_q_d_n = Util.option_bind (Xml.member "FQDN" xml) String.parse
      ; i_a_m_role_name = Util.option_bind (Xml.member "IAMRoleName" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.i_a_m_role_name (fun f ->
               Query.Pair ("IAMRoleName", String.to_query f))
         ; Util.option_map v.f_q_d_n (fun f -> Query.Pair ("FQDN", String.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.domain (fun f -> Query.Pair ("Domain", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.i_a_m_role_name (fun f ->
               "i_a_m_role_name", String.to_json f)
         ; Util.option_map v.f_q_d_n (fun f -> "f_q_d_n", String.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.domain (fun f -> "domain", String.to_json f)
         ])

  let of_json j =
    { domain = Util.option_map (Json.lookup j "domain") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; f_q_d_n = Util.option_map (Json.lookup j "f_q_d_n") String.of_json
    ; i_a_m_role_name = Util.option_map (Json.lookup j "i_a_m_role_name") String.of_json
    }
end

module OptionGroupMembership = struct
  type t =
    { option_group_name : String.t option
    ; status : String.t option
    }

  let make ?option_group_name ?status () = { option_group_name; status }

  let parse xml =
    Some
      { option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ])

  let of_json j =
    { option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    }
end

module PendingCloudwatchLogsExports = struct
  type t =
    { log_types_to_enable : LogTypeList.t
    ; log_types_to_disable : LogTypeList.t
    }

  let make ?(log_types_to_enable = []) ?(log_types_to_disable = []) () =
    { log_types_to_enable; log_types_to_disable }

  let parse xml =
    Some
      { log_types_to_enable =
          Util.of_option
            []
            (Util.option_bind (Xml.member "LogTypesToEnable" xml) LogTypeList.parse)
      ; log_types_to_disable =
          Util.of_option
            []
            (Util.option_bind (Xml.member "LogTypesToDisable" xml) LogTypeList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("LogTypesToDisable.member", LogTypeList.to_query v.log_types_to_disable))
         ; Some
             (Query.Pair
                ("LogTypesToEnable.member", LogTypeList.to_query v.log_types_to_enable))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("log_types_to_disable", LogTypeList.to_json v.log_types_to_disable)
         ; Some ("log_types_to_enable", LogTypeList.to_json v.log_types_to_enable)
         ])

  let of_json j =
    { log_types_to_enable =
        LogTypeList.of_json (Util.of_option_exn (Json.lookup j "log_types_to_enable"))
    ; log_types_to_disable =
        LogTypeList.of_json (Util.of_option_exn (Json.lookup j "log_types_to_disable"))
    }
end

module ProcessorFeatureList = struct
  type t = ProcessorFeature.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map ProcessorFeature.parse (Xml.members "ProcessorFeature" xml))

  let to_query v = Query.to_query_list ProcessorFeature.to_query v

  let to_json v = `List (List.map ProcessorFeature.to_json v)

  let of_json j = Json.to_list ProcessorFeature.of_json j
end

module AvailableProcessorFeature = struct
  type t =
    { name : String.t option
    ; default_value : String.t option
    ; allowed_values : String.t option
    }

  let make ?name ?default_value ?allowed_values () =
    { name; default_value; allowed_values }

  let parse xml =
    Some
      { name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; default_value = Util.option_bind (Xml.member "DefaultValue" xml) String.parse
      ; allowed_values = Util.option_bind (Xml.member "AllowedValues" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.allowed_values (fun f ->
               Query.Pair ("AllowedValues", String.to_query f))
         ; Util.option_map v.default_value (fun f ->
               Query.Pair ("DefaultValue", String.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.allowed_values (fun f -> "allowed_values", String.to_json f)
         ; Util.option_map v.default_value (fun f -> "default_value", String.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ])

  let of_json j =
    { name = Util.option_map (Json.lookup j "name") String.of_json
    ; default_value = Util.option_map (Json.lookup j "default_value") String.of_json
    ; allowed_values = Util.option_map (Json.lookup j "allowed_values") String.of_json
    }
end

module DBClusterMember = struct
  type t =
    { d_b_instance_identifier : String.t option
    ; is_cluster_writer : Boolean.t option
    ; d_b_cluster_parameter_group_status : String.t option
    ; promotion_tier : Integer.t option
    }

  let make
      ?d_b_instance_identifier
      ?is_cluster_writer
      ?d_b_cluster_parameter_group_status
      ?promotion_tier
      () =
    { d_b_instance_identifier
    ; is_cluster_writer
    ; d_b_cluster_parameter_group_status
    ; promotion_tier
    }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse
      ; is_cluster_writer =
          Util.option_bind (Xml.member "IsClusterWriter" xml) Boolean.parse
      ; d_b_cluster_parameter_group_status =
          Util.option_bind (Xml.member "DBClusterParameterGroupStatus" xml) String.parse
      ; promotion_tier = Util.option_bind (Xml.member "PromotionTier" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.promotion_tier (fun f ->
               Query.Pair ("PromotionTier", Integer.to_query f))
         ; Util.option_map v.d_b_cluster_parameter_group_status (fun f ->
               Query.Pair ("DBClusterParameterGroupStatus", String.to_query f))
         ; Util.option_map v.is_cluster_writer (fun f ->
               Query.Pair ("IsClusterWriter", Boolean.to_query f))
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               Query.Pair ("DBInstanceIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.promotion_tier (fun f -> "promotion_tier", Integer.to_json f)
         ; Util.option_map v.d_b_cluster_parameter_group_status (fun f ->
               "d_b_cluster_parameter_group_status", String.to_json f)
         ; Util.option_map v.is_cluster_writer (fun f ->
               "is_cluster_writer", Boolean.to_json f)
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               "d_b_instance_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_instance_identifier =
        Util.option_map (Json.lookup j "d_b_instance_identifier") String.of_json
    ; is_cluster_writer =
        Util.option_map (Json.lookup j "is_cluster_writer") Boolean.of_json
    ; d_b_cluster_parameter_group_status =
        Util.option_map
          (Json.lookup j "d_b_cluster_parameter_group_status")
          String.of_json
    ; promotion_tier = Util.option_map (Json.lookup j "promotion_tier") Integer.of_json
    }
end

module DBClusterOptionGroupStatus = struct
  type t =
    { d_b_cluster_option_group_name : String.t option
    ; status : String.t option
    }

  let make ?d_b_cluster_option_group_name ?status () =
    { d_b_cluster_option_group_name; status }

  let parse xml =
    Some
      { d_b_cluster_option_group_name =
          Util.option_bind (Xml.member "DBClusterOptionGroupName" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.d_b_cluster_option_group_name (fun f ->
               Query.Pair ("DBClusterOptionGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.d_b_cluster_option_group_name (fun f ->
               "d_b_cluster_option_group_name", String.to_json f)
         ])

  let of_json j =
    { d_b_cluster_option_group_name =
        Util.option_map (Json.lookup j "d_b_cluster_option_group_name") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    }
end

module DBClusterRole = struct
  type t =
    { role_arn : String.t option
    ; status : String.t option
    ; feature_name : String.t option
    }

  let make ?role_arn ?status ?feature_name () = { role_arn; status; feature_name }

  let parse xml =
    Some
      { role_arn = Util.option_bind (Xml.member "RoleArn" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; feature_name = Util.option_bind (Xml.member "FeatureName" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.feature_name (fun f ->
               Query.Pair ("FeatureName", String.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.role_arn (fun f -> Query.Pair ("RoleArn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.feature_name (fun f -> "feature_name", String.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.role_arn (fun f -> "role_arn", String.to_json f)
         ])

  let of_json j =
    { role_arn = Util.option_map (Json.lookup j "role_arn") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; feature_name = Util.option_map (Json.lookup j "feature_name") String.of_json
    }
end

module EC2SecurityGroup = struct
  type t =
    { status : String.t option
    ; e_c2_security_group_name : String.t option
    ; e_c2_security_group_id : String.t option
    ; e_c2_security_group_owner_id : String.t option
    }

  let make
      ?status
      ?e_c2_security_group_name
      ?e_c2_security_group_id
      ?e_c2_security_group_owner_id
      () =
    { status
    ; e_c2_security_group_name
    ; e_c2_security_group_id
    ; e_c2_security_group_owner_id
    }

  let parse xml =
    Some
      { status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; e_c2_security_group_name =
          Util.option_bind (Xml.member "EC2SecurityGroupName" xml) String.parse
      ; e_c2_security_group_id =
          Util.option_bind (Xml.member "EC2SecurityGroupId" xml) String.parse
      ; e_c2_security_group_owner_id =
          Util.option_bind (Xml.member "EC2SecurityGroupOwnerId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.e_c2_security_group_owner_id (fun f ->
               Query.Pair ("EC2SecurityGroupOwnerId", String.to_query f))
         ; Util.option_map v.e_c2_security_group_id (fun f ->
               Query.Pair ("EC2SecurityGroupId", String.to_query f))
         ; Util.option_map v.e_c2_security_group_name (fun f ->
               Query.Pair ("EC2SecurityGroupName", String.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.e_c2_security_group_owner_id (fun f ->
               "e_c2_security_group_owner_id", String.to_json f)
         ; Util.option_map v.e_c2_security_group_id (fun f ->
               "e_c2_security_group_id", String.to_json f)
         ; Util.option_map v.e_c2_security_group_name (fun f ->
               "e_c2_security_group_name", String.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ])

  let of_json j =
    { status = Util.option_map (Json.lookup j "status") String.of_json
    ; e_c2_security_group_name =
        Util.option_map (Json.lookup j "e_c2_security_group_name") String.of_json
    ; e_c2_security_group_id =
        Util.option_map (Json.lookup j "e_c2_security_group_id") String.of_json
    ; e_c2_security_group_owner_id =
        Util.option_map (Json.lookup j "e_c2_security_group_owner_id") String.of_json
    }
end

module IPRange = struct
  type t =
    { status : String.t option
    ; c_i_d_r_i_p : String.t option
    }

  let make ?status ?c_i_d_r_i_p () = { status; c_i_d_r_i_p }

  let parse xml =
    Some
      { status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; c_i_d_r_i_p = Util.option_bind (Xml.member "CIDRIP" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.c_i_d_r_i_p (fun f ->
               Query.Pair ("CIDRIP", String.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.c_i_d_r_i_p (fun f -> "c_i_d_r_i_p", String.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ])

  let of_json j =
    { status = Util.option_map (Json.lookup j "status") String.of_json
    ; c_i_d_r_i_p = Util.option_map (Json.lookup j "c_i_d_r_i_p") String.of_json
    }
end

module OptionGroupOptionSettingsList = struct
  type t = OptionGroupOptionSetting.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map
         OptionGroupOptionSetting.parse
         (Xml.members "OptionGroupOptionSetting" xml))

  let to_query v = Query.to_query_list OptionGroupOptionSetting.to_query v

  let to_json v = `List (List.map OptionGroupOptionSetting.to_json v)

  let of_json j = Json.to_list OptionGroupOptionSetting.of_json j
end

module OptionGroupOptionVersionsList = struct
  type t = OptionVersion.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map OptionVersion.parse (Xml.members "OptionVersion" xml))

  let to_query v = Query.to_query_list OptionVersion.to_query v

  let to_json v = `List (List.map OptionVersion.to_json v)

  let of_json j = Json.to_list OptionVersion.of_json j
end

module OptionsConflictsWith = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map String.parse (Xml.members "OptionConflictName" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module OptionsDependedOn = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "OptionName" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module FilterValueList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "Value" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module ValidStorageOptions = struct
  type t =
    { storage_type : String.t option
    ; storage_size : RangeList.t
    ; provisioned_iops : RangeList.t
    ; iops_to_storage_ratio : DoubleRangeList.t
    ; supports_storage_autoscaling : Boolean.t option
    }

  let make
      ?storage_type
      ?(storage_size = [])
      ?(provisioned_iops = [])
      ?(iops_to_storage_ratio = [])
      ?supports_storage_autoscaling
      () =
    { storage_type
    ; storage_size
    ; provisioned_iops
    ; iops_to_storage_ratio
    ; supports_storage_autoscaling
    }

  let parse xml =
    Some
      { storage_type = Util.option_bind (Xml.member "StorageType" xml) String.parse
      ; storage_size =
          Util.of_option
            []
            (Util.option_bind (Xml.member "StorageSize" xml) RangeList.parse)
      ; provisioned_iops =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ProvisionedIops" xml) RangeList.parse)
      ; iops_to_storage_ratio =
          Util.of_option
            []
            (Util.option_bind (Xml.member "IopsToStorageRatio" xml) DoubleRangeList.parse)
      ; supports_storage_autoscaling =
          Util.option_bind (Xml.member "SupportsStorageAutoscaling" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.supports_storage_autoscaling (fun f ->
               Query.Pair ("SupportsStorageAutoscaling", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "IopsToStorageRatio.member"
                , DoubleRangeList.to_query v.iops_to_storage_ratio ))
         ; Some
             (Query.Pair ("ProvisionedIops.member", RangeList.to_query v.provisioned_iops))
         ; Some (Query.Pair ("StorageSize.member", RangeList.to_query v.storage_size))
         ; Util.option_map v.storage_type (fun f ->
               Query.Pair ("StorageType", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.supports_storage_autoscaling (fun f ->
               "supports_storage_autoscaling", Boolean.to_json f)
         ; Some ("iops_to_storage_ratio", DoubleRangeList.to_json v.iops_to_storage_ratio)
         ; Some ("provisioned_iops", RangeList.to_json v.provisioned_iops)
         ; Some ("storage_size", RangeList.to_json v.storage_size)
         ; Util.option_map v.storage_type (fun f -> "storage_type", String.to_json f)
         ])

  let of_json j =
    { storage_type = Util.option_map (Json.lookup j "storage_type") String.of_json
    ; storage_size = RangeList.of_json (Util.of_option_exn (Json.lookup j "storage_size"))
    ; provisioned_iops =
        RangeList.of_json (Util.of_option_exn (Json.lookup j "provisioned_iops"))
    ; iops_to_storage_ratio =
        DoubleRangeList.of_json
          (Util.of_option_exn (Json.lookup j "iops_to_storage_ratio"))
    ; supports_storage_autoscaling =
        Util.option_map (Json.lookup j "supports_storage_autoscaling") Boolean.of_json
    }
end

module VpnDetails = struct
  type t =
    { vpn_id : String.t option
    ; vpn_tunnel_originator_i_p : String.t option
    ; vpn_gateway_ip : String.t option
    ; vpn_p_s_k : String.t option
    ; vpn_name : String.t option
    ; vpn_state : String.t option
    }

  let make
      ?vpn_id
      ?vpn_tunnel_originator_i_p
      ?vpn_gateway_ip
      ?vpn_p_s_k
      ?vpn_name
      ?vpn_state
      () =
    { vpn_id; vpn_tunnel_originator_i_p; vpn_gateway_ip; vpn_p_s_k; vpn_name; vpn_state }

  let parse xml =
    Some
      { vpn_id = Util.option_bind (Xml.member "VpnId" xml) String.parse
      ; vpn_tunnel_originator_i_p =
          Util.option_bind (Xml.member "VpnTunnelOriginatorIP" xml) String.parse
      ; vpn_gateway_ip = Util.option_bind (Xml.member "VpnGatewayIp" xml) String.parse
      ; vpn_p_s_k = Util.option_bind (Xml.member "VpnPSK" xml) String.parse
      ; vpn_name = Util.option_bind (Xml.member "VpnName" xml) String.parse
      ; vpn_state = Util.option_bind (Xml.member "VpnState" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.vpn_state (fun f ->
               Query.Pair ("VpnState", String.to_query f))
         ; Util.option_map v.vpn_name (fun f -> Query.Pair ("VpnName", String.to_query f))
         ; Util.option_map v.vpn_p_s_k (fun f -> Query.Pair ("VpnPSK", String.to_query f))
         ; Util.option_map v.vpn_gateway_ip (fun f ->
               Query.Pair ("VpnGatewayIp", String.to_query f))
         ; Util.option_map v.vpn_tunnel_originator_i_p (fun f ->
               Query.Pair ("VpnTunnelOriginatorIP", String.to_query f))
         ; Util.option_map v.vpn_id (fun f -> Query.Pair ("VpnId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.vpn_state (fun f -> "vpn_state", String.to_json f)
         ; Util.option_map v.vpn_name (fun f -> "vpn_name", String.to_json f)
         ; Util.option_map v.vpn_p_s_k (fun f -> "vpn_p_s_k", String.to_json f)
         ; Util.option_map v.vpn_gateway_ip (fun f -> "vpn_gateway_ip", String.to_json f)
         ; Util.option_map v.vpn_tunnel_originator_i_p (fun f ->
               "vpn_tunnel_originator_i_p", String.to_json f)
         ; Util.option_map v.vpn_id (fun f -> "vpn_id", String.to_json f)
         ])

  let of_json j =
    { vpn_id = Util.option_map (Json.lookup j "vpn_id") String.of_json
    ; vpn_tunnel_originator_i_p =
        Util.option_map (Json.lookup j "vpn_tunnel_originator_i_p") String.of_json
    ; vpn_gateway_ip = Util.option_map (Json.lookup j "vpn_gateway_ip") String.of_json
    ; vpn_p_s_k = Util.option_map (Json.lookup j "vpn_p_s_k") String.of_json
    ; vpn_name = Util.option_map (Json.lookup j "vpn_name") String.of_json
    ; vpn_state = Util.option_map (Json.lookup j "vpn_state") String.of_json
    }
end

module Parameter = struct
  type t =
    { parameter_name : String.t option
    ; parameter_value : String.t option
    ; description : String.t option
    ; source : String.t option
    ; apply_type : String.t option
    ; data_type : String.t option
    ; allowed_values : String.t option
    ; is_modifiable : Boolean.t option
    ; minimum_engine_version : String.t option
    ; apply_method : ApplyMethod.t option
    ; supported_engine_modes : EngineModeList.t
    }

  let make
      ?parameter_name
      ?parameter_value
      ?description
      ?source
      ?apply_type
      ?data_type
      ?allowed_values
      ?is_modifiable
      ?minimum_engine_version
      ?apply_method
      ?(supported_engine_modes = [])
      () =
    { parameter_name
    ; parameter_value
    ; description
    ; source
    ; apply_type
    ; data_type
    ; allowed_values
    ; is_modifiable
    ; minimum_engine_version
    ; apply_method
    ; supported_engine_modes
    }

  let parse xml =
    Some
      { parameter_name = Util.option_bind (Xml.member "ParameterName" xml) String.parse
      ; parameter_value = Util.option_bind (Xml.member "ParameterValue" xml) String.parse
      ; description = Util.option_bind (Xml.member "Description" xml) String.parse
      ; source = Util.option_bind (Xml.member "Source" xml) String.parse
      ; apply_type = Util.option_bind (Xml.member "ApplyType" xml) String.parse
      ; data_type = Util.option_bind (Xml.member "DataType" xml) String.parse
      ; allowed_values = Util.option_bind (Xml.member "AllowedValues" xml) String.parse
      ; is_modifiable = Util.option_bind (Xml.member "IsModifiable" xml) Boolean.parse
      ; minimum_engine_version =
          Util.option_bind (Xml.member "MinimumEngineVersion" xml) String.parse
      ; apply_method = Util.option_bind (Xml.member "ApplyMethod" xml) ApplyMethod.parse
      ; supported_engine_modes =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "SupportedEngineModes" xml)
               EngineModeList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "SupportedEngineModes.member"
                , EngineModeList.to_query v.supported_engine_modes ))
         ; Util.option_map v.apply_method (fun f ->
               Query.Pair ("ApplyMethod", ApplyMethod.to_query f))
         ; Util.option_map v.minimum_engine_version (fun f ->
               Query.Pair ("MinimumEngineVersion", String.to_query f))
         ; Util.option_map v.is_modifiable (fun f ->
               Query.Pair ("IsModifiable", Boolean.to_query f))
         ; Util.option_map v.allowed_values (fun f ->
               Query.Pair ("AllowedValues", String.to_query f))
         ; Util.option_map v.data_type (fun f ->
               Query.Pair ("DataType", String.to_query f))
         ; Util.option_map v.apply_type (fun f ->
               Query.Pair ("ApplyType", String.to_query f))
         ; Util.option_map v.source (fun f -> Query.Pair ("Source", String.to_query f))
         ; Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ; Util.option_map v.parameter_value (fun f ->
               Query.Pair ("ParameterValue", String.to_query f))
         ; Util.option_map v.parameter_name (fun f ->
               Query.Pair ("ParameterName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("supported_engine_modes", EngineModeList.to_json v.supported_engine_modes)
         ; Util.option_map v.apply_method (fun f -> "apply_method", ApplyMethod.to_json f)
         ; Util.option_map v.minimum_engine_version (fun f ->
               "minimum_engine_version", String.to_json f)
         ; Util.option_map v.is_modifiable (fun f -> "is_modifiable", Boolean.to_json f)
         ; Util.option_map v.allowed_values (fun f -> "allowed_values", String.to_json f)
         ; Util.option_map v.data_type (fun f -> "data_type", String.to_json f)
         ; Util.option_map v.apply_type (fun f -> "apply_type", String.to_json f)
         ; Util.option_map v.source (fun f -> "source", String.to_json f)
         ; Util.option_map v.description (fun f -> "description", String.to_json f)
         ; Util.option_map v.parameter_value (fun f ->
               "parameter_value", String.to_json f)
         ; Util.option_map v.parameter_name (fun f -> "parameter_name", String.to_json f)
         ])

  let of_json j =
    { parameter_name = Util.option_map (Json.lookup j "parameter_name") String.of_json
    ; parameter_value = Util.option_map (Json.lookup j "parameter_value") String.of_json
    ; description = Util.option_map (Json.lookup j "description") String.of_json
    ; source = Util.option_map (Json.lookup j "source") String.of_json
    ; apply_type = Util.option_map (Json.lookup j "apply_type") String.of_json
    ; data_type = Util.option_map (Json.lookup j "data_type") String.of_json
    ; allowed_values = Util.option_map (Json.lookup j "allowed_values") String.of_json
    ; is_modifiable = Util.option_map (Json.lookup j "is_modifiable") Boolean.of_json
    ; minimum_engine_version =
        Util.option_map (Json.lookup j "minimum_engine_version") String.of_json
    ; apply_method = Util.option_map (Json.lookup j "apply_method") ApplyMethod.of_json
    ; supported_engine_modes =
        EngineModeList.of_json
          (Util.of_option_exn (Json.lookup j "supported_engine_modes"))
    }
end

module RecurringChargeList = struct
  type t = RecurringCharge.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map RecurringCharge.parse (Xml.members "RecurringCharge" xml))

  let to_query v = Query.to_query_list RecurringCharge.to_query v

  let to_json v = `List (List.map RecurringCharge.to_json v)

  let of_json j = Json.to_list RecurringCharge.of_json j
end

module DBClusterSnapshotAttribute = struct
  type t =
    { attribute_name : String.t option
    ; attribute_values : AttributeValueList.t
    }

  let make ?attribute_name ?(attribute_values = []) () =
    { attribute_name; attribute_values }

  let parse xml =
    Some
      { attribute_name = Util.option_bind (Xml.member "AttributeName" xml) String.parse
      ; attribute_values =
          Util.of_option
            []
            (Util.option_bind (Xml.member "AttributeValues" xml) AttributeValueList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("AttributeValues.member", AttributeValueList.to_query v.attribute_values))
         ; Util.option_map v.attribute_name (fun f ->
               Query.Pair ("AttributeName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("attribute_values", AttributeValueList.to_json v.attribute_values)
         ; Util.option_map v.attribute_name (fun f -> "attribute_name", String.to_json f)
         ])

  let of_json j =
    { attribute_name = Util.option_map (Json.lookup j "attribute_name") String.of_json
    ; attribute_values =
        AttributeValueList.of_json (Util.of_option_exn (Json.lookup j "attribute_values"))
    }
end

module PendingMaintenanceActionDetails = struct
  type t = PendingMaintenanceAction.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map
         PendingMaintenanceAction.parse
         (Xml.members "PendingMaintenanceAction" xml))

  let to_query v = Query.to_query_list PendingMaintenanceAction.to_query v

  let to_json v = `List (List.map PendingMaintenanceAction.to_json v)

  let of_json j = Json.to_list PendingMaintenanceAction.of_json j
end

module ConnectionPoolConfigurationInfo = struct
  type t =
    { max_connections_percent : Integer.t option
    ; max_idle_connections_percent : Integer.t option
    ; connection_borrow_timeout : Integer.t option
    ; session_pinning_filters : StringList.t
    ; init_query : String.t option
    }

  let make
      ?max_connections_percent
      ?max_idle_connections_percent
      ?connection_borrow_timeout
      ?(session_pinning_filters = [])
      ?init_query
      () =
    { max_connections_percent
    ; max_idle_connections_percent
    ; connection_borrow_timeout
    ; session_pinning_filters
    ; init_query
    }

  let parse xml =
    Some
      { max_connections_percent =
          Util.option_bind (Xml.member "MaxConnectionsPercent" xml) Integer.parse
      ; max_idle_connections_percent =
          Util.option_bind (Xml.member "MaxIdleConnectionsPercent" xml) Integer.parse
      ; connection_borrow_timeout =
          Util.option_bind (Xml.member "ConnectionBorrowTimeout" xml) Integer.parse
      ; session_pinning_filters =
          Util.of_option
            []
            (Util.option_bind (Xml.member "SessionPinningFilters" xml) StringList.parse)
      ; init_query = Util.option_bind (Xml.member "InitQuery" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.init_query (fun f ->
               Query.Pair ("InitQuery", String.to_query f))
         ; Some
             (Query.Pair
                ( "SessionPinningFilters.member"
                , StringList.to_query v.session_pinning_filters ))
         ; Util.option_map v.connection_borrow_timeout (fun f ->
               Query.Pair ("ConnectionBorrowTimeout", Integer.to_query f))
         ; Util.option_map v.max_idle_connections_percent (fun f ->
               Query.Pair ("MaxIdleConnectionsPercent", Integer.to_query f))
         ; Util.option_map v.max_connections_percent (fun f ->
               Query.Pair ("MaxConnectionsPercent", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.init_query (fun f -> "init_query", String.to_json f)
         ; Some ("session_pinning_filters", StringList.to_json v.session_pinning_filters)
         ; Util.option_map v.connection_borrow_timeout (fun f ->
               "connection_borrow_timeout", Integer.to_json f)
         ; Util.option_map v.max_idle_connections_percent (fun f ->
               "max_idle_connections_percent", Integer.to_json f)
         ; Util.option_map v.max_connections_percent (fun f ->
               "max_connections_percent", Integer.to_json f)
         ])

  let of_json j =
    { max_connections_percent =
        Util.option_map (Json.lookup j "max_connections_percent") Integer.of_json
    ; max_idle_connections_percent =
        Util.option_map (Json.lookup j "max_idle_connections_percent") Integer.of_json
    ; connection_borrow_timeout =
        Util.option_map (Json.lookup j "connection_borrow_timeout") Integer.of_json
    ; session_pinning_filters =
        StringList.of_json (Util.of_option_exn (Json.lookup j "session_pinning_filters"))
    ; init_query = Util.option_map (Json.lookup j "init_query") String.of_json
    }
end

module TargetHealth = struct
  type t =
    { state : TargetState.t option
    ; reason : TargetHealthReason.t option
    ; description : String.t option
    }

  let make ?state ?reason ?description () = { state; reason; description }

  let parse xml =
    Some
      { state = Util.option_bind (Xml.member "State" xml) TargetState.parse
      ; reason = Util.option_bind (Xml.member "Reason" xml) TargetHealthReason.parse
      ; description = Util.option_bind (Xml.member "Description" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ; Util.option_map v.reason (fun f ->
               Query.Pair ("Reason", TargetHealthReason.to_query f))
         ; Util.option_map v.state (fun f -> Query.Pair ("State", TargetState.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.description (fun f -> "description", String.to_json f)
         ; Util.option_map v.reason (fun f -> "reason", TargetHealthReason.to_json f)
         ; Util.option_map v.state (fun f -> "state", TargetState.to_json f)
         ])

  let of_json j =
    { state = Util.option_map (Json.lookup j "state") TargetState.of_json
    ; reason = Util.option_map (Json.lookup j "reason") TargetHealthReason.of_json
    ; description = Util.option_map (Json.lookup j "description") String.of_json
    }
end

module TargetType = struct
  type t =
    | RDS_INSTANCE
    | RDS_SERVERLESS_ENDPOINT
    | TRACKED_CLUSTER

  let str_to_t =
    [ "TRACKED_CLUSTER", TRACKED_CLUSTER
    ; "RDS_SERVERLESS_ENDPOINT", RDS_SERVERLESS_ENDPOINT
    ; "RDS_INSTANCE", RDS_INSTANCE
    ]

  let t_to_str =
    [ TRACKED_CLUSTER, "TRACKED_CLUSTER"
    ; RDS_SERVERLESS_ENDPOINT, "RDS_SERVERLESS_ENDPOINT"
    ; RDS_INSTANCE, "RDS_INSTANCE"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module FeatureNameList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module SupportedCharacterSetsList = struct
  type t = CharacterSet.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map CharacterSet.parse (Xml.members "CharacterSet" xml))

  let to_query v = Query.to_query_list CharacterSet.to_query v

  let to_json v = `List (List.map CharacterSet.to_json v)

  let of_json j = Json.to_list CharacterSet.of_json j
end

module SupportedTimezonesList = struct
  type t = Timezone.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Timezone.parse (Xml.members "Timezone" xml))

  let to_query v = Query.to_query_list Timezone.to_query v

  let to_json v = `List (List.map Timezone.to_json v)

  let of_json j = Json.to_list Timezone.of_json j
end

module ValidUpgradeTargetList = struct
  type t = UpgradeTarget.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map UpgradeTarget.parse (Xml.members "UpgradeTarget" xml))

  let to_query v = Query.to_query_list UpgradeTarget.to_query v

  let to_json v = `List (List.map UpgradeTarget.to_json v)

  let of_json j = Json.to_list UpgradeTarget.of_json j
end

module EventCategoriesList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map String.parse (Xml.members "EventCategory" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module GlobalClusterMemberList = struct
  type t = GlobalClusterMember.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map GlobalClusterMember.parse (Xml.members "GlobalClusterMember" xml))

  let to_query v = Query.to_query_list GlobalClusterMember.to_query v

  let to_json v = `List (List.map GlobalClusterMember.to_json v)

  let of_json j = Json.to_list GlobalClusterMember.of_json j
end

module OptionsList = struct
  type t = Option.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Option.parse (Xml.members "Option" xml))

  let to_query v = Query.to_query_list Option.to_query v

  let to_json v = `List (List.map Option.to_json v)

  let of_json j = Json.to_list Option.of_json j
end

module AvailabilityZones = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map String.parse (Xml.members "AvailabilityZone" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module TagList = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Tag.parse (Xml.members "Tag" xml))

  let to_query v = Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Json.to_list Tag.of_json j
end

module SourceIdsList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "SourceId" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module DBSecurityGroupNameList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map String.parse (Xml.members "DBSecurityGroupName" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module OptionSettingsList = struct
  type t = OptionSetting.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map OptionSetting.parse (Xml.members "OptionSetting" xml))

  let to_query v = Query.to_query_list OptionSetting.to_query v

  let to_json v = `List (List.map OptionSetting.to_json v)

  let of_json j = Json.to_list OptionSetting.of_json j
end

module VpcSecurityGroupIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map String.parse (Xml.members "VpcSecurityGroupId" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module DBSnapshotAttribute = struct
  type t =
    { attribute_name : String.t option
    ; attribute_values : AttributeValueList.t
    }

  let make ?attribute_name ?(attribute_values = []) () =
    { attribute_name; attribute_values }

  let parse xml =
    Some
      { attribute_name = Util.option_bind (Xml.member "AttributeName" xml) String.parse
      ; attribute_values =
          Util.of_option
            []
            (Util.option_bind (Xml.member "AttributeValues" xml) AttributeValueList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("AttributeValues.member", AttributeValueList.to_query v.attribute_values))
         ; Util.option_map v.attribute_name (fun f ->
               Query.Pair ("AttributeName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("attribute_values", AttributeValueList.to_json v.attribute_values)
         ; Util.option_map v.attribute_name (fun f -> "attribute_name", String.to_json f)
         ])

  let of_json j =
    { attribute_name = Util.option_map (Json.lookup j "attribute_name") String.of_json
    ; attribute_values =
        AttributeValueList.of_json (Util.of_option_exn (Json.lookup j "attribute_values"))
    }
end

module DBProxyStatus = struct
  type t =
    | Available
    | Modifying
    | Incompatible_network
    | Insufficient_resource_limits
    | Creating
    | Deleting
    | Suspended
    | Suspending
    | Reactivating

  let str_to_t =
    [ "reactivating", Reactivating
    ; "suspending", Suspending
    ; "suspended", Suspended
    ; "deleting", Deleting
    ; "creating", Creating
    ; "insufficient-resource-limits", Insufficient_resource_limits
    ; "incompatible-network", Incompatible_network
    ; "modifying", Modifying
    ; "available", Available
    ]

  let t_to_str =
    [ Reactivating, "reactivating"
    ; Suspending, "suspending"
    ; Suspended, "suspended"
    ; Deleting, "deleting"
    ; Creating, "creating"
    ; Insufficient_resource_limits, "insufficient-resource-limits"
    ; Incompatible_network, "incompatible-network"
    ; Modifying, "modifying"
    ; Available, "available"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module UserAuthConfigInfoList = struct
  type t = UserAuthConfigInfo.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map UserAuthConfigInfo.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list UserAuthConfigInfo.to_query v

  let to_json v = `List (List.map UserAuthConfigInfo.to_json v)

  let of_json j = Json.to_list UserAuthConfigInfo.of_json j
end

module DBInstanceRoles = struct
  type t = DBInstanceRole.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DBInstanceRole.parse (Xml.members "DBInstanceRole" xml))

  let to_query v = Query.to_query_list DBInstanceRole.to_query v

  let to_json v = `List (List.map DBInstanceRole.to_json v)

  let of_json j = Json.to_list DBInstanceRole.of_json j
end

module DBInstanceStatusInfoList = struct
  type t = DBInstanceStatusInfo.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map DBInstanceStatusInfo.parse (Xml.members "DBInstanceStatusInfo" xml))

  let to_query v = Query.to_query_list DBInstanceStatusInfo.to_query v

  let to_json v = `List (List.map DBInstanceStatusInfo.to_json v)

  let of_json j = Json.to_list DBInstanceStatusInfo.of_json j
end

module DBParameterGroupStatusList = struct
  type t = DBParameterGroupStatus.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map DBParameterGroupStatus.parse (Xml.members "DBParameterGroup" xml))

  let to_query v = Query.to_query_list DBParameterGroupStatus.to_query v

  let to_json v = `List (List.map DBParameterGroupStatus.to_json v)

  let of_json j = Json.to_list DBParameterGroupStatus.of_json j
end

module DBSubnetGroup = struct
  type t =
    { d_b_subnet_group_name : String.t option
    ; d_b_subnet_group_description : String.t option
    ; vpc_id : String.t option
    ; subnet_group_status : String.t option
    ; subnets : SubnetList.t
    ; d_b_subnet_group_arn : String.t option
    }

  let make
      ?d_b_subnet_group_name
      ?d_b_subnet_group_description
      ?vpc_id
      ?subnet_group_status
      ?(subnets = [])
      ?d_b_subnet_group_arn
      () =
    { d_b_subnet_group_name
    ; d_b_subnet_group_description
    ; vpc_id
    ; subnet_group_status
    ; subnets
    ; d_b_subnet_group_arn
    }

  let parse xml =
    Some
      { d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; d_b_subnet_group_description =
          Util.option_bind (Xml.member "DBSubnetGroupDescription" xml) String.parse
      ; vpc_id = Util.option_bind (Xml.member "VpcId" xml) String.parse
      ; subnet_group_status =
          Util.option_bind (Xml.member "SubnetGroupStatus" xml) String.parse
      ; subnets =
          Util.of_option [] (Util.option_bind (Xml.member "Subnets" xml) SubnetList.parse)
      ; d_b_subnet_group_arn =
          Util.option_bind (Xml.member "DBSubnetGroupArn" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_subnet_group_arn (fun f ->
               Query.Pair ("DBSubnetGroupArn", String.to_query f))
         ; Some (Query.Pair ("Subnets.member", SubnetList.to_query v.subnets))
         ; Util.option_map v.subnet_group_status (fun f ->
               Query.Pair ("SubnetGroupStatus", String.to_query f))
         ; Util.option_map v.vpc_id (fun f -> Query.Pair ("VpcId", String.to_query f))
         ; Util.option_map v.d_b_subnet_group_description (fun f ->
               Query.Pair ("DBSubnetGroupDescription", String.to_query f))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_subnet_group_arn (fun f ->
               "d_b_subnet_group_arn", String.to_json f)
         ; Some ("subnets", SubnetList.to_json v.subnets)
         ; Util.option_map v.subnet_group_status (fun f ->
               "subnet_group_status", String.to_json f)
         ; Util.option_map v.vpc_id (fun f -> "vpc_id", String.to_json f)
         ; Util.option_map v.d_b_subnet_group_description (fun f ->
               "d_b_subnet_group_description", String.to_json f)
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ])

  let of_json j =
    { d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; d_b_subnet_group_description =
        Util.option_map (Json.lookup j "d_b_subnet_group_description") String.of_json
    ; vpc_id = Util.option_map (Json.lookup j "vpc_id") String.of_json
    ; subnet_group_status =
        Util.option_map (Json.lookup j "subnet_group_status") String.of_json
    ; subnets = SubnetList.of_json (Util.of_option_exn (Json.lookup j "subnets"))
    ; d_b_subnet_group_arn =
        Util.option_map (Json.lookup j "d_b_subnet_group_arn") String.of_json
    }
end

module DomainMembershipList = struct
  type t = DomainMembership.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DomainMembership.parse (Xml.members "DomainMembership" xml))

  let to_query v = Query.to_query_list DomainMembership.to_query v

  let to_json v = `List (List.map DomainMembership.to_json v)

  let of_json j = Json.to_list DomainMembership.of_json j
end

module Endpoint = struct
  type t =
    { address : String.t option
    ; port : Integer.t option
    ; hosted_zone_id : String.t option
    }

  let make ?address ?port ?hosted_zone_id () = { address; port; hosted_zone_id }

  let parse xml =
    Some
      { address = Util.option_bind (Xml.member "Address" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; hosted_zone_id = Util.option_bind (Xml.member "HostedZoneId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.hosted_zone_id (fun f ->
               Query.Pair ("HostedZoneId", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.address (fun f -> Query.Pair ("Address", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.hosted_zone_id (fun f -> "hosted_zone_id", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.address (fun f -> "address", String.to_json f)
         ])

  let of_json j =
    { address = Util.option_map (Json.lookup j "address") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; hosted_zone_id = Util.option_map (Json.lookup j "hosted_zone_id") String.of_json
    }
end

module OptionGroupMembershipList = struct
  type t = OptionGroupMembership.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map OptionGroupMembership.parse (Xml.members "OptionGroupMembership" xml))

  let to_query v = Query.to_query_list OptionGroupMembership.to_query v

  let to_json v = `List (List.map OptionGroupMembership.to_json v)

  let of_json j = Json.to_list OptionGroupMembership.of_json j
end

module PendingModifiedValues = struct
  type t =
    { d_b_instance_class : String.t option
    ; allocated_storage : Integer.t option
    ; master_user_password : String.t option
    ; port : Integer.t option
    ; backup_retention_period : Integer.t option
    ; multi_a_z : Boolean.t option
    ; engine_version : String.t option
    ; license_model : String.t option
    ; iops : Integer.t option
    ; d_b_instance_identifier : String.t option
    ; storage_type : String.t option
    ; c_a_certificate_identifier : String.t option
    ; d_b_subnet_group_name : String.t option
    ; pending_cloudwatch_logs_exports : PendingCloudwatchLogsExports.t option
    ; processor_features : ProcessorFeatureList.t
    }

  let make
      ?d_b_instance_class
      ?allocated_storage
      ?master_user_password
      ?port
      ?backup_retention_period
      ?multi_a_z
      ?engine_version
      ?license_model
      ?iops
      ?d_b_instance_identifier
      ?storage_type
      ?c_a_certificate_identifier
      ?d_b_subnet_group_name
      ?pending_cloudwatch_logs_exports
      ?(processor_features = [])
      () =
    { d_b_instance_class
    ; allocated_storage
    ; master_user_password
    ; port
    ; backup_retention_period
    ; multi_a_z
    ; engine_version
    ; license_model
    ; iops
    ; d_b_instance_identifier
    ; storage_type
    ; c_a_certificate_identifier
    ; d_b_subnet_group_name
    ; pending_cloudwatch_logs_exports
    ; processor_features
    }

  let parse xml =
    Some
      { d_b_instance_class =
          Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse
      ; allocated_storage =
          Util.option_bind (Xml.member "AllocatedStorage" xml) Integer.parse
      ; master_user_password =
          Util.option_bind (Xml.member "MasterUserPassword" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; backup_retention_period =
          Util.option_bind (Xml.member "BackupRetentionPeriod" xml) Integer.parse
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; license_model = Util.option_bind (Xml.member "LicenseModel" xml) String.parse
      ; iops = Util.option_bind (Xml.member "Iops" xml) Integer.parse
      ; d_b_instance_identifier =
          Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse
      ; storage_type = Util.option_bind (Xml.member "StorageType" xml) String.parse
      ; c_a_certificate_identifier =
          Util.option_bind (Xml.member "CACertificateIdentifier" xml) String.parse
      ; d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; pending_cloudwatch_logs_exports =
          Util.option_bind
            (Xml.member "PendingCloudwatchLogsExports" xml)
            PendingCloudwatchLogsExports.parse
      ; processor_features =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ProcessorFeatures" xml)
               ProcessorFeatureList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "ProcessorFeatures.member"
                , ProcessorFeatureList.to_query v.processor_features ))
         ; Util.option_map v.pending_cloudwatch_logs_exports (fun f ->
               Query.Pair
                 ("PendingCloudwatchLogsExports", PendingCloudwatchLogsExports.to_query f))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ; Util.option_map v.c_a_certificate_identifier (fun f ->
               Query.Pair ("CACertificateIdentifier", String.to_query f))
         ; Util.option_map v.storage_type (fun f ->
               Query.Pair ("StorageType", String.to_query f))
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               Query.Pair ("DBInstanceIdentifier", String.to_query f))
         ; Util.option_map v.iops (fun f -> Query.Pair ("Iops", Integer.to_query f))
         ; Util.option_map v.license_model (fun f ->
               Query.Pair ("LicenseModel", String.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Util.option_map v.backup_retention_period (fun f ->
               Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.master_user_password (fun f ->
               Query.Pair ("MasterUserPassword", String.to_query f))
         ; Util.option_map v.allocated_storage (fun f ->
               Query.Pair ("AllocatedStorage", Integer.to_query f))
         ; Util.option_map v.d_b_instance_class (fun f ->
               Query.Pair ("DBInstanceClass", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("processor_features", ProcessorFeatureList.to_json v.processor_features)
         ; Util.option_map v.pending_cloudwatch_logs_exports (fun f ->
               "pending_cloudwatch_logs_exports", PendingCloudwatchLogsExports.to_json f)
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ; Util.option_map v.c_a_certificate_identifier (fun f ->
               "c_a_certificate_identifier", String.to_json f)
         ; Util.option_map v.storage_type (fun f -> "storage_type", String.to_json f)
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               "d_b_instance_identifier", String.to_json f)
         ; Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
         ; Util.option_map v.license_model (fun f -> "license_model", String.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Util.option_map v.backup_retention_period (fun f ->
               "backup_retention_period", Integer.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.master_user_password (fun f ->
               "master_user_password", String.to_json f)
         ; Util.option_map v.allocated_storage (fun f ->
               "allocated_storage", Integer.to_json f)
         ; Util.option_map v.d_b_instance_class (fun f ->
               "d_b_instance_class", String.to_json f)
         ])

  let of_json j =
    { d_b_instance_class =
        Util.option_map (Json.lookup j "d_b_instance_class") String.of_json
    ; allocated_storage =
        Util.option_map (Json.lookup j "allocated_storage") Integer.of_json
    ; master_user_password =
        Util.option_map (Json.lookup j "master_user_password") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; backup_retention_period =
        Util.option_map (Json.lookup j "backup_retention_period") Integer.of_json
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; license_model = Util.option_map (Json.lookup j "license_model") String.of_json
    ; iops = Util.option_map (Json.lookup j "iops") Integer.of_json
    ; d_b_instance_identifier =
        Util.option_map (Json.lookup j "d_b_instance_identifier") String.of_json
    ; storage_type = Util.option_map (Json.lookup j "storage_type") String.of_json
    ; c_a_certificate_identifier =
        Util.option_map (Json.lookup j "c_a_certificate_identifier") String.of_json
    ; d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; pending_cloudwatch_logs_exports =
        Util.option_map
          (Json.lookup j "pending_cloudwatch_logs_exports")
          PendingCloudwatchLogsExports.of_json
    ; processor_features =
        ProcessorFeatureList.of_json
          (Util.of_option_exn (Json.lookup j "processor_features"))
    }
end

module ReadReplicaDBClusterIdentifierList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map String.parse (Xml.members "ReadReplicaDBClusterIdentifier" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module ReadReplicaDBInstanceIdentifierList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map String.parse (Xml.members "ReadReplicaDBInstanceIdentifier" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module ReplicaMode = struct
  type t =
    | Open_read_only
    | Mounted

  let str_to_t = [ "mounted", Mounted; "open-read-only", Open_read_only ]

  let t_to_str = [ Mounted, "mounted"; Open_read_only, "open-read-only" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module InstallationMediaFailureCause = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "Message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("Message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module RestoreWindow = struct
  type t =
    { earliest_time : DateTime.t option
    ; latest_time : DateTime.t option
    }

  let make ?earliest_time ?latest_time () = { earliest_time; latest_time }

  let parse xml =
    Some
      { earliest_time = Util.option_bind (Xml.member "EarliestTime" xml) DateTime.parse
      ; latest_time = Util.option_bind (Xml.member "LatestTime" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.latest_time (fun f ->
               Query.Pair ("LatestTime", DateTime.to_query f))
         ; Util.option_map v.earliest_time (fun f ->
               Query.Pair ("EarliestTime", DateTime.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.latest_time (fun f -> "latest_time", DateTime.to_json f)
         ; Util.option_map v.earliest_time (fun f -> "earliest_time", DateTime.to_json f)
         ])

  let of_json j =
    { earliest_time = Util.option_map (Json.lookup j "earliest_time") DateTime.of_json
    ; latest_time = Util.option_map (Json.lookup j "latest_time") DateTime.of_json
    }
end

module SourceType = struct
  type t =
    | Db_instance
    | Db_parameter_group
    | Db_security_group
    | Db_snapshot
    | Db_cluster
    | Db_cluster_snapshot

  let str_to_t =
    [ "db-cluster-snapshot", Db_cluster_snapshot
    ; "db-cluster", Db_cluster
    ; "db-snapshot", Db_snapshot
    ; "db-security-group", Db_security_group
    ; "db-parameter-group", Db_parameter_group
    ; "db-instance", Db_instance
    ]

  let t_to_str =
    [ Db_cluster_snapshot, "db-cluster-snapshot"
    ; Db_cluster, "db-cluster"
    ; Db_snapshot, "db-snapshot"
    ; Db_security_group, "db-security-group"
    ; Db_parameter_group, "db-parameter-group"
    ; Db_instance, "db-instance"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module AvailabilityZoneList = struct
  type t = AvailabilityZone.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map AvailabilityZone.parse (Xml.members "AvailabilityZone" xml))

  let to_query v = Query.to_query_list AvailabilityZone.to_query v

  let to_json v = `List (List.map AvailabilityZone.to_json v)

  let of_json j = Json.to_list AvailabilityZone.of_json j
end

module AvailableProcessorFeatureList = struct
  type t = AvailableProcessorFeature.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map
         AvailableProcessorFeature.parse
         (Xml.members "AvailableProcessorFeature" xml))

  let to_query v = Query.to_query_list AvailableProcessorFeature.to_query v

  let to_json v = `List (List.map AvailableProcessorFeature.to_json v)

  let of_json j = Json.to_list AvailableProcessorFeature.of_json j
end

module ActivityStreamMode = struct
  type t =
    | Sync
    | Async

  let str_to_t = [ "async", Async; "sync", Sync ]

  let t_to_str = [ Async, "async"; Sync, "sync" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ActivityStreamStatus = struct
  type t =
    | Stopped
    | Starting
    | Started
    | Stopping

  let str_to_t =
    [ "stopping", Stopping; "started", Started; "starting", Starting; "stopped", Stopped ]

  let t_to_str =
    [ Stopping, "stopping"; Started, "started"; Starting, "starting"; Stopped, "stopped" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module DBClusterMemberList = struct
  type t = DBClusterMember.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DBClusterMember.parse (Xml.members "DBClusterMember" xml))

  let to_query v = Query.to_query_list DBClusterMember.to_query v

  let to_json v = `List (List.map DBClusterMember.to_json v)

  let of_json j = Json.to_list DBClusterMember.of_json j
end

module DBClusterOptionGroupMemberships = struct
  type t = DBClusterOptionGroupStatus.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map DBClusterOptionGroupStatus.parse (Xml.members "DBClusterOptionGroup" xml))

  let to_query v = Query.to_query_list DBClusterOptionGroupStatus.to_query v

  let to_json v = `List (List.map DBClusterOptionGroupStatus.to_json v)

  let of_json j = Json.to_list DBClusterOptionGroupStatus.of_json j
end

module DBClusterRoles = struct
  type t = DBClusterRole.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DBClusterRole.parse (Xml.members "DBClusterRole" xml))

  let to_query v = Query.to_query_list DBClusterRole.to_query v

  let to_json v = `List (List.map DBClusterRole.to_json v)

  let of_json j = Json.to_list DBClusterRole.of_json j
end

module ReadReplicaIdentifierList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map String.parse (Xml.members "ReadReplicaIdentifier" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module ScalingConfigurationInfo = struct
  type t =
    { min_capacity : Integer.t option
    ; max_capacity : Integer.t option
    ; auto_pause : Boolean.t option
    ; seconds_until_auto_pause : Integer.t option
    ; timeout_action : String.t option
    }

  let make
      ?min_capacity
      ?max_capacity
      ?auto_pause
      ?seconds_until_auto_pause
      ?timeout_action
      () =
    { min_capacity; max_capacity; auto_pause; seconds_until_auto_pause; timeout_action }

  let parse xml =
    Some
      { min_capacity = Util.option_bind (Xml.member "MinCapacity" xml) Integer.parse
      ; max_capacity = Util.option_bind (Xml.member "MaxCapacity" xml) Integer.parse
      ; auto_pause = Util.option_bind (Xml.member "AutoPause" xml) Boolean.parse
      ; seconds_until_auto_pause =
          Util.option_bind (Xml.member "SecondsUntilAutoPause" xml) Integer.parse
      ; timeout_action = Util.option_bind (Xml.member "TimeoutAction" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.timeout_action (fun f ->
               Query.Pair ("TimeoutAction", String.to_query f))
         ; Util.option_map v.seconds_until_auto_pause (fun f ->
               Query.Pair ("SecondsUntilAutoPause", Integer.to_query f))
         ; Util.option_map v.auto_pause (fun f ->
               Query.Pair ("AutoPause", Boolean.to_query f))
         ; Util.option_map v.max_capacity (fun f ->
               Query.Pair ("MaxCapacity", Integer.to_query f))
         ; Util.option_map v.min_capacity (fun f ->
               Query.Pair ("MinCapacity", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.timeout_action (fun f -> "timeout_action", String.to_json f)
         ; Util.option_map v.seconds_until_auto_pause (fun f ->
               "seconds_until_auto_pause", Integer.to_json f)
         ; Util.option_map v.auto_pause (fun f -> "auto_pause", Boolean.to_json f)
         ; Util.option_map v.max_capacity (fun f -> "max_capacity", Integer.to_json f)
         ; Util.option_map v.min_capacity (fun f -> "min_capacity", Integer.to_json f)
         ])

  let of_json j =
    { min_capacity = Util.option_map (Json.lookup j "min_capacity") Integer.of_json
    ; max_capacity = Util.option_map (Json.lookup j "max_capacity") Integer.of_json
    ; auto_pause = Util.option_map (Json.lookup j "auto_pause") Boolean.of_json
    ; seconds_until_auto_pause =
        Util.option_map (Json.lookup j "seconds_until_auto_pause") Integer.of_json
    ; timeout_action = Util.option_map (Json.lookup j "timeout_action") String.of_json
    }
end

module EC2SecurityGroupList = struct
  type t = EC2SecurityGroup.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map EC2SecurityGroup.parse (Xml.members "EC2SecurityGroup" xml))

  let to_query v = Query.to_query_list EC2SecurityGroup.to_query v

  let to_json v = `List (List.map EC2SecurityGroup.to_json v)

  let of_json j = Json.to_list EC2SecurityGroup.of_json j
end

module IPRangeList = struct
  type t = IPRange.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map IPRange.parse (Xml.members "IPRange" xml))

  let to_query v = Query.to_query_list IPRange.to_query v

  let to_json v = `List (List.map IPRange.to_json v)

  let of_json j = Json.to_list IPRange.of_json j
end

module OptionGroupOption = struct
  type t =
    { name : String.t option
    ; description : String.t option
    ; engine_name : String.t option
    ; major_engine_version : String.t option
    ; minimum_required_minor_engine_version : String.t option
    ; port_required : Boolean.t option
    ; default_port : Integer.t option
    ; options_depended_on : OptionsDependedOn.t
    ; options_conflicts_with : OptionsConflictsWith.t
    ; persistent : Boolean.t option
    ; permanent : Boolean.t option
    ; requires_auto_minor_engine_version_upgrade : Boolean.t option
    ; vpc_only : Boolean.t option
    ; supports_option_version_downgrade : Boolean.t option
    ; option_group_option_settings : OptionGroupOptionSettingsList.t
    ; option_group_option_versions : OptionGroupOptionVersionsList.t
    }

  let make
      ?name
      ?description
      ?engine_name
      ?major_engine_version
      ?minimum_required_minor_engine_version
      ?port_required
      ?default_port
      ?(options_depended_on = [])
      ?(options_conflicts_with = [])
      ?persistent
      ?permanent
      ?requires_auto_minor_engine_version_upgrade
      ?vpc_only
      ?supports_option_version_downgrade
      ?(option_group_option_settings = [])
      ?(option_group_option_versions = [])
      () =
    { name
    ; description
    ; engine_name
    ; major_engine_version
    ; minimum_required_minor_engine_version
    ; port_required
    ; default_port
    ; options_depended_on
    ; options_conflicts_with
    ; persistent
    ; permanent
    ; requires_auto_minor_engine_version_upgrade
    ; vpc_only
    ; supports_option_version_downgrade
    ; option_group_option_settings
    ; option_group_option_versions
    }

  let parse xml =
    Some
      { name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; description = Util.option_bind (Xml.member "Description" xml) String.parse
      ; engine_name = Util.option_bind (Xml.member "EngineName" xml) String.parse
      ; major_engine_version =
          Util.option_bind (Xml.member "MajorEngineVersion" xml) String.parse
      ; minimum_required_minor_engine_version =
          Util.option_bind
            (Xml.member "MinimumRequiredMinorEngineVersion" xml)
            String.parse
      ; port_required = Util.option_bind (Xml.member "PortRequired" xml) Boolean.parse
      ; default_port = Util.option_bind (Xml.member "DefaultPort" xml) Integer.parse
      ; options_depended_on =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "OptionsDependedOn" xml)
               OptionsDependedOn.parse)
      ; options_conflicts_with =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "OptionsConflictsWith" xml)
               OptionsConflictsWith.parse)
      ; persistent = Util.option_bind (Xml.member "Persistent" xml) Boolean.parse
      ; permanent = Util.option_bind (Xml.member "Permanent" xml) Boolean.parse
      ; requires_auto_minor_engine_version_upgrade =
          Util.option_bind
            (Xml.member "RequiresAutoMinorEngineVersionUpgrade" xml)
            Boolean.parse
      ; vpc_only = Util.option_bind (Xml.member "VpcOnly" xml) Boolean.parse
      ; supports_option_version_downgrade =
          Util.option_bind (Xml.member "SupportsOptionVersionDowngrade" xml) Boolean.parse
      ; option_group_option_settings =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "OptionGroupOptionSettings" xml)
               OptionGroupOptionSettingsList.parse)
      ; option_group_option_versions =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "OptionGroupOptionVersions" xml)
               OptionGroupOptionVersionsList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "OptionGroupOptionVersions.member"
                , OptionGroupOptionVersionsList.to_query v.option_group_option_versions ))
         ; Some
             (Query.Pair
                ( "OptionGroupOptionSettings.member"
                , OptionGroupOptionSettingsList.to_query v.option_group_option_settings ))
         ; Util.option_map v.supports_option_version_downgrade (fun f ->
               Query.Pair ("SupportsOptionVersionDowngrade", Boolean.to_query f))
         ; Util.option_map v.vpc_only (fun f ->
               Query.Pair ("VpcOnly", Boolean.to_query f))
         ; Util.option_map v.requires_auto_minor_engine_version_upgrade (fun f ->
               Query.Pair ("RequiresAutoMinorEngineVersionUpgrade", Boolean.to_query f))
         ; Util.option_map v.permanent (fun f ->
               Query.Pair ("Permanent", Boolean.to_query f))
         ; Util.option_map v.persistent (fun f ->
               Query.Pair ("Persistent", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "OptionsConflictsWith.member"
                , OptionsConflictsWith.to_query v.options_conflicts_with ))
         ; Some
             (Query.Pair
                ( "OptionsDependedOn.member"
                , OptionsDependedOn.to_query v.options_depended_on ))
         ; Util.option_map v.default_port (fun f ->
               Query.Pair ("DefaultPort", Integer.to_query f))
         ; Util.option_map v.port_required (fun f ->
               Query.Pair ("PortRequired", Boolean.to_query f))
         ; Util.option_map v.minimum_required_minor_engine_version (fun f ->
               Query.Pair ("MinimumRequiredMinorEngineVersion", String.to_query f))
         ; Util.option_map v.major_engine_version (fun f ->
               Query.Pair ("MajorEngineVersion", String.to_query f))
         ; Util.option_map v.engine_name (fun f ->
               Query.Pair ("EngineName", String.to_query f))
         ; Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "option_group_option_versions"
             , OptionGroupOptionVersionsList.to_json v.option_group_option_versions )
         ; Some
             ( "option_group_option_settings"
             , OptionGroupOptionSettingsList.to_json v.option_group_option_settings )
         ; Util.option_map v.supports_option_version_downgrade (fun f ->
               "supports_option_version_downgrade", Boolean.to_json f)
         ; Util.option_map v.vpc_only (fun f -> "vpc_only", Boolean.to_json f)
         ; Util.option_map v.requires_auto_minor_engine_version_upgrade (fun f ->
               "requires_auto_minor_engine_version_upgrade", Boolean.to_json f)
         ; Util.option_map v.permanent (fun f -> "permanent", Boolean.to_json f)
         ; Util.option_map v.persistent (fun f -> "persistent", Boolean.to_json f)
         ; Some
             ( "options_conflicts_with"
             , OptionsConflictsWith.to_json v.options_conflicts_with )
         ; Some ("options_depended_on", OptionsDependedOn.to_json v.options_depended_on)
         ; Util.option_map v.default_port (fun f -> "default_port", Integer.to_json f)
         ; Util.option_map v.port_required (fun f -> "port_required", Boolean.to_json f)
         ; Util.option_map v.minimum_required_minor_engine_version (fun f ->
               "minimum_required_minor_engine_version", String.to_json f)
         ; Util.option_map v.major_engine_version (fun f ->
               "major_engine_version", String.to_json f)
         ; Util.option_map v.engine_name (fun f -> "engine_name", String.to_json f)
         ; Util.option_map v.description (fun f -> "description", String.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ])

  let of_json j =
    { name = Util.option_map (Json.lookup j "name") String.of_json
    ; description = Util.option_map (Json.lookup j "description") String.of_json
    ; engine_name = Util.option_map (Json.lookup j "engine_name") String.of_json
    ; major_engine_version =
        Util.option_map (Json.lookup j "major_engine_version") String.of_json
    ; minimum_required_minor_engine_version =
        Util.option_map
          (Json.lookup j "minimum_required_minor_engine_version")
          String.of_json
    ; port_required = Util.option_map (Json.lookup j "port_required") Boolean.of_json
    ; default_port = Util.option_map (Json.lookup j "default_port") Integer.of_json
    ; options_depended_on =
        OptionsDependedOn.of_json
          (Util.of_option_exn (Json.lookup j "options_depended_on"))
    ; options_conflicts_with =
        OptionsConflictsWith.of_json
          (Util.of_option_exn (Json.lookup j "options_conflicts_with"))
    ; persistent = Util.option_map (Json.lookup j "persistent") Boolean.of_json
    ; permanent = Util.option_map (Json.lookup j "permanent") Boolean.of_json
    ; requires_auto_minor_engine_version_upgrade =
        Util.option_map
          (Json.lookup j "requires_auto_minor_engine_version_upgrade")
          Boolean.of_json
    ; vpc_only = Util.option_map (Json.lookup j "vpc_only") Boolean.of_json
    ; supports_option_version_downgrade =
        Util.option_map
          (Json.lookup j "supports_option_version_downgrade")
          Boolean.of_json
    ; option_group_option_settings =
        OptionGroupOptionSettingsList.of_json
          (Util.of_option_exn (Json.lookup j "option_group_option_settings"))
    ; option_group_option_versions =
        OptionGroupOptionVersionsList.of_json
          (Util.of_option_exn (Json.lookup j "option_group_option_versions"))
    }
end

module Filter = struct
  type t =
    { name : String.t
    ; values : FilterValueList.t
    }

  let make ~name ~values () = { name; values }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; values =
          Xml.required
            "Values"
            (Util.option_bind (Xml.member "Values" xml) FilterValueList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Values.member", FilterValueList.to_query v.values))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("values", FilterValueList.to_json v.values)
         ; Some ("name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; values = FilterValueList.of_json (Util.of_option_exn (Json.lookup j "values"))
    }
end

module ValidStorageOptionsList = struct
  type t = ValidStorageOptions.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map ValidStorageOptions.parse (Xml.members "ValidStorageOptions" xml))

  let to_query v = Query.to_query_list ValidStorageOptions.to_query v

  let to_json v = `List (List.map ValidStorageOptions.to_json v)

  let of_json j = Json.to_list ValidStorageOptions.of_json j
end

module CustomAvailabilityZone = struct
  type t =
    { custom_availability_zone_id : String.t option
    ; custom_availability_zone_name : String.t option
    ; custom_availability_zone_status : String.t option
    ; vpn_details : VpnDetails.t option
    }

  let make
      ?custom_availability_zone_id
      ?custom_availability_zone_name
      ?custom_availability_zone_status
      ?vpn_details
      () =
    { custom_availability_zone_id
    ; custom_availability_zone_name
    ; custom_availability_zone_status
    ; vpn_details
    }

  let parse xml =
    Some
      { custom_availability_zone_id =
          Util.option_bind (Xml.member "CustomAvailabilityZoneId" xml) String.parse
      ; custom_availability_zone_name =
          Util.option_bind (Xml.member "CustomAvailabilityZoneName" xml) String.parse
      ; custom_availability_zone_status =
          Util.option_bind (Xml.member "CustomAvailabilityZoneStatus" xml) String.parse
      ; vpn_details = Util.option_bind (Xml.member "VpnDetails" xml) VpnDetails.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.vpn_details (fun f ->
               Query.Pair ("VpnDetails", VpnDetails.to_query f))
         ; Util.option_map v.custom_availability_zone_status (fun f ->
               Query.Pair ("CustomAvailabilityZoneStatus", String.to_query f))
         ; Util.option_map v.custom_availability_zone_name (fun f ->
               Query.Pair ("CustomAvailabilityZoneName", String.to_query f))
         ; Util.option_map v.custom_availability_zone_id (fun f ->
               Query.Pair ("CustomAvailabilityZoneId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.vpn_details (fun f -> "vpn_details", VpnDetails.to_json f)
         ; Util.option_map v.custom_availability_zone_status (fun f ->
               "custom_availability_zone_status", String.to_json f)
         ; Util.option_map v.custom_availability_zone_name (fun f ->
               "custom_availability_zone_name", String.to_json f)
         ; Util.option_map v.custom_availability_zone_id (fun f ->
               "custom_availability_zone_id", String.to_json f)
         ])

  let of_json j =
    { custom_availability_zone_id =
        Util.option_map (Json.lookup j "custom_availability_zone_id") String.of_json
    ; custom_availability_zone_name =
        Util.option_map (Json.lookup j "custom_availability_zone_name") String.of_json
    ; custom_availability_zone_status =
        Util.option_map (Json.lookup j "custom_availability_zone_status") String.of_json
    ; vpn_details = Util.option_map (Json.lookup j "vpn_details") VpnDetails.of_json
    }
end

module ParametersList = struct
  type t = Parameter.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Parameter.parse (Xml.members "Parameter" xml))

  let to_query v = Query.to_query_list Parameter.to_query v

  let to_json v = `List (List.map Parameter.to_json v)

  let of_json j = Json.to_list Parameter.of_json j
end

module ReservedDBInstance = struct
  type t =
    { reserved_d_b_instance_id : String.t option
    ; reserved_d_b_instances_offering_id : String.t option
    ; d_b_instance_class : String.t option
    ; start_time : DateTime.t option
    ; duration : Integer.t option
    ; fixed_price : Double.t option
    ; usage_price : Double.t option
    ; currency_code : String.t option
    ; d_b_instance_count : Integer.t option
    ; product_description : String.t option
    ; offering_type : String.t option
    ; multi_a_z : Boolean.t option
    ; state : String.t option
    ; recurring_charges : RecurringChargeList.t
    ; reserved_d_b_instance_arn : String.t option
    ; lease_id : String.t option
    }

  let make
      ?reserved_d_b_instance_id
      ?reserved_d_b_instances_offering_id
      ?d_b_instance_class
      ?start_time
      ?duration
      ?fixed_price
      ?usage_price
      ?currency_code
      ?d_b_instance_count
      ?product_description
      ?offering_type
      ?multi_a_z
      ?state
      ?(recurring_charges = [])
      ?reserved_d_b_instance_arn
      ?lease_id
      () =
    { reserved_d_b_instance_id
    ; reserved_d_b_instances_offering_id
    ; d_b_instance_class
    ; start_time
    ; duration
    ; fixed_price
    ; usage_price
    ; currency_code
    ; d_b_instance_count
    ; product_description
    ; offering_type
    ; multi_a_z
    ; state
    ; recurring_charges
    ; reserved_d_b_instance_arn
    ; lease_id
    }

  let parse xml =
    Some
      { reserved_d_b_instance_id =
          Util.option_bind (Xml.member "ReservedDBInstanceId" xml) String.parse
      ; reserved_d_b_instances_offering_id =
          Util.option_bind (Xml.member "ReservedDBInstancesOfferingId" xml) String.parse
      ; d_b_instance_class =
          Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse
      ; start_time = Util.option_bind (Xml.member "StartTime" xml) DateTime.parse
      ; duration = Util.option_bind (Xml.member "Duration" xml) Integer.parse
      ; fixed_price = Util.option_bind (Xml.member "FixedPrice" xml) Double.parse
      ; usage_price = Util.option_bind (Xml.member "UsagePrice" xml) Double.parse
      ; currency_code = Util.option_bind (Xml.member "CurrencyCode" xml) String.parse
      ; d_b_instance_count =
          Util.option_bind (Xml.member "DBInstanceCount" xml) Integer.parse
      ; product_description =
          Util.option_bind (Xml.member "ProductDescription" xml) String.parse
      ; offering_type = Util.option_bind (Xml.member "OfferingType" xml) String.parse
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; state = Util.option_bind (Xml.member "State" xml) String.parse
      ; recurring_charges =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "RecurringCharges" xml)
               RecurringChargeList.parse)
      ; reserved_d_b_instance_arn =
          Util.option_bind (Xml.member "ReservedDBInstanceArn" xml) String.parse
      ; lease_id = Util.option_bind (Xml.member "LeaseId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.lease_id (fun f -> Query.Pair ("LeaseId", String.to_query f))
         ; Util.option_map v.reserved_d_b_instance_arn (fun f ->
               Query.Pair ("ReservedDBInstanceArn", String.to_query f))
         ; Some
             (Query.Pair
                ( "RecurringCharges.member"
                , RecurringChargeList.to_query v.recurring_charges ))
         ; Util.option_map v.state (fun f -> Query.Pair ("State", String.to_query f))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Util.option_map v.offering_type (fun f ->
               Query.Pair ("OfferingType", String.to_query f))
         ; Util.option_map v.product_description (fun f ->
               Query.Pair ("ProductDescription", String.to_query f))
         ; Util.option_map v.d_b_instance_count (fun f ->
               Query.Pair ("DBInstanceCount", Integer.to_query f))
         ; Util.option_map v.currency_code (fun f ->
               Query.Pair ("CurrencyCode", String.to_query f))
         ; Util.option_map v.usage_price (fun f ->
               Query.Pair ("UsagePrice", Double.to_query f))
         ; Util.option_map v.fixed_price (fun f ->
               Query.Pair ("FixedPrice", Double.to_query f))
         ; Util.option_map v.duration (fun f ->
               Query.Pair ("Duration", Integer.to_query f))
         ; Util.option_map v.start_time (fun f ->
               Query.Pair ("StartTime", DateTime.to_query f))
         ; Util.option_map v.d_b_instance_class (fun f ->
               Query.Pair ("DBInstanceClass", String.to_query f))
         ; Util.option_map v.reserved_d_b_instances_offering_id (fun f ->
               Query.Pair ("ReservedDBInstancesOfferingId", String.to_query f))
         ; Util.option_map v.reserved_d_b_instance_id (fun f ->
               Query.Pair ("ReservedDBInstanceId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.lease_id (fun f -> "lease_id", String.to_json f)
         ; Util.option_map v.reserved_d_b_instance_arn (fun f ->
               "reserved_d_b_instance_arn", String.to_json f)
         ; Some ("recurring_charges", RecurringChargeList.to_json v.recurring_charges)
         ; Util.option_map v.state (fun f -> "state", String.to_json f)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Util.option_map v.offering_type (fun f -> "offering_type", String.to_json f)
         ; Util.option_map v.product_description (fun f ->
               "product_description", String.to_json f)
         ; Util.option_map v.d_b_instance_count (fun f ->
               "d_b_instance_count", Integer.to_json f)
         ; Util.option_map v.currency_code (fun f -> "currency_code", String.to_json f)
         ; Util.option_map v.usage_price (fun f -> "usage_price", Double.to_json f)
         ; Util.option_map v.fixed_price (fun f -> "fixed_price", Double.to_json f)
         ; Util.option_map v.duration (fun f -> "duration", Integer.to_json f)
         ; Util.option_map v.start_time (fun f -> "start_time", DateTime.to_json f)
         ; Util.option_map v.d_b_instance_class (fun f ->
               "d_b_instance_class", String.to_json f)
         ; Util.option_map v.reserved_d_b_instances_offering_id (fun f ->
               "reserved_d_b_instances_offering_id", String.to_json f)
         ; Util.option_map v.reserved_d_b_instance_id (fun f ->
               "reserved_d_b_instance_id", String.to_json f)
         ])

  let of_json j =
    { reserved_d_b_instance_id =
        Util.option_map (Json.lookup j "reserved_d_b_instance_id") String.of_json
    ; reserved_d_b_instances_offering_id =
        Util.option_map
          (Json.lookup j "reserved_d_b_instances_offering_id")
          String.of_json
    ; d_b_instance_class =
        Util.option_map (Json.lookup j "d_b_instance_class") String.of_json
    ; start_time = Util.option_map (Json.lookup j "start_time") DateTime.of_json
    ; duration = Util.option_map (Json.lookup j "duration") Integer.of_json
    ; fixed_price = Util.option_map (Json.lookup j "fixed_price") Double.of_json
    ; usage_price = Util.option_map (Json.lookup j "usage_price") Double.of_json
    ; currency_code = Util.option_map (Json.lookup j "currency_code") String.of_json
    ; d_b_instance_count =
        Util.option_map (Json.lookup j "d_b_instance_count") Integer.of_json
    ; product_description =
        Util.option_map (Json.lookup j "product_description") String.of_json
    ; offering_type = Util.option_map (Json.lookup j "offering_type") String.of_json
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; state = Util.option_map (Json.lookup j "state") String.of_json
    ; recurring_charges =
        RecurringChargeList.of_json
          (Util.of_option_exn (Json.lookup j "recurring_charges"))
    ; reserved_d_b_instance_arn =
        Util.option_map (Json.lookup j "reserved_d_b_instance_arn") String.of_json
    ; lease_id = Util.option_map (Json.lookup j "lease_id") String.of_json
    }
end

module DBClusterSnapshotAttributeList = struct
  type t = DBClusterSnapshotAttribute.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map
         DBClusterSnapshotAttribute.parse
         (Xml.members "DBClusterSnapshotAttribute" xml))

  let to_query v = Query.to_query_list DBClusterSnapshotAttribute.to_query v

  let to_json v = `List (List.map DBClusterSnapshotAttribute.to_json v)

  let of_json j = Json.to_list DBClusterSnapshotAttribute.of_json j
end

module ResourcePendingMaintenanceActions = struct
  type t =
    { resource_identifier : String.t option
    ; pending_maintenance_action_details : PendingMaintenanceActionDetails.t
    }

  let make ?resource_identifier ?(pending_maintenance_action_details = []) () =
    { resource_identifier; pending_maintenance_action_details }

  let parse xml =
    Some
      { resource_identifier =
          Util.option_bind (Xml.member "ResourceIdentifier" xml) String.parse
      ; pending_maintenance_action_details =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "PendingMaintenanceActionDetails" xml)
               PendingMaintenanceActionDetails.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "PendingMaintenanceActionDetails.member"
                , PendingMaintenanceActionDetails.to_query
                    v.pending_maintenance_action_details ))
         ; Util.option_map v.resource_identifier (fun f ->
               Query.Pair ("ResourceIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "pending_maintenance_action_details"
             , PendingMaintenanceActionDetails.to_json
                 v.pending_maintenance_action_details )
         ; Util.option_map v.resource_identifier (fun f ->
               "resource_identifier", String.to_json f)
         ])

  let of_json j =
    { resource_identifier =
        Util.option_map (Json.lookup j "resource_identifier") String.of_json
    ; pending_maintenance_action_details =
        PendingMaintenanceActionDetails.of_json
          (Util.of_option_exn (Json.lookup j "pending_maintenance_action_details"))
    }
end

module DBProxyTargetGroup = struct
  type t =
    { d_b_proxy_name : String.t option
    ; target_group_name : String.t option
    ; target_group_arn : String.t option
    ; is_default : Boolean.t option
    ; status : String.t option
    ; connection_pool_config : ConnectionPoolConfigurationInfo.t option
    ; created_date : DateTime.t option
    ; updated_date : DateTime.t option
    }

  let make
      ?d_b_proxy_name
      ?target_group_name
      ?target_group_arn
      ?is_default
      ?status
      ?connection_pool_config
      ?created_date
      ?updated_date
      () =
    { d_b_proxy_name
    ; target_group_name
    ; target_group_arn
    ; is_default
    ; status
    ; connection_pool_config
    ; created_date
    ; updated_date
    }

  let parse xml =
    Some
      { d_b_proxy_name = Util.option_bind (Xml.member "DBProxyName" xml) String.parse
      ; target_group_name =
          Util.option_bind (Xml.member "TargetGroupName" xml) String.parse
      ; target_group_arn = Util.option_bind (Xml.member "TargetGroupArn" xml) String.parse
      ; is_default = Util.option_bind (Xml.member "IsDefault" xml) Boolean.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; connection_pool_config =
          Util.option_bind
            (Xml.member "ConnectionPoolConfig" xml)
            ConnectionPoolConfigurationInfo.parse
      ; created_date = Util.option_bind (Xml.member "CreatedDate" xml) DateTime.parse
      ; updated_date = Util.option_bind (Xml.member "UpdatedDate" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.updated_date (fun f ->
               Query.Pair ("UpdatedDate", DateTime.to_query f))
         ; Util.option_map v.created_date (fun f ->
               Query.Pair ("CreatedDate", DateTime.to_query f))
         ; Util.option_map v.connection_pool_config (fun f ->
               Query.Pair
                 ("ConnectionPoolConfig", ConnectionPoolConfigurationInfo.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.is_default (fun f ->
               Query.Pair ("IsDefault", Boolean.to_query f))
         ; Util.option_map v.target_group_arn (fun f ->
               Query.Pair ("TargetGroupArn", String.to_query f))
         ; Util.option_map v.target_group_name (fun f ->
               Query.Pair ("TargetGroupName", String.to_query f))
         ; Util.option_map v.d_b_proxy_name (fun f ->
               Query.Pair ("DBProxyName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.updated_date (fun f -> "updated_date", DateTime.to_json f)
         ; Util.option_map v.created_date (fun f -> "created_date", DateTime.to_json f)
         ; Util.option_map v.connection_pool_config (fun f ->
               "connection_pool_config", ConnectionPoolConfigurationInfo.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.is_default (fun f -> "is_default", Boolean.to_json f)
         ; Util.option_map v.target_group_arn (fun f ->
               "target_group_arn", String.to_json f)
         ; Util.option_map v.target_group_name (fun f ->
               "target_group_name", String.to_json f)
         ; Util.option_map v.d_b_proxy_name (fun f -> "d_b_proxy_name", String.to_json f)
         ])

  let of_json j =
    { d_b_proxy_name = Util.option_map (Json.lookup j "d_b_proxy_name") String.of_json
    ; target_group_name =
        Util.option_map (Json.lookup j "target_group_name") String.of_json
    ; target_group_arn = Util.option_map (Json.lookup j "target_group_arn") String.of_json
    ; is_default = Util.option_map (Json.lookup j "is_default") Boolean.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; connection_pool_config =
        Util.option_map
          (Json.lookup j "connection_pool_config")
          ConnectionPoolConfigurationInfo.of_json
    ; created_date = Util.option_map (Json.lookup j "created_date") DateTime.of_json
    ; updated_date = Util.option_map (Json.lookup j "updated_date") DateTime.of_json
    }
end

module DBProxyTarget = struct
  type t =
    { target_arn : String.t option
    ; endpoint : String.t option
    ; tracked_cluster_id : String.t option
    ; rds_resource_id : String.t option
    ; port : Integer.t option
    ; type_ : TargetType.t option
    ; target_health : TargetHealth.t option
    }

  let make
      ?target_arn
      ?endpoint
      ?tracked_cluster_id
      ?rds_resource_id
      ?port
      ?type_
      ?target_health
      () =
    { target_arn
    ; endpoint
    ; tracked_cluster_id
    ; rds_resource_id
    ; port
    ; type_
    ; target_health
    }

  let parse xml =
    Some
      { target_arn = Util.option_bind (Xml.member "TargetArn" xml) String.parse
      ; endpoint = Util.option_bind (Xml.member "Endpoint" xml) String.parse
      ; tracked_cluster_id =
          Util.option_bind (Xml.member "TrackedClusterId" xml) String.parse
      ; rds_resource_id = Util.option_bind (Xml.member "RdsResourceId" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; type_ = Util.option_bind (Xml.member "Type" xml) TargetType.parse
      ; target_health =
          Util.option_bind (Xml.member "TargetHealth" xml) TargetHealth.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.target_health (fun f ->
               Query.Pair ("TargetHealth", TargetHealth.to_query f))
         ; Util.option_map v.type_ (fun f -> Query.Pair ("Type", TargetType.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.rds_resource_id (fun f ->
               Query.Pair ("RdsResourceId", String.to_query f))
         ; Util.option_map v.tracked_cluster_id (fun f ->
               Query.Pair ("TrackedClusterId", String.to_query f))
         ; Util.option_map v.endpoint (fun f ->
               Query.Pair ("Endpoint", String.to_query f))
         ; Util.option_map v.target_arn (fun f ->
               Query.Pair ("TargetArn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.target_health (fun f ->
               "target_health", TargetHealth.to_json f)
         ; Util.option_map v.type_ (fun f -> "type_", TargetType.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.rds_resource_id (fun f ->
               "rds_resource_id", String.to_json f)
         ; Util.option_map v.tracked_cluster_id (fun f ->
               "tracked_cluster_id", String.to_json f)
         ; Util.option_map v.endpoint (fun f -> "endpoint", String.to_json f)
         ; Util.option_map v.target_arn (fun f -> "target_arn", String.to_json f)
         ])

  let of_json j =
    { target_arn = Util.option_map (Json.lookup j "target_arn") String.of_json
    ; endpoint = Util.option_map (Json.lookup j "endpoint") String.of_json
    ; tracked_cluster_id =
        Util.option_map (Json.lookup j "tracked_cluster_id") String.of_json
    ; rds_resource_id = Util.option_map (Json.lookup j "rds_resource_id") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; type_ = Util.option_map (Json.lookup j "type_") TargetType.of_json
    ; target_health = Util.option_map (Json.lookup j "target_health") TargetHealth.of_json
    }
end

module DBEngineVersion = struct
  type t =
    { engine : String.t option
    ; engine_version : String.t option
    ; d_b_parameter_group_family : String.t option
    ; d_b_engine_description : String.t option
    ; d_b_engine_version_description : String.t option
    ; default_character_set : CharacterSet.t option
    ; supported_character_sets : SupportedCharacterSetsList.t
    ; supported_nchar_character_sets : SupportedCharacterSetsList.t
    ; valid_upgrade_target : ValidUpgradeTargetList.t
    ; supported_timezones : SupportedTimezonesList.t
    ; exportable_log_types : LogTypeList.t
    ; supports_log_exports_to_cloudwatch_logs : Boolean.t option
    ; supports_read_replica : Boolean.t option
    ; supported_engine_modes : EngineModeList.t
    ; supported_feature_names : FeatureNameList.t
    ; status : String.t option
    ; supports_parallel_query : Boolean.t option
    ; supports_global_databases : Boolean.t option
    }

  let make
      ?engine
      ?engine_version
      ?d_b_parameter_group_family
      ?d_b_engine_description
      ?d_b_engine_version_description
      ?default_character_set
      ?(supported_character_sets = [])
      ?(supported_nchar_character_sets = [])
      ?(valid_upgrade_target = [])
      ?(supported_timezones = [])
      ?(exportable_log_types = [])
      ?supports_log_exports_to_cloudwatch_logs
      ?supports_read_replica
      ?(supported_engine_modes = [])
      ?(supported_feature_names = [])
      ?status
      ?supports_parallel_query
      ?supports_global_databases
      () =
    { engine
    ; engine_version
    ; d_b_parameter_group_family
    ; d_b_engine_description
    ; d_b_engine_version_description
    ; default_character_set
    ; supported_character_sets
    ; supported_nchar_character_sets
    ; valid_upgrade_target
    ; supported_timezones
    ; exportable_log_types
    ; supports_log_exports_to_cloudwatch_logs
    ; supports_read_replica
    ; supported_engine_modes
    ; supported_feature_names
    ; status
    ; supports_parallel_query
    ; supports_global_databases
    }

  let parse xml =
    Some
      { engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; d_b_parameter_group_family =
          Util.option_bind (Xml.member "DBParameterGroupFamily" xml) String.parse
      ; d_b_engine_description =
          Util.option_bind (Xml.member "DBEngineDescription" xml) String.parse
      ; d_b_engine_version_description =
          Util.option_bind (Xml.member "DBEngineVersionDescription" xml) String.parse
      ; default_character_set =
          Util.option_bind (Xml.member "DefaultCharacterSet" xml) CharacterSet.parse
      ; supported_character_sets =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "SupportedCharacterSets" xml)
               SupportedCharacterSetsList.parse)
      ; supported_nchar_character_sets =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "SupportedNcharCharacterSets" xml)
               SupportedCharacterSetsList.parse)
      ; valid_upgrade_target =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ValidUpgradeTarget" xml)
               ValidUpgradeTargetList.parse)
      ; supported_timezones =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "SupportedTimezones" xml)
               SupportedTimezonesList.parse)
      ; exportable_log_types =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ExportableLogTypes" xml) LogTypeList.parse)
      ; supports_log_exports_to_cloudwatch_logs =
          Util.option_bind
            (Xml.member "SupportsLogExportsToCloudwatchLogs" xml)
            Boolean.parse
      ; supports_read_replica =
          Util.option_bind (Xml.member "SupportsReadReplica" xml) Boolean.parse
      ; supported_engine_modes =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "SupportedEngineModes" xml)
               EngineModeList.parse)
      ; supported_feature_names =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "SupportedFeatureNames" xml)
               FeatureNameList.parse)
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; supports_parallel_query =
          Util.option_bind (Xml.member "SupportsParallelQuery" xml) Boolean.parse
      ; supports_global_databases =
          Util.option_bind (Xml.member "SupportsGlobalDatabases" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.supports_global_databases (fun f ->
               Query.Pair ("SupportsGlobalDatabases", Boolean.to_query f))
         ; Util.option_map v.supports_parallel_query (fun f ->
               Query.Pair ("SupportsParallelQuery", Boolean.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Some
             (Query.Pair
                ( "SupportedFeatureNames.member"
                , FeatureNameList.to_query v.supported_feature_names ))
         ; Some
             (Query.Pair
                ( "SupportedEngineModes.member"
                , EngineModeList.to_query v.supported_engine_modes ))
         ; Util.option_map v.supports_read_replica (fun f ->
               Query.Pair ("SupportsReadReplica", Boolean.to_query f))
         ; Util.option_map v.supports_log_exports_to_cloudwatch_logs (fun f ->
               Query.Pair ("SupportsLogExportsToCloudwatchLogs", Boolean.to_query f))
         ; Some
             (Query.Pair
                ("ExportableLogTypes.member", LogTypeList.to_query v.exportable_log_types))
         ; Some
             (Query.Pair
                ( "SupportedTimezones.member"
                , SupportedTimezonesList.to_query v.supported_timezones ))
         ; Some
             (Query.Pair
                ( "ValidUpgradeTarget.member"
                , ValidUpgradeTargetList.to_query v.valid_upgrade_target ))
         ; Some
             (Query.Pair
                ( "SupportedNcharCharacterSets.member"
                , SupportedCharacterSetsList.to_query v.supported_nchar_character_sets ))
         ; Some
             (Query.Pair
                ( "SupportedCharacterSets.member"
                , SupportedCharacterSetsList.to_query v.supported_character_sets ))
         ; Util.option_map v.default_character_set (fun f ->
               Query.Pair ("DefaultCharacterSet", CharacterSet.to_query f))
         ; Util.option_map v.d_b_engine_version_description (fun f ->
               Query.Pair ("DBEngineVersionDescription", String.to_query f))
         ; Util.option_map v.d_b_engine_description (fun f ->
               Query.Pair ("DBEngineDescription", String.to_query f))
         ; Util.option_map v.d_b_parameter_group_family (fun f ->
               Query.Pair ("DBParameterGroupFamily", String.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.supports_global_databases (fun f ->
               "supports_global_databases", Boolean.to_json f)
         ; Util.option_map v.supports_parallel_query (fun f ->
               "supports_parallel_query", Boolean.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Some
             ("supported_feature_names", FeatureNameList.to_json v.supported_feature_names)
         ; Some ("supported_engine_modes", EngineModeList.to_json v.supported_engine_modes)
         ; Util.option_map v.supports_read_replica (fun f ->
               "supports_read_replica", Boolean.to_json f)
         ; Util.option_map v.supports_log_exports_to_cloudwatch_logs (fun f ->
               "supports_log_exports_to_cloudwatch_logs", Boolean.to_json f)
         ; Some ("exportable_log_types", LogTypeList.to_json v.exportable_log_types)
         ; Some
             ("supported_timezones", SupportedTimezonesList.to_json v.supported_timezones)
         ; Some
             ( "valid_upgrade_target"
             , ValidUpgradeTargetList.to_json v.valid_upgrade_target )
         ; Some
             ( "supported_nchar_character_sets"
             , SupportedCharacterSetsList.to_json v.supported_nchar_character_sets )
         ; Some
             ( "supported_character_sets"
             , SupportedCharacterSetsList.to_json v.supported_character_sets )
         ; Util.option_map v.default_character_set (fun f ->
               "default_character_set", CharacterSet.to_json f)
         ; Util.option_map v.d_b_engine_version_description (fun f ->
               "d_b_engine_version_description", String.to_json f)
         ; Util.option_map v.d_b_engine_description (fun f ->
               "d_b_engine_description", String.to_json f)
         ; Util.option_map v.d_b_parameter_group_family (fun f ->
               "d_b_parameter_group_family", String.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ])

  let of_json j =
    { engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; d_b_parameter_group_family =
        Util.option_map (Json.lookup j "d_b_parameter_group_family") String.of_json
    ; d_b_engine_description =
        Util.option_map (Json.lookup j "d_b_engine_description") String.of_json
    ; d_b_engine_version_description =
        Util.option_map (Json.lookup j "d_b_engine_version_description") String.of_json
    ; default_character_set =
        Util.option_map (Json.lookup j "default_character_set") CharacterSet.of_json
    ; supported_character_sets =
        SupportedCharacterSetsList.of_json
          (Util.of_option_exn (Json.lookup j "supported_character_sets"))
    ; supported_nchar_character_sets =
        SupportedCharacterSetsList.of_json
          (Util.of_option_exn (Json.lookup j "supported_nchar_character_sets"))
    ; valid_upgrade_target =
        ValidUpgradeTargetList.of_json
          (Util.of_option_exn (Json.lookup j "valid_upgrade_target"))
    ; supported_timezones =
        SupportedTimezonesList.of_json
          (Util.of_option_exn (Json.lookup j "supported_timezones"))
    ; exportable_log_types =
        LogTypeList.of_json (Util.of_option_exn (Json.lookup j "exportable_log_types"))
    ; supports_log_exports_to_cloudwatch_logs =
        Util.option_map
          (Json.lookup j "supports_log_exports_to_cloudwatch_logs")
          Boolean.of_json
    ; supports_read_replica =
        Util.option_map (Json.lookup j "supports_read_replica") Boolean.of_json
    ; supported_engine_modes =
        EngineModeList.of_json
          (Util.of_option_exn (Json.lookup j "supported_engine_modes"))
    ; supported_feature_names =
        FeatureNameList.of_json
          (Util.of_option_exn (Json.lookup j "supported_feature_names"))
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; supports_parallel_query =
        Util.option_map (Json.lookup j "supports_parallel_query") Boolean.of_json
    ; supports_global_databases =
        Util.option_map (Json.lookup j "supports_global_databases") Boolean.of_json
    }
end

module EventCategoriesMap = struct
  type t =
    { source_type : String.t option
    ; event_categories : EventCategoriesList.t
    }

  let make ?source_type ?(event_categories = []) () = { source_type; event_categories }

  let parse xml =
    Some
      { source_type = Util.option_bind (Xml.member "SourceType" xml) String.parse
      ; event_categories =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EventCategories" xml)
               EventCategoriesList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("EventCategories.member", EventCategoriesList.to_query v.event_categories))
         ; Util.option_map v.source_type (fun f ->
               Query.Pair ("SourceType", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("event_categories", EventCategoriesList.to_json v.event_categories)
         ; Util.option_map v.source_type (fun f -> "source_type", String.to_json f)
         ])

  let of_json j =
    { source_type = Util.option_map (Json.lookup j "source_type") String.of_json
    ; event_categories =
        EventCategoriesList.of_json
          (Util.of_option_exn (Json.lookup j "event_categories"))
    }
end

module GlobalCluster = struct
  type t =
    { global_cluster_identifier : String.t option
    ; global_cluster_resource_id : String.t option
    ; global_cluster_arn : String.t option
    ; status : String.t option
    ; engine : String.t option
    ; engine_version : String.t option
    ; database_name : String.t option
    ; storage_encrypted : Boolean.t option
    ; deletion_protection : Boolean.t option
    ; global_cluster_members : GlobalClusterMemberList.t
    }

  let make
      ?global_cluster_identifier
      ?global_cluster_resource_id
      ?global_cluster_arn
      ?status
      ?engine
      ?engine_version
      ?database_name
      ?storage_encrypted
      ?deletion_protection
      ?(global_cluster_members = [])
      () =
    { global_cluster_identifier
    ; global_cluster_resource_id
    ; global_cluster_arn
    ; status
    ; engine
    ; engine_version
    ; database_name
    ; storage_encrypted
    ; deletion_protection
    ; global_cluster_members
    }

  let parse xml =
    Some
      { global_cluster_identifier =
          Util.option_bind (Xml.member "GlobalClusterIdentifier" xml) String.parse
      ; global_cluster_resource_id =
          Util.option_bind (Xml.member "GlobalClusterResourceId" xml) String.parse
      ; global_cluster_arn =
          Util.option_bind (Xml.member "GlobalClusterArn" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; database_name = Util.option_bind (Xml.member "DatabaseName" xml) String.parse
      ; storage_encrypted =
          Util.option_bind (Xml.member "StorageEncrypted" xml) Boolean.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; global_cluster_members =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "GlobalClusterMembers" xml)
               GlobalClusterMemberList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "GlobalClusterMembers.member"
                , GlobalClusterMemberList.to_query v.global_cluster_members ))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.storage_encrypted (fun f ->
               Query.Pair ("StorageEncrypted", Boolean.to_query f))
         ; Util.option_map v.database_name (fun f ->
               Query.Pair ("DatabaseName", String.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.global_cluster_arn (fun f ->
               Query.Pair ("GlobalClusterArn", String.to_query f))
         ; Util.option_map v.global_cluster_resource_id (fun f ->
               Query.Pair ("GlobalClusterResourceId", String.to_query f))
         ; Util.option_map v.global_cluster_identifier (fun f ->
               Query.Pair ("GlobalClusterIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "global_cluster_members"
             , GlobalClusterMemberList.to_json v.global_cluster_members )
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.storage_encrypted (fun f ->
               "storage_encrypted", Boolean.to_json f)
         ; Util.option_map v.database_name (fun f -> "database_name", String.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.global_cluster_arn (fun f ->
               "global_cluster_arn", String.to_json f)
         ; Util.option_map v.global_cluster_resource_id (fun f ->
               "global_cluster_resource_id", String.to_json f)
         ; Util.option_map v.global_cluster_identifier (fun f ->
               "global_cluster_identifier", String.to_json f)
         ])

  let of_json j =
    { global_cluster_identifier =
        Util.option_map (Json.lookup j "global_cluster_identifier") String.of_json
    ; global_cluster_resource_id =
        Util.option_map (Json.lookup j "global_cluster_resource_id") String.of_json
    ; global_cluster_arn =
        Util.option_map (Json.lookup j "global_cluster_arn") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; database_name = Util.option_map (Json.lookup j "database_name") String.of_json
    ; storage_encrypted =
        Util.option_map (Json.lookup j "storage_encrypted") Boolean.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; global_cluster_members =
        GlobalClusterMemberList.of_json
          (Util.of_option_exn (Json.lookup j "global_cluster_members"))
    }
end

module DBClusterParameterGroup = struct
  type t =
    { d_b_cluster_parameter_group_name : String.t option
    ; d_b_parameter_group_family : String.t option
    ; description : String.t option
    ; d_b_cluster_parameter_group_arn : String.t option
    }

  let make
      ?d_b_cluster_parameter_group_name
      ?d_b_parameter_group_family
      ?description
      ?d_b_cluster_parameter_group_arn
      () =
    { d_b_cluster_parameter_group_name
    ; d_b_parameter_group_family
    ; description
    ; d_b_cluster_parameter_group_arn
    }

  let parse xml =
    Some
      { d_b_cluster_parameter_group_name =
          Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse
      ; d_b_parameter_group_family =
          Util.option_bind (Xml.member "DBParameterGroupFamily" xml) String.parse
      ; description = Util.option_bind (Xml.member "Description" xml) String.parse
      ; d_b_cluster_parameter_group_arn =
          Util.option_bind (Xml.member "DBClusterParameterGroupArn" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_parameter_group_arn (fun f ->
               Query.Pair ("DBClusterParameterGroupArn", String.to_query f))
         ; Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ; Util.option_map v.d_b_parameter_group_family (fun f ->
               Query.Pair ("DBParameterGroupFamily", String.to_query f))
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               Query.Pair ("DBClusterParameterGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_parameter_group_arn (fun f ->
               "d_b_cluster_parameter_group_arn", String.to_json f)
         ; Util.option_map v.description (fun f -> "description", String.to_json f)
         ; Util.option_map v.d_b_parameter_group_family (fun f ->
               "d_b_parameter_group_family", String.to_json f)
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               "d_b_cluster_parameter_group_name", String.to_json f)
         ])

  let of_json j =
    { d_b_cluster_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name") String.of_json
    ; d_b_parameter_group_family =
        Util.option_map (Json.lookup j "d_b_parameter_group_family") String.of_json
    ; description = Util.option_map (Json.lookup j "description") String.of_json
    ; d_b_cluster_parameter_group_arn =
        Util.option_map (Json.lookup j "d_b_cluster_parameter_group_arn") String.of_json
    }
end

module DescribeDBLogFilesDetails = struct
  type t =
    { log_file_name : String.t option
    ; last_written : Long.t option
    ; size : Long.t option
    }

  let make ?log_file_name ?last_written ?size () = { log_file_name; last_written; size }

  let parse xml =
    Some
      { log_file_name = Util.option_bind (Xml.member "LogFileName" xml) String.parse
      ; last_written = Util.option_bind (Xml.member "LastWritten" xml) Long.parse
      ; size = Util.option_bind (Xml.member "Size" xml) Long.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.size (fun f -> Query.Pair ("Size", Long.to_query f))
         ; Util.option_map v.last_written (fun f ->
               Query.Pair ("LastWritten", Long.to_query f))
         ; Util.option_map v.log_file_name (fun f ->
               Query.Pair ("LogFileName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.size (fun f -> "size", Long.to_json f)
         ; Util.option_map v.last_written (fun f -> "last_written", Long.to_json f)
         ; Util.option_map v.log_file_name (fun f -> "log_file_name", String.to_json f)
         ])

  let of_json j =
    { log_file_name = Util.option_map (Json.lookup j "log_file_name") String.of_json
    ; last_written = Util.option_map (Json.lookup j "last_written") Long.of_json
    ; size = Util.option_map (Json.lookup j "size") Long.of_json
    }
end

module Certificate = struct
  type t =
    { certificate_identifier : String.t option
    ; certificate_type : String.t option
    ; thumbprint : String.t option
    ; valid_from : DateTime.t option
    ; valid_till : DateTime.t option
    ; certificate_arn : String.t option
    ; customer_override : Boolean.t option
    ; customer_override_valid_till : DateTime.t option
    }

  let make
      ?certificate_identifier
      ?certificate_type
      ?thumbprint
      ?valid_from
      ?valid_till
      ?certificate_arn
      ?customer_override
      ?customer_override_valid_till
      () =
    { certificate_identifier
    ; certificate_type
    ; thumbprint
    ; valid_from
    ; valid_till
    ; certificate_arn
    ; customer_override
    ; customer_override_valid_till
    }

  let parse xml =
    Some
      { certificate_identifier =
          Util.option_bind (Xml.member "CertificateIdentifier" xml) String.parse
      ; certificate_type =
          Util.option_bind (Xml.member "CertificateType" xml) String.parse
      ; thumbprint = Util.option_bind (Xml.member "Thumbprint" xml) String.parse
      ; valid_from = Util.option_bind (Xml.member "ValidFrom" xml) DateTime.parse
      ; valid_till = Util.option_bind (Xml.member "ValidTill" xml) DateTime.parse
      ; certificate_arn = Util.option_bind (Xml.member "CertificateArn" xml) String.parse
      ; customer_override =
          Util.option_bind (Xml.member "CustomerOverride" xml) Boolean.parse
      ; customer_override_valid_till =
          Util.option_bind (Xml.member "CustomerOverrideValidTill" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.customer_override_valid_till (fun f ->
               Query.Pair ("CustomerOverrideValidTill", DateTime.to_query f))
         ; Util.option_map v.customer_override (fun f ->
               Query.Pair ("CustomerOverride", Boolean.to_query f))
         ; Util.option_map v.certificate_arn (fun f ->
               Query.Pair ("CertificateArn", String.to_query f))
         ; Util.option_map v.valid_till (fun f ->
               Query.Pair ("ValidTill", DateTime.to_query f))
         ; Util.option_map v.valid_from (fun f ->
               Query.Pair ("ValidFrom", DateTime.to_query f))
         ; Util.option_map v.thumbprint (fun f ->
               Query.Pair ("Thumbprint", String.to_query f))
         ; Util.option_map v.certificate_type (fun f ->
               Query.Pair ("CertificateType", String.to_query f))
         ; Util.option_map v.certificate_identifier (fun f ->
               Query.Pair ("CertificateIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.customer_override_valid_till (fun f ->
               "customer_override_valid_till", DateTime.to_json f)
         ; Util.option_map v.customer_override (fun f ->
               "customer_override", Boolean.to_json f)
         ; Util.option_map v.certificate_arn (fun f ->
               "certificate_arn", String.to_json f)
         ; Util.option_map v.valid_till (fun f -> "valid_till", DateTime.to_json f)
         ; Util.option_map v.valid_from (fun f -> "valid_from", DateTime.to_json f)
         ; Util.option_map v.thumbprint (fun f -> "thumbprint", String.to_json f)
         ; Util.option_map v.certificate_type (fun f ->
               "certificate_type", String.to_json f)
         ; Util.option_map v.certificate_identifier (fun f ->
               "certificate_identifier", String.to_json f)
         ])

  let of_json j =
    { certificate_identifier =
        Util.option_map (Json.lookup j "certificate_identifier") String.of_json
    ; certificate_type = Util.option_map (Json.lookup j "certificate_type") String.of_json
    ; thumbprint = Util.option_map (Json.lookup j "thumbprint") String.of_json
    ; valid_from = Util.option_map (Json.lookup j "valid_from") DateTime.of_json
    ; valid_till = Util.option_map (Json.lookup j "valid_till") DateTime.of_json
    ; certificate_arn = Util.option_map (Json.lookup j "certificate_arn") String.of_json
    ; customer_override =
        Util.option_map (Json.lookup j "customer_override") Boolean.of_json
    ; customer_override_valid_till =
        Util.option_map (Json.lookup j "customer_override_valid_till") DateTime.of_json
    }
end

module OptionGroup = struct
  type t =
    { option_group_name : String.t option
    ; option_group_description : String.t option
    ; engine_name : String.t option
    ; major_engine_version : String.t option
    ; options : OptionsList.t
    ; allows_vpc_and_non_vpc_instance_memberships : Boolean.t option
    ; vpc_id : String.t option
    ; option_group_arn : String.t option
    }

  let make
      ?option_group_name
      ?option_group_description
      ?engine_name
      ?major_engine_version
      ?(options = [])
      ?allows_vpc_and_non_vpc_instance_memberships
      ?vpc_id
      ?option_group_arn
      () =
    { option_group_name
    ; option_group_description
    ; engine_name
    ; major_engine_version
    ; options
    ; allows_vpc_and_non_vpc_instance_memberships
    ; vpc_id
    ; option_group_arn
    }

  let parse xml =
    Some
      { option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; option_group_description =
          Util.option_bind (Xml.member "OptionGroupDescription" xml) String.parse
      ; engine_name = Util.option_bind (Xml.member "EngineName" xml) String.parse
      ; major_engine_version =
          Util.option_bind (Xml.member "MajorEngineVersion" xml) String.parse
      ; options =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Options" xml) OptionsList.parse)
      ; allows_vpc_and_non_vpc_instance_memberships =
          Util.option_bind
            (Xml.member "AllowsVpcAndNonVpcInstanceMemberships" xml)
            Boolean.parse
      ; vpc_id = Util.option_bind (Xml.member "VpcId" xml) String.parse
      ; option_group_arn = Util.option_bind (Xml.member "OptionGroupArn" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.option_group_arn (fun f ->
               Query.Pair ("OptionGroupArn", String.to_query f))
         ; Util.option_map v.vpc_id (fun f -> Query.Pair ("VpcId", String.to_query f))
         ; Util.option_map v.allows_vpc_and_non_vpc_instance_memberships (fun f ->
               Query.Pair ("AllowsVpcAndNonVpcInstanceMemberships", Boolean.to_query f))
         ; Some (Query.Pair ("Options.member", OptionsList.to_query v.options))
         ; Util.option_map v.major_engine_version (fun f ->
               Query.Pair ("MajorEngineVersion", String.to_query f))
         ; Util.option_map v.engine_name (fun f ->
               Query.Pair ("EngineName", String.to_query f))
         ; Util.option_map v.option_group_description (fun f ->
               Query.Pair ("OptionGroupDescription", String.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.option_group_arn (fun f ->
               "option_group_arn", String.to_json f)
         ; Util.option_map v.vpc_id (fun f -> "vpc_id", String.to_json f)
         ; Util.option_map v.allows_vpc_and_non_vpc_instance_memberships (fun f ->
               "allows_vpc_and_non_vpc_instance_memberships", Boolean.to_json f)
         ; Some ("options", OptionsList.to_json v.options)
         ; Util.option_map v.major_engine_version (fun f ->
               "major_engine_version", String.to_json f)
         ; Util.option_map v.engine_name (fun f -> "engine_name", String.to_json f)
         ; Util.option_map v.option_group_description (fun f ->
               "option_group_description", String.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ])

  let of_json j =
    { option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; option_group_description =
        Util.option_map (Json.lookup j "option_group_description") String.of_json
    ; engine_name = Util.option_map (Json.lookup j "engine_name") String.of_json
    ; major_engine_version =
        Util.option_map (Json.lookup j "major_engine_version") String.of_json
    ; options = OptionsList.of_json (Util.of_option_exn (Json.lookup j "options"))
    ; allows_vpc_and_non_vpc_instance_memberships =
        Util.option_map
          (Json.lookup j "allows_vpc_and_non_vpc_instance_memberships")
          Boolean.of_json
    ; vpc_id = Util.option_map (Json.lookup j "vpc_id") String.of_json
    ; option_group_arn = Util.option_map (Json.lookup j "option_group_arn") String.of_json
    }
end

module UserAuthConfig = struct
  type t =
    { description : String.t option
    ; user_name : String.t option
    ; auth_scheme : AuthScheme.t option
    ; secret_arn : String.t option
    ; i_a_m_auth : IAMAuthMode.t option
    }

  let make ?description ?user_name ?auth_scheme ?secret_arn ?i_a_m_auth () =
    { description; user_name; auth_scheme; secret_arn; i_a_m_auth }

  let parse xml =
    Some
      { description = Util.option_bind (Xml.member "Description" xml) String.parse
      ; user_name = Util.option_bind (Xml.member "UserName" xml) String.parse
      ; auth_scheme = Util.option_bind (Xml.member "AuthScheme" xml) AuthScheme.parse
      ; secret_arn = Util.option_bind (Xml.member "SecretArn" xml) String.parse
      ; i_a_m_auth = Util.option_bind (Xml.member "IAMAuth" xml) IAMAuthMode.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.i_a_m_auth (fun f ->
               Query.Pair ("IAMAuth", IAMAuthMode.to_query f))
         ; Util.option_map v.secret_arn (fun f ->
               Query.Pair ("SecretArn", String.to_query f))
         ; Util.option_map v.auth_scheme (fun f ->
               Query.Pair ("AuthScheme", AuthScheme.to_query f))
         ; Util.option_map v.user_name (fun f ->
               Query.Pair ("UserName", String.to_query f))
         ; Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.i_a_m_auth (fun f -> "i_a_m_auth", IAMAuthMode.to_json f)
         ; Util.option_map v.secret_arn (fun f -> "secret_arn", String.to_json f)
         ; Util.option_map v.auth_scheme (fun f -> "auth_scheme", AuthScheme.to_json f)
         ; Util.option_map v.user_name (fun f -> "user_name", String.to_json f)
         ; Util.option_map v.description (fun f -> "description", String.to_json f)
         ])

  let of_json j =
    { description = Util.option_map (Json.lookup j "description") String.of_json
    ; user_name = Util.option_map (Json.lookup j "user_name") String.of_json
    ; auth_scheme = Util.option_map (Json.lookup j "auth_scheme") AuthScheme.of_json
    ; secret_arn = Util.option_map (Json.lookup j "secret_arn") String.of_json
    ; i_a_m_auth = Util.option_map (Json.lookup j "i_a_m_auth") IAMAuthMode.of_json
    }
end

module SourceRegion = struct
  type t =
    { region_name : String.t option
    ; endpoint : String.t option
    ; status : String.t option
    }

  let make ?region_name ?endpoint ?status () = { region_name; endpoint; status }

  let parse xml =
    Some
      { region_name = Util.option_bind (Xml.member "RegionName" xml) String.parse
      ; endpoint = Util.option_bind (Xml.member "Endpoint" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.endpoint (fun f ->
               Query.Pair ("Endpoint", String.to_query f))
         ; Util.option_map v.region_name (fun f ->
               Query.Pair ("RegionName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.endpoint (fun f -> "endpoint", String.to_json f)
         ; Util.option_map v.region_name (fun f -> "region_name", String.to_json f)
         ])

  let of_json j =
    { region_name = Util.option_map (Json.lookup j "region_name") String.of_json
    ; endpoint = Util.option_map (Json.lookup j "endpoint") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    }
end

module DBClusterSnapshot = struct
  type t =
    { availability_zones : AvailabilityZones.t
    ; d_b_cluster_snapshot_identifier : String.t option
    ; d_b_cluster_identifier : String.t option
    ; snapshot_create_time : DateTime.t option
    ; engine : String.t option
    ; allocated_storage : Integer.t option
    ; status : String.t option
    ; port : Integer.t option
    ; vpc_id : String.t option
    ; cluster_create_time : DateTime.t option
    ; master_username : String.t option
    ; engine_version : String.t option
    ; license_model : String.t option
    ; snapshot_type : String.t option
    ; percent_progress : Integer.t option
    ; storage_encrypted : Boolean.t option
    ; kms_key_id : String.t option
    ; d_b_cluster_snapshot_arn : String.t option
    ; source_d_b_cluster_snapshot_arn : String.t option
    ; i_a_m_database_authentication_enabled : Boolean.t option
    ; tag_list : TagList.t
    }

  let make
      ?(availability_zones = [])
      ?d_b_cluster_snapshot_identifier
      ?d_b_cluster_identifier
      ?snapshot_create_time
      ?engine
      ?allocated_storage
      ?status
      ?port
      ?vpc_id
      ?cluster_create_time
      ?master_username
      ?engine_version
      ?license_model
      ?snapshot_type
      ?percent_progress
      ?storage_encrypted
      ?kms_key_id
      ?d_b_cluster_snapshot_arn
      ?source_d_b_cluster_snapshot_arn
      ?i_a_m_database_authentication_enabled
      ?(tag_list = [])
      () =
    { availability_zones
    ; d_b_cluster_snapshot_identifier
    ; d_b_cluster_identifier
    ; snapshot_create_time
    ; engine
    ; allocated_storage
    ; status
    ; port
    ; vpc_id
    ; cluster_create_time
    ; master_username
    ; engine_version
    ; license_model
    ; snapshot_type
    ; percent_progress
    ; storage_encrypted
    ; kms_key_id
    ; d_b_cluster_snapshot_arn
    ; source_d_b_cluster_snapshot_arn
    ; i_a_m_database_authentication_enabled
    ; tag_list
    }

  let parse xml =
    Some
      { availability_zones =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      ; d_b_cluster_snapshot_identifier =
          Util.option_bind (Xml.member "DBClusterSnapshotIdentifier" xml) String.parse
      ; d_b_cluster_identifier =
          Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse
      ; snapshot_create_time =
          Util.option_bind (Xml.member "SnapshotCreateTime" xml) DateTime.parse
      ; engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; allocated_storage =
          Util.option_bind (Xml.member "AllocatedStorage" xml) Integer.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; vpc_id = Util.option_bind (Xml.member "VpcId" xml) String.parse
      ; cluster_create_time =
          Util.option_bind (Xml.member "ClusterCreateTime" xml) DateTime.parse
      ; master_username = Util.option_bind (Xml.member "MasterUsername" xml) String.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; license_model = Util.option_bind (Xml.member "LicenseModel" xml) String.parse
      ; snapshot_type = Util.option_bind (Xml.member "SnapshotType" xml) String.parse
      ; percent_progress =
          Util.option_bind (Xml.member "PercentProgress" xml) Integer.parse
      ; storage_encrypted =
          Util.option_bind (Xml.member "StorageEncrypted" xml) Boolean.parse
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; d_b_cluster_snapshot_arn =
          Util.option_bind (Xml.member "DBClusterSnapshotArn" xml) String.parse
      ; source_d_b_cluster_snapshot_arn =
          Util.option_bind (Xml.member "SourceDBClusterSnapshotArn" xml) String.parse
      ; i_a_m_database_authentication_enabled =
          Util.option_bind
            (Xml.member "IAMDatabaseAuthenticationEnabled" xml)
            Boolean.parse
      ; tag_list =
          Util.of_option [] (Util.option_bind (Xml.member "TagList" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("TagList.member", TagList.to_query v.tag_list))
         ; Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
               Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
         ; Util.option_map v.source_d_b_cluster_snapshot_arn (fun f ->
               Query.Pair ("SourceDBClusterSnapshotArn", String.to_query f))
         ; Util.option_map v.d_b_cluster_snapshot_arn (fun f ->
               Query.Pair ("DBClusterSnapshotArn", String.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Util.option_map v.storage_encrypted (fun f ->
               Query.Pair ("StorageEncrypted", Boolean.to_query f))
         ; Util.option_map v.percent_progress (fun f ->
               Query.Pair ("PercentProgress", Integer.to_query f))
         ; Util.option_map v.snapshot_type (fun f ->
               Query.Pair ("SnapshotType", String.to_query f))
         ; Util.option_map v.license_model (fun f ->
               Query.Pair ("LicenseModel", String.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.master_username (fun f ->
               Query.Pair ("MasterUsername", String.to_query f))
         ; Util.option_map v.cluster_create_time (fun f ->
               Query.Pair ("ClusterCreateTime", DateTime.to_query f))
         ; Util.option_map v.vpc_id (fun f -> Query.Pair ("VpcId", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.allocated_storage (fun f ->
               Query.Pair ("AllocatedStorage", Integer.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ; Util.option_map v.snapshot_create_time (fun f ->
               Query.Pair ("SnapshotCreateTime", DateTime.to_query f))
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               Query.Pair ("DBClusterIdentifier", String.to_query f))
         ; Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
               Query.Pair ("DBClusterSnapshotIdentifier", String.to_query f))
         ; Some
             (Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tag_list", TagList.to_json v.tag_list)
         ; Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
               "i_a_m_database_authentication_enabled", Boolean.to_json f)
         ; Util.option_map v.source_d_b_cluster_snapshot_arn (fun f ->
               "source_d_b_cluster_snapshot_arn", String.to_json f)
         ; Util.option_map v.d_b_cluster_snapshot_arn (fun f ->
               "d_b_cluster_snapshot_arn", String.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Util.option_map v.storage_encrypted (fun f ->
               "storage_encrypted", Boolean.to_json f)
         ; Util.option_map v.percent_progress (fun f ->
               "percent_progress", Integer.to_json f)
         ; Util.option_map v.snapshot_type (fun f -> "snapshot_type", String.to_json f)
         ; Util.option_map v.license_model (fun f -> "license_model", String.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.master_username (fun f ->
               "master_username", String.to_json f)
         ; Util.option_map v.cluster_create_time (fun f ->
               "cluster_create_time", DateTime.to_json f)
         ; Util.option_map v.vpc_id (fun f -> "vpc_id", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.allocated_storage (fun f ->
               "allocated_storage", Integer.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ; Util.option_map v.snapshot_create_time (fun f ->
               "snapshot_create_time", DateTime.to_json f)
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               "d_b_cluster_identifier", String.to_json f)
         ; Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
               "d_b_cluster_snapshot_identifier", String.to_json f)
         ; Some ("availability_zones", AvailabilityZones.to_json v.availability_zones)
         ])

  let of_json j =
    { availability_zones =
        AvailabilityZones.of_json
          (Util.of_option_exn (Json.lookup j "availability_zones"))
    ; d_b_cluster_snapshot_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_snapshot_identifier") String.of_json
    ; d_b_cluster_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_identifier") String.of_json
    ; snapshot_create_time =
        Util.option_map (Json.lookup j "snapshot_create_time") DateTime.of_json
    ; engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; allocated_storage =
        Util.option_map (Json.lookup j "allocated_storage") Integer.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; vpc_id = Util.option_map (Json.lookup j "vpc_id") String.of_json
    ; cluster_create_time =
        Util.option_map (Json.lookup j "cluster_create_time") DateTime.of_json
    ; master_username = Util.option_map (Json.lookup j "master_username") String.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; license_model = Util.option_map (Json.lookup j "license_model") String.of_json
    ; snapshot_type = Util.option_map (Json.lookup j "snapshot_type") String.of_json
    ; percent_progress =
        Util.option_map (Json.lookup j "percent_progress") Integer.of_json
    ; storage_encrypted =
        Util.option_map (Json.lookup j "storage_encrypted") Boolean.of_json
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; d_b_cluster_snapshot_arn =
        Util.option_map (Json.lookup j "d_b_cluster_snapshot_arn") String.of_json
    ; source_d_b_cluster_snapshot_arn =
        Util.option_map (Json.lookup j "source_d_b_cluster_snapshot_arn") String.of_json
    ; i_a_m_database_authentication_enabled =
        Util.option_map
          (Json.lookup j "i_a_m_database_authentication_enabled")
          Boolean.of_json
    ; tag_list = TagList.of_json (Util.of_option_exn (Json.lookup j "tag_list"))
    }
end

module DBClusterEndpoint = struct
  type t =
    { d_b_cluster_endpoint_identifier : String.t option
    ; d_b_cluster_identifier : String.t option
    ; d_b_cluster_endpoint_resource_identifier : String.t option
    ; endpoint : String.t option
    ; status : String.t option
    ; endpoint_type : String.t option
    ; custom_endpoint_type : String.t option
    ; static_members : StringList.t
    ; excluded_members : StringList.t
    ; d_b_cluster_endpoint_arn : String.t option
    }

  let make
      ?d_b_cluster_endpoint_identifier
      ?d_b_cluster_identifier
      ?d_b_cluster_endpoint_resource_identifier
      ?endpoint
      ?status
      ?endpoint_type
      ?custom_endpoint_type
      ?(static_members = [])
      ?(excluded_members = [])
      ?d_b_cluster_endpoint_arn
      () =
    { d_b_cluster_endpoint_identifier
    ; d_b_cluster_identifier
    ; d_b_cluster_endpoint_resource_identifier
    ; endpoint
    ; status
    ; endpoint_type
    ; custom_endpoint_type
    ; static_members
    ; excluded_members
    ; d_b_cluster_endpoint_arn
    }

  let parse xml =
    Some
      { d_b_cluster_endpoint_identifier =
          Util.option_bind (Xml.member "DBClusterEndpointIdentifier" xml) String.parse
      ; d_b_cluster_identifier =
          Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse
      ; d_b_cluster_endpoint_resource_identifier =
          Util.option_bind
            (Xml.member "DBClusterEndpointResourceIdentifier" xml)
            String.parse
      ; endpoint = Util.option_bind (Xml.member "Endpoint" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; endpoint_type = Util.option_bind (Xml.member "EndpointType" xml) String.parse
      ; custom_endpoint_type =
          Util.option_bind (Xml.member "CustomEndpointType" xml) String.parse
      ; static_members =
          Util.of_option
            []
            (Util.option_bind (Xml.member "StaticMembers" xml) StringList.parse)
      ; excluded_members =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ExcludedMembers" xml) StringList.parse)
      ; d_b_cluster_endpoint_arn =
          Util.option_bind (Xml.member "DBClusterEndpointArn" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_endpoint_arn (fun f ->
               Query.Pair ("DBClusterEndpointArn", String.to_query f))
         ; Some
             (Query.Pair ("ExcludedMembers.member", StringList.to_query v.excluded_members))
         ; Some
             (Query.Pair ("StaticMembers.member", StringList.to_query v.static_members))
         ; Util.option_map v.custom_endpoint_type (fun f ->
               Query.Pair ("CustomEndpointType", String.to_query f))
         ; Util.option_map v.endpoint_type (fun f ->
               Query.Pair ("EndpointType", String.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.endpoint (fun f ->
               Query.Pair ("Endpoint", String.to_query f))
         ; Util.option_map v.d_b_cluster_endpoint_resource_identifier (fun f ->
               Query.Pair ("DBClusterEndpointResourceIdentifier", String.to_query f))
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               Query.Pair ("DBClusterIdentifier", String.to_query f))
         ; Util.option_map v.d_b_cluster_endpoint_identifier (fun f ->
               Query.Pair ("DBClusterEndpointIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_endpoint_arn (fun f ->
               "d_b_cluster_endpoint_arn", String.to_json f)
         ; Some ("excluded_members", StringList.to_json v.excluded_members)
         ; Some ("static_members", StringList.to_json v.static_members)
         ; Util.option_map v.custom_endpoint_type (fun f ->
               "custom_endpoint_type", String.to_json f)
         ; Util.option_map v.endpoint_type (fun f -> "endpoint_type", String.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.endpoint (fun f -> "endpoint", String.to_json f)
         ; Util.option_map v.d_b_cluster_endpoint_resource_identifier (fun f ->
               "d_b_cluster_endpoint_resource_identifier", String.to_json f)
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               "d_b_cluster_identifier", String.to_json f)
         ; Util.option_map v.d_b_cluster_endpoint_identifier (fun f ->
               "d_b_cluster_endpoint_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_cluster_endpoint_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_endpoint_identifier") String.of_json
    ; d_b_cluster_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_identifier") String.of_json
    ; d_b_cluster_endpoint_resource_identifier =
        Util.option_map
          (Json.lookup j "d_b_cluster_endpoint_resource_identifier")
          String.of_json
    ; endpoint = Util.option_map (Json.lookup j "endpoint") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; endpoint_type = Util.option_map (Json.lookup j "endpoint_type") String.of_json
    ; custom_endpoint_type =
        Util.option_map (Json.lookup j "custom_endpoint_type") String.of_json
    ; static_members =
        StringList.of_json (Util.of_option_exn (Json.lookup j "static_members"))
    ; excluded_members =
        StringList.of_json (Util.of_option_exn (Json.lookup j "excluded_members"))
    ; d_b_cluster_endpoint_arn =
        Util.option_map (Json.lookup j "d_b_cluster_endpoint_arn") String.of_json
    }
end

module EventSubscription = struct
  type t =
    { customer_aws_id : String.t option
    ; cust_subscription_id : String.t option
    ; sns_topic_arn : String.t option
    ; status : String.t option
    ; subscription_creation_time : String.t option
    ; source_type : String.t option
    ; source_ids_list : SourceIdsList.t
    ; event_categories_list : EventCategoriesList.t
    ; enabled : Boolean.t option
    ; event_subscription_arn : String.t option
    }

  let make
      ?customer_aws_id
      ?cust_subscription_id
      ?sns_topic_arn
      ?status
      ?subscription_creation_time
      ?source_type
      ?(source_ids_list = [])
      ?(event_categories_list = [])
      ?enabled
      ?event_subscription_arn
      () =
    { customer_aws_id
    ; cust_subscription_id
    ; sns_topic_arn
    ; status
    ; subscription_creation_time
    ; source_type
    ; source_ids_list
    ; event_categories_list
    ; enabled
    ; event_subscription_arn
    }

  let parse xml =
    Some
      { customer_aws_id = Util.option_bind (Xml.member "CustomerAwsId" xml) String.parse
      ; cust_subscription_id =
          Util.option_bind (Xml.member "CustSubscriptionId" xml) String.parse
      ; sns_topic_arn = Util.option_bind (Xml.member "SnsTopicArn" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; subscription_creation_time =
          Util.option_bind (Xml.member "SubscriptionCreationTime" xml) String.parse
      ; source_type = Util.option_bind (Xml.member "SourceType" xml) String.parse
      ; source_ids_list =
          Util.of_option
            []
            (Util.option_bind (Xml.member "SourceIdsList" xml) SourceIdsList.parse)
      ; event_categories_list =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EventCategoriesList" xml)
               EventCategoriesList.parse)
      ; enabled = Util.option_bind (Xml.member "Enabled" xml) Boolean.parse
      ; event_subscription_arn =
          Util.option_bind (Xml.member "EventSubscriptionArn" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.event_subscription_arn (fun f ->
               Query.Pair ("EventSubscriptionArn", String.to_query f))
         ; Util.option_map v.enabled (fun f -> Query.Pair ("Enabled", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "EventCategoriesList.member"
                , EventCategoriesList.to_query v.event_categories_list ))
         ; Some
             (Query.Pair ("SourceIdsList.member", SourceIdsList.to_query v.source_ids_list))
         ; Util.option_map v.source_type (fun f ->
               Query.Pair ("SourceType", String.to_query f))
         ; Util.option_map v.subscription_creation_time (fun f ->
               Query.Pair ("SubscriptionCreationTime", String.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.sns_topic_arn (fun f ->
               Query.Pair ("SnsTopicArn", String.to_query f))
         ; Util.option_map v.cust_subscription_id (fun f ->
               Query.Pair ("CustSubscriptionId", String.to_query f))
         ; Util.option_map v.customer_aws_id (fun f ->
               Query.Pair ("CustomerAwsId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.event_subscription_arn (fun f ->
               "event_subscription_arn", String.to_json f)
         ; Util.option_map v.enabled (fun f -> "enabled", Boolean.to_json f)
         ; Some
             ("event_categories_list", EventCategoriesList.to_json v.event_categories_list)
         ; Some ("source_ids_list", SourceIdsList.to_json v.source_ids_list)
         ; Util.option_map v.source_type (fun f -> "source_type", String.to_json f)
         ; Util.option_map v.subscription_creation_time (fun f ->
               "subscription_creation_time", String.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.sns_topic_arn (fun f -> "sns_topic_arn", String.to_json f)
         ; Util.option_map v.cust_subscription_id (fun f ->
               "cust_subscription_id", String.to_json f)
         ; Util.option_map v.customer_aws_id (fun f ->
               "customer_aws_id", String.to_json f)
         ])

  let of_json j =
    { customer_aws_id = Util.option_map (Json.lookup j "customer_aws_id") String.of_json
    ; cust_subscription_id =
        Util.option_map (Json.lookup j "cust_subscription_id") String.of_json
    ; sns_topic_arn = Util.option_map (Json.lookup j "sns_topic_arn") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; subscription_creation_time =
        Util.option_map (Json.lookup j "subscription_creation_time") String.of_json
    ; source_type = Util.option_map (Json.lookup j "source_type") String.of_json
    ; source_ids_list =
        SourceIdsList.of_json (Util.of_option_exn (Json.lookup j "source_ids_list"))
    ; event_categories_list =
        EventCategoriesList.of_json
          (Util.of_option_exn (Json.lookup j "event_categories_list"))
    ; enabled = Util.option_map (Json.lookup j "enabled") Boolean.of_json
    ; event_subscription_arn =
        Util.option_map (Json.lookup j "event_subscription_arn") String.of_json
    }
end

module OptionConfiguration = struct
  type t =
    { option_name : String.t
    ; port : Integer.t option
    ; option_version : String.t option
    ; d_b_security_group_memberships : DBSecurityGroupNameList.t
    ; vpc_security_group_memberships : VpcSecurityGroupIdList.t
    ; option_settings : OptionSettingsList.t
    }

  let make
      ~option_name
      ?port
      ?option_version
      ?(d_b_security_group_memberships = [])
      ?(vpc_security_group_memberships = [])
      ?(option_settings = [])
      () =
    { option_name
    ; port
    ; option_version
    ; d_b_security_group_memberships
    ; vpc_security_group_memberships
    ; option_settings
    }

  let parse xml =
    Some
      { option_name =
          Xml.required
            "OptionName"
            (Util.option_bind (Xml.member "OptionName" xml) String.parse)
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; option_version = Util.option_bind (Xml.member "OptionVersion" xml) String.parse
      ; d_b_security_group_memberships =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBSecurityGroupMemberships" xml)
               DBSecurityGroupNameList.parse)
      ; vpc_security_group_memberships =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupMemberships" xml)
               VpcSecurityGroupIdList.parse)
      ; option_settings =
          Util.of_option
            []
            (Util.option_bind (Xml.member "OptionSettings" xml) OptionSettingsList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("OptionSettings.member", OptionSettingsList.to_query v.option_settings))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupMemberships.member"
                , VpcSecurityGroupIdList.to_query v.vpc_security_group_memberships ))
         ; Some
             (Query.Pair
                ( "DBSecurityGroupMemberships.member"
                , DBSecurityGroupNameList.to_query v.d_b_security_group_memberships ))
         ; Util.option_map v.option_version (fun f ->
               Query.Pair ("OptionVersion", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Some (Query.Pair ("OptionName", String.to_query v.option_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("option_settings", OptionSettingsList.to_json v.option_settings)
         ; Some
             ( "vpc_security_group_memberships"
             , VpcSecurityGroupIdList.to_json v.vpc_security_group_memberships )
         ; Some
             ( "d_b_security_group_memberships"
             , DBSecurityGroupNameList.to_json v.d_b_security_group_memberships )
         ; Util.option_map v.option_version (fun f -> "option_version", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Some ("option_name", String.to_json v.option_name)
         ])

  let of_json j =
    { option_name = String.of_json (Util.of_option_exn (Json.lookup j "option_name"))
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; option_version = Util.option_map (Json.lookup j "option_version") String.of_json
    ; d_b_security_group_memberships =
        DBSecurityGroupNameList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_security_group_memberships"))
    ; vpc_security_group_memberships =
        VpcSecurityGroupIdList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_memberships"))
    ; option_settings =
        OptionSettingsList.of_json (Util.of_option_exn (Json.lookup j "option_settings"))
    }
end

module DBSnapshot = struct
  type t =
    { d_b_snapshot_identifier : String.t option
    ; d_b_instance_identifier : String.t option
    ; snapshot_create_time : DateTime.t option
    ; engine : String.t option
    ; allocated_storage : Integer.t option
    ; status : String.t option
    ; port : Integer.t option
    ; availability_zone : String.t option
    ; vpc_id : String.t option
    ; instance_create_time : DateTime.t option
    ; master_username : String.t option
    ; engine_version : String.t option
    ; license_model : String.t option
    ; snapshot_type : String.t option
    ; iops : Integer.t option
    ; option_group_name : String.t option
    ; percent_progress : Integer.t option
    ; source_region : String.t option
    ; source_d_b_snapshot_identifier : String.t option
    ; storage_type : String.t option
    ; tde_credential_arn : String.t option
    ; encrypted : Boolean.t option
    ; kms_key_id : String.t option
    ; d_b_snapshot_arn : String.t option
    ; timezone : String.t option
    ; i_a_m_database_authentication_enabled : Boolean.t option
    ; processor_features : ProcessorFeatureList.t
    ; dbi_resource_id : String.t option
    ; tag_list : TagList.t
    }

  let make
      ?d_b_snapshot_identifier
      ?d_b_instance_identifier
      ?snapshot_create_time
      ?engine
      ?allocated_storage
      ?status
      ?port
      ?availability_zone
      ?vpc_id
      ?instance_create_time
      ?master_username
      ?engine_version
      ?license_model
      ?snapshot_type
      ?iops
      ?option_group_name
      ?percent_progress
      ?source_region
      ?source_d_b_snapshot_identifier
      ?storage_type
      ?tde_credential_arn
      ?encrypted
      ?kms_key_id
      ?d_b_snapshot_arn
      ?timezone
      ?i_a_m_database_authentication_enabled
      ?(processor_features = [])
      ?dbi_resource_id
      ?(tag_list = [])
      () =
    { d_b_snapshot_identifier
    ; d_b_instance_identifier
    ; snapshot_create_time
    ; engine
    ; allocated_storage
    ; status
    ; port
    ; availability_zone
    ; vpc_id
    ; instance_create_time
    ; master_username
    ; engine_version
    ; license_model
    ; snapshot_type
    ; iops
    ; option_group_name
    ; percent_progress
    ; source_region
    ; source_d_b_snapshot_identifier
    ; storage_type
    ; tde_credential_arn
    ; encrypted
    ; kms_key_id
    ; d_b_snapshot_arn
    ; timezone
    ; i_a_m_database_authentication_enabled
    ; processor_features
    ; dbi_resource_id
    ; tag_list
    }

  let parse xml =
    Some
      { d_b_snapshot_identifier =
          Util.option_bind (Xml.member "DBSnapshotIdentifier" xml) String.parse
      ; d_b_instance_identifier =
          Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse
      ; snapshot_create_time =
          Util.option_bind (Xml.member "SnapshotCreateTime" xml) DateTime.parse
      ; engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; allocated_storage =
          Util.option_bind (Xml.member "AllocatedStorage" xml) Integer.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; availability_zone =
          Util.option_bind (Xml.member "AvailabilityZone" xml) String.parse
      ; vpc_id = Util.option_bind (Xml.member "VpcId" xml) String.parse
      ; instance_create_time =
          Util.option_bind (Xml.member "InstanceCreateTime" xml) DateTime.parse
      ; master_username = Util.option_bind (Xml.member "MasterUsername" xml) String.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; license_model = Util.option_bind (Xml.member "LicenseModel" xml) String.parse
      ; snapshot_type = Util.option_bind (Xml.member "SnapshotType" xml) String.parse
      ; iops = Util.option_bind (Xml.member "Iops" xml) Integer.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; percent_progress =
          Util.option_bind (Xml.member "PercentProgress" xml) Integer.parse
      ; source_region = Util.option_bind (Xml.member "SourceRegion" xml) String.parse
      ; source_d_b_snapshot_identifier =
          Util.option_bind (Xml.member "SourceDBSnapshotIdentifier" xml) String.parse
      ; storage_type = Util.option_bind (Xml.member "StorageType" xml) String.parse
      ; tde_credential_arn =
          Util.option_bind (Xml.member "TdeCredentialArn" xml) String.parse
      ; encrypted = Util.option_bind (Xml.member "Encrypted" xml) Boolean.parse
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; d_b_snapshot_arn = Util.option_bind (Xml.member "DBSnapshotArn" xml) String.parse
      ; timezone = Util.option_bind (Xml.member "Timezone" xml) String.parse
      ; i_a_m_database_authentication_enabled =
          Util.option_bind
            (Xml.member "IAMDatabaseAuthenticationEnabled" xml)
            Boolean.parse
      ; processor_features =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ProcessorFeatures" xml)
               ProcessorFeatureList.parse)
      ; dbi_resource_id = Util.option_bind (Xml.member "DbiResourceId" xml) String.parse
      ; tag_list =
          Util.of_option [] (Util.option_bind (Xml.member "TagList" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("TagList.member", TagList.to_query v.tag_list))
         ; Util.option_map v.dbi_resource_id (fun f ->
               Query.Pair ("DbiResourceId", String.to_query f))
         ; Some
             (Query.Pair
                ( "ProcessorFeatures.member"
                , ProcessorFeatureList.to_query v.processor_features ))
         ; Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
               Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
         ; Util.option_map v.timezone (fun f ->
               Query.Pair ("Timezone", String.to_query f))
         ; Util.option_map v.d_b_snapshot_arn (fun f ->
               Query.Pair ("DBSnapshotArn", String.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Util.option_map v.encrypted (fun f ->
               Query.Pair ("Encrypted", Boolean.to_query f))
         ; Util.option_map v.tde_credential_arn (fun f ->
               Query.Pair ("TdeCredentialArn", String.to_query f))
         ; Util.option_map v.storage_type (fun f ->
               Query.Pair ("StorageType", String.to_query f))
         ; Util.option_map v.source_d_b_snapshot_identifier (fun f ->
               Query.Pair ("SourceDBSnapshotIdentifier", String.to_query f))
         ; Util.option_map v.source_region (fun f ->
               Query.Pair ("SourceRegion", String.to_query f))
         ; Util.option_map v.percent_progress (fun f ->
               Query.Pair ("PercentProgress", Integer.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.iops (fun f -> Query.Pair ("Iops", Integer.to_query f))
         ; Util.option_map v.snapshot_type (fun f ->
               Query.Pair ("SnapshotType", String.to_query f))
         ; Util.option_map v.license_model (fun f ->
               Query.Pair ("LicenseModel", String.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.master_username (fun f ->
               Query.Pair ("MasterUsername", String.to_query f))
         ; Util.option_map v.instance_create_time (fun f ->
               Query.Pair ("InstanceCreateTime", DateTime.to_query f))
         ; Util.option_map v.vpc_id (fun f -> Query.Pair ("VpcId", String.to_query f))
         ; Util.option_map v.availability_zone (fun f ->
               Query.Pair ("AvailabilityZone", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.allocated_storage (fun f ->
               Query.Pair ("AllocatedStorage", Integer.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ; Util.option_map v.snapshot_create_time (fun f ->
               Query.Pair ("SnapshotCreateTime", DateTime.to_query f))
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               Query.Pair ("DBInstanceIdentifier", String.to_query f))
         ; Util.option_map v.d_b_snapshot_identifier (fun f ->
               Query.Pair ("DBSnapshotIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tag_list", TagList.to_json v.tag_list)
         ; Util.option_map v.dbi_resource_id (fun f ->
               "dbi_resource_id", String.to_json f)
         ; Some ("processor_features", ProcessorFeatureList.to_json v.processor_features)
         ; Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
               "i_a_m_database_authentication_enabled", Boolean.to_json f)
         ; Util.option_map v.timezone (fun f -> "timezone", String.to_json f)
         ; Util.option_map v.d_b_snapshot_arn (fun f ->
               "d_b_snapshot_arn", String.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Util.option_map v.encrypted (fun f -> "encrypted", Boolean.to_json f)
         ; Util.option_map v.tde_credential_arn (fun f ->
               "tde_credential_arn", String.to_json f)
         ; Util.option_map v.storage_type (fun f -> "storage_type", String.to_json f)
         ; Util.option_map v.source_d_b_snapshot_identifier (fun f ->
               "source_d_b_snapshot_identifier", String.to_json f)
         ; Util.option_map v.source_region (fun f -> "source_region", String.to_json f)
         ; Util.option_map v.percent_progress (fun f ->
               "percent_progress", Integer.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
         ; Util.option_map v.snapshot_type (fun f -> "snapshot_type", String.to_json f)
         ; Util.option_map v.license_model (fun f -> "license_model", String.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.master_username (fun f ->
               "master_username", String.to_json f)
         ; Util.option_map v.instance_create_time (fun f ->
               "instance_create_time", DateTime.to_json f)
         ; Util.option_map v.vpc_id (fun f -> "vpc_id", String.to_json f)
         ; Util.option_map v.availability_zone (fun f ->
               "availability_zone", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.allocated_storage (fun f ->
               "allocated_storage", Integer.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ; Util.option_map v.snapshot_create_time (fun f ->
               "snapshot_create_time", DateTime.to_json f)
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               "d_b_instance_identifier", String.to_json f)
         ; Util.option_map v.d_b_snapshot_identifier (fun f ->
               "d_b_snapshot_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_snapshot_identifier =
        Util.option_map (Json.lookup j "d_b_snapshot_identifier") String.of_json
    ; d_b_instance_identifier =
        Util.option_map (Json.lookup j "d_b_instance_identifier") String.of_json
    ; snapshot_create_time =
        Util.option_map (Json.lookup j "snapshot_create_time") DateTime.of_json
    ; engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; allocated_storage =
        Util.option_map (Json.lookup j "allocated_storage") Integer.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; availability_zone =
        Util.option_map (Json.lookup j "availability_zone") String.of_json
    ; vpc_id = Util.option_map (Json.lookup j "vpc_id") String.of_json
    ; instance_create_time =
        Util.option_map (Json.lookup j "instance_create_time") DateTime.of_json
    ; master_username = Util.option_map (Json.lookup j "master_username") String.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; license_model = Util.option_map (Json.lookup j "license_model") String.of_json
    ; snapshot_type = Util.option_map (Json.lookup j "snapshot_type") String.of_json
    ; iops = Util.option_map (Json.lookup j "iops") Integer.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; percent_progress =
        Util.option_map (Json.lookup j "percent_progress") Integer.of_json
    ; source_region = Util.option_map (Json.lookup j "source_region") String.of_json
    ; source_d_b_snapshot_identifier =
        Util.option_map (Json.lookup j "source_d_b_snapshot_identifier") String.of_json
    ; storage_type = Util.option_map (Json.lookup j "storage_type") String.of_json
    ; tde_credential_arn =
        Util.option_map (Json.lookup j "tde_credential_arn") String.of_json
    ; encrypted = Util.option_map (Json.lookup j "encrypted") Boolean.of_json
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; d_b_snapshot_arn = Util.option_map (Json.lookup j "d_b_snapshot_arn") String.of_json
    ; timezone = Util.option_map (Json.lookup j "timezone") String.of_json
    ; i_a_m_database_authentication_enabled =
        Util.option_map
          (Json.lookup j "i_a_m_database_authentication_enabled")
          Boolean.of_json
    ; processor_features =
        ProcessorFeatureList.of_json
          (Util.of_option_exn (Json.lookup j "processor_features"))
    ; dbi_resource_id = Util.option_map (Json.lookup j "dbi_resource_id") String.of_json
    ; tag_list = TagList.of_json (Util.of_option_exn (Json.lookup j "tag_list"))
    }
end

module DBSnapshotAttributeList = struct
  type t = DBSnapshotAttribute.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map DBSnapshotAttribute.parse (Xml.members "DBSnapshotAttribute" xml))

  let to_query v = Query.to_query_list DBSnapshotAttribute.to_query v

  let to_json v = `List (List.map DBSnapshotAttribute.to_json v)

  let of_json j = Json.to_list DBSnapshotAttribute.of_json j
end

module DBProxy = struct
  type t =
    { d_b_proxy_name : String.t option
    ; d_b_proxy_arn : String.t option
    ; status : DBProxyStatus.t option
    ; engine_family : String.t option
    ; vpc_security_group_ids : StringList.t
    ; vpc_subnet_ids : StringList.t
    ; auth : UserAuthConfigInfoList.t
    ; role_arn : String.t option
    ; endpoint : String.t option
    ; require_t_l_s : Boolean.t option
    ; idle_client_timeout : Integer.t option
    ; debug_logging : Boolean.t option
    ; created_date : DateTime.t option
    ; updated_date : DateTime.t option
    }

  let make
      ?d_b_proxy_name
      ?d_b_proxy_arn
      ?status
      ?engine_family
      ?(vpc_security_group_ids = [])
      ?(vpc_subnet_ids = [])
      ?(auth = [])
      ?role_arn
      ?endpoint
      ?require_t_l_s
      ?idle_client_timeout
      ?debug_logging
      ?created_date
      ?updated_date
      () =
    { d_b_proxy_name
    ; d_b_proxy_arn
    ; status
    ; engine_family
    ; vpc_security_group_ids
    ; vpc_subnet_ids
    ; auth
    ; role_arn
    ; endpoint
    ; require_t_l_s
    ; idle_client_timeout
    ; debug_logging
    ; created_date
    ; updated_date
    }

  let parse xml =
    Some
      { d_b_proxy_name = Util.option_bind (Xml.member "DBProxyName" xml) String.parse
      ; d_b_proxy_arn = Util.option_bind (Xml.member "DBProxyArn" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) DBProxyStatus.parse
      ; engine_family = Util.option_bind (Xml.member "EngineFamily" xml) String.parse
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind (Xml.member "VpcSecurityGroupIds" xml) StringList.parse)
      ; vpc_subnet_ids =
          Util.of_option
            []
            (Util.option_bind (Xml.member "VpcSubnetIds" xml) StringList.parse)
      ; auth =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Auth" xml) UserAuthConfigInfoList.parse)
      ; role_arn = Util.option_bind (Xml.member "RoleArn" xml) String.parse
      ; endpoint = Util.option_bind (Xml.member "Endpoint" xml) String.parse
      ; require_t_l_s = Util.option_bind (Xml.member "RequireTLS" xml) Boolean.parse
      ; idle_client_timeout =
          Util.option_bind (Xml.member "IdleClientTimeout" xml) Integer.parse
      ; debug_logging = Util.option_bind (Xml.member "DebugLogging" xml) Boolean.parse
      ; created_date = Util.option_bind (Xml.member "CreatedDate" xml) DateTime.parse
      ; updated_date = Util.option_bind (Xml.member "UpdatedDate" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.updated_date (fun f ->
               Query.Pair ("UpdatedDate", DateTime.to_query f))
         ; Util.option_map v.created_date (fun f ->
               Query.Pair ("CreatedDate", DateTime.to_query f))
         ; Util.option_map v.debug_logging (fun f ->
               Query.Pair ("DebugLogging", Boolean.to_query f))
         ; Util.option_map v.idle_client_timeout (fun f ->
               Query.Pair ("IdleClientTimeout", Integer.to_query f))
         ; Util.option_map v.require_t_l_s (fun f ->
               Query.Pair ("RequireTLS", Boolean.to_query f))
         ; Util.option_map v.endpoint (fun f ->
               Query.Pair ("Endpoint", String.to_query f))
         ; Util.option_map v.role_arn (fun f -> Query.Pair ("RoleArn", String.to_query f))
         ; Some (Query.Pair ("Auth.member", UserAuthConfigInfoList.to_query v.auth))
         ; Some (Query.Pair ("VpcSubnetIds.member", StringList.to_query v.vpc_subnet_ids))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , StringList.to_query v.vpc_security_group_ids ))
         ; Util.option_map v.engine_family (fun f ->
               Query.Pair ("EngineFamily", String.to_query f))
         ; Util.option_map v.status (fun f ->
               Query.Pair ("Status", DBProxyStatus.to_query f))
         ; Util.option_map v.d_b_proxy_arn (fun f ->
               Query.Pair ("DBProxyArn", String.to_query f))
         ; Util.option_map v.d_b_proxy_name (fun f ->
               Query.Pair ("DBProxyName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.updated_date (fun f -> "updated_date", DateTime.to_json f)
         ; Util.option_map v.created_date (fun f -> "created_date", DateTime.to_json f)
         ; Util.option_map v.debug_logging (fun f -> "debug_logging", Boolean.to_json f)
         ; Util.option_map v.idle_client_timeout (fun f ->
               "idle_client_timeout", Integer.to_json f)
         ; Util.option_map v.require_t_l_s (fun f -> "require_t_l_s", Boolean.to_json f)
         ; Util.option_map v.endpoint (fun f -> "endpoint", String.to_json f)
         ; Util.option_map v.role_arn (fun f -> "role_arn", String.to_json f)
         ; Some ("auth", UserAuthConfigInfoList.to_json v.auth)
         ; Some ("vpc_subnet_ids", StringList.to_json v.vpc_subnet_ids)
         ; Some ("vpc_security_group_ids", StringList.to_json v.vpc_security_group_ids)
         ; Util.option_map v.engine_family (fun f -> "engine_family", String.to_json f)
         ; Util.option_map v.status (fun f -> "status", DBProxyStatus.to_json f)
         ; Util.option_map v.d_b_proxy_arn (fun f -> "d_b_proxy_arn", String.to_json f)
         ; Util.option_map v.d_b_proxy_name (fun f -> "d_b_proxy_name", String.to_json f)
         ])

  let of_json j =
    { d_b_proxy_name = Util.option_map (Json.lookup j "d_b_proxy_name") String.of_json
    ; d_b_proxy_arn = Util.option_map (Json.lookup j "d_b_proxy_arn") String.of_json
    ; status = Util.option_map (Json.lookup j "status") DBProxyStatus.of_json
    ; engine_family = Util.option_map (Json.lookup j "engine_family") String.of_json
    ; vpc_security_group_ids =
        StringList.of_json (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; vpc_subnet_ids =
        StringList.of_json (Util.of_option_exn (Json.lookup j "vpc_subnet_ids"))
    ; auth = UserAuthConfigInfoList.of_json (Util.of_option_exn (Json.lookup j "auth"))
    ; role_arn = Util.option_map (Json.lookup j "role_arn") String.of_json
    ; endpoint = Util.option_map (Json.lookup j "endpoint") String.of_json
    ; require_t_l_s = Util.option_map (Json.lookup j "require_t_l_s") Boolean.of_json
    ; idle_client_timeout =
        Util.option_map (Json.lookup j "idle_client_timeout") Integer.of_json
    ; debug_logging = Util.option_map (Json.lookup j "debug_logging") Boolean.of_json
    ; created_date = Util.option_map (Json.lookup j "created_date") DateTime.of_json
    ; updated_date = Util.option_map (Json.lookup j "updated_date") DateTime.of_json
    }
end

module DBInstance = struct
  type t =
    { d_b_instance_identifier : String.t option
    ; d_b_instance_class : String.t option
    ; engine : String.t option
    ; d_b_instance_status : String.t option
    ; master_username : String.t option
    ; d_b_name : String.t option
    ; endpoint : Endpoint.t option
    ; allocated_storage : Integer.t option
    ; instance_create_time : DateTime.t option
    ; preferred_backup_window : String.t option
    ; backup_retention_period : Integer.t option
    ; d_b_security_groups : DBSecurityGroupMembershipList.t
    ; vpc_security_groups : VpcSecurityGroupMembershipList.t
    ; d_b_parameter_groups : DBParameterGroupStatusList.t
    ; availability_zone : String.t option
    ; d_b_subnet_group : DBSubnetGroup.t option
    ; preferred_maintenance_window : String.t option
    ; pending_modified_values : PendingModifiedValues.t option
    ; latest_restorable_time : DateTime.t option
    ; multi_a_z : Boolean.t option
    ; engine_version : String.t option
    ; auto_minor_version_upgrade : Boolean.t option
    ; read_replica_source_d_b_instance_identifier : String.t option
    ; read_replica_d_b_instance_identifiers : ReadReplicaDBInstanceIdentifierList.t
    ; read_replica_d_b_cluster_identifiers : ReadReplicaDBClusterIdentifierList.t
    ; replica_mode : ReplicaMode.t option
    ; license_model : String.t option
    ; iops : Integer.t option
    ; option_group_memberships : OptionGroupMembershipList.t
    ; character_set_name : String.t option
    ; nchar_character_set_name : String.t option
    ; secondary_availability_zone : String.t option
    ; publicly_accessible : Boolean.t option
    ; status_infos : DBInstanceStatusInfoList.t
    ; storage_type : String.t option
    ; tde_credential_arn : String.t option
    ; db_instance_port : Integer.t option
    ; d_b_cluster_identifier : String.t option
    ; storage_encrypted : Boolean.t option
    ; kms_key_id : String.t option
    ; dbi_resource_id : String.t option
    ; c_a_certificate_identifier : String.t option
    ; domain_memberships : DomainMembershipList.t
    ; copy_tags_to_snapshot : Boolean.t option
    ; monitoring_interval : Integer.t option
    ; enhanced_monitoring_resource_arn : String.t option
    ; monitoring_role_arn : String.t option
    ; promotion_tier : Integer.t option
    ; d_b_instance_arn : String.t option
    ; timezone : String.t option
    ; i_a_m_database_authentication_enabled : Boolean.t option
    ; performance_insights_enabled : Boolean.t option
    ; performance_insights_k_m_s_key_id : String.t option
    ; performance_insights_retention_period : Integer.t option
    ; enabled_cloudwatch_logs_exports : LogTypeList.t
    ; processor_features : ProcessorFeatureList.t
    ; deletion_protection : Boolean.t option
    ; associated_roles : DBInstanceRoles.t
    ; listener_endpoint : Endpoint.t option
    ; max_allocated_storage : Integer.t option
    ; tag_list : TagList.t
    }

  let make
      ?d_b_instance_identifier
      ?d_b_instance_class
      ?engine
      ?d_b_instance_status
      ?master_username
      ?d_b_name
      ?endpoint
      ?allocated_storage
      ?instance_create_time
      ?preferred_backup_window
      ?backup_retention_period
      ?(d_b_security_groups = [])
      ?(vpc_security_groups = [])
      ?(d_b_parameter_groups = [])
      ?availability_zone
      ?d_b_subnet_group
      ?preferred_maintenance_window
      ?pending_modified_values
      ?latest_restorable_time
      ?multi_a_z
      ?engine_version
      ?auto_minor_version_upgrade
      ?read_replica_source_d_b_instance_identifier
      ?(read_replica_d_b_instance_identifiers = [])
      ?(read_replica_d_b_cluster_identifiers = [])
      ?replica_mode
      ?license_model
      ?iops
      ?(option_group_memberships = [])
      ?character_set_name
      ?nchar_character_set_name
      ?secondary_availability_zone
      ?publicly_accessible
      ?(status_infos = [])
      ?storage_type
      ?tde_credential_arn
      ?db_instance_port
      ?d_b_cluster_identifier
      ?storage_encrypted
      ?kms_key_id
      ?dbi_resource_id
      ?c_a_certificate_identifier
      ?(domain_memberships = [])
      ?copy_tags_to_snapshot
      ?monitoring_interval
      ?enhanced_monitoring_resource_arn
      ?monitoring_role_arn
      ?promotion_tier
      ?d_b_instance_arn
      ?timezone
      ?i_a_m_database_authentication_enabled
      ?performance_insights_enabled
      ?performance_insights_k_m_s_key_id
      ?performance_insights_retention_period
      ?(enabled_cloudwatch_logs_exports = [])
      ?(processor_features = [])
      ?deletion_protection
      ?(associated_roles = [])
      ?listener_endpoint
      ?max_allocated_storage
      ?(tag_list = [])
      () =
    { d_b_instance_identifier
    ; d_b_instance_class
    ; engine
    ; d_b_instance_status
    ; master_username
    ; d_b_name
    ; endpoint
    ; allocated_storage
    ; instance_create_time
    ; preferred_backup_window
    ; backup_retention_period
    ; d_b_security_groups
    ; vpc_security_groups
    ; d_b_parameter_groups
    ; availability_zone
    ; d_b_subnet_group
    ; preferred_maintenance_window
    ; pending_modified_values
    ; latest_restorable_time
    ; multi_a_z
    ; engine_version
    ; auto_minor_version_upgrade
    ; read_replica_source_d_b_instance_identifier
    ; read_replica_d_b_instance_identifiers
    ; read_replica_d_b_cluster_identifiers
    ; replica_mode
    ; license_model
    ; iops
    ; option_group_memberships
    ; character_set_name
    ; nchar_character_set_name
    ; secondary_availability_zone
    ; publicly_accessible
    ; status_infos
    ; storage_type
    ; tde_credential_arn
    ; db_instance_port
    ; d_b_cluster_identifier
    ; storage_encrypted
    ; kms_key_id
    ; dbi_resource_id
    ; c_a_certificate_identifier
    ; domain_memberships
    ; copy_tags_to_snapshot
    ; monitoring_interval
    ; enhanced_monitoring_resource_arn
    ; monitoring_role_arn
    ; promotion_tier
    ; d_b_instance_arn
    ; timezone
    ; i_a_m_database_authentication_enabled
    ; performance_insights_enabled
    ; performance_insights_k_m_s_key_id
    ; performance_insights_retention_period
    ; enabled_cloudwatch_logs_exports
    ; processor_features
    ; deletion_protection
    ; associated_roles
    ; listener_endpoint
    ; max_allocated_storage
    ; tag_list
    }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse
      ; d_b_instance_class =
          Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse
      ; engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; d_b_instance_status =
          Util.option_bind (Xml.member "DBInstanceStatus" xml) String.parse
      ; master_username = Util.option_bind (Xml.member "MasterUsername" xml) String.parse
      ; d_b_name = Util.option_bind (Xml.member "DBName" xml) String.parse
      ; endpoint = Util.option_bind (Xml.member "Endpoint" xml) Endpoint.parse
      ; allocated_storage =
          Util.option_bind (Xml.member "AllocatedStorage" xml) Integer.parse
      ; instance_create_time =
          Util.option_bind (Xml.member "InstanceCreateTime" xml) DateTime.parse
      ; preferred_backup_window =
          Util.option_bind (Xml.member "PreferredBackupWindow" xml) String.parse
      ; backup_retention_period =
          Util.option_bind (Xml.member "BackupRetentionPeriod" xml) Integer.parse
      ; d_b_security_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBSecurityGroups" xml)
               DBSecurityGroupMembershipList.parse)
      ; vpc_security_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroups" xml)
               VpcSecurityGroupMembershipList.parse)
      ; d_b_parameter_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBParameterGroups" xml)
               DBParameterGroupStatusList.parse)
      ; availability_zone =
          Util.option_bind (Xml.member "AvailabilityZone" xml) String.parse
      ; d_b_subnet_group =
          Util.option_bind (Xml.member "DBSubnetGroup" xml) DBSubnetGroup.parse
      ; preferred_maintenance_window =
          Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml) String.parse
      ; pending_modified_values =
          Util.option_bind
            (Xml.member "PendingModifiedValues" xml)
            PendingModifiedValues.parse
      ; latest_restorable_time =
          Util.option_bind (Xml.member "LatestRestorableTime" xml) DateTime.parse
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; auto_minor_version_upgrade =
          Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
      ; read_replica_source_d_b_instance_identifier =
          Util.option_bind
            (Xml.member "ReadReplicaSourceDBInstanceIdentifier" xml)
            String.parse
      ; read_replica_d_b_instance_identifiers =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ReadReplicaDBInstanceIdentifiers" xml)
               ReadReplicaDBInstanceIdentifierList.parse)
      ; read_replica_d_b_cluster_identifiers =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ReadReplicaDBClusterIdentifiers" xml)
               ReadReplicaDBClusterIdentifierList.parse)
      ; replica_mode = Util.option_bind (Xml.member "ReplicaMode" xml) ReplicaMode.parse
      ; license_model = Util.option_bind (Xml.member "LicenseModel" xml) String.parse
      ; iops = Util.option_bind (Xml.member "Iops" xml) Integer.parse
      ; option_group_memberships =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "OptionGroupMemberships" xml)
               OptionGroupMembershipList.parse)
      ; character_set_name =
          Util.option_bind (Xml.member "CharacterSetName" xml) String.parse
      ; nchar_character_set_name =
          Util.option_bind (Xml.member "NcharCharacterSetName" xml) String.parse
      ; secondary_availability_zone =
          Util.option_bind (Xml.member "SecondaryAvailabilityZone" xml) String.parse
      ; publicly_accessible =
          Util.option_bind (Xml.member "PubliclyAccessible" xml) Boolean.parse
      ; status_infos =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "StatusInfos" xml)
               DBInstanceStatusInfoList.parse)
      ; storage_type = Util.option_bind (Xml.member "StorageType" xml) String.parse
      ; tde_credential_arn =
          Util.option_bind (Xml.member "TdeCredentialArn" xml) String.parse
      ; db_instance_port =
          Util.option_bind (Xml.member "DbInstancePort" xml) Integer.parse
      ; d_b_cluster_identifier =
          Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse
      ; storage_encrypted =
          Util.option_bind (Xml.member "StorageEncrypted" xml) Boolean.parse
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; dbi_resource_id = Util.option_bind (Xml.member "DbiResourceId" xml) String.parse
      ; c_a_certificate_identifier =
          Util.option_bind (Xml.member "CACertificateIdentifier" xml) String.parse
      ; domain_memberships =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DomainMemberships" xml)
               DomainMembershipList.parse)
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; monitoring_interval =
          Util.option_bind (Xml.member "MonitoringInterval" xml) Integer.parse
      ; enhanced_monitoring_resource_arn =
          Util.option_bind (Xml.member "EnhancedMonitoringResourceArn" xml) String.parse
      ; monitoring_role_arn =
          Util.option_bind (Xml.member "MonitoringRoleArn" xml) String.parse
      ; promotion_tier = Util.option_bind (Xml.member "PromotionTier" xml) Integer.parse
      ; d_b_instance_arn = Util.option_bind (Xml.member "DBInstanceArn" xml) String.parse
      ; timezone = Util.option_bind (Xml.member "Timezone" xml) String.parse
      ; i_a_m_database_authentication_enabled =
          Util.option_bind
            (Xml.member "IAMDatabaseAuthenticationEnabled" xml)
            Boolean.parse
      ; performance_insights_enabled =
          Util.option_bind (Xml.member "PerformanceInsightsEnabled" xml) Boolean.parse
      ; performance_insights_k_m_s_key_id =
          Util.option_bind (Xml.member "PerformanceInsightsKMSKeyId" xml) String.parse
      ; performance_insights_retention_period =
          Util.option_bind
            (Xml.member "PerformanceInsightsRetentionPeriod" xml)
            Integer.parse
      ; enabled_cloudwatch_logs_exports =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EnabledCloudwatchLogsExports" xml)
               LogTypeList.parse)
      ; processor_features =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ProcessorFeatures" xml)
               ProcessorFeatureList.parse)
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; associated_roles =
          Util.of_option
            []
            (Util.option_bind (Xml.member "AssociatedRoles" xml) DBInstanceRoles.parse)
      ; listener_endpoint =
          Util.option_bind (Xml.member "ListenerEndpoint" xml) Endpoint.parse
      ; max_allocated_storage =
          Util.option_bind (Xml.member "MaxAllocatedStorage" xml) Integer.parse
      ; tag_list =
          Util.of_option [] (Util.option_bind (Xml.member "TagList" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("TagList.member", TagList.to_query v.tag_list))
         ; Util.option_map v.max_allocated_storage (fun f ->
               Query.Pair ("MaxAllocatedStorage", Integer.to_query f))
         ; Util.option_map v.listener_endpoint (fun f ->
               Query.Pair ("ListenerEndpoint", Endpoint.to_query f))
         ; Some
             (Query.Pair
                ("AssociatedRoles.member", DBInstanceRoles.to_query v.associated_roles))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "ProcessorFeatures.member"
                , ProcessorFeatureList.to_query v.processor_features ))
         ; Some
             (Query.Pair
                ( "EnabledCloudwatchLogsExports.member"
                , LogTypeList.to_query v.enabled_cloudwatch_logs_exports ))
         ; Util.option_map v.performance_insights_retention_period (fun f ->
               Query.Pair ("PerformanceInsightsRetentionPeriod", Integer.to_query f))
         ; Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
               Query.Pair ("PerformanceInsightsKMSKeyId", String.to_query f))
         ; Util.option_map v.performance_insights_enabled (fun f ->
               Query.Pair ("PerformanceInsightsEnabled", Boolean.to_query f))
         ; Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
               Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
         ; Util.option_map v.timezone (fun f ->
               Query.Pair ("Timezone", String.to_query f))
         ; Util.option_map v.d_b_instance_arn (fun f ->
               Query.Pair ("DBInstanceArn", String.to_query f))
         ; Util.option_map v.promotion_tier (fun f ->
               Query.Pair ("PromotionTier", Integer.to_query f))
         ; Util.option_map v.monitoring_role_arn (fun f ->
               Query.Pair ("MonitoringRoleArn", String.to_query f))
         ; Util.option_map v.enhanced_monitoring_resource_arn (fun f ->
               Query.Pair ("EnhancedMonitoringResourceArn", String.to_query f))
         ; Util.option_map v.monitoring_interval (fun f ->
               Query.Pair ("MonitoringInterval", Integer.to_query f))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "DomainMemberships.member"
                , DomainMembershipList.to_query v.domain_memberships ))
         ; Util.option_map v.c_a_certificate_identifier (fun f ->
               Query.Pair ("CACertificateIdentifier", String.to_query f))
         ; Util.option_map v.dbi_resource_id (fun f ->
               Query.Pair ("DbiResourceId", String.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Util.option_map v.storage_encrypted (fun f ->
               Query.Pair ("StorageEncrypted", Boolean.to_query f))
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               Query.Pair ("DBClusterIdentifier", String.to_query f))
         ; Util.option_map v.db_instance_port (fun f ->
               Query.Pair ("DbInstancePort", Integer.to_query f))
         ; Util.option_map v.tde_credential_arn (fun f ->
               Query.Pair ("TdeCredentialArn", String.to_query f))
         ; Util.option_map v.storage_type (fun f ->
               Query.Pair ("StorageType", String.to_query f))
         ; Some
             (Query.Pair
                ("StatusInfos.member", DBInstanceStatusInfoList.to_query v.status_infos))
         ; Util.option_map v.publicly_accessible (fun f ->
               Query.Pair ("PubliclyAccessible", Boolean.to_query f))
         ; Util.option_map v.secondary_availability_zone (fun f ->
               Query.Pair ("SecondaryAvailabilityZone", String.to_query f))
         ; Util.option_map v.nchar_character_set_name (fun f ->
               Query.Pair ("NcharCharacterSetName", String.to_query f))
         ; Util.option_map v.character_set_name (fun f ->
               Query.Pair ("CharacterSetName", String.to_query f))
         ; Some
             (Query.Pair
                ( "OptionGroupMemberships.member"
                , OptionGroupMembershipList.to_query v.option_group_memberships ))
         ; Util.option_map v.iops (fun f -> Query.Pair ("Iops", Integer.to_query f))
         ; Util.option_map v.license_model (fun f ->
               Query.Pair ("LicenseModel", String.to_query f))
         ; Util.option_map v.replica_mode (fun f ->
               Query.Pair ("ReplicaMode", ReplicaMode.to_query f))
         ; Some
             (Query.Pair
                ( "ReadReplicaDBClusterIdentifiers.member"
                , ReadReplicaDBClusterIdentifierList.to_query
                    v.read_replica_d_b_cluster_identifiers ))
         ; Some
             (Query.Pair
                ( "ReadReplicaDBInstanceIdentifiers.member"
                , ReadReplicaDBInstanceIdentifierList.to_query
                    v.read_replica_d_b_instance_identifiers ))
         ; Util.option_map v.read_replica_source_d_b_instance_identifier (fun f ->
               Query.Pair ("ReadReplicaSourceDBInstanceIdentifier", String.to_query f))
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Util.option_map v.latest_restorable_time (fun f ->
               Query.Pair ("LatestRestorableTime", DateTime.to_query f))
         ; Util.option_map v.pending_modified_values (fun f ->
               Query.Pair ("PendingModifiedValues", PendingModifiedValues.to_query f))
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Util.option_map v.d_b_subnet_group (fun f ->
               Query.Pair ("DBSubnetGroup", DBSubnetGroup.to_query f))
         ; Util.option_map v.availability_zone (fun f ->
               Query.Pair ("AvailabilityZone", String.to_query f))
         ; Some
             (Query.Pair
                ( "DBParameterGroups.member"
                , DBParameterGroupStatusList.to_query v.d_b_parameter_groups ))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroups.member"
                , VpcSecurityGroupMembershipList.to_query v.vpc_security_groups ))
         ; Some
             (Query.Pair
                ( "DBSecurityGroups.member"
                , DBSecurityGroupMembershipList.to_query v.d_b_security_groups ))
         ; Util.option_map v.backup_retention_period (fun f ->
               Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
         ; Util.option_map v.preferred_backup_window (fun f ->
               Query.Pair ("PreferredBackupWindow", String.to_query f))
         ; Util.option_map v.instance_create_time (fun f ->
               Query.Pair ("InstanceCreateTime", DateTime.to_query f))
         ; Util.option_map v.allocated_storage (fun f ->
               Query.Pair ("AllocatedStorage", Integer.to_query f))
         ; Util.option_map v.endpoint (fun f ->
               Query.Pair ("Endpoint", Endpoint.to_query f))
         ; Util.option_map v.d_b_name (fun f -> Query.Pair ("DBName", String.to_query f))
         ; Util.option_map v.master_username (fun f ->
               Query.Pair ("MasterUsername", String.to_query f))
         ; Util.option_map v.d_b_instance_status (fun f ->
               Query.Pair ("DBInstanceStatus", String.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ; Util.option_map v.d_b_instance_class (fun f ->
               Query.Pair ("DBInstanceClass", String.to_query f))
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               Query.Pair ("DBInstanceIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tag_list", TagList.to_json v.tag_list)
         ; Util.option_map v.max_allocated_storage (fun f ->
               "max_allocated_storage", Integer.to_json f)
         ; Util.option_map v.listener_endpoint (fun f ->
               "listener_endpoint", Endpoint.to_json f)
         ; Some ("associated_roles", DBInstanceRoles.to_json v.associated_roles)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Some ("processor_features", ProcessorFeatureList.to_json v.processor_features)
         ; Some
             ( "enabled_cloudwatch_logs_exports"
             , LogTypeList.to_json v.enabled_cloudwatch_logs_exports )
         ; Util.option_map v.performance_insights_retention_period (fun f ->
               "performance_insights_retention_period", Integer.to_json f)
         ; Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
               "performance_insights_k_m_s_key_id", String.to_json f)
         ; Util.option_map v.performance_insights_enabled (fun f ->
               "performance_insights_enabled", Boolean.to_json f)
         ; Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
               "i_a_m_database_authentication_enabled", Boolean.to_json f)
         ; Util.option_map v.timezone (fun f -> "timezone", String.to_json f)
         ; Util.option_map v.d_b_instance_arn (fun f ->
               "d_b_instance_arn", String.to_json f)
         ; Util.option_map v.promotion_tier (fun f -> "promotion_tier", Integer.to_json f)
         ; Util.option_map v.monitoring_role_arn (fun f ->
               "monitoring_role_arn", String.to_json f)
         ; Util.option_map v.enhanced_monitoring_resource_arn (fun f ->
               "enhanced_monitoring_resource_arn", String.to_json f)
         ; Util.option_map v.monitoring_interval (fun f ->
               "monitoring_interval", Integer.to_json f)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Some ("domain_memberships", DomainMembershipList.to_json v.domain_memberships)
         ; Util.option_map v.c_a_certificate_identifier (fun f ->
               "c_a_certificate_identifier", String.to_json f)
         ; Util.option_map v.dbi_resource_id (fun f ->
               "dbi_resource_id", String.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Util.option_map v.storage_encrypted (fun f ->
               "storage_encrypted", Boolean.to_json f)
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               "d_b_cluster_identifier", String.to_json f)
         ; Util.option_map v.db_instance_port (fun f ->
               "db_instance_port", Integer.to_json f)
         ; Util.option_map v.tde_credential_arn (fun f ->
               "tde_credential_arn", String.to_json f)
         ; Util.option_map v.storage_type (fun f -> "storage_type", String.to_json f)
         ; Some ("status_infos", DBInstanceStatusInfoList.to_json v.status_infos)
         ; Util.option_map v.publicly_accessible (fun f ->
               "publicly_accessible", Boolean.to_json f)
         ; Util.option_map v.secondary_availability_zone (fun f ->
               "secondary_availability_zone", String.to_json f)
         ; Util.option_map v.nchar_character_set_name (fun f ->
               "nchar_character_set_name", String.to_json f)
         ; Util.option_map v.character_set_name (fun f ->
               "character_set_name", String.to_json f)
         ; Some
             ( "option_group_memberships"
             , OptionGroupMembershipList.to_json v.option_group_memberships )
         ; Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
         ; Util.option_map v.license_model (fun f -> "license_model", String.to_json f)
         ; Util.option_map v.replica_mode (fun f -> "replica_mode", ReplicaMode.to_json f)
         ; Some
             ( "read_replica_d_b_cluster_identifiers"
             , ReadReplicaDBClusterIdentifierList.to_json
                 v.read_replica_d_b_cluster_identifiers )
         ; Some
             ( "read_replica_d_b_instance_identifiers"
             , ReadReplicaDBInstanceIdentifierList.to_json
                 v.read_replica_d_b_instance_identifiers )
         ; Util.option_map v.read_replica_source_d_b_instance_identifier (fun f ->
               "read_replica_source_d_b_instance_identifier", String.to_json f)
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               "auto_minor_version_upgrade", Boolean.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Util.option_map v.latest_restorable_time (fun f ->
               "latest_restorable_time", DateTime.to_json f)
         ; Util.option_map v.pending_modified_values (fun f ->
               "pending_modified_values", PendingModifiedValues.to_json f)
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               "preferred_maintenance_window", String.to_json f)
         ; Util.option_map v.d_b_subnet_group (fun f ->
               "d_b_subnet_group", DBSubnetGroup.to_json f)
         ; Util.option_map v.availability_zone (fun f ->
               "availability_zone", String.to_json f)
         ; Some
             ( "d_b_parameter_groups"
             , DBParameterGroupStatusList.to_json v.d_b_parameter_groups )
         ; Some
             ( "vpc_security_groups"
             , VpcSecurityGroupMembershipList.to_json v.vpc_security_groups )
         ; Some
             ( "d_b_security_groups"
             , DBSecurityGroupMembershipList.to_json v.d_b_security_groups )
         ; Util.option_map v.backup_retention_period (fun f ->
               "backup_retention_period", Integer.to_json f)
         ; Util.option_map v.preferred_backup_window (fun f ->
               "preferred_backup_window", String.to_json f)
         ; Util.option_map v.instance_create_time (fun f ->
               "instance_create_time", DateTime.to_json f)
         ; Util.option_map v.allocated_storage (fun f ->
               "allocated_storage", Integer.to_json f)
         ; Util.option_map v.endpoint (fun f -> "endpoint", Endpoint.to_json f)
         ; Util.option_map v.d_b_name (fun f -> "d_b_name", String.to_json f)
         ; Util.option_map v.master_username (fun f ->
               "master_username", String.to_json f)
         ; Util.option_map v.d_b_instance_status (fun f ->
               "d_b_instance_status", String.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ; Util.option_map v.d_b_instance_class (fun f ->
               "d_b_instance_class", String.to_json f)
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               "d_b_instance_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_instance_identifier =
        Util.option_map (Json.lookup j "d_b_instance_identifier") String.of_json
    ; d_b_instance_class =
        Util.option_map (Json.lookup j "d_b_instance_class") String.of_json
    ; engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; d_b_instance_status =
        Util.option_map (Json.lookup j "d_b_instance_status") String.of_json
    ; master_username = Util.option_map (Json.lookup j "master_username") String.of_json
    ; d_b_name = Util.option_map (Json.lookup j "d_b_name") String.of_json
    ; endpoint = Util.option_map (Json.lookup j "endpoint") Endpoint.of_json
    ; allocated_storage =
        Util.option_map (Json.lookup j "allocated_storage") Integer.of_json
    ; instance_create_time =
        Util.option_map (Json.lookup j "instance_create_time") DateTime.of_json
    ; preferred_backup_window =
        Util.option_map (Json.lookup j "preferred_backup_window") String.of_json
    ; backup_retention_period =
        Util.option_map (Json.lookup j "backup_retention_period") Integer.of_json
    ; d_b_security_groups =
        DBSecurityGroupMembershipList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_security_groups"))
    ; vpc_security_groups =
        VpcSecurityGroupMembershipList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_groups"))
    ; d_b_parameter_groups =
        DBParameterGroupStatusList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_parameter_groups"))
    ; availability_zone =
        Util.option_map (Json.lookup j "availability_zone") String.of_json
    ; d_b_subnet_group =
        Util.option_map (Json.lookup j "d_b_subnet_group") DBSubnetGroup.of_json
    ; preferred_maintenance_window =
        Util.option_map (Json.lookup j "preferred_maintenance_window") String.of_json
    ; pending_modified_values =
        Util.option_map
          (Json.lookup j "pending_modified_values")
          PendingModifiedValues.of_json
    ; latest_restorable_time =
        Util.option_map (Json.lookup j "latest_restorable_time") DateTime.of_json
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; auto_minor_version_upgrade =
        Util.option_map (Json.lookup j "auto_minor_version_upgrade") Boolean.of_json
    ; read_replica_source_d_b_instance_identifier =
        Util.option_map
          (Json.lookup j "read_replica_source_d_b_instance_identifier")
          String.of_json
    ; read_replica_d_b_instance_identifiers =
        ReadReplicaDBInstanceIdentifierList.of_json
          (Util.of_option_exn (Json.lookup j "read_replica_d_b_instance_identifiers"))
    ; read_replica_d_b_cluster_identifiers =
        ReadReplicaDBClusterIdentifierList.of_json
          (Util.of_option_exn (Json.lookup j "read_replica_d_b_cluster_identifiers"))
    ; replica_mode = Util.option_map (Json.lookup j "replica_mode") ReplicaMode.of_json
    ; license_model = Util.option_map (Json.lookup j "license_model") String.of_json
    ; iops = Util.option_map (Json.lookup j "iops") Integer.of_json
    ; option_group_memberships =
        OptionGroupMembershipList.of_json
          (Util.of_option_exn (Json.lookup j "option_group_memberships"))
    ; character_set_name =
        Util.option_map (Json.lookup j "character_set_name") String.of_json
    ; nchar_character_set_name =
        Util.option_map (Json.lookup j "nchar_character_set_name") String.of_json
    ; secondary_availability_zone =
        Util.option_map (Json.lookup j "secondary_availability_zone") String.of_json
    ; publicly_accessible =
        Util.option_map (Json.lookup j "publicly_accessible") Boolean.of_json
    ; status_infos =
        DBInstanceStatusInfoList.of_json
          (Util.of_option_exn (Json.lookup j "status_infos"))
    ; storage_type = Util.option_map (Json.lookup j "storage_type") String.of_json
    ; tde_credential_arn =
        Util.option_map (Json.lookup j "tde_credential_arn") String.of_json
    ; db_instance_port =
        Util.option_map (Json.lookup j "db_instance_port") Integer.of_json
    ; d_b_cluster_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_identifier") String.of_json
    ; storage_encrypted =
        Util.option_map (Json.lookup j "storage_encrypted") Boolean.of_json
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; dbi_resource_id = Util.option_map (Json.lookup j "dbi_resource_id") String.of_json
    ; c_a_certificate_identifier =
        Util.option_map (Json.lookup j "c_a_certificate_identifier") String.of_json
    ; domain_memberships =
        DomainMembershipList.of_json
          (Util.of_option_exn (Json.lookup j "domain_memberships"))
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; monitoring_interval =
        Util.option_map (Json.lookup j "monitoring_interval") Integer.of_json
    ; enhanced_monitoring_resource_arn =
        Util.option_map (Json.lookup j "enhanced_monitoring_resource_arn") String.of_json
    ; monitoring_role_arn =
        Util.option_map (Json.lookup j "monitoring_role_arn") String.of_json
    ; promotion_tier = Util.option_map (Json.lookup j "promotion_tier") Integer.of_json
    ; d_b_instance_arn = Util.option_map (Json.lookup j "d_b_instance_arn") String.of_json
    ; timezone = Util.option_map (Json.lookup j "timezone") String.of_json
    ; i_a_m_database_authentication_enabled =
        Util.option_map
          (Json.lookup j "i_a_m_database_authentication_enabled")
          Boolean.of_json
    ; performance_insights_enabled =
        Util.option_map (Json.lookup j "performance_insights_enabled") Boolean.of_json
    ; performance_insights_k_m_s_key_id =
        Util.option_map (Json.lookup j "performance_insights_k_m_s_key_id") String.of_json
    ; performance_insights_retention_period =
        Util.option_map
          (Json.lookup j "performance_insights_retention_period")
          Integer.of_json
    ; enabled_cloudwatch_logs_exports =
        LogTypeList.of_json
          (Util.of_option_exn (Json.lookup j "enabled_cloudwatch_logs_exports"))
    ; processor_features =
        ProcessorFeatureList.of_json
          (Util.of_option_exn (Json.lookup j "processor_features"))
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; associated_roles =
        DBInstanceRoles.of_json (Util.of_option_exn (Json.lookup j "associated_roles"))
    ; listener_endpoint =
        Util.option_map (Json.lookup j "listener_endpoint") Endpoint.of_json
    ; max_allocated_storage =
        Util.option_map (Json.lookup j "max_allocated_storage") Integer.of_json
    ; tag_list = TagList.of_json (Util.of_option_exn (Json.lookup j "tag_list"))
    }
end

module InstallationMedia = struct
  type t =
    { installation_media_id : String.t option
    ; custom_availability_zone_id : String.t option
    ; engine : String.t option
    ; engine_version : String.t option
    ; engine_installation_media_path : String.t option
    ; o_s_installation_media_path : String.t option
    ; status : String.t option
    ; failure_cause : InstallationMediaFailureCause.t option
    }

  let make
      ?installation_media_id
      ?custom_availability_zone_id
      ?engine
      ?engine_version
      ?engine_installation_media_path
      ?o_s_installation_media_path
      ?status
      ?failure_cause
      () =
    { installation_media_id
    ; custom_availability_zone_id
    ; engine
    ; engine_version
    ; engine_installation_media_path
    ; o_s_installation_media_path
    ; status
    ; failure_cause
    }

  let parse xml =
    Some
      { installation_media_id =
          Util.option_bind (Xml.member "InstallationMediaId" xml) String.parse
      ; custom_availability_zone_id =
          Util.option_bind (Xml.member "CustomAvailabilityZoneId" xml) String.parse
      ; engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; engine_installation_media_path =
          Util.option_bind (Xml.member "EngineInstallationMediaPath" xml) String.parse
      ; o_s_installation_media_path =
          Util.option_bind (Xml.member "OSInstallationMediaPath" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; failure_cause =
          Util.option_bind
            (Xml.member "FailureCause" xml)
            InstallationMediaFailureCause.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.failure_cause (fun f ->
               Query.Pair ("FailureCause", InstallationMediaFailureCause.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.o_s_installation_media_path (fun f ->
               Query.Pair ("OSInstallationMediaPath", String.to_query f))
         ; Util.option_map v.engine_installation_media_path (fun f ->
               Query.Pair ("EngineInstallationMediaPath", String.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ; Util.option_map v.custom_availability_zone_id (fun f ->
               Query.Pair ("CustomAvailabilityZoneId", String.to_query f))
         ; Util.option_map v.installation_media_id (fun f ->
               Query.Pair ("InstallationMediaId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.failure_cause (fun f ->
               "failure_cause", InstallationMediaFailureCause.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.o_s_installation_media_path (fun f ->
               "o_s_installation_media_path", String.to_json f)
         ; Util.option_map v.engine_installation_media_path (fun f ->
               "engine_installation_media_path", String.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ; Util.option_map v.custom_availability_zone_id (fun f ->
               "custom_availability_zone_id", String.to_json f)
         ; Util.option_map v.installation_media_id (fun f ->
               "installation_media_id", String.to_json f)
         ])

  let of_json j =
    { installation_media_id =
        Util.option_map (Json.lookup j "installation_media_id") String.of_json
    ; custom_availability_zone_id =
        Util.option_map (Json.lookup j "custom_availability_zone_id") String.of_json
    ; engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; engine_installation_media_path =
        Util.option_map (Json.lookup j "engine_installation_media_path") String.of_json
    ; o_s_installation_media_path =
        Util.option_map (Json.lookup j "o_s_installation_media_path") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; failure_cause =
        Util.option_map
          (Json.lookup j "failure_cause")
          InstallationMediaFailureCause.of_json
    }
end

module ExportTask = struct
  type t =
    { export_task_identifier : String.t option
    ; source_arn : String.t option
    ; export_only : StringList.t
    ; snapshot_time : DateTime.t option
    ; task_start_time : DateTime.t option
    ; task_end_time : DateTime.t option
    ; s3_bucket : String.t option
    ; s3_prefix : String.t option
    ; iam_role_arn : String.t option
    ; kms_key_id : String.t option
    ; status : String.t option
    ; percent_progress : Integer.t option
    ; total_extracted_data_in_g_b : Integer.t option
    ; failure_cause : String.t option
    ; warning_message : String.t option
    }

  let make
      ?export_task_identifier
      ?source_arn
      ?(export_only = [])
      ?snapshot_time
      ?task_start_time
      ?task_end_time
      ?s3_bucket
      ?s3_prefix
      ?iam_role_arn
      ?kms_key_id
      ?status
      ?percent_progress
      ?total_extracted_data_in_g_b
      ?failure_cause
      ?warning_message
      () =
    { export_task_identifier
    ; source_arn
    ; export_only
    ; snapshot_time
    ; task_start_time
    ; task_end_time
    ; s3_bucket
    ; s3_prefix
    ; iam_role_arn
    ; kms_key_id
    ; status
    ; percent_progress
    ; total_extracted_data_in_g_b
    ; failure_cause
    ; warning_message
    }

  let parse xml =
    Some
      { export_task_identifier =
          Util.option_bind (Xml.member "ExportTaskIdentifier" xml) String.parse
      ; source_arn = Util.option_bind (Xml.member "SourceArn" xml) String.parse
      ; export_only =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ExportOnly" xml) StringList.parse)
      ; snapshot_time = Util.option_bind (Xml.member "SnapshotTime" xml) DateTime.parse
      ; task_start_time = Util.option_bind (Xml.member "TaskStartTime" xml) DateTime.parse
      ; task_end_time = Util.option_bind (Xml.member "TaskEndTime" xml) DateTime.parse
      ; s3_bucket = Util.option_bind (Xml.member "S3Bucket" xml) String.parse
      ; s3_prefix = Util.option_bind (Xml.member "S3Prefix" xml) String.parse
      ; iam_role_arn = Util.option_bind (Xml.member "IamRoleArn" xml) String.parse
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; percent_progress =
          Util.option_bind (Xml.member "PercentProgress" xml) Integer.parse
      ; total_extracted_data_in_g_b =
          Util.option_bind (Xml.member "TotalExtractedDataInGB" xml) Integer.parse
      ; failure_cause = Util.option_bind (Xml.member "FailureCause" xml) String.parse
      ; warning_message = Util.option_bind (Xml.member "WarningMessage" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.warning_message (fun f ->
               Query.Pair ("WarningMessage", String.to_query f))
         ; Util.option_map v.failure_cause (fun f ->
               Query.Pair ("FailureCause", String.to_query f))
         ; Util.option_map v.total_extracted_data_in_g_b (fun f ->
               Query.Pair ("TotalExtractedDataInGB", Integer.to_query f))
         ; Util.option_map v.percent_progress (fun f ->
               Query.Pair ("PercentProgress", Integer.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Util.option_map v.iam_role_arn (fun f ->
               Query.Pair ("IamRoleArn", String.to_query f))
         ; Util.option_map v.s3_prefix (fun f ->
               Query.Pair ("S3Prefix", String.to_query f))
         ; Util.option_map v.s3_bucket (fun f ->
               Query.Pair ("S3Bucket", String.to_query f))
         ; Util.option_map v.task_end_time (fun f ->
               Query.Pair ("TaskEndTime", DateTime.to_query f))
         ; Util.option_map v.task_start_time (fun f ->
               Query.Pair ("TaskStartTime", DateTime.to_query f))
         ; Util.option_map v.snapshot_time (fun f ->
               Query.Pair ("SnapshotTime", DateTime.to_query f))
         ; Some (Query.Pair ("ExportOnly.member", StringList.to_query v.export_only))
         ; Util.option_map v.source_arn (fun f ->
               Query.Pair ("SourceArn", String.to_query f))
         ; Util.option_map v.export_task_identifier (fun f ->
               Query.Pair ("ExportTaskIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.warning_message (fun f ->
               "warning_message", String.to_json f)
         ; Util.option_map v.failure_cause (fun f -> "failure_cause", String.to_json f)
         ; Util.option_map v.total_extracted_data_in_g_b (fun f ->
               "total_extracted_data_in_g_b", Integer.to_json f)
         ; Util.option_map v.percent_progress (fun f ->
               "percent_progress", Integer.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Util.option_map v.iam_role_arn (fun f -> "iam_role_arn", String.to_json f)
         ; Util.option_map v.s3_prefix (fun f -> "s3_prefix", String.to_json f)
         ; Util.option_map v.s3_bucket (fun f -> "s3_bucket", String.to_json f)
         ; Util.option_map v.task_end_time (fun f -> "task_end_time", DateTime.to_json f)
         ; Util.option_map v.task_start_time (fun f ->
               "task_start_time", DateTime.to_json f)
         ; Util.option_map v.snapshot_time (fun f -> "snapshot_time", DateTime.to_json f)
         ; Some ("export_only", StringList.to_json v.export_only)
         ; Util.option_map v.source_arn (fun f -> "source_arn", String.to_json f)
         ; Util.option_map v.export_task_identifier (fun f ->
               "export_task_identifier", String.to_json f)
         ])

  let of_json j =
    { export_task_identifier =
        Util.option_map (Json.lookup j "export_task_identifier") String.of_json
    ; source_arn = Util.option_map (Json.lookup j "source_arn") String.of_json
    ; export_only = StringList.of_json (Util.of_option_exn (Json.lookup j "export_only"))
    ; snapshot_time = Util.option_map (Json.lookup j "snapshot_time") DateTime.of_json
    ; task_start_time = Util.option_map (Json.lookup j "task_start_time") DateTime.of_json
    ; task_end_time = Util.option_map (Json.lookup j "task_end_time") DateTime.of_json
    ; s3_bucket = Util.option_map (Json.lookup j "s3_bucket") String.of_json
    ; s3_prefix = Util.option_map (Json.lookup j "s3_prefix") String.of_json
    ; iam_role_arn = Util.option_map (Json.lookup j "iam_role_arn") String.of_json
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; percent_progress =
        Util.option_map (Json.lookup j "percent_progress") Integer.of_json
    ; total_extracted_data_in_g_b =
        Util.option_map (Json.lookup j "total_extracted_data_in_g_b") Integer.of_json
    ; failure_cause = Util.option_map (Json.lookup j "failure_cause") String.of_json
    ; warning_message = Util.option_map (Json.lookup j "warning_message") String.of_json
    }
end

module AccountQuota = struct
  type t =
    { account_quota_name : String.t option
    ; used : Long.t option
    ; max : Long.t option
    }

  let make ?account_quota_name ?used ?max () = { account_quota_name; used; max }

  let parse xml =
    Some
      { account_quota_name =
          Util.option_bind (Xml.member "AccountQuotaName" xml) String.parse
      ; used = Util.option_bind (Xml.member "Used" xml) Long.parse
      ; max = Util.option_bind (Xml.member "Max" xml) Long.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max (fun f -> Query.Pair ("Max", Long.to_query f))
         ; Util.option_map v.used (fun f -> Query.Pair ("Used", Long.to_query f))
         ; Util.option_map v.account_quota_name (fun f ->
               Query.Pair ("AccountQuotaName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max (fun f -> "max", Long.to_json f)
         ; Util.option_map v.used (fun f -> "used", Long.to_json f)
         ; Util.option_map v.account_quota_name (fun f ->
               "account_quota_name", String.to_json f)
         ])

  let of_json j =
    { account_quota_name =
        Util.option_map (Json.lookup j "account_quota_name") String.of_json
    ; used = Util.option_map (Json.lookup j "used") Long.of_json
    ; max = Util.option_map (Json.lookup j "max") Long.of_json
    }
end

module DBInstanceAutomatedBackup = struct
  type t =
    { d_b_instance_arn : String.t option
    ; dbi_resource_id : String.t option
    ; region : String.t option
    ; d_b_instance_identifier : String.t option
    ; restore_window : RestoreWindow.t option
    ; allocated_storage : Integer.t option
    ; status : String.t option
    ; port : Integer.t option
    ; availability_zone : String.t option
    ; vpc_id : String.t option
    ; instance_create_time : DateTime.t option
    ; master_username : String.t option
    ; engine : String.t option
    ; engine_version : String.t option
    ; license_model : String.t option
    ; iops : Integer.t option
    ; option_group_name : String.t option
    ; tde_credential_arn : String.t option
    ; encrypted : Boolean.t option
    ; storage_type : String.t option
    ; kms_key_id : String.t option
    ; timezone : String.t option
    ; i_a_m_database_authentication_enabled : Boolean.t option
    }

  let make
      ?d_b_instance_arn
      ?dbi_resource_id
      ?region
      ?d_b_instance_identifier
      ?restore_window
      ?allocated_storage
      ?status
      ?port
      ?availability_zone
      ?vpc_id
      ?instance_create_time
      ?master_username
      ?engine
      ?engine_version
      ?license_model
      ?iops
      ?option_group_name
      ?tde_credential_arn
      ?encrypted
      ?storage_type
      ?kms_key_id
      ?timezone
      ?i_a_m_database_authentication_enabled
      () =
    { d_b_instance_arn
    ; dbi_resource_id
    ; region
    ; d_b_instance_identifier
    ; restore_window
    ; allocated_storage
    ; status
    ; port
    ; availability_zone
    ; vpc_id
    ; instance_create_time
    ; master_username
    ; engine
    ; engine_version
    ; license_model
    ; iops
    ; option_group_name
    ; tde_credential_arn
    ; encrypted
    ; storage_type
    ; kms_key_id
    ; timezone
    ; i_a_m_database_authentication_enabled
    }

  let parse xml =
    Some
      { d_b_instance_arn = Util.option_bind (Xml.member "DBInstanceArn" xml) String.parse
      ; dbi_resource_id = Util.option_bind (Xml.member "DbiResourceId" xml) String.parse
      ; region = Util.option_bind (Xml.member "Region" xml) String.parse
      ; d_b_instance_identifier =
          Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse
      ; restore_window =
          Util.option_bind (Xml.member "RestoreWindow" xml) RestoreWindow.parse
      ; allocated_storage =
          Util.option_bind (Xml.member "AllocatedStorage" xml) Integer.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; availability_zone =
          Util.option_bind (Xml.member "AvailabilityZone" xml) String.parse
      ; vpc_id = Util.option_bind (Xml.member "VpcId" xml) String.parse
      ; instance_create_time =
          Util.option_bind (Xml.member "InstanceCreateTime" xml) DateTime.parse
      ; master_username = Util.option_bind (Xml.member "MasterUsername" xml) String.parse
      ; engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; license_model = Util.option_bind (Xml.member "LicenseModel" xml) String.parse
      ; iops = Util.option_bind (Xml.member "Iops" xml) Integer.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; tde_credential_arn =
          Util.option_bind (Xml.member "TdeCredentialArn" xml) String.parse
      ; encrypted = Util.option_bind (Xml.member "Encrypted" xml) Boolean.parse
      ; storage_type = Util.option_bind (Xml.member "StorageType" xml) String.parse
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; timezone = Util.option_bind (Xml.member "Timezone" xml) String.parse
      ; i_a_m_database_authentication_enabled =
          Util.option_bind
            (Xml.member "IAMDatabaseAuthenticationEnabled" xml)
            Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
               Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
         ; Util.option_map v.timezone (fun f ->
               Query.Pair ("Timezone", String.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Util.option_map v.storage_type (fun f ->
               Query.Pair ("StorageType", String.to_query f))
         ; Util.option_map v.encrypted (fun f ->
               Query.Pair ("Encrypted", Boolean.to_query f))
         ; Util.option_map v.tde_credential_arn (fun f ->
               Query.Pair ("TdeCredentialArn", String.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.iops (fun f -> Query.Pair ("Iops", Integer.to_query f))
         ; Util.option_map v.license_model (fun f ->
               Query.Pair ("LicenseModel", String.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ; Util.option_map v.master_username (fun f ->
               Query.Pair ("MasterUsername", String.to_query f))
         ; Util.option_map v.instance_create_time (fun f ->
               Query.Pair ("InstanceCreateTime", DateTime.to_query f))
         ; Util.option_map v.vpc_id (fun f -> Query.Pair ("VpcId", String.to_query f))
         ; Util.option_map v.availability_zone (fun f ->
               Query.Pair ("AvailabilityZone", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.allocated_storage (fun f ->
               Query.Pair ("AllocatedStorage", Integer.to_query f))
         ; Util.option_map v.restore_window (fun f ->
               Query.Pair ("RestoreWindow", RestoreWindow.to_query f))
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               Query.Pair ("DBInstanceIdentifier", String.to_query f))
         ; Util.option_map v.region (fun f -> Query.Pair ("Region", String.to_query f))
         ; Util.option_map v.dbi_resource_id (fun f ->
               Query.Pair ("DbiResourceId", String.to_query f))
         ; Util.option_map v.d_b_instance_arn (fun f ->
               Query.Pair ("DBInstanceArn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
               "i_a_m_database_authentication_enabled", Boolean.to_json f)
         ; Util.option_map v.timezone (fun f -> "timezone", String.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Util.option_map v.storage_type (fun f -> "storage_type", String.to_json f)
         ; Util.option_map v.encrypted (fun f -> "encrypted", Boolean.to_json f)
         ; Util.option_map v.tde_credential_arn (fun f ->
               "tde_credential_arn", String.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
         ; Util.option_map v.license_model (fun f -> "license_model", String.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ; Util.option_map v.master_username (fun f ->
               "master_username", String.to_json f)
         ; Util.option_map v.instance_create_time (fun f ->
               "instance_create_time", DateTime.to_json f)
         ; Util.option_map v.vpc_id (fun f -> "vpc_id", String.to_json f)
         ; Util.option_map v.availability_zone (fun f ->
               "availability_zone", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.allocated_storage (fun f ->
               "allocated_storage", Integer.to_json f)
         ; Util.option_map v.restore_window (fun f ->
               "restore_window", RestoreWindow.to_json f)
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               "d_b_instance_identifier", String.to_json f)
         ; Util.option_map v.region (fun f -> "region", String.to_json f)
         ; Util.option_map v.dbi_resource_id (fun f ->
               "dbi_resource_id", String.to_json f)
         ; Util.option_map v.d_b_instance_arn (fun f ->
               "d_b_instance_arn", String.to_json f)
         ])

  let of_json j =
    { d_b_instance_arn = Util.option_map (Json.lookup j "d_b_instance_arn") String.of_json
    ; dbi_resource_id = Util.option_map (Json.lookup j "dbi_resource_id") String.of_json
    ; region = Util.option_map (Json.lookup j "region") String.of_json
    ; d_b_instance_identifier =
        Util.option_map (Json.lookup j "d_b_instance_identifier") String.of_json
    ; restore_window =
        Util.option_map (Json.lookup j "restore_window") RestoreWindow.of_json
    ; allocated_storage =
        Util.option_map (Json.lookup j "allocated_storage") Integer.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; availability_zone =
        Util.option_map (Json.lookup j "availability_zone") String.of_json
    ; vpc_id = Util.option_map (Json.lookup j "vpc_id") String.of_json
    ; instance_create_time =
        Util.option_map (Json.lookup j "instance_create_time") DateTime.of_json
    ; master_username = Util.option_map (Json.lookup j "master_username") String.of_json
    ; engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; license_model = Util.option_map (Json.lookup j "license_model") String.of_json
    ; iops = Util.option_map (Json.lookup j "iops") Integer.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; tde_credential_arn =
        Util.option_map (Json.lookup j "tde_credential_arn") String.of_json
    ; encrypted = Util.option_map (Json.lookup j "encrypted") Boolean.of_json
    ; storage_type = Util.option_map (Json.lookup j "storage_type") String.of_json
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; timezone = Util.option_map (Json.lookup j "timezone") String.of_json
    ; i_a_m_database_authentication_enabled =
        Util.option_map
          (Json.lookup j "i_a_m_database_authentication_enabled")
          Boolean.of_json
    }
end

module DBParameterGroup = struct
  type t =
    { d_b_parameter_group_name : String.t option
    ; d_b_parameter_group_family : String.t option
    ; description : String.t option
    ; d_b_parameter_group_arn : String.t option
    }

  let make
      ?d_b_parameter_group_name
      ?d_b_parameter_group_family
      ?description
      ?d_b_parameter_group_arn
      () =
    { d_b_parameter_group_name
    ; d_b_parameter_group_family
    ; description
    ; d_b_parameter_group_arn
    }

  let parse xml =
    Some
      { d_b_parameter_group_name =
          Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse
      ; d_b_parameter_group_family =
          Util.option_bind (Xml.member "DBParameterGroupFamily" xml) String.parse
      ; description = Util.option_bind (Xml.member "Description" xml) String.parse
      ; d_b_parameter_group_arn =
          Util.option_bind (Xml.member "DBParameterGroupArn" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_parameter_group_arn (fun f ->
               Query.Pair ("DBParameterGroupArn", String.to_query f))
         ; Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ; Util.option_map v.d_b_parameter_group_family (fun f ->
               Query.Pair ("DBParameterGroupFamily", String.to_query f))
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               Query.Pair ("DBParameterGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_parameter_group_arn (fun f ->
               "d_b_parameter_group_arn", String.to_json f)
         ; Util.option_map v.description (fun f -> "description", String.to_json f)
         ; Util.option_map v.d_b_parameter_group_family (fun f ->
               "d_b_parameter_group_family", String.to_json f)
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               "d_b_parameter_group_name", String.to_json f)
         ])

  let of_json j =
    { d_b_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_parameter_group_name") String.of_json
    ; d_b_parameter_group_family =
        Util.option_map (Json.lookup j "d_b_parameter_group_family") String.of_json
    ; description = Util.option_map (Json.lookup j "description") String.of_json
    ; d_b_parameter_group_arn =
        Util.option_map (Json.lookup j "d_b_parameter_group_arn") String.of_json
    }
end

module Event = struct
  type t =
    { source_identifier : String.t option
    ; source_type : SourceType.t option
    ; message : String.t option
    ; event_categories : EventCategoriesList.t
    ; date : DateTime.t option
    ; source_arn : String.t option
    }

  let make
      ?source_identifier
      ?source_type
      ?message
      ?(event_categories = [])
      ?date
      ?source_arn
      () =
    { source_identifier; source_type; message; event_categories; date; source_arn }

  let parse xml =
    Some
      { source_identifier =
          Util.option_bind (Xml.member "SourceIdentifier" xml) String.parse
      ; source_type = Util.option_bind (Xml.member "SourceType" xml) SourceType.parse
      ; message = Util.option_bind (Xml.member "Message" xml) String.parse
      ; event_categories =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EventCategories" xml)
               EventCategoriesList.parse)
      ; date = Util.option_bind (Xml.member "Date" xml) DateTime.parse
      ; source_arn = Util.option_bind (Xml.member "SourceArn" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.source_arn (fun f ->
               Query.Pair ("SourceArn", String.to_query f))
         ; Util.option_map v.date (fun f -> Query.Pair ("Date", DateTime.to_query f))
         ; Some
             (Query.Pair
                ("EventCategories.member", EventCategoriesList.to_query v.event_categories))
         ; Util.option_map v.message (fun f -> Query.Pair ("Message", String.to_query f))
         ; Util.option_map v.source_type (fun f ->
               Query.Pair ("SourceType", SourceType.to_query f))
         ; Util.option_map v.source_identifier (fun f ->
               Query.Pair ("SourceIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.source_arn (fun f -> "source_arn", String.to_json f)
         ; Util.option_map v.date (fun f -> "date", DateTime.to_json f)
         ; Some ("event_categories", EventCategoriesList.to_json v.event_categories)
         ; Util.option_map v.message (fun f -> "message", String.to_json f)
         ; Util.option_map v.source_type (fun f -> "source_type", SourceType.to_json f)
         ; Util.option_map v.source_identifier (fun f ->
               "source_identifier", String.to_json f)
         ])

  let of_json j =
    { source_identifier =
        Util.option_map (Json.lookup j "source_identifier") String.of_json
    ; source_type = Util.option_map (Json.lookup j "source_type") SourceType.of_json
    ; message = Util.option_map (Json.lookup j "message") String.of_json
    ; event_categories =
        EventCategoriesList.of_json
          (Util.of_option_exn (Json.lookup j "event_categories"))
    ; date = Util.option_map (Json.lookup j "date") DateTime.of_json
    ; source_arn = Util.option_map (Json.lookup j "source_arn") String.of_json
    }
end

module OrderableDBInstanceOption = struct
  type t =
    { engine : String.t option
    ; engine_version : String.t option
    ; d_b_instance_class : String.t option
    ; license_model : String.t option
    ; availability_zone_group : String.t option
    ; availability_zones : AvailabilityZoneList.t
    ; multi_a_z_capable : Boolean.t option
    ; read_replica_capable : Boolean.t option
    ; vpc : Boolean.t option
    ; supports_storage_encryption : Boolean.t option
    ; storage_type : String.t option
    ; supports_iops : Boolean.t option
    ; supports_enhanced_monitoring : Boolean.t option
    ; supports_i_a_m_database_authentication : Boolean.t option
    ; supports_performance_insights : Boolean.t option
    ; min_storage_size : Integer.t option
    ; max_storage_size : Integer.t option
    ; min_iops_per_db_instance : Integer.t option
    ; max_iops_per_db_instance : Integer.t option
    ; min_iops_per_gib : Double.t option
    ; max_iops_per_gib : Double.t option
    ; available_processor_features : AvailableProcessorFeatureList.t
    ; supported_engine_modes : EngineModeList.t
    ; supports_storage_autoscaling : Boolean.t option
    ; supports_kerberos_authentication : Boolean.t option
    ; outpost_capable : Boolean.t option
    ; supports_global_databases : Boolean.t option
    }

  let make
      ?engine
      ?engine_version
      ?d_b_instance_class
      ?license_model
      ?availability_zone_group
      ?(availability_zones = [])
      ?multi_a_z_capable
      ?read_replica_capable
      ?vpc
      ?supports_storage_encryption
      ?storage_type
      ?supports_iops
      ?supports_enhanced_monitoring
      ?supports_i_a_m_database_authentication
      ?supports_performance_insights
      ?min_storage_size
      ?max_storage_size
      ?min_iops_per_db_instance
      ?max_iops_per_db_instance
      ?min_iops_per_gib
      ?max_iops_per_gib
      ?(available_processor_features = [])
      ?(supported_engine_modes = [])
      ?supports_storage_autoscaling
      ?supports_kerberos_authentication
      ?outpost_capable
      ?supports_global_databases
      () =
    { engine
    ; engine_version
    ; d_b_instance_class
    ; license_model
    ; availability_zone_group
    ; availability_zones
    ; multi_a_z_capable
    ; read_replica_capable
    ; vpc
    ; supports_storage_encryption
    ; storage_type
    ; supports_iops
    ; supports_enhanced_monitoring
    ; supports_i_a_m_database_authentication
    ; supports_performance_insights
    ; min_storage_size
    ; max_storage_size
    ; min_iops_per_db_instance
    ; max_iops_per_db_instance
    ; min_iops_per_gib
    ; max_iops_per_gib
    ; available_processor_features
    ; supported_engine_modes
    ; supports_storage_autoscaling
    ; supports_kerberos_authentication
    ; outpost_capable
    ; supports_global_databases
    }

  let parse xml =
    Some
      { engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; d_b_instance_class =
          Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse
      ; license_model = Util.option_bind (Xml.member "LicenseModel" xml) String.parse
      ; availability_zone_group =
          Util.option_bind (Xml.member "AvailabilityZoneGroup" xml) String.parse
      ; availability_zones =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AvailabilityZones" xml)
               AvailabilityZoneList.parse)
      ; multi_a_z_capable =
          Util.option_bind (Xml.member "MultiAZCapable" xml) Boolean.parse
      ; read_replica_capable =
          Util.option_bind (Xml.member "ReadReplicaCapable" xml) Boolean.parse
      ; vpc = Util.option_bind (Xml.member "Vpc" xml) Boolean.parse
      ; supports_storage_encryption =
          Util.option_bind (Xml.member "SupportsStorageEncryption" xml) Boolean.parse
      ; storage_type = Util.option_bind (Xml.member "StorageType" xml) String.parse
      ; supports_iops = Util.option_bind (Xml.member "SupportsIops" xml) Boolean.parse
      ; supports_enhanced_monitoring =
          Util.option_bind (Xml.member "SupportsEnhancedMonitoring" xml) Boolean.parse
      ; supports_i_a_m_database_authentication =
          Util.option_bind
            (Xml.member "SupportsIAMDatabaseAuthentication" xml)
            Boolean.parse
      ; supports_performance_insights =
          Util.option_bind (Xml.member "SupportsPerformanceInsights" xml) Boolean.parse
      ; min_storage_size =
          Util.option_bind (Xml.member "MinStorageSize" xml) Integer.parse
      ; max_storage_size =
          Util.option_bind (Xml.member "MaxStorageSize" xml) Integer.parse
      ; min_iops_per_db_instance =
          Util.option_bind (Xml.member "MinIopsPerDbInstance" xml) Integer.parse
      ; max_iops_per_db_instance =
          Util.option_bind (Xml.member "MaxIopsPerDbInstance" xml) Integer.parse
      ; min_iops_per_gib = Util.option_bind (Xml.member "MinIopsPerGib" xml) Double.parse
      ; max_iops_per_gib = Util.option_bind (Xml.member "MaxIopsPerGib" xml) Double.parse
      ; available_processor_features =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AvailableProcessorFeatures" xml)
               AvailableProcessorFeatureList.parse)
      ; supported_engine_modes =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "SupportedEngineModes" xml)
               EngineModeList.parse)
      ; supports_storage_autoscaling =
          Util.option_bind (Xml.member "SupportsStorageAutoscaling" xml) Boolean.parse
      ; supports_kerberos_authentication =
          Util.option_bind (Xml.member "SupportsKerberosAuthentication" xml) Boolean.parse
      ; outpost_capable = Util.option_bind (Xml.member "OutpostCapable" xml) Boolean.parse
      ; supports_global_databases =
          Util.option_bind (Xml.member "SupportsGlobalDatabases" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.supports_global_databases (fun f ->
               Query.Pair ("SupportsGlobalDatabases", Boolean.to_query f))
         ; Util.option_map v.outpost_capable (fun f ->
               Query.Pair ("OutpostCapable", Boolean.to_query f))
         ; Util.option_map v.supports_kerberos_authentication (fun f ->
               Query.Pair ("SupportsKerberosAuthentication", Boolean.to_query f))
         ; Util.option_map v.supports_storage_autoscaling (fun f ->
               Query.Pair ("SupportsStorageAutoscaling", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "SupportedEngineModes.member"
                , EngineModeList.to_query v.supported_engine_modes ))
         ; Some
             (Query.Pair
                ( "AvailableProcessorFeatures.member"
                , AvailableProcessorFeatureList.to_query v.available_processor_features ))
         ; Util.option_map v.max_iops_per_gib (fun f ->
               Query.Pair ("MaxIopsPerGib", Double.to_query f))
         ; Util.option_map v.min_iops_per_gib (fun f ->
               Query.Pair ("MinIopsPerGib", Double.to_query f))
         ; Util.option_map v.max_iops_per_db_instance (fun f ->
               Query.Pair ("MaxIopsPerDbInstance", Integer.to_query f))
         ; Util.option_map v.min_iops_per_db_instance (fun f ->
               Query.Pair ("MinIopsPerDbInstance", Integer.to_query f))
         ; Util.option_map v.max_storage_size (fun f ->
               Query.Pair ("MaxStorageSize", Integer.to_query f))
         ; Util.option_map v.min_storage_size (fun f ->
               Query.Pair ("MinStorageSize", Integer.to_query f))
         ; Util.option_map v.supports_performance_insights (fun f ->
               Query.Pair ("SupportsPerformanceInsights", Boolean.to_query f))
         ; Util.option_map v.supports_i_a_m_database_authentication (fun f ->
               Query.Pair ("SupportsIAMDatabaseAuthentication", Boolean.to_query f))
         ; Util.option_map v.supports_enhanced_monitoring (fun f ->
               Query.Pair ("SupportsEnhancedMonitoring", Boolean.to_query f))
         ; Util.option_map v.supports_iops (fun f ->
               Query.Pair ("SupportsIops", Boolean.to_query f))
         ; Util.option_map v.storage_type (fun f ->
               Query.Pair ("StorageType", String.to_query f))
         ; Util.option_map v.supports_storage_encryption (fun f ->
               Query.Pair ("SupportsStorageEncryption", Boolean.to_query f))
         ; Util.option_map v.vpc (fun f -> Query.Pair ("Vpc", Boolean.to_query f))
         ; Util.option_map v.read_replica_capable (fun f ->
               Query.Pair ("ReadReplicaCapable", Boolean.to_query f))
         ; Util.option_map v.multi_a_z_capable (fun f ->
               Query.Pair ("MultiAZCapable", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZoneList.to_query v.availability_zones ))
         ; Util.option_map v.availability_zone_group (fun f ->
               Query.Pair ("AvailabilityZoneGroup", String.to_query f))
         ; Util.option_map v.license_model (fun f ->
               Query.Pair ("LicenseModel", String.to_query f))
         ; Util.option_map v.d_b_instance_class (fun f ->
               Query.Pair ("DBInstanceClass", String.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.supports_global_databases (fun f ->
               "supports_global_databases", Boolean.to_json f)
         ; Util.option_map v.outpost_capable (fun f ->
               "outpost_capable", Boolean.to_json f)
         ; Util.option_map v.supports_kerberos_authentication (fun f ->
               "supports_kerberos_authentication", Boolean.to_json f)
         ; Util.option_map v.supports_storage_autoscaling (fun f ->
               "supports_storage_autoscaling", Boolean.to_json f)
         ; Some ("supported_engine_modes", EngineModeList.to_json v.supported_engine_modes)
         ; Some
             ( "available_processor_features"
             , AvailableProcessorFeatureList.to_json v.available_processor_features )
         ; Util.option_map v.max_iops_per_gib (fun f ->
               "max_iops_per_gib", Double.to_json f)
         ; Util.option_map v.min_iops_per_gib (fun f ->
               "min_iops_per_gib", Double.to_json f)
         ; Util.option_map v.max_iops_per_db_instance (fun f ->
               "max_iops_per_db_instance", Integer.to_json f)
         ; Util.option_map v.min_iops_per_db_instance (fun f ->
               "min_iops_per_db_instance", Integer.to_json f)
         ; Util.option_map v.max_storage_size (fun f ->
               "max_storage_size", Integer.to_json f)
         ; Util.option_map v.min_storage_size (fun f ->
               "min_storage_size", Integer.to_json f)
         ; Util.option_map v.supports_performance_insights (fun f ->
               "supports_performance_insights", Boolean.to_json f)
         ; Util.option_map v.supports_i_a_m_database_authentication (fun f ->
               "supports_i_a_m_database_authentication", Boolean.to_json f)
         ; Util.option_map v.supports_enhanced_monitoring (fun f ->
               "supports_enhanced_monitoring", Boolean.to_json f)
         ; Util.option_map v.supports_iops (fun f -> "supports_iops", Boolean.to_json f)
         ; Util.option_map v.storage_type (fun f -> "storage_type", String.to_json f)
         ; Util.option_map v.supports_storage_encryption (fun f ->
               "supports_storage_encryption", Boolean.to_json f)
         ; Util.option_map v.vpc (fun f -> "vpc", Boolean.to_json f)
         ; Util.option_map v.read_replica_capable (fun f ->
               "read_replica_capable", Boolean.to_json f)
         ; Util.option_map v.multi_a_z_capable (fun f ->
               "multi_a_z_capable", Boolean.to_json f)
         ; Some ("availability_zones", AvailabilityZoneList.to_json v.availability_zones)
         ; Util.option_map v.availability_zone_group (fun f ->
               "availability_zone_group", String.to_json f)
         ; Util.option_map v.license_model (fun f -> "license_model", String.to_json f)
         ; Util.option_map v.d_b_instance_class (fun f ->
               "d_b_instance_class", String.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ])

  let of_json j =
    { engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; d_b_instance_class =
        Util.option_map (Json.lookup j "d_b_instance_class") String.of_json
    ; license_model = Util.option_map (Json.lookup j "license_model") String.of_json
    ; availability_zone_group =
        Util.option_map (Json.lookup j "availability_zone_group") String.of_json
    ; availability_zones =
        AvailabilityZoneList.of_json
          (Util.of_option_exn (Json.lookup j "availability_zones"))
    ; multi_a_z_capable =
        Util.option_map (Json.lookup j "multi_a_z_capable") Boolean.of_json
    ; read_replica_capable =
        Util.option_map (Json.lookup j "read_replica_capable") Boolean.of_json
    ; vpc = Util.option_map (Json.lookup j "vpc") Boolean.of_json
    ; supports_storage_encryption =
        Util.option_map (Json.lookup j "supports_storage_encryption") Boolean.of_json
    ; storage_type = Util.option_map (Json.lookup j "storage_type") String.of_json
    ; supports_iops = Util.option_map (Json.lookup j "supports_iops") Boolean.of_json
    ; supports_enhanced_monitoring =
        Util.option_map (Json.lookup j "supports_enhanced_monitoring") Boolean.of_json
    ; supports_i_a_m_database_authentication =
        Util.option_map
          (Json.lookup j "supports_i_a_m_database_authentication")
          Boolean.of_json
    ; supports_performance_insights =
        Util.option_map (Json.lookup j "supports_performance_insights") Boolean.of_json
    ; min_storage_size =
        Util.option_map (Json.lookup j "min_storage_size") Integer.of_json
    ; max_storage_size =
        Util.option_map (Json.lookup j "max_storage_size") Integer.of_json
    ; min_iops_per_db_instance =
        Util.option_map (Json.lookup j "min_iops_per_db_instance") Integer.of_json
    ; max_iops_per_db_instance =
        Util.option_map (Json.lookup j "max_iops_per_db_instance") Integer.of_json
    ; min_iops_per_gib = Util.option_map (Json.lookup j "min_iops_per_gib") Double.of_json
    ; max_iops_per_gib = Util.option_map (Json.lookup j "max_iops_per_gib") Double.of_json
    ; available_processor_features =
        AvailableProcessorFeatureList.of_json
          (Util.of_option_exn (Json.lookup j "available_processor_features"))
    ; supported_engine_modes =
        EngineModeList.of_json
          (Util.of_option_exn (Json.lookup j "supported_engine_modes"))
    ; supports_storage_autoscaling =
        Util.option_map (Json.lookup j "supports_storage_autoscaling") Boolean.of_json
    ; supports_kerberos_authentication =
        Util.option_map (Json.lookup j "supports_kerberos_authentication") Boolean.of_json
    ; outpost_capable = Util.option_map (Json.lookup j "outpost_capable") Boolean.of_json
    ; supports_global_databases =
        Util.option_map (Json.lookup j "supports_global_databases") Boolean.of_json
    }
end

module ReservedDBInstancesOffering = struct
  type t =
    { reserved_d_b_instances_offering_id : String.t option
    ; d_b_instance_class : String.t option
    ; duration : Integer.t option
    ; fixed_price : Double.t option
    ; usage_price : Double.t option
    ; currency_code : String.t option
    ; product_description : String.t option
    ; offering_type : String.t option
    ; multi_a_z : Boolean.t option
    ; recurring_charges : RecurringChargeList.t
    }

  let make
      ?reserved_d_b_instances_offering_id
      ?d_b_instance_class
      ?duration
      ?fixed_price
      ?usage_price
      ?currency_code
      ?product_description
      ?offering_type
      ?multi_a_z
      ?(recurring_charges = [])
      () =
    { reserved_d_b_instances_offering_id
    ; d_b_instance_class
    ; duration
    ; fixed_price
    ; usage_price
    ; currency_code
    ; product_description
    ; offering_type
    ; multi_a_z
    ; recurring_charges
    }

  let parse xml =
    Some
      { reserved_d_b_instances_offering_id =
          Util.option_bind (Xml.member "ReservedDBInstancesOfferingId" xml) String.parse
      ; d_b_instance_class =
          Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse
      ; duration = Util.option_bind (Xml.member "Duration" xml) Integer.parse
      ; fixed_price = Util.option_bind (Xml.member "FixedPrice" xml) Double.parse
      ; usage_price = Util.option_bind (Xml.member "UsagePrice" xml) Double.parse
      ; currency_code = Util.option_bind (Xml.member "CurrencyCode" xml) String.parse
      ; product_description =
          Util.option_bind (Xml.member "ProductDescription" xml) String.parse
      ; offering_type = Util.option_bind (Xml.member "OfferingType" xml) String.parse
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; recurring_charges =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "RecurringCharges" xml)
               RecurringChargeList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "RecurringCharges.member"
                , RecurringChargeList.to_query v.recurring_charges ))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Util.option_map v.offering_type (fun f ->
               Query.Pair ("OfferingType", String.to_query f))
         ; Util.option_map v.product_description (fun f ->
               Query.Pair ("ProductDescription", String.to_query f))
         ; Util.option_map v.currency_code (fun f ->
               Query.Pair ("CurrencyCode", String.to_query f))
         ; Util.option_map v.usage_price (fun f ->
               Query.Pair ("UsagePrice", Double.to_query f))
         ; Util.option_map v.fixed_price (fun f ->
               Query.Pair ("FixedPrice", Double.to_query f))
         ; Util.option_map v.duration (fun f ->
               Query.Pair ("Duration", Integer.to_query f))
         ; Util.option_map v.d_b_instance_class (fun f ->
               Query.Pair ("DBInstanceClass", String.to_query f))
         ; Util.option_map v.reserved_d_b_instances_offering_id (fun f ->
               Query.Pair ("ReservedDBInstancesOfferingId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("recurring_charges", RecurringChargeList.to_json v.recurring_charges)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Util.option_map v.offering_type (fun f -> "offering_type", String.to_json f)
         ; Util.option_map v.product_description (fun f ->
               "product_description", String.to_json f)
         ; Util.option_map v.currency_code (fun f -> "currency_code", String.to_json f)
         ; Util.option_map v.usage_price (fun f -> "usage_price", Double.to_json f)
         ; Util.option_map v.fixed_price (fun f -> "fixed_price", Double.to_json f)
         ; Util.option_map v.duration (fun f -> "duration", Integer.to_json f)
         ; Util.option_map v.d_b_instance_class (fun f ->
               "d_b_instance_class", String.to_json f)
         ; Util.option_map v.reserved_d_b_instances_offering_id (fun f ->
               "reserved_d_b_instances_offering_id", String.to_json f)
         ])

  let of_json j =
    { reserved_d_b_instances_offering_id =
        Util.option_map
          (Json.lookup j "reserved_d_b_instances_offering_id")
          String.of_json
    ; d_b_instance_class =
        Util.option_map (Json.lookup j "d_b_instance_class") String.of_json
    ; duration = Util.option_map (Json.lookup j "duration") Integer.of_json
    ; fixed_price = Util.option_map (Json.lookup j "fixed_price") Double.of_json
    ; usage_price = Util.option_map (Json.lookup j "usage_price") Double.of_json
    ; currency_code = Util.option_map (Json.lookup j "currency_code") String.of_json
    ; product_description =
        Util.option_map (Json.lookup j "product_description") String.of_json
    ; offering_type = Util.option_map (Json.lookup j "offering_type") String.of_json
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; recurring_charges =
        RecurringChargeList.of_json
          (Util.of_option_exn (Json.lookup j "recurring_charges"))
    }
end

module DBClusterBacktrack = struct
  type t =
    { d_b_cluster_identifier : String.t option
    ; backtrack_identifier : String.t option
    ; backtrack_to : DateTime.t option
    ; backtracked_from : DateTime.t option
    ; backtrack_request_creation_time : DateTime.t option
    ; status : String.t option
    }

  let make
      ?d_b_cluster_identifier
      ?backtrack_identifier
      ?backtrack_to
      ?backtracked_from
      ?backtrack_request_creation_time
      ?status
      () =
    { d_b_cluster_identifier
    ; backtrack_identifier
    ; backtrack_to
    ; backtracked_from
    ; backtrack_request_creation_time
    ; status
    }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse
      ; backtrack_identifier =
          Util.option_bind (Xml.member "BacktrackIdentifier" xml) String.parse
      ; backtrack_to = Util.option_bind (Xml.member "BacktrackTo" xml) DateTime.parse
      ; backtracked_from =
          Util.option_bind (Xml.member "BacktrackedFrom" xml) DateTime.parse
      ; backtrack_request_creation_time =
          Util.option_bind (Xml.member "BacktrackRequestCreationTime" xml) DateTime.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.backtrack_request_creation_time (fun f ->
               Query.Pair ("BacktrackRequestCreationTime", DateTime.to_query f))
         ; Util.option_map v.backtracked_from (fun f ->
               Query.Pair ("BacktrackedFrom", DateTime.to_query f))
         ; Util.option_map v.backtrack_to (fun f ->
               Query.Pair ("BacktrackTo", DateTime.to_query f))
         ; Util.option_map v.backtrack_identifier (fun f ->
               Query.Pair ("BacktrackIdentifier", String.to_query f))
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               Query.Pair ("DBClusterIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.backtrack_request_creation_time (fun f ->
               "backtrack_request_creation_time", DateTime.to_json f)
         ; Util.option_map v.backtracked_from (fun f ->
               "backtracked_from", DateTime.to_json f)
         ; Util.option_map v.backtrack_to (fun f -> "backtrack_to", DateTime.to_json f)
         ; Util.option_map v.backtrack_identifier (fun f ->
               "backtrack_identifier", String.to_json f)
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               "d_b_cluster_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_identifier") String.of_json
    ; backtrack_identifier =
        Util.option_map (Json.lookup j "backtrack_identifier") String.of_json
    ; backtrack_to = Util.option_map (Json.lookup j "backtrack_to") DateTime.of_json
    ; backtracked_from =
        Util.option_map (Json.lookup j "backtracked_from") DateTime.of_json
    ; backtrack_request_creation_time =
        Util.option_map (Json.lookup j "backtrack_request_creation_time") DateTime.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    }
end

module DBCluster = struct
  type t =
    { allocated_storage : Integer.t option
    ; availability_zones : AvailabilityZones.t
    ; backup_retention_period : Integer.t option
    ; character_set_name : String.t option
    ; database_name : String.t option
    ; d_b_cluster_identifier : String.t option
    ; d_b_cluster_parameter_group : String.t option
    ; d_b_subnet_group : String.t option
    ; status : String.t option
    ; percent_progress : String.t option
    ; earliest_restorable_time : DateTime.t option
    ; endpoint : String.t option
    ; reader_endpoint : String.t option
    ; custom_endpoints : StringList.t
    ; multi_a_z : Boolean.t option
    ; engine : String.t option
    ; engine_version : String.t option
    ; latest_restorable_time : DateTime.t option
    ; port : Integer.t option
    ; master_username : String.t option
    ; d_b_cluster_option_group_memberships : DBClusterOptionGroupMemberships.t
    ; preferred_backup_window : String.t option
    ; preferred_maintenance_window : String.t option
    ; replication_source_identifier : String.t option
    ; read_replica_identifiers : ReadReplicaIdentifierList.t
    ; d_b_cluster_members : DBClusterMemberList.t
    ; vpc_security_groups : VpcSecurityGroupMembershipList.t
    ; hosted_zone_id : String.t option
    ; storage_encrypted : Boolean.t option
    ; kms_key_id : String.t option
    ; db_cluster_resource_id : String.t option
    ; d_b_cluster_arn : String.t option
    ; associated_roles : DBClusterRoles.t
    ; i_a_m_database_authentication_enabled : Boolean.t option
    ; clone_group_id : String.t option
    ; cluster_create_time : DateTime.t option
    ; earliest_backtrack_time : DateTime.t option
    ; backtrack_window : Long.t option
    ; backtrack_consumed_change_records : Long.t option
    ; enabled_cloudwatch_logs_exports : LogTypeList.t
    ; capacity : Integer.t option
    ; engine_mode : String.t option
    ; scaling_configuration_info : ScalingConfigurationInfo.t option
    ; deletion_protection : Boolean.t option
    ; http_endpoint_enabled : Boolean.t option
    ; activity_stream_mode : ActivityStreamMode.t option
    ; activity_stream_status : ActivityStreamStatus.t option
    ; activity_stream_kms_key_id : String.t option
    ; activity_stream_kinesis_stream_name : String.t option
    ; copy_tags_to_snapshot : Boolean.t option
    ; cross_account_clone : Boolean.t option
    ; domain_memberships : DomainMembershipList.t
    ; tag_list : TagList.t
    ; global_write_forwarding_status : WriteForwardingStatus.t option
    ; global_write_forwarding_requested : Boolean.t option
    }

  let make
      ?allocated_storage
      ?(availability_zones = [])
      ?backup_retention_period
      ?character_set_name
      ?database_name
      ?d_b_cluster_identifier
      ?d_b_cluster_parameter_group
      ?d_b_subnet_group
      ?status
      ?percent_progress
      ?earliest_restorable_time
      ?endpoint
      ?reader_endpoint
      ?(custom_endpoints = [])
      ?multi_a_z
      ?engine
      ?engine_version
      ?latest_restorable_time
      ?port
      ?master_username
      ?(d_b_cluster_option_group_memberships = [])
      ?preferred_backup_window
      ?preferred_maintenance_window
      ?replication_source_identifier
      ?(read_replica_identifiers = [])
      ?(d_b_cluster_members = [])
      ?(vpc_security_groups = [])
      ?hosted_zone_id
      ?storage_encrypted
      ?kms_key_id
      ?db_cluster_resource_id
      ?d_b_cluster_arn
      ?(associated_roles = [])
      ?i_a_m_database_authentication_enabled
      ?clone_group_id
      ?cluster_create_time
      ?earliest_backtrack_time
      ?backtrack_window
      ?backtrack_consumed_change_records
      ?(enabled_cloudwatch_logs_exports = [])
      ?capacity
      ?engine_mode
      ?scaling_configuration_info
      ?deletion_protection
      ?http_endpoint_enabled
      ?activity_stream_mode
      ?activity_stream_status
      ?activity_stream_kms_key_id
      ?activity_stream_kinesis_stream_name
      ?copy_tags_to_snapshot
      ?cross_account_clone
      ?(domain_memberships = [])
      ?(tag_list = [])
      ?global_write_forwarding_status
      ?global_write_forwarding_requested
      () =
    { allocated_storage
    ; availability_zones
    ; backup_retention_period
    ; character_set_name
    ; database_name
    ; d_b_cluster_identifier
    ; d_b_cluster_parameter_group
    ; d_b_subnet_group
    ; status
    ; percent_progress
    ; earliest_restorable_time
    ; endpoint
    ; reader_endpoint
    ; custom_endpoints
    ; multi_a_z
    ; engine
    ; engine_version
    ; latest_restorable_time
    ; port
    ; master_username
    ; d_b_cluster_option_group_memberships
    ; preferred_backup_window
    ; preferred_maintenance_window
    ; replication_source_identifier
    ; read_replica_identifiers
    ; d_b_cluster_members
    ; vpc_security_groups
    ; hosted_zone_id
    ; storage_encrypted
    ; kms_key_id
    ; db_cluster_resource_id
    ; d_b_cluster_arn
    ; associated_roles
    ; i_a_m_database_authentication_enabled
    ; clone_group_id
    ; cluster_create_time
    ; earliest_backtrack_time
    ; backtrack_window
    ; backtrack_consumed_change_records
    ; enabled_cloudwatch_logs_exports
    ; capacity
    ; engine_mode
    ; scaling_configuration_info
    ; deletion_protection
    ; http_endpoint_enabled
    ; activity_stream_mode
    ; activity_stream_status
    ; activity_stream_kms_key_id
    ; activity_stream_kinesis_stream_name
    ; copy_tags_to_snapshot
    ; cross_account_clone
    ; domain_memberships
    ; tag_list
    ; global_write_forwarding_status
    ; global_write_forwarding_requested
    }

  let parse xml =
    Some
      { allocated_storage =
          Util.option_bind (Xml.member "AllocatedStorage" xml) Integer.parse
      ; availability_zones =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      ; backup_retention_period =
          Util.option_bind (Xml.member "BackupRetentionPeriod" xml) Integer.parse
      ; character_set_name =
          Util.option_bind (Xml.member "CharacterSetName" xml) String.parse
      ; database_name = Util.option_bind (Xml.member "DatabaseName" xml) String.parse
      ; d_b_cluster_identifier =
          Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse
      ; d_b_cluster_parameter_group =
          Util.option_bind (Xml.member "DBClusterParameterGroup" xml) String.parse
      ; d_b_subnet_group = Util.option_bind (Xml.member "DBSubnetGroup" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) String.parse
      ; percent_progress =
          Util.option_bind (Xml.member "PercentProgress" xml) String.parse
      ; earliest_restorable_time =
          Util.option_bind (Xml.member "EarliestRestorableTime" xml) DateTime.parse
      ; endpoint = Util.option_bind (Xml.member "Endpoint" xml) String.parse
      ; reader_endpoint = Util.option_bind (Xml.member "ReaderEndpoint" xml) String.parse
      ; custom_endpoints =
          Util.of_option
            []
            (Util.option_bind (Xml.member "CustomEndpoints" xml) StringList.parse)
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; latest_restorable_time =
          Util.option_bind (Xml.member "LatestRestorableTime" xml) DateTime.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; master_username = Util.option_bind (Xml.member "MasterUsername" xml) String.parse
      ; d_b_cluster_option_group_memberships =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBClusterOptionGroupMemberships" xml)
               DBClusterOptionGroupMemberships.parse)
      ; preferred_backup_window =
          Util.option_bind (Xml.member "PreferredBackupWindow" xml) String.parse
      ; preferred_maintenance_window =
          Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml) String.parse
      ; replication_source_identifier =
          Util.option_bind (Xml.member "ReplicationSourceIdentifier" xml) String.parse
      ; read_replica_identifiers =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ReadReplicaIdentifiers" xml)
               ReadReplicaIdentifierList.parse)
      ; d_b_cluster_members =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBClusterMembers" xml)
               DBClusterMemberList.parse)
      ; vpc_security_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroups" xml)
               VpcSecurityGroupMembershipList.parse)
      ; hosted_zone_id = Util.option_bind (Xml.member "HostedZoneId" xml) String.parse
      ; storage_encrypted =
          Util.option_bind (Xml.member "StorageEncrypted" xml) Boolean.parse
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; db_cluster_resource_id =
          Util.option_bind (Xml.member "DbClusterResourceId" xml) String.parse
      ; d_b_cluster_arn = Util.option_bind (Xml.member "DBClusterArn" xml) String.parse
      ; associated_roles =
          Util.of_option
            []
            (Util.option_bind (Xml.member "AssociatedRoles" xml) DBClusterRoles.parse)
      ; i_a_m_database_authentication_enabled =
          Util.option_bind
            (Xml.member "IAMDatabaseAuthenticationEnabled" xml)
            Boolean.parse
      ; clone_group_id = Util.option_bind (Xml.member "CloneGroupId" xml) String.parse
      ; cluster_create_time =
          Util.option_bind (Xml.member "ClusterCreateTime" xml) DateTime.parse
      ; earliest_backtrack_time =
          Util.option_bind (Xml.member "EarliestBacktrackTime" xml) DateTime.parse
      ; backtrack_window = Util.option_bind (Xml.member "BacktrackWindow" xml) Long.parse
      ; backtrack_consumed_change_records =
          Util.option_bind (Xml.member "BacktrackConsumedChangeRecords" xml) Long.parse
      ; enabled_cloudwatch_logs_exports =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EnabledCloudwatchLogsExports" xml)
               LogTypeList.parse)
      ; capacity = Util.option_bind (Xml.member "Capacity" xml) Integer.parse
      ; engine_mode = Util.option_bind (Xml.member "EngineMode" xml) String.parse
      ; scaling_configuration_info =
          Util.option_bind
            (Xml.member "ScalingConfigurationInfo" xml)
            ScalingConfigurationInfo.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; http_endpoint_enabled =
          Util.option_bind (Xml.member "HttpEndpointEnabled" xml) Boolean.parse
      ; activity_stream_mode =
          Util.option_bind (Xml.member "ActivityStreamMode" xml) ActivityStreamMode.parse
      ; activity_stream_status =
          Util.option_bind
            (Xml.member "ActivityStreamStatus" xml)
            ActivityStreamStatus.parse
      ; activity_stream_kms_key_id =
          Util.option_bind (Xml.member "ActivityStreamKmsKeyId" xml) String.parse
      ; activity_stream_kinesis_stream_name =
          Util.option_bind (Xml.member "ActivityStreamKinesisStreamName" xml) String.parse
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; cross_account_clone =
          Util.option_bind (Xml.member "CrossAccountClone" xml) Boolean.parse
      ; domain_memberships =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DomainMemberships" xml)
               DomainMembershipList.parse)
      ; tag_list =
          Util.of_option [] (Util.option_bind (Xml.member "TagList" xml) TagList.parse)
      ; global_write_forwarding_status =
          Util.option_bind
            (Xml.member "GlobalWriteForwardingStatus" xml)
            WriteForwardingStatus.parse
      ; global_write_forwarding_requested =
          Util.option_bind (Xml.member "GlobalWriteForwardingRequested" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.global_write_forwarding_requested (fun f ->
               Query.Pair ("GlobalWriteForwardingRequested", Boolean.to_query f))
         ; Util.option_map v.global_write_forwarding_status (fun f ->
               Query.Pair ("GlobalWriteForwardingStatus", WriteForwardingStatus.to_query f))
         ; Some (Query.Pair ("TagList.member", TagList.to_query v.tag_list))
         ; Some
             (Query.Pair
                ( "DomainMemberships.member"
                , DomainMembershipList.to_query v.domain_memberships ))
         ; Util.option_map v.cross_account_clone (fun f ->
               Query.Pair ("CrossAccountClone", Boolean.to_query f))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Util.option_map v.activity_stream_kinesis_stream_name (fun f ->
               Query.Pair ("ActivityStreamKinesisStreamName", String.to_query f))
         ; Util.option_map v.activity_stream_kms_key_id (fun f ->
               Query.Pair ("ActivityStreamKmsKeyId", String.to_query f))
         ; Util.option_map v.activity_stream_status (fun f ->
               Query.Pair ("ActivityStreamStatus", ActivityStreamStatus.to_query f))
         ; Util.option_map v.activity_stream_mode (fun f ->
               Query.Pair ("ActivityStreamMode", ActivityStreamMode.to_query f))
         ; Util.option_map v.http_endpoint_enabled (fun f ->
               Query.Pair ("HttpEndpointEnabled", Boolean.to_query f))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.scaling_configuration_info (fun f ->
               Query.Pair ("ScalingConfigurationInfo", ScalingConfigurationInfo.to_query f))
         ; Util.option_map v.engine_mode (fun f ->
               Query.Pair ("EngineMode", String.to_query f))
         ; Util.option_map v.capacity (fun f ->
               Query.Pair ("Capacity", Integer.to_query f))
         ; Some
             (Query.Pair
                ( "EnabledCloudwatchLogsExports.member"
                , LogTypeList.to_query v.enabled_cloudwatch_logs_exports ))
         ; Util.option_map v.backtrack_consumed_change_records (fun f ->
               Query.Pair ("BacktrackConsumedChangeRecords", Long.to_query f))
         ; Util.option_map v.backtrack_window (fun f ->
               Query.Pair ("BacktrackWindow", Long.to_query f))
         ; Util.option_map v.earliest_backtrack_time (fun f ->
               Query.Pair ("EarliestBacktrackTime", DateTime.to_query f))
         ; Util.option_map v.cluster_create_time (fun f ->
               Query.Pair ("ClusterCreateTime", DateTime.to_query f))
         ; Util.option_map v.clone_group_id (fun f ->
               Query.Pair ("CloneGroupId", String.to_query f))
         ; Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
               Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
         ; Some
             (Query.Pair
                ("AssociatedRoles.member", DBClusterRoles.to_query v.associated_roles))
         ; Util.option_map v.d_b_cluster_arn (fun f ->
               Query.Pair ("DBClusterArn", String.to_query f))
         ; Util.option_map v.db_cluster_resource_id (fun f ->
               Query.Pair ("DbClusterResourceId", String.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Util.option_map v.storage_encrypted (fun f ->
               Query.Pair ("StorageEncrypted", Boolean.to_query f))
         ; Util.option_map v.hosted_zone_id (fun f ->
               Query.Pair ("HostedZoneId", String.to_query f))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroups.member"
                , VpcSecurityGroupMembershipList.to_query v.vpc_security_groups ))
         ; Some
             (Query.Pair
                ( "DBClusterMembers.member"
                , DBClusterMemberList.to_query v.d_b_cluster_members ))
         ; Some
             (Query.Pair
                ( "ReadReplicaIdentifiers.member"
                , ReadReplicaIdentifierList.to_query v.read_replica_identifiers ))
         ; Util.option_map v.replication_source_identifier (fun f ->
               Query.Pair ("ReplicationSourceIdentifier", String.to_query f))
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Util.option_map v.preferred_backup_window (fun f ->
               Query.Pair ("PreferredBackupWindow", String.to_query f))
         ; Some
             (Query.Pair
                ( "DBClusterOptionGroupMemberships.member"
                , DBClusterOptionGroupMemberships.to_query
                    v.d_b_cluster_option_group_memberships ))
         ; Util.option_map v.master_username (fun f ->
               Query.Pair ("MasterUsername", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.latest_restorable_time (fun f ->
               Query.Pair ("LatestRestorableTime", DateTime.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Some
             (Query.Pair ("CustomEndpoints.member", StringList.to_query v.custom_endpoints))
         ; Util.option_map v.reader_endpoint (fun f ->
               Query.Pair ("ReaderEndpoint", String.to_query f))
         ; Util.option_map v.endpoint (fun f ->
               Query.Pair ("Endpoint", String.to_query f))
         ; Util.option_map v.earliest_restorable_time (fun f ->
               Query.Pair ("EarliestRestorableTime", DateTime.to_query f))
         ; Util.option_map v.percent_progress (fun f ->
               Query.Pair ("PercentProgress", String.to_query f))
         ; Util.option_map v.status (fun f -> Query.Pair ("Status", String.to_query f))
         ; Util.option_map v.d_b_subnet_group (fun f ->
               Query.Pair ("DBSubnetGroup", String.to_query f))
         ; Util.option_map v.d_b_cluster_parameter_group (fun f ->
               Query.Pair ("DBClusterParameterGroup", String.to_query f))
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               Query.Pair ("DBClusterIdentifier", String.to_query f))
         ; Util.option_map v.database_name (fun f ->
               Query.Pair ("DatabaseName", String.to_query f))
         ; Util.option_map v.character_set_name (fun f ->
               Query.Pair ("CharacterSetName", String.to_query f))
         ; Util.option_map v.backup_retention_period (fun f ->
               Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
         ; Some
             (Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ; Util.option_map v.allocated_storage (fun f ->
               Query.Pair ("AllocatedStorage", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.global_write_forwarding_requested (fun f ->
               "global_write_forwarding_requested", Boolean.to_json f)
         ; Util.option_map v.global_write_forwarding_status (fun f ->
               "global_write_forwarding_status", WriteForwardingStatus.to_json f)
         ; Some ("tag_list", TagList.to_json v.tag_list)
         ; Some ("domain_memberships", DomainMembershipList.to_json v.domain_memberships)
         ; Util.option_map v.cross_account_clone (fun f ->
               "cross_account_clone", Boolean.to_json f)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Util.option_map v.activity_stream_kinesis_stream_name (fun f ->
               "activity_stream_kinesis_stream_name", String.to_json f)
         ; Util.option_map v.activity_stream_kms_key_id (fun f ->
               "activity_stream_kms_key_id", String.to_json f)
         ; Util.option_map v.activity_stream_status (fun f ->
               "activity_stream_status", ActivityStreamStatus.to_json f)
         ; Util.option_map v.activity_stream_mode (fun f ->
               "activity_stream_mode", ActivityStreamMode.to_json f)
         ; Util.option_map v.http_endpoint_enabled (fun f ->
               "http_endpoint_enabled", Boolean.to_json f)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.scaling_configuration_info (fun f ->
               "scaling_configuration_info", ScalingConfigurationInfo.to_json f)
         ; Util.option_map v.engine_mode (fun f -> "engine_mode", String.to_json f)
         ; Util.option_map v.capacity (fun f -> "capacity", Integer.to_json f)
         ; Some
             ( "enabled_cloudwatch_logs_exports"
             , LogTypeList.to_json v.enabled_cloudwatch_logs_exports )
         ; Util.option_map v.backtrack_consumed_change_records (fun f ->
               "backtrack_consumed_change_records", Long.to_json f)
         ; Util.option_map v.backtrack_window (fun f ->
               "backtrack_window", Long.to_json f)
         ; Util.option_map v.earliest_backtrack_time (fun f ->
               "earliest_backtrack_time", DateTime.to_json f)
         ; Util.option_map v.cluster_create_time (fun f ->
               "cluster_create_time", DateTime.to_json f)
         ; Util.option_map v.clone_group_id (fun f -> "clone_group_id", String.to_json f)
         ; Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
               "i_a_m_database_authentication_enabled", Boolean.to_json f)
         ; Some ("associated_roles", DBClusterRoles.to_json v.associated_roles)
         ; Util.option_map v.d_b_cluster_arn (fun f ->
               "d_b_cluster_arn", String.to_json f)
         ; Util.option_map v.db_cluster_resource_id (fun f ->
               "db_cluster_resource_id", String.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Util.option_map v.storage_encrypted (fun f ->
               "storage_encrypted", Boolean.to_json f)
         ; Util.option_map v.hosted_zone_id (fun f -> "hosted_zone_id", String.to_json f)
         ; Some
             ( "vpc_security_groups"
             , VpcSecurityGroupMembershipList.to_json v.vpc_security_groups )
         ; Some ("d_b_cluster_members", DBClusterMemberList.to_json v.d_b_cluster_members)
         ; Some
             ( "read_replica_identifiers"
             , ReadReplicaIdentifierList.to_json v.read_replica_identifiers )
         ; Util.option_map v.replication_source_identifier (fun f ->
               "replication_source_identifier", String.to_json f)
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               "preferred_maintenance_window", String.to_json f)
         ; Util.option_map v.preferred_backup_window (fun f ->
               "preferred_backup_window", String.to_json f)
         ; Some
             ( "d_b_cluster_option_group_memberships"
             , DBClusterOptionGroupMemberships.to_json
                 v.d_b_cluster_option_group_memberships )
         ; Util.option_map v.master_username (fun f ->
               "master_username", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.latest_restorable_time (fun f ->
               "latest_restorable_time", DateTime.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Some ("custom_endpoints", StringList.to_json v.custom_endpoints)
         ; Util.option_map v.reader_endpoint (fun f ->
               "reader_endpoint", String.to_json f)
         ; Util.option_map v.endpoint (fun f -> "endpoint", String.to_json f)
         ; Util.option_map v.earliest_restorable_time (fun f ->
               "earliest_restorable_time", DateTime.to_json f)
         ; Util.option_map v.percent_progress (fun f ->
               "percent_progress", String.to_json f)
         ; Util.option_map v.status (fun f -> "status", String.to_json f)
         ; Util.option_map v.d_b_subnet_group (fun f ->
               "d_b_subnet_group", String.to_json f)
         ; Util.option_map v.d_b_cluster_parameter_group (fun f ->
               "d_b_cluster_parameter_group", String.to_json f)
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               "d_b_cluster_identifier", String.to_json f)
         ; Util.option_map v.database_name (fun f -> "database_name", String.to_json f)
         ; Util.option_map v.character_set_name (fun f ->
               "character_set_name", String.to_json f)
         ; Util.option_map v.backup_retention_period (fun f ->
               "backup_retention_period", Integer.to_json f)
         ; Some ("availability_zones", AvailabilityZones.to_json v.availability_zones)
         ; Util.option_map v.allocated_storage (fun f ->
               "allocated_storage", Integer.to_json f)
         ])

  let of_json j =
    { allocated_storage =
        Util.option_map (Json.lookup j "allocated_storage") Integer.of_json
    ; availability_zones =
        AvailabilityZones.of_json
          (Util.of_option_exn (Json.lookup j "availability_zones"))
    ; backup_retention_period =
        Util.option_map (Json.lookup j "backup_retention_period") Integer.of_json
    ; character_set_name =
        Util.option_map (Json.lookup j "character_set_name") String.of_json
    ; database_name = Util.option_map (Json.lookup j "database_name") String.of_json
    ; d_b_cluster_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_identifier") String.of_json
    ; d_b_cluster_parameter_group =
        Util.option_map (Json.lookup j "d_b_cluster_parameter_group") String.of_json
    ; d_b_subnet_group = Util.option_map (Json.lookup j "d_b_subnet_group") String.of_json
    ; status = Util.option_map (Json.lookup j "status") String.of_json
    ; percent_progress = Util.option_map (Json.lookup j "percent_progress") String.of_json
    ; earliest_restorable_time =
        Util.option_map (Json.lookup j "earliest_restorable_time") DateTime.of_json
    ; endpoint = Util.option_map (Json.lookup j "endpoint") String.of_json
    ; reader_endpoint = Util.option_map (Json.lookup j "reader_endpoint") String.of_json
    ; custom_endpoints =
        StringList.of_json (Util.of_option_exn (Json.lookup j "custom_endpoints"))
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; latest_restorable_time =
        Util.option_map (Json.lookup j "latest_restorable_time") DateTime.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; master_username = Util.option_map (Json.lookup j "master_username") String.of_json
    ; d_b_cluster_option_group_memberships =
        DBClusterOptionGroupMemberships.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_option_group_memberships"))
    ; preferred_backup_window =
        Util.option_map (Json.lookup j "preferred_backup_window") String.of_json
    ; preferred_maintenance_window =
        Util.option_map (Json.lookup j "preferred_maintenance_window") String.of_json
    ; replication_source_identifier =
        Util.option_map (Json.lookup j "replication_source_identifier") String.of_json
    ; read_replica_identifiers =
        ReadReplicaIdentifierList.of_json
          (Util.of_option_exn (Json.lookup j "read_replica_identifiers"))
    ; d_b_cluster_members =
        DBClusterMemberList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_members"))
    ; vpc_security_groups =
        VpcSecurityGroupMembershipList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_groups"))
    ; hosted_zone_id = Util.option_map (Json.lookup j "hosted_zone_id") String.of_json
    ; storage_encrypted =
        Util.option_map (Json.lookup j "storage_encrypted") Boolean.of_json
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; db_cluster_resource_id =
        Util.option_map (Json.lookup j "db_cluster_resource_id") String.of_json
    ; d_b_cluster_arn = Util.option_map (Json.lookup j "d_b_cluster_arn") String.of_json
    ; associated_roles =
        DBClusterRoles.of_json (Util.of_option_exn (Json.lookup j "associated_roles"))
    ; i_a_m_database_authentication_enabled =
        Util.option_map
          (Json.lookup j "i_a_m_database_authentication_enabled")
          Boolean.of_json
    ; clone_group_id = Util.option_map (Json.lookup j "clone_group_id") String.of_json
    ; cluster_create_time =
        Util.option_map (Json.lookup j "cluster_create_time") DateTime.of_json
    ; earliest_backtrack_time =
        Util.option_map (Json.lookup j "earliest_backtrack_time") DateTime.of_json
    ; backtrack_window = Util.option_map (Json.lookup j "backtrack_window") Long.of_json
    ; backtrack_consumed_change_records =
        Util.option_map (Json.lookup j "backtrack_consumed_change_records") Long.of_json
    ; enabled_cloudwatch_logs_exports =
        LogTypeList.of_json
          (Util.of_option_exn (Json.lookup j "enabled_cloudwatch_logs_exports"))
    ; capacity = Util.option_map (Json.lookup j "capacity") Integer.of_json
    ; engine_mode = Util.option_map (Json.lookup j "engine_mode") String.of_json
    ; scaling_configuration_info =
        Util.option_map
          (Json.lookup j "scaling_configuration_info")
          ScalingConfigurationInfo.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; http_endpoint_enabled =
        Util.option_map (Json.lookup j "http_endpoint_enabled") Boolean.of_json
    ; activity_stream_mode =
        Util.option_map (Json.lookup j "activity_stream_mode") ActivityStreamMode.of_json
    ; activity_stream_status =
        Util.option_map
          (Json.lookup j "activity_stream_status")
          ActivityStreamStatus.of_json
    ; activity_stream_kms_key_id =
        Util.option_map (Json.lookup j "activity_stream_kms_key_id") String.of_json
    ; activity_stream_kinesis_stream_name =
        Util.option_map
          (Json.lookup j "activity_stream_kinesis_stream_name")
          String.of_json
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; cross_account_clone =
        Util.option_map (Json.lookup j "cross_account_clone") Boolean.of_json
    ; domain_memberships =
        DomainMembershipList.of_json
          (Util.of_option_exn (Json.lookup j "domain_memberships"))
    ; tag_list = TagList.of_json (Util.of_option_exn (Json.lookup j "tag_list"))
    ; global_write_forwarding_status =
        Util.option_map
          (Json.lookup j "global_write_forwarding_status")
          WriteForwardingStatus.of_json
    ; global_write_forwarding_requested =
        Util.option_map
          (Json.lookup j "global_write_forwarding_requested")
          Boolean.of_json
    }
end

module DBSecurityGroup = struct
  type t =
    { owner_id : String.t option
    ; d_b_security_group_name : String.t option
    ; d_b_security_group_description : String.t option
    ; vpc_id : String.t option
    ; e_c2_security_groups : EC2SecurityGroupList.t
    ; i_p_ranges : IPRangeList.t
    ; d_b_security_group_arn : String.t option
    }

  let make
      ?owner_id
      ?d_b_security_group_name
      ?d_b_security_group_description
      ?vpc_id
      ?(e_c2_security_groups = [])
      ?(i_p_ranges = [])
      ?d_b_security_group_arn
      () =
    { owner_id
    ; d_b_security_group_name
    ; d_b_security_group_description
    ; vpc_id
    ; e_c2_security_groups
    ; i_p_ranges
    ; d_b_security_group_arn
    }

  let parse xml =
    Some
      { owner_id = Util.option_bind (Xml.member "OwnerId" xml) String.parse
      ; d_b_security_group_name =
          Util.option_bind (Xml.member "DBSecurityGroupName" xml) String.parse
      ; d_b_security_group_description =
          Util.option_bind (Xml.member "DBSecurityGroupDescription" xml) String.parse
      ; vpc_id = Util.option_bind (Xml.member "VpcId" xml) String.parse
      ; e_c2_security_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EC2SecurityGroups" xml)
               EC2SecurityGroupList.parse)
      ; i_p_ranges =
          Util.of_option
            []
            (Util.option_bind (Xml.member "IPRanges" xml) IPRangeList.parse)
      ; d_b_security_group_arn =
          Util.option_bind (Xml.member "DBSecurityGroupArn" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_security_group_arn (fun f ->
               Query.Pair ("DBSecurityGroupArn", String.to_query f))
         ; Some (Query.Pair ("IPRanges.member", IPRangeList.to_query v.i_p_ranges))
         ; Some
             (Query.Pair
                ( "EC2SecurityGroups.member"
                , EC2SecurityGroupList.to_query v.e_c2_security_groups ))
         ; Util.option_map v.vpc_id (fun f -> Query.Pair ("VpcId", String.to_query f))
         ; Util.option_map v.d_b_security_group_description (fun f ->
               Query.Pair ("DBSecurityGroupDescription", String.to_query f))
         ; Util.option_map v.d_b_security_group_name (fun f ->
               Query.Pair ("DBSecurityGroupName", String.to_query f))
         ; Util.option_map v.owner_id (fun f -> Query.Pair ("OwnerId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_security_group_arn (fun f ->
               "d_b_security_group_arn", String.to_json f)
         ; Some ("i_p_ranges", IPRangeList.to_json v.i_p_ranges)
         ; Some
             ("e_c2_security_groups", EC2SecurityGroupList.to_json v.e_c2_security_groups)
         ; Util.option_map v.vpc_id (fun f -> "vpc_id", String.to_json f)
         ; Util.option_map v.d_b_security_group_description (fun f ->
               "d_b_security_group_description", String.to_json f)
         ; Util.option_map v.d_b_security_group_name (fun f ->
               "d_b_security_group_name", String.to_json f)
         ; Util.option_map v.owner_id (fun f -> "owner_id", String.to_json f)
         ])

  let of_json j =
    { owner_id = Util.option_map (Json.lookup j "owner_id") String.of_json
    ; d_b_security_group_name =
        Util.option_map (Json.lookup j "d_b_security_group_name") String.of_json
    ; d_b_security_group_description =
        Util.option_map (Json.lookup j "d_b_security_group_description") String.of_json
    ; vpc_id = Util.option_map (Json.lookup j "vpc_id") String.of_json
    ; e_c2_security_groups =
        EC2SecurityGroupList.of_json
          (Util.of_option_exn (Json.lookup j "e_c2_security_groups"))
    ; i_p_ranges = IPRangeList.of_json (Util.of_option_exn (Json.lookup j "i_p_ranges"))
    ; d_b_security_group_arn =
        Util.option_map (Json.lookup j "d_b_security_group_arn") String.of_json
    }
end

module OptionGroupOptionsList = struct
  type t = OptionGroupOption.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map OptionGroupOption.parse (Xml.members "OptionGroupOption" xml))

  let to_query v = Query.to_query_list OptionGroupOption.to_query v

  let to_json v = `List (List.map OptionGroupOption.to_json v)

  let of_json j = Json.to_list OptionGroupOption.of_json j
end

module FilterList = struct
  type t = Filter.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Filter.parse (Xml.members "Filter" xml))

  let to_query v = Query.to_query_list Filter.to_query v

  let to_json v = `List (List.map Filter.to_json v)

  let of_json j = Json.to_list Filter.of_json j
end

module SubnetIdentifierList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map String.parse (Xml.members "SubnetIdentifier" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module ValidDBInstanceModificationsMessage = struct
  type t =
    { storage : ValidStorageOptionsList.t
    ; valid_processor_features : AvailableProcessorFeatureList.t
    }

  let make ?(storage = []) ?(valid_processor_features = []) () =
    { storage; valid_processor_features }

  let parse xml =
    Some
      { storage =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Storage" xml) ValidStorageOptionsList.parse)
      ; valid_processor_features =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ValidProcessorFeatures" xml)
               AvailableProcessorFeatureList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "ValidProcessorFeatures.member"
                , AvailableProcessorFeatureList.to_query v.valid_processor_features ))
         ; Some
             (Query.Pair ("Storage.member", ValidStorageOptionsList.to_query v.storage))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "valid_processor_features"
             , AvailableProcessorFeatureList.to_json v.valid_processor_features )
         ; Some ("storage", ValidStorageOptionsList.to_json v.storage)
         ])

  let of_json j =
    { storage =
        ValidStorageOptionsList.of_json (Util.of_option_exn (Json.lookup j "storage"))
    ; valid_processor_features =
        AvailableProcessorFeatureList.of_json
          (Util.of_option_exn (Json.lookup j "valid_processor_features"))
    }
end

module CustomAvailabilityZoneList = struct
  type t = CustomAvailabilityZone.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map CustomAvailabilityZone.parse (Xml.members "CustomAvailabilityZone" xml))

  let to_query v = Query.to_query_list CustomAvailabilityZone.to_query v

  let to_json v = `List (List.map CustomAvailabilityZone.to_json v)

  let of_json j = Json.to_list CustomAvailabilityZone.of_json j
end

module EngineDefaults = struct
  type t =
    { d_b_parameter_group_family : String.t option
    ; marker : String.t option
    ; parameters : ParametersList.t
    }

  let make ?d_b_parameter_group_family ?marker ?(parameters = []) () =
    { d_b_parameter_group_family; marker; parameters }

  let parse xml =
    Some
      { d_b_parameter_group_family =
          Util.option_bind (Xml.member "DBParameterGroupFamily" xml) String.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; parameters =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Parameters" xml) ParametersList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.d_b_parameter_group_family (fun f ->
               Query.Pair ("DBParameterGroupFamily", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("parameters", ParametersList.to_json v.parameters)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.d_b_parameter_group_family (fun f ->
               "d_b_parameter_group_family", String.to_json f)
         ])

  let of_json j =
    { d_b_parameter_group_family =
        Util.option_map (Json.lookup j "d_b_parameter_group_family") String.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; parameters =
        ParametersList.of_json (Util.of_option_exn (Json.lookup j "parameters"))
    }
end

module ReservedDBInstanceList = struct
  type t = ReservedDBInstance.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map ReservedDBInstance.parse (Xml.members "ReservedDBInstance" xml))

  let to_query v = Query.to_query_list ReservedDBInstance.to_query v

  let to_json v = `List (List.map ReservedDBInstance.to_json v)

  let of_json j = Json.to_list ReservedDBInstance.of_json j
end

module DBClusterSnapshotAttributesResult = struct
  type t =
    { d_b_cluster_snapshot_identifier : String.t option
    ; d_b_cluster_snapshot_attributes : DBClusterSnapshotAttributeList.t
    }

  let make ?d_b_cluster_snapshot_identifier ?(d_b_cluster_snapshot_attributes = []) () =
    { d_b_cluster_snapshot_identifier; d_b_cluster_snapshot_attributes }

  let parse xml =
    Some
      { d_b_cluster_snapshot_identifier =
          Util.option_bind (Xml.member "DBClusterSnapshotIdentifier" xml) String.parse
      ; d_b_cluster_snapshot_attributes =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBClusterSnapshotAttributes" xml)
               DBClusterSnapshotAttributeList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBClusterSnapshotAttributes.member"
                , DBClusterSnapshotAttributeList.to_query
                    v.d_b_cluster_snapshot_attributes ))
         ; Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
               Query.Pair ("DBClusterSnapshotIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "d_b_cluster_snapshot_attributes"
             , DBClusterSnapshotAttributeList.to_json v.d_b_cluster_snapshot_attributes )
         ; Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
               "d_b_cluster_snapshot_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_cluster_snapshot_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_snapshot_identifier") String.of_json
    ; d_b_cluster_snapshot_attributes =
        DBClusterSnapshotAttributeList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_snapshot_attributes"))
    }
end

module PendingMaintenanceActions = struct
  type t = ResourcePendingMaintenanceActions.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map
         ResourcePendingMaintenanceActions.parse
         (Xml.members "ResourcePendingMaintenanceActions" xml))

  let to_query v = Query.to_query_list ResourcePendingMaintenanceActions.to_query v

  let to_json v = `List (List.map ResourcePendingMaintenanceActions.to_json v)

  let of_json j = Json.to_list ResourcePendingMaintenanceActions.of_json j
end

module TargetGroupList = struct
  type t = DBProxyTargetGroup.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DBProxyTargetGroup.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list DBProxyTargetGroup.to_query v

  let to_json v = `List (List.map DBProxyTargetGroup.to_json v)

  let of_json j = Json.to_list DBProxyTargetGroup.of_json j
end

module TargetList = struct
  type t = DBProxyTarget.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DBProxyTarget.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list DBProxyTarget.to_query v

  let to_json v = `List (List.map DBProxyTarget.to_json v)

  let of_json j = Json.to_list DBProxyTarget.of_json j
end

module DBEngineVersionList = struct
  type t = DBEngineVersion.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DBEngineVersion.parse (Xml.members "DBEngineVersion" xml))

  let to_query v = Query.to_query_list DBEngineVersion.to_query v

  let to_json v = `List (List.map DBEngineVersion.to_json v)

  let of_json j = Json.to_list DBEngineVersion.of_json j
end

module EventCategoriesMapList = struct
  type t = EventCategoriesMap.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map EventCategoriesMap.parse (Xml.members "EventCategoriesMap" xml))

  let to_query v = Query.to_query_list EventCategoriesMap.to_query v

  let to_json v = `List (List.map EventCategoriesMap.to_json v)

  let of_json j = Json.to_list EventCategoriesMap.of_json j
end

module ScalingConfiguration = struct
  type t =
    { min_capacity : Integer.t option
    ; max_capacity : Integer.t option
    ; auto_pause : Boolean.t option
    ; seconds_until_auto_pause : Integer.t option
    ; timeout_action : String.t option
    }

  let make
      ?min_capacity
      ?max_capacity
      ?auto_pause
      ?seconds_until_auto_pause
      ?timeout_action
      () =
    { min_capacity; max_capacity; auto_pause; seconds_until_auto_pause; timeout_action }

  let parse xml =
    Some
      { min_capacity = Util.option_bind (Xml.member "MinCapacity" xml) Integer.parse
      ; max_capacity = Util.option_bind (Xml.member "MaxCapacity" xml) Integer.parse
      ; auto_pause = Util.option_bind (Xml.member "AutoPause" xml) Boolean.parse
      ; seconds_until_auto_pause =
          Util.option_bind (Xml.member "SecondsUntilAutoPause" xml) Integer.parse
      ; timeout_action = Util.option_bind (Xml.member "TimeoutAction" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.timeout_action (fun f ->
               Query.Pair ("TimeoutAction", String.to_query f))
         ; Util.option_map v.seconds_until_auto_pause (fun f ->
               Query.Pair ("SecondsUntilAutoPause", Integer.to_query f))
         ; Util.option_map v.auto_pause (fun f ->
               Query.Pair ("AutoPause", Boolean.to_query f))
         ; Util.option_map v.max_capacity (fun f ->
               Query.Pair ("MaxCapacity", Integer.to_query f))
         ; Util.option_map v.min_capacity (fun f ->
               Query.Pair ("MinCapacity", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.timeout_action (fun f -> "timeout_action", String.to_json f)
         ; Util.option_map v.seconds_until_auto_pause (fun f ->
               "seconds_until_auto_pause", Integer.to_json f)
         ; Util.option_map v.auto_pause (fun f -> "auto_pause", Boolean.to_json f)
         ; Util.option_map v.max_capacity (fun f -> "max_capacity", Integer.to_json f)
         ; Util.option_map v.min_capacity (fun f -> "min_capacity", Integer.to_json f)
         ])

  let of_json j =
    { min_capacity = Util.option_map (Json.lookup j "min_capacity") Integer.of_json
    ; max_capacity = Util.option_map (Json.lookup j "max_capacity") Integer.of_json
    ; auto_pause = Util.option_map (Json.lookup j "auto_pause") Boolean.of_json
    ; seconds_until_auto_pause =
        Util.option_map (Json.lookup j "seconds_until_auto_pause") Integer.of_json
    ; timeout_action = Util.option_map (Json.lookup j "timeout_action") String.of_json
    }
end

module CloudwatchLogsExportConfiguration = struct
  type t =
    { enable_log_types : LogTypeList.t
    ; disable_log_types : LogTypeList.t
    }

  let make ?(enable_log_types = []) ?(disable_log_types = []) () =
    { enable_log_types; disable_log_types }

  let parse xml =
    Some
      { enable_log_types =
          Util.of_option
            []
            (Util.option_bind (Xml.member "EnableLogTypes" xml) LogTypeList.parse)
      ; disable_log_types =
          Util.of_option
            []
            (Util.option_bind (Xml.member "DisableLogTypes" xml) LogTypeList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("DisableLogTypes.member", LogTypeList.to_query v.disable_log_types))
         ; Some
             (Query.Pair ("EnableLogTypes.member", LogTypeList.to_query v.enable_log_types))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("disable_log_types", LogTypeList.to_json v.disable_log_types)
         ; Some ("enable_log_types", LogTypeList.to_json v.enable_log_types)
         ])

  let of_json j =
    { enable_log_types =
        LogTypeList.of_json (Util.of_option_exn (Json.lookup j "enable_log_types"))
    ; disable_log_types =
        LogTypeList.of_json (Util.of_option_exn (Json.lookup j "disable_log_types"))
    }
end

module GlobalClusterList = struct
  type t = GlobalCluster.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map GlobalCluster.parse (Xml.members "GlobalClusterMember" xml))

  let to_query v = Query.to_query_list GlobalCluster.to_query v

  let to_json v = `List (List.map GlobalCluster.to_json v)

  let of_json j = Json.to_list GlobalCluster.of_json j
end

module DBClusterParameterGroupList = struct
  type t = DBClusterParameterGroup.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map DBClusterParameterGroup.parse (Xml.members "DBClusterParameterGroup" xml))

  let to_query v = Query.to_query_list DBClusterParameterGroup.to_query v

  let to_json v = `List (List.map DBClusterParameterGroup.to_json v)

  let of_json j = Json.to_list DBClusterParameterGroup.of_json j
end

module DescribeDBLogFilesList = struct
  type t = DescribeDBLogFilesDetails.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map
         DescribeDBLogFilesDetails.parse
         (Xml.members "DescribeDBLogFilesDetails" xml))

  let to_query v = Query.to_query_list DescribeDBLogFilesDetails.to_query v

  let to_json v = `List (List.map DescribeDBLogFilesDetails.to_json v)

  let of_json j = Json.to_list DescribeDBLogFilesDetails.of_json j
end

module CertificateList = struct
  type t = Certificate.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map Certificate.parse (Xml.members "Certificate" xml))

  let to_query v = Query.to_query_list Certificate.to_query v

  let to_json v = `List (List.map Certificate.to_json v)

  let of_json j = Json.to_list Certificate.of_json j
end

module OptionGroupsList = struct
  type t = OptionGroup.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map OptionGroup.parse (Xml.members "OptionGroup" xml))

  let to_query v = Query.to_query_list OptionGroup.to_query v

  let to_json v = `List (List.map OptionGroup.to_json v)

  let of_json j = Json.to_list OptionGroup.of_json j
end

module UserAuthConfigList = struct
  type t = UserAuthConfig.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map UserAuthConfig.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list UserAuthConfig.to_query v

  let to_json v = `List (List.map UserAuthConfig.to_json v)

  let of_json j = Json.to_list UserAuthConfig.of_json j
end

module SourceRegionList = struct
  type t = SourceRegion.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map SourceRegion.parse (Xml.members "SourceRegion" xml))

  let to_query v = Query.to_query_list SourceRegion.to_query v

  let to_json v = `List (List.map SourceRegion.to_json v)

  let of_json j = Json.to_list SourceRegion.of_json j
end

module DBClusterSnapshotList = struct
  type t = DBClusterSnapshot.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map DBClusterSnapshot.parse (Xml.members "DBClusterSnapshot" xml))

  let to_query v = Query.to_query_list DBClusterSnapshot.to_query v

  let to_json v = `List (List.map DBClusterSnapshot.to_json v)

  let of_json j = Json.to_list DBClusterSnapshot.of_json j
end

module ConnectionPoolConfiguration = struct
  type t =
    { max_connections_percent : Integer.t option
    ; max_idle_connections_percent : Integer.t option
    ; connection_borrow_timeout : Integer.t option
    ; session_pinning_filters : StringList.t
    ; init_query : String.t option
    }

  let make
      ?max_connections_percent
      ?max_idle_connections_percent
      ?connection_borrow_timeout
      ?(session_pinning_filters = [])
      ?init_query
      () =
    { max_connections_percent
    ; max_idle_connections_percent
    ; connection_borrow_timeout
    ; session_pinning_filters
    ; init_query
    }

  let parse xml =
    Some
      { max_connections_percent =
          Util.option_bind (Xml.member "MaxConnectionsPercent" xml) Integer.parse
      ; max_idle_connections_percent =
          Util.option_bind (Xml.member "MaxIdleConnectionsPercent" xml) Integer.parse
      ; connection_borrow_timeout =
          Util.option_bind (Xml.member "ConnectionBorrowTimeout" xml) Integer.parse
      ; session_pinning_filters =
          Util.of_option
            []
            (Util.option_bind (Xml.member "SessionPinningFilters" xml) StringList.parse)
      ; init_query = Util.option_bind (Xml.member "InitQuery" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.init_query (fun f ->
               Query.Pair ("InitQuery", String.to_query f))
         ; Some
             (Query.Pair
                ( "SessionPinningFilters.member"
                , StringList.to_query v.session_pinning_filters ))
         ; Util.option_map v.connection_borrow_timeout (fun f ->
               Query.Pair ("ConnectionBorrowTimeout", Integer.to_query f))
         ; Util.option_map v.max_idle_connections_percent (fun f ->
               Query.Pair ("MaxIdleConnectionsPercent", Integer.to_query f))
         ; Util.option_map v.max_connections_percent (fun f ->
               Query.Pair ("MaxConnectionsPercent", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.init_query (fun f -> "init_query", String.to_json f)
         ; Some ("session_pinning_filters", StringList.to_json v.session_pinning_filters)
         ; Util.option_map v.connection_borrow_timeout (fun f ->
               "connection_borrow_timeout", Integer.to_json f)
         ; Util.option_map v.max_idle_connections_percent (fun f ->
               "max_idle_connections_percent", Integer.to_json f)
         ; Util.option_map v.max_connections_percent (fun f ->
               "max_connections_percent", Integer.to_json f)
         ])

  let of_json j =
    { max_connections_percent =
        Util.option_map (Json.lookup j "max_connections_percent") Integer.of_json
    ; max_idle_connections_percent =
        Util.option_map (Json.lookup j "max_idle_connections_percent") Integer.of_json
    ; connection_borrow_timeout =
        Util.option_map (Json.lookup j "connection_borrow_timeout") Integer.of_json
    ; session_pinning_filters =
        StringList.of_json (Util.of_option_exn (Json.lookup j "session_pinning_filters"))
    ; init_query = Util.option_map (Json.lookup j "init_query") String.of_json
    }
end

module DBClusterEndpointList = struct
  type t = DBClusterEndpoint.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map DBClusterEndpoint.parse (Xml.members "DBClusterEndpointList" xml))

  let to_query v = Query.to_query_list DBClusterEndpoint.to_query v

  let to_json v = `List (List.map DBClusterEndpoint.to_json v)

  let of_json j = Json.to_list DBClusterEndpoint.of_json j
end

module EventSubscriptionsList = struct
  type t = EventSubscription.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map EventSubscription.parse (Xml.members "EventSubscription" xml))

  let to_query v = Query.to_query_list EventSubscription.to_query v

  let to_json v = `List (List.map EventSubscription.to_json v)

  let of_json j = Json.to_list EventSubscription.of_json j
end

module OptionConfigurationList = struct
  type t = OptionConfiguration.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map OptionConfiguration.parse (Xml.members "OptionConfiguration" xml))

  let to_query v = Query.to_query_list OptionConfiguration.to_query v

  let to_json v = `List (List.map OptionConfiguration.to_json v)

  let of_json j = Json.to_list OptionConfiguration.of_json j
end

module OptionNamesList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module DBSubnetGroups = struct
  type t = DBSubnetGroup.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DBSubnetGroup.parse (Xml.members "DBSubnetGroup" xml))

  let to_query v = Query.to_query_list DBSubnetGroup.to_query v

  let to_json v = `List (List.map DBSubnetGroup.to_json v)

  let of_json j = Json.to_list DBSubnetGroup.of_json j
end

module DBSnapshotList = struct
  type t = DBSnapshot.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DBSnapshot.parse (Xml.members "DBSnapshot" xml))

  let to_query v = Query.to_query_list DBSnapshot.to_query v

  let to_json v = `List (List.map DBSnapshot.to_json v)

  let of_json j = Json.to_list DBSnapshot.of_json j
end

module DBSnapshotAttributesResult = struct
  type t =
    { d_b_snapshot_identifier : String.t option
    ; d_b_snapshot_attributes : DBSnapshotAttributeList.t
    }

  let make ?d_b_snapshot_identifier ?(d_b_snapshot_attributes = []) () =
    { d_b_snapshot_identifier; d_b_snapshot_attributes }

  let parse xml =
    Some
      { d_b_snapshot_identifier =
          Util.option_bind (Xml.member "DBSnapshotIdentifier" xml) String.parse
      ; d_b_snapshot_attributes =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBSnapshotAttributes" xml)
               DBSnapshotAttributeList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBSnapshotAttributes.member"
                , DBSnapshotAttributeList.to_query v.d_b_snapshot_attributes ))
         ; Util.option_map v.d_b_snapshot_identifier (fun f ->
               Query.Pair ("DBSnapshotIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "d_b_snapshot_attributes"
             , DBSnapshotAttributeList.to_json v.d_b_snapshot_attributes )
         ; Util.option_map v.d_b_snapshot_identifier (fun f ->
               "d_b_snapshot_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_snapshot_identifier =
        Util.option_map (Json.lookup j "d_b_snapshot_identifier") String.of_json
    ; d_b_snapshot_attributes =
        DBSnapshotAttributeList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_snapshot_attributes"))
    }
end

module DBProxyList = struct
  type t = DBProxy.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map DBProxy.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list DBProxy.to_query v

  let to_json v = `List (List.map DBProxy.to_json v)

  let of_json j = Json.to_list DBProxy.of_json j
end

module DBInstanceList = struct
  type t = DBInstance.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DBInstance.parse (Xml.members "DBInstance" xml))

  let to_query v = Query.to_query_list DBInstance.to_query v

  let to_json v = `List (List.map DBInstance.to_json v)

  let of_json j = Json.to_list DBInstance.of_json j
end

module InstallationMediaList = struct
  type t = InstallationMedia.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map InstallationMedia.parse (Xml.members "InstallationMedia" xml))

  let to_query v = Query.to_query_list InstallationMedia.to_query v

  let to_json v = `List (List.map InstallationMedia.to_json v)

  let of_json j = Json.to_list InstallationMedia.of_json j
end

module ExportTasksList = struct
  type t = ExportTask.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map ExportTask.parse (Xml.members "ExportTask" xml))

  let to_query v = Query.to_query_list ExportTask.to_query v

  let to_json v = `List (List.map ExportTask.to_json v)

  let of_json j = Json.to_list ExportTask.of_json j
end

module AccountQuotaList = struct
  type t = AccountQuota.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map AccountQuota.parse (Xml.members "AccountQuota" xml))

  let to_query v = Query.to_query_list AccountQuota.to_query v

  let to_json v = `List (List.map AccountQuota.to_json v)

  let of_json j = Json.to_list AccountQuota.of_json j
end

module DBInstanceAutomatedBackupList = struct
  type t = DBInstanceAutomatedBackup.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map
         DBInstanceAutomatedBackup.parse
         (Xml.members "DBInstanceAutomatedBackup" xml))

  let to_query v = Query.to_query_list DBInstanceAutomatedBackup.to_query v

  let to_json v = `List (List.map DBInstanceAutomatedBackup.to_json v)

  let of_json j = Json.to_list DBInstanceAutomatedBackup.of_json j
end

module EngineFamily = struct
  type t =
    | MYSQL
    | POSTGRESQL

  let str_to_t = [ "POSTGRESQL", POSTGRESQL; "MYSQL", MYSQL ]

  let t_to_str = [ POSTGRESQL, "POSTGRESQL"; MYSQL, "MYSQL" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module DBParameterGroupList = struct
  type t = DBParameterGroup.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DBParameterGroup.parse (Xml.members "DBParameterGroup" xml))

  let to_query v = Query.to_query_list DBParameterGroup.to_query v

  let to_json v = `List (List.map DBParameterGroup.to_json v)

  let of_json j = Json.to_list DBParameterGroup.of_json j
end

module EventList = struct
  type t = Event.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Event.parse (Xml.members "Event" xml))

  let to_query v = Query.to_query_list Event.to_query v

  let to_json v = `List (List.map Event.to_json v)

  let of_json j = Json.to_list Event.of_json j
end

module OrderableDBInstanceOptionsList = struct
  type t = OrderableDBInstanceOption.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map
         OrderableDBInstanceOption.parse
         (Xml.members "OrderableDBInstanceOption" xml))

  let to_query v = Query.to_query_list OrderableDBInstanceOption.to_query v

  let to_json v = `List (List.map OrderableDBInstanceOption.to_json v)

  let of_json j = Json.to_list OrderableDBInstanceOption.of_json j
end

module KeyList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module ReservedDBInstancesOfferingList = struct
  type t = ReservedDBInstancesOffering.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map
         ReservedDBInstancesOffering.parse
         (Xml.members "ReservedDBInstancesOffering" xml))

  let to_query v = Query.to_query_list ReservedDBInstancesOffering.to_query v

  let to_json v = `List (List.map ReservedDBInstancesOffering.to_json v)

  let of_json j = Json.to_list ReservedDBInstancesOffering.of_json j
end

module DBClusterBacktrackList = struct
  type t = DBClusterBacktrack.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map DBClusterBacktrack.parse (Xml.members "DBClusterBacktrack" xml))

  let to_query v = Query.to_query_list DBClusterBacktrack.to_query v

  let to_json v = `List (List.map DBClusterBacktrack.to_json v)

  let of_json j = Json.to_list DBClusterBacktrack.of_json j
end

module DBClusterList = struct
  type t = DBCluster.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map DBCluster.parse (Xml.members "DBCluster" xml))

  let to_query v = Query.to_query_list DBCluster.to_query v

  let to_json v = `List (List.map DBCluster.to_json v)

  let of_json j = Json.to_list DBCluster.of_json j
end

module DBSecurityGroups = struct
  type t = DBSecurityGroup.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DBSecurityGroup.parse (Xml.members "DBSecurityGroup" xml))

  let to_query v = Query.to_query_list DBSecurityGroup.to_query v

  let to_json v = `List (List.map DBSecurityGroup.to_json v)

  let of_json j = Json.to_list DBSecurityGroup.of_json j
end

module CreateGlobalClusterMessage = struct
  type t =
    { global_cluster_identifier : String.t option
    ; source_d_b_cluster_identifier : String.t option
    ; engine : String.t option
    ; engine_version : String.t option
    ; deletion_protection : Boolean.t option
    ; database_name : String.t option
    ; storage_encrypted : Boolean.t option
    }

  let make
      ?global_cluster_identifier
      ?source_d_b_cluster_identifier
      ?engine
      ?engine_version
      ?deletion_protection
      ?database_name
      ?storage_encrypted
      () =
    { global_cluster_identifier
    ; source_d_b_cluster_identifier
    ; engine
    ; engine_version
    ; deletion_protection
    ; database_name
    ; storage_encrypted
    }

  let parse xml =
    Some
      { global_cluster_identifier =
          Util.option_bind (Xml.member "GlobalClusterIdentifier" xml) String.parse
      ; source_d_b_cluster_identifier =
          Util.option_bind (Xml.member "SourceDBClusterIdentifier" xml) String.parse
      ; engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; database_name = Util.option_bind (Xml.member "DatabaseName" xml) String.parse
      ; storage_encrypted =
          Util.option_bind (Xml.member "StorageEncrypted" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.storage_encrypted (fun f ->
               Query.Pair ("StorageEncrypted", Boolean.to_query f))
         ; Util.option_map v.database_name (fun f ->
               Query.Pair ("DatabaseName", String.to_query f))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ; Util.option_map v.source_d_b_cluster_identifier (fun f ->
               Query.Pair ("SourceDBClusterIdentifier", String.to_query f))
         ; Util.option_map v.global_cluster_identifier (fun f ->
               Query.Pair ("GlobalClusterIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.storage_encrypted (fun f ->
               "storage_encrypted", Boolean.to_json f)
         ; Util.option_map v.database_name (fun f -> "database_name", String.to_json f)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ; Util.option_map v.source_d_b_cluster_identifier (fun f ->
               "source_d_b_cluster_identifier", String.to_json f)
         ; Util.option_map v.global_cluster_identifier (fun f ->
               "global_cluster_identifier", String.to_json f)
         ])

  let of_json j =
    { global_cluster_identifier =
        Util.option_map (Json.lookup j "global_cluster_identifier") String.of_json
    ; source_d_b_cluster_identifier =
        Util.option_map (Json.lookup j "source_d_b_cluster_identifier") String.of_json
    ; engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; database_name = Util.option_map (Json.lookup j "database_name") String.of_json
    ; storage_encrypted =
        Util.option_map (Json.lookup j "storage_encrypted") Boolean.of_json
    }
end

module CopyOptionGroupMessage = struct
  type t =
    { source_option_group_identifier : String.t
    ; target_option_group_identifier : String.t
    ; target_option_group_description : String.t
    ; tags : TagList.t
    }

  let make
      ~source_option_group_identifier
      ~target_option_group_identifier
      ~target_option_group_description
      ?(tags = [])
      () =
    { source_option_group_identifier
    ; target_option_group_identifier
    ; target_option_group_description
    ; tags
    }

  let parse xml =
    Some
      { source_option_group_identifier =
          Xml.required
            "SourceOptionGroupIdentifier"
            (Util.option_bind (Xml.member "SourceOptionGroupIdentifier" xml) String.parse)
      ; target_option_group_identifier =
          Xml.required
            "TargetOptionGroupIdentifier"
            (Util.option_bind (Xml.member "TargetOptionGroupIdentifier" xml) String.parse)
      ; target_option_group_description =
          Xml.required
            "TargetOptionGroupDescription"
            (Util.option_bind
               (Xml.member "TargetOptionGroupDescription" xml)
               String.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Query.Pair
                ( "TargetOptionGroupDescription"
                , String.to_query v.target_option_group_description ))
         ; Some
             (Query.Pair
                ( "TargetOptionGroupIdentifier"
                , String.to_query v.target_option_group_identifier ))
         ; Some
             (Query.Pair
                ( "SourceOptionGroupIdentifier"
                , String.to_query v.source_option_group_identifier ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some
             ( "target_option_group_description"
             , String.to_json v.target_option_group_description )
         ; Some
             ( "target_option_group_identifier"
             , String.to_json v.target_option_group_identifier )
         ; Some
             ( "source_option_group_identifier"
             , String.to_json v.source_option_group_identifier )
         ])

  let of_json j =
    { source_option_group_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "source_option_group_identifier"))
    ; target_option_group_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "target_option_group_identifier"))
    ; target_option_group_description =
        String.of_json
          (Util.of_option_exn (Json.lookup j "target_option_group_description"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module DBInstanceAutomatedBackupQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBInstanceMessage = struct
  type t =
    { d_b_instance_identifier : String.t
    ; skip_final_snapshot : Boolean.t option
    ; final_d_b_snapshot_identifier : String.t option
    ; delete_automated_backups : Boolean.t option
    }

  let make
      ~d_b_instance_identifier
      ?skip_final_snapshot
      ?final_d_b_snapshot_identifier
      ?delete_automated_backups
      () =
    { d_b_instance_identifier
    ; skip_final_snapshot
    ; final_d_b_snapshot_identifier
    ; delete_automated_backups
    }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; skip_final_snapshot =
          Util.option_bind (Xml.member "SkipFinalSnapshot" xml) Boolean.parse
      ; final_d_b_snapshot_identifier =
          Util.option_bind (Xml.member "FinalDBSnapshotIdentifier" xml) String.parse
      ; delete_automated_backups =
          Util.option_bind (Xml.member "DeleteAutomatedBackups" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.delete_automated_backups (fun f ->
               Query.Pair ("DeleteAutomatedBackups", Boolean.to_query f))
         ; Util.option_map v.final_d_b_snapshot_identifier (fun f ->
               Query.Pair ("FinalDBSnapshotIdentifier", String.to_query f))
         ; Util.option_map v.skip_final_snapshot (fun f ->
               Query.Pair ("SkipFinalSnapshot", Boolean.to_query f))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.delete_automated_backups (fun f ->
               "delete_automated_backups", Boolean.to_json f)
         ; Util.option_map v.final_d_b_snapshot_identifier (fun f ->
               "final_d_b_snapshot_identifier", String.to_json f)
         ; Util.option_map v.skip_final_snapshot (fun f ->
               "skip_final_snapshot", Boolean.to_json f)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; skip_final_snapshot =
        Util.option_map (Json.lookup j "skip_final_snapshot") Boolean.of_json
    ; final_d_b_snapshot_identifier =
        Util.option_map (Json.lookup j "final_d_b_snapshot_identifier") String.of_json
    ; delete_automated_backups =
        Util.option_map (Json.lookup j "delete_automated_backups") Boolean.of_json
    }
end

module PromoteReadReplicaMessage = struct
  type t =
    { d_b_instance_identifier : String.t
    ; backup_retention_period : Integer.t option
    ; preferred_backup_window : String.t option
    }

  let make ~d_b_instance_identifier ?backup_retention_period ?preferred_backup_window () =
    { d_b_instance_identifier; backup_retention_period; preferred_backup_window }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; backup_retention_period =
          Util.option_bind (Xml.member "BackupRetentionPeriod" xml) Integer.parse
      ; preferred_backup_window =
          Util.option_bind (Xml.member "PreferredBackupWindow" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.preferred_backup_window (fun f ->
               Query.Pair ("PreferredBackupWindow", String.to_query f))
         ; Util.option_map v.backup_retention_period (fun f ->
               Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.preferred_backup_window (fun f ->
               "preferred_backup_window", String.to_json f)
         ; Util.option_map v.backup_retention_period (fun f ->
               "backup_retention_period", Integer.to_json f)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; backup_retention_period =
        Util.option_map (Json.lookup j "backup_retention_period") Integer.of_json
    ; preferred_backup_window =
        Util.option_map (Json.lookup j "preferred_backup_window") String.of_json
    }
end

module FailoverDBClusterResult = struct
  type t = { d_b_cluster : DBCluster.t option }

  let make ?d_b_cluster () = { d_b_cluster }

  let parse xml =
    Some { d_b_cluster = Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f ->
               Query.Pair ("DBCluster", DBCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f -> "d_b_cluster", DBCluster.to_json f) ])

  let of_json j =
    { d_b_cluster = Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json }
end

module OptionGroupOptionsMessage = struct
  type t =
    { option_group_options : OptionGroupOptionsList.t
    ; marker : String.t option
    }

  let make ?(option_group_options = []) ?marker () = { option_group_options; marker }

  let parse xml =
    Some
      { option_group_options =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "OptionGroupOptions" xml)
               OptionGroupOptionsList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair
                ( "OptionGroupOptions.member"
                , OptionGroupOptionsList.to_query v.option_group_options ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some
             ( "option_group_options"
             , OptionGroupOptionsList.to_json v.option_group_options )
         ])

  let of_json j =
    { option_group_options =
        OptionGroupOptionsList.of_json
          (Util.of_option_exn (Json.lookup j "option_group_options"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module StartDBInstanceResult = struct
  type t = { d_b_instance : DBInstance.t option }

  let make ?d_b_instance () = { d_b_instance }

  let parse xml =
    Some
      { d_b_instance = Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f ->
               Query.Pair ("DBInstance", DBInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f -> "d_b_instance", DBInstance.to_json f)
         ])

  let of_json j =
    { d_b_instance = Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json }
end

module DescribeDBLogFilesMessage = struct
  type t =
    { d_b_instance_identifier : String.t
    ; filename_contains : String.t option
    ; file_last_written : Long.t option
    ; file_size : Long.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ~d_b_instance_identifier
      ?filename_contains
      ?file_last_written
      ?file_size
      ?(filters = [])
      ?max_records
      ?marker
      () =
    { d_b_instance_identifier
    ; filename_contains
    ; file_last_written
    ; file_size
    ; filters
    ; max_records
    ; marker
    }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; filename_contains =
          Util.option_bind (Xml.member "FilenameContains" xml) String.parse
      ; file_last_written = Util.option_bind (Xml.member "FileLastWritten" xml) Long.parse
      ; file_size = Util.option_bind (Xml.member "FileSize" xml) Long.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.file_size (fun f -> Query.Pair ("FileSize", Long.to_query f))
         ; Util.option_map v.file_last_written (fun f ->
               Query.Pair ("FileLastWritten", Long.to_query f))
         ; Util.option_map v.filename_contains (fun f ->
               Query.Pair ("FilenameContains", String.to_query f))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.file_size (fun f -> "file_size", Long.to_json f)
         ; Util.option_map v.file_last_written (fun f ->
               "file_last_written", Long.to_json f)
         ; Util.option_map v.filename_contains (fun f ->
               "filename_contains", String.to_json f)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; filename_contains =
        Util.option_map (Json.lookup j "filename_contains") String.of_json
    ; file_last_written = Util.option_map (Json.lookup j "file_last_written") Long.of_json
    ; file_size = Util.option_map (Json.lookup j "file_size") Long.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module BacktrackDBClusterMessage = struct
  type t =
    { d_b_cluster_identifier : String.t
    ; backtrack_to : DateTime.t
    ; force : Boolean.t option
    ; use_earliest_time_on_point_in_time_unavailable : Boolean.t option
    }

  let make
      ~d_b_cluster_identifier
      ~backtrack_to
      ?force
      ?use_earliest_time_on_point_in_time_unavailable
      () =
    { d_b_cluster_identifier
    ; backtrack_to
    ; force
    ; use_earliest_time_on_point_in_time_unavailable
    }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; backtrack_to =
          Xml.required
            "BacktrackTo"
            (Util.option_bind (Xml.member "BacktrackTo" xml) DateTime.parse)
      ; force = Util.option_bind (Xml.member "Force" xml) Boolean.parse
      ; use_earliest_time_on_point_in_time_unavailable =
          Util.option_bind
            (Xml.member "UseEarliestTimeOnPointInTimeUnavailable" xml)
            Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.use_earliest_time_on_point_in_time_unavailable (fun f ->
               Query.Pair ("UseEarliestTimeOnPointInTimeUnavailable", Boolean.to_query f))
         ; Util.option_map v.force (fun f -> Query.Pair ("Force", Boolean.to_query f))
         ; Some (Query.Pair ("BacktrackTo", DateTime.to_query v.backtrack_to))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.use_earliest_time_on_point_in_time_unavailable (fun f ->
               "use_earliest_time_on_point_in_time_unavailable", Boolean.to_json f)
         ; Util.option_map v.force (fun f -> "force", Boolean.to_json f)
         ; Some ("backtrack_to", DateTime.to_json v.backtrack_to)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; backtrack_to = DateTime.of_json (Util.of_option_exn (Json.lookup j "backtrack_to"))
    ; force = Util.option_map (Json.lookup j "force") Boolean.of_json
    ; use_earliest_time_on_point_in_time_unavailable =
        Util.option_map
          (Json.lookup j "use_earliest_time_on_point_in_time_unavailable")
          Boolean.of_json
    }
end

module InsufficientStorageClusterCapacityFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module StopActivityStreamResponse = struct
  type t =
    { kms_key_id : String.t option
    ; kinesis_stream_name : String.t option
    ; status : ActivityStreamStatus.t option
    }

  let make ?kms_key_id ?kinesis_stream_name ?status () =
    { kms_key_id; kinesis_stream_name; status }

  let parse xml =
    Some
      { kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; kinesis_stream_name =
          Util.option_bind (Xml.member "KinesisStreamName" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) ActivityStreamStatus.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f ->
               Query.Pair ("Status", ActivityStreamStatus.to_query f))
         ; Util.option_map v.kinesis_stream_name (fun f ->
               Query.Pair ("KinesisStreamName", String.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> "status", ActivityStreamStatus.to_json f)
         ; Util.option_map v.kinesis_stream_name (fun f ->
               "kinesis_stream_name", String.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ])

  let of_json j =
    { kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; kinesis_stream_name =
        Util.option_map (Json.lookup j "kinesis_stream_name") String.of_json
    ; status = Util.option_map (Json.lookup j "status") ActivityStreamStatus.of_json
    }
end

module AuthorizationNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBClusterQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBSubnetGroupMessage = struct
  type t =
    { d_b_subnet_group_name : String.t
    ; d_b_subnet_group_description : String.t
    ; subnet_ids : SubnetIdentifierList.t
    ; tags : TagList.t
    }

  let make
      ~d_b_subnet_group_name
      ~d_b_subnet_group_description
      ~subnet_ids
      ?(tags = [])
      () =
    { d_b_subnet_group_name; d_b_subnet_group_description; subnet_ids; tags }

  let parse xml =
    Some
      { d_b_subnet_group_name =
          Xml.required
            "DBSubnetGroupName"
            (Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse)
      ; d_b_subnet_group_description =
          Xml.required
            "DBSubnetGroupDescription"
            (Util.option_bind (Xml.member "DBSubnetGroupDescription" xml) String.parse)
      ; subnet_ids =
          Xml.required
            "SubnetIds"
            (Util.option_bind (Xml.member "SubnetIds" xml) SubnetIdentifierList.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Query.Pair ("SubnetIds.member", SubnetIdentifierList.to_query v.subnet_ids))
         ; Some
             (Query.Pair
                ( "DBSubnetGroupDescription"
                , String.to_query v.d_b_subnet_group_description ))
         ; Some
             (Query.Pair ("DBSubnetGroupName", String.to_query v.d_b_subnet_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some ("subnet_ids", SubnetIdentifierList.to_json v.subnet_ids)
         ; Some
             ( "d_b_subnet_group_description"
             , String.to_json v.d_b_subnet_group_description )
         ; Some ("d_b_subnet_group_name", String.to_json v.d_b_subnet_group_name)
         ])

  let of_json j =
    { d_b_subnet_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_subnet_group_name"))
    ; d_b_subnet_group_description =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_subnet_group_description"))
    ; subnet_ids =
        SubnetIdentifierList.of_json (Util.of_option_exn (Json.lookup j "subnet_ids"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module DeleteDBSnapshotMessage = struct
  type t = { d_b_snapshot_identifier : String.t }

  let make ~d_b_snapshot_identifier () = { d_b_snapshot_identifier }

  let parse xml =
    Some
      { d_b_snapshot_identifier =
          Xml.required
            "DBSnapshotIdentifier"
            (Util.option_bind (Xml.member "DBSnapshotIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("DBSnapshotIdentifier", String.to_query v.d_b_snapshot_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_snapshot_identifier", String.to_json v.d_b_snapshot_identifier) ])

  let of_json j =
    { d_b_snapshot_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_snapshot_identifier"))
    }
end

module CopyDBClusterParameterGroupResult = struct
  type t = { d_b_cluster_parameter_group : DBClusterParameterGroup.t option }

  let make ?d_b_cluster_parameter_group () = { d_b_cluster_parameter_group }

  let parse xml =
    Some
      { d_b_cluster_parameter_group =
          Util.option_bind
            (Xml.member "DBClusterParameterGroup" xml)
            DBClusterParameterGroup.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_parameter_group (fun f ->
               Query.Pair ("DBClusterParameterGroup", DBClusterParameterGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_parameter_group (fun f ->
               "d_b_cluster_parameter_group", DBClusterParameterGroup.to_json f)
         ])

  let of_json j =
    { d_b_cluster_parameter_group =
        Util.option_map
          (Json.lookup j "d_b_cluster_parameter_group")
          DBClusterParameterGroup.of_json
    }
end

module DescribeValidDBInstanceModificationsResult = struct
  type t =
    { valid_d_b_instance_modifications_message :
        ValidDBInstanceModificationsMessage.t option
    }

  let make ?valid_d_b_instance_modifications_message () =
    { valid_d_b_instance_modifications_message }

  let parse xml =
    Some
      { valid_d_b_instance_modifications_message =
          Util.option_bind
            (Xml.member "ValidDBInstanceModificationsMessage" xml)
            ValidDBInstanceModificationsMessage.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.valid_d_b_instance_modifications_message (fun f ->
               Query.Pair
                 ( "ValidDBInstanceModificationsMessage"
                 , ValidDBInstanceModificationsMessage.to_query f ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.valid_d_b_instance_modifications_message (fun f ->
               ( "valid_d_b_instance_modifications_message"
               , ValidDBInstanceModificationsMessage.to_json f ))
         ])

  let of_json j =
    { valid_d_b_instance_modifications_message =
        Util.option_map
          (Json.lookup j "valid_d_b_instance_modifications_message")
          ValidDBInstanceModificationsMessage.of_json
    }
end

module CreateDBInstanceReadReplicaResult = struct
  type t = { d_b_instance : DBInstance.t option }

  let make ?d_b_instance () = { d_b_instance }

  let parse xml =
    Some
      { d_b_instance = Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f ->
               Query.Pair ("DBInstance", DBInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f -> "d_b_instance", DBInstance.to_json f)
         ])

  let of_json j =
    { d_b_instance = Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json }
end

module DeleteDBSubnetGroupMessage = struct
  type t = { d_b_subnet_group_name : String.t }

  let make ~d_b_subnet_group_name () = { d_b_subnet_group_name }

  let parse xml =
    Some
      { d_b_subnet_group_name =
          Xml.required
            "DBSubnetGroupName"
            (Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("DBSubnetGroupName", String.to_query v.d_b_subnet_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_subnet_group_name", String.to_json v.d_b_subnet_group_name) ])

  let of_json j =
    { d_b_subnet_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_subnet_group_name"))
    }
end

module AddSourceIdentifierToSubscriptionResult = struct
  type t = { event_subscription : EventSubscription.t option }

  let make ?event_subscription () = { event_subscription }

  let parse xml =
    Some
      { event_subscription =
          Util.option_bind (Xml.member "EventSubscription" xml) EventSubscription.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.event_subscription (fun f ->
               Query.Pair ("EventSubscription", EventSubscription.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.event_subscription (fun f ->
               "event_subscription", EventSubscription.to_json f)
         ])

  let of_json j =
    { event_subscription =
        Util.option_map (Json.lookup j "event_subscription") EventSubscription.of_json
    }
end

module DescribeEventSubscriptionsMessage = struct
  type t =
    { subscription_name : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?subscription_name ?(filters = []) ?max_records ?marker () =
    { subscription_name; filters; max_records; marker }

  let parse xml =
    Some
      { subscription_name =
          Util.option_bind (Xml.member "SubscriptionName" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.subscription_name (fun f ->
               Query.Pair ("SubscriptionName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.subscription_name (fun f ->
               "subscription_name", String.to_json f)
         ])

  let of_json j =
    { subscription_name =
        Util.option_map (Json.lookup j "subscription_name") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module DBClusterRoleAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeAccountAttributesMessage = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module AddRoleToDBClusterMessage = struct
  type t =
    { d_b_cluster_identifier : String.t
    ; role_arn : String.t
    ; feature_name : String.t option
    }

  let make ~d_b_cluster_identifier ~role_arn ?feature_name () =
    { d_b_cluster_identifier; role_arn; feature_name }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; role_arn =
          Xml.required
            "RoleArn"
            (Util.option_bind (Xml.member "RoleArn" xml) String.parse)
      ; feature_name = Util.option_bind (Xml.member "FeatureName" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.feature_name (fun f ->
               Query.Pair ("FeatureName", String.to_query f))
         ; Some (Query.Pair ("RoleArn", String.to_query v.role_arn))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.feature_name (fun f -> "feature_name", String.to_json f)
         ; Some ("role_arn", String.to_json v.role_arn)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; role_arn = String.of_json (Util.of_option_exn (Json.lookup j "role_arn"))
    ; feature_name = Util.option_map (Json.lookup j "feature_name") String.of_json
    }
end

module DBInstanceNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidS3BucketFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeGlobalClustersMessage = struct
  type t =
    { global_cluster_identifier : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?global_cluster_identifier ?(filters = []) ?max_records ?marker () =
    { global_cluster_identifier; filters; max_records; marker }

  let parse xml =
    Some
      { global_cluster_identifier =
          Util.option_bind (Xml.member "GlobalClusterIdentifier" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.global_cluster_identifier (fun f ->
               Query.Pair ("GlobalClusterIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.global_cluster_identifier (fun f ->
               "global_cluster_identifier", String.to_json f)
         ])

  let of_json j =
    { global_cluster_identifier =
        Util.option_map (Json.lookup j "global_cluster_identifier") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module IamRoleNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CustomAvailabilityZoneMessage = struct
  type t =
    { marker : String.t option
    ; custom_availability_zones : CustomAvailabilityZoneList.t
    }

  let make ?marker ?(custom_availability_zones = []) () =
    { marker; custom_availability_zones }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; custom_availability_zones =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "CustomAvailabilityZones" xml)
               CustomAvailabilityZoneList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "CustomAvailabilityZones.member"
                , CustomAvailabilityZoneList.to_query v.custom_availability_zones ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "custom_availability_zones"
             , CustomAvailabilityZoneList.to_json v.custom_availability_zones )
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; custom_availability_zones =
        CustomAvailabilityZoneList.of_json
          (Util.of_option_exn (Json.lookup j "custom_availability_zones"))
    }
end

module InvalidDBSubnetGroupStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBClusterSnapshotResult = struct
  type t = { d_b_cluster_snapshot : DBClusterSnapshot.t option }

  let make ?d_b_cluster_snapshot () = { d_b_cluster_snapshot }

  let parse xml =
    Some
      { d_b_cluster_snapshot =
          Util.option_bind (Xml.member "DBClusterSnapshot" xml) DBClusterSnapshot.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_snapshot (fun f ->
               Query.Pair ("DBClusterSnapshot", DBClusterSnapshot.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_snapshot (fun f ->
               "d_b_cluster_snapshot", DBClusterSnapshot.to_json f)
         ])

  let of_json j =
    { d_b_cluster_snapshot =
        Util.option_map (Json.lookup j "d_b_cluster_snapshot") DBClusterSnapshot.of_json
    }
end

module DBSnapshotAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBInstanceResult = struct
  type t = { d_b_instance : DBInstance.t option }

  let make ?d_b_instance () = { d_b_instance }

  let parse xml =
    Some
      { d_b_instance = Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f ->
               Query.Pair ("DBInstance", DBInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f -> "d_b_instance", DBInstance.to_json f)
         ])

  let of_json j =
    { d_b_instance = Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json }
end

module RemoveFromGlobalClusterResult = struct
  type t = { global_cluster : GlobalCluster.t option }

  let make ?global_cluster () = { global_cluster }

  let parse xml =
    Some
      { global_cluster =
          Util.option_bind (Xml.member "GlobalCluster" xml) GlobalCluster.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.global_cluster (fun f ->
               Query.Pair ("GlobalCluster", GlobalCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.global_cluster (fun f ->
               "global_cluster", GlobalCluster.to_json f)
         ])

  let of_json j =
    { global_cluster =
        Util.option_map (Json.lookup j "global_cluster") GlobalCluster.of_json
    }
end

module DeleteDBClusterSnapshotMessage = struct
  type t = { d_b_cluster_snapshot_identifier : String.t }

  let make ~d_b_cluster_snapshot_identifier () = { d_b_cluster_snapshot_identifier }

  let parse xml =
    Some
      { d_b_cluster_snapshot_identifier =
          Xml.required
            "DBClusterSnapshotIdentifier"
            (Util.option_bind (Xml.member "DBClusterSnapshotIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBClusterSnapshotIdentifier"
                , String.to_query v.d_b_cluster_snapshot_identifier ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "d_b_cluster_snapshot_identifier"
             , String.to_json v.d_b_cluster_snapshot_identifier )
         ])

  let of_json j =
    { d_b_cluster_snapshot_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_snapshot_identifier"))
    }
end

module RestoreDBClusterFromS3Message = struct
  type t =
    { availability_zones : AvailabilityZones.t
    ; backup_retention_period : Integer.t option
    ; character_set_name : String.t option
    ; database_name : String.t option
    ; d_b_cluster_identifier : String.t
    ; d_b_cluster_parameter_group_name : String.t option
    ; vpc_security_group_ids : VpcSecurityGroupIdList.t
    ; d_b_subnet_group_name : String.t option
    ; engine : String.t
    ; engine_version : String.t option
    ; port : Integer.t option
    ; master_username : String.t
    ; master_user_password : String.t
    ; option_group_name : String.t option
    ; preferred_backup_window : String.t option
    ; preferred_maintenance_window : String.t option
    ; tags : TagList.t
    ; storage_encrypted : Boolean.t option
    ; kms_key_id : String.t option
    ; enable_i_a_m_database_authentication : Boolean.t option
    ; source_engine : String.t
    ; source_engine_version : String.t
    ; s3_bucket_name : String.t
    ; s3_prefix : String.t option
    ; s3_ingestion_role_arn : String.t
    ; backtrack_window : Long.t option
    ; enable_cloudwatch_logs_exports : LogTypeList.t
    ; deletion_protection : Boolean.t option
    ; copy_tags_to_snapshot : Boolean.t option
    ; domain : String.t option
    ; domain_i_a_m_role_name : String.t option
    }

  let make
      ?(availability_zones = [])
      ?backup_retention_period
      ?character_set_name
      ?database_name
      ~d_b_cluster_identifier
      ?d_b_cluster_parameter_group_name
      ?(vpc_security_group_ids = [])
      ?d_b_subnet_group_name
      ~engine
      ?engine_version
      ?port
      ~master_username
      ~master_user_password
      ?option_group_name
      ?preferred_backup_window
      ?preferred_maintenance_window
      ?(tags = [])
      ?storage_encrypted
      ?kms_key_id
      ?enable_i_a_m_database_authentication
      ~source_engine
      ~source_engine_version
      ~s3_bucket_name
      ?s3_prefix
      ~s3_ingestion_role_arn
      ?backtrack_window
      ?(enable_cloudwatch_logs_exports = [])
      ?deletion_protection
      ?copy_tags_to_snapshot
      ?domain
      ?domain_i_a_m_role_name
      () =
    { availability_zones
    ; backup_retention_period
    ; character_set_name
    ; database_name
    ; d_b_cluster_identifier
    ; d_b_cluster_parameter_group_name
    ; vpc_security_group_ids
    ; d_b_subnet_group_name
    ; engine
    ; engine_version
    ; port
    ; master_username
    ; master_user_password
    ; option_group_name
    ; preferred_backup_window
    ; preferred_maintenance_window
    ; tags
    ; storage_encrypted
    ; kms_key_id
    ; enable_i_a_m_database_authentication
    ; source_engine
    ; source_engine_version
    ; s3_bucket_name
    ; s3_prefix
    ; s3_ingestion_role_arn
    ; backtrack_window
    ; enable_cloudwatch_logs_exports
    ; deletion_protection
    ; copy_tags_to_snapshot
    ; domain
    ; domain_i_a_m_role_name
    }

  let parse xml =
    Some
      { availability_zones =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      ; backup_retention_period =
          Util.option_bind (Xml.member "BackupRetentionPeriod" xml) Integer.parse
      ; character_set_name =
          Util.option_bind (Xml.member "CharacterSetName" xml) String.parse
      ; database_name = Util.option_bind (Xml.member "DatabaseName" xml) String.parse
      ; d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; d_b_cluster_parameter_group_name =
          Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupIds" xml)
               VpcSecurityGroupIdList.parse)
      ; d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; engine =
          Xml.required "Engine" (Util.option_bind (Xml.member "Engine" xml) String.parse)
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; master_username =
          Xml.required
            "MasterUsername"
            (Util.option_bind (Xml.member "MasterUsername" xml) String.parse)
      ; master_user_password =
          Xml.required
            "MasterUserPassword"
            (Util.option_bind (Xml.member "MasterUserPassword" xml) String.parse)
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; preferred_backup_window =
          Util.option_bind (Xml.member "PreferredBackupWindow" xml) String.parse
      ; preferred_maintenance_window =
          Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml) String.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; storage_encrypted =
          Util.option_bind (Xml.member "StorageEncrypted" xml) Boolean.parse
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; enable_i_a_m_database_authentication =
          Util.option_bind
            (Xml.member "EnableIAMDatabaseAuthentication" xml)
            Boolean.parse
      ; source_engine =
          Xml.required
            "SourceEngine"
            (Util.option_bind (Xml.member "SourceEngine" xml) String.parse)
      ; source_engine_version =
          Xml.required
            "SourceEngineVersion"
            (Util.option_bind (Xml.member "SourceEngineVersion" xml) String.parse)
      ; s3_bucket_name =
          Xml.required
            "S3BucketName"
            (Util.option_bind (Xml.member "S3BucketName" xml) String.parse)
      ; s3_prefix = Util.option_bind (Xml.member "S3Prefix" xml) String.parse
      ; s3_ingestion_role_arn =
          Xml.required
            "S3IngestionRoleArn"
            (Util.option_bind (Xml.member "S3IngestionRoleArn" xml) String.parse)
      ; backtrack_window = Util.option_bind (Xml.member "BacktrackWindow" xml) Long.parse
      ; enable_cloudwatch_logs_exports =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EnableCloudwatchLogsExports" xml)
               LogTypeList.parse)
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; domain = Util.option_bind (Xml.member "Domain" xml) String.parse
      ; domain_i_a_m_role_name =
          Util.option_bind (Xml.member "DomainIAMRoleName" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.domain_i_a_m_role_name (fun f ->
               Query.Pair ("DomainIAMRoleName", String.to_query f))
         ; Util.option_map v.domain (fun f -> Query.Pair ("Domain", String.to_query f))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "EnableCloudwatchLogsExports.member"
                , LogTypeList.to_query v.enable_cloudwatch_logs_exports ))
         ; Util.option_map v.backtrack_window (fun f ->
               Query.Pair ("BacktrackWindow", Long.to_query f))
         ; Some
             (Query.Pair ("S3IngestionRoleArn", String.to_query v.s3_ingestion_role_arn))
         ; Util.option_map v.s3_prefix (fun f ->
               Query.Pair ("S3Prefix", String.to_query f))
         ; Some (Query.Pair ("S3BucketName", String.to_query v.s3_bucket_name))
         ; Some
             (Query.Pair ("SourceEngineVersion", String.to_query v.source_engine_version))
         ; Some (Query.Pair ("SourceEngine", String.to_query v.source_engine))
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Util.option_map v.storage_encrypted (fun f ->
               Query.Pair ("StorageEncrypted", Boolean.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Util.option_map v.preferred_backup_window (fun f ->
               Query.Pair ("PreferredBackupWindow", String.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Some
             (Query.Pair ("MasterUserPassword", String.to_query v.master_user_password))
         ; Some (Query.Pair ("MasterUsername", String.to_query v.master_username))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Some (Query.Pair ("Engine", String.to_query v.engine))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               Query.Pair ("DBClusterParameterGroupName", String.to_query f))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ; Util.option_map v.database_name (fun f ->
               Query.Pair ("DatabaseName", String.to_query f))
         ; Util.option_map v.character_set_name (fun f ->
               Query.Pair ("CharacterSetName", String.to_query f))
         ; Util.option_map v.backup_retention_period (fun f ->
               Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
         ; Some
             (Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.domain_i_a_m_role_name (fun f ->
               "domain_i_a_m_role_name", String.to_json f)
         ; Util.option_map v.domain (fun f -> "domain", String.to_json f)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Some
             ( "enable_cloudwatch_logs_exports"
             , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
         ; Util.option_map v.backtrack_window (fun f ->
               "backtrack_window", Long.to_json f)
         ; Some ("s3_ingestion_role_arn", String.to_json v.s3_ingestion_role_arn)
         ; Util.option_map v.s3_prefix (fun f -> "s3_prefix", String.to_json f)
         ; Some ("s3_bucket_name", String.to_json v.s3_bucket_name)
         ; Some ("source_engine_version", String.to_json v.source_engine_version)
         ; Some ("source_engine", String.to_json v.source_engine)
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               "enable_i_a_m_database_authentication", Boolean.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Util.option_map v.storage_encrypted (fun f ->
               "storage_encrypted", Boolean.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               "preferred_maintenance_window", String.to_json f)
         ; Util.option_map v.preferred_backup_window (fun f ->
               "preferred_backup_window", String.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Some ("master_user_password", String.to_json v.master_user_password)
         ; Some ("master_username", String.to_json v.master_username)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Some ("engine", String.to_json v.engine)
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ; Some
             ( "vpc_security_group_ids"
             , VpcSecurityGroupIdList.to_json v.vpc_security_group_ids )
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               "d_b_cluster_parameter_group_name", String.to_json f)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ; Util.option_map v.database_name (fun f -> "database_name", String.to_json f)
         ; Util.option_map v.character_set_name (fun f ->
               "character_set_name", String.to_json f)
         ; Util.option_map v.backup_retention_period (fun f ->
               "backup_retention_period", Integer.to_json f)
         ; Some ("availability_zones", AvailabilityZones.to_json v.availability_zones)
         ])

  let of_json j =
    { availability_zones =
        AvailabilityZones.of_json
          (Util.of_option_exn (Json.lookup j "availability_zones"))
    ; backup_retention_period =
        Util.option_map (Json.lookup j "backup_retention_period") Integer.of_json
    ; character_set_name =
        Util.option_map (Json.lookup j "character_set_name") String.of_json
    ; database_name = Util.option_map (Json.lookup j "database_name") String.of_json
    ; d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; d_b_cluster_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name") String.of_json
    ; vpc_security_group_ids =
        VpcSecurityGroupIdList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; engine = String.of_json (Util.of_option_exn (Json.lookup j "engine"))
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; master_username =
        String.of_json (Util.of_option_exn (Json.lookup j "master_username"))
    ; master_user_password =
        String.of_json (Util.of_option_exn (Json.lookup j "master_user_password"))
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; preferred_backup_window =
        Util.option_map (Json.lookup j "preferred_backup_window") String.of_json
    ; preferred_maintenance_window =
        Util.option_map (Json.lookup j "preferred_maintenance_window") String.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; storage_encrypted =
        Util.option_map (Json.lookup j "storage_encrypted") Boolean.of_json
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; enable_i_a_m_database_authentication =
        Util.option_map
          (Json.lookup j "enable_i_a_m_database_authentication")
          Boolean.of_json
    ; source_engine = String.of_json (Util.of_option_exn (Json.lookup j "source_engine"))
    ; source_engine_version =
        String.of_json (Util.of_option_exn (Json.lookup j "source_engine_version"))
    ; s3_bucket_name =
        String.of_json (Util.of_option_exn (Json.lookup j "s3_bucket_name"))
    ; s3_prefix = Util.option_map (Json.lookup j "s3_prefix") String.of_json
    ; s3_ingestion_role_arn =
        String.of_json (Util.of_option_exn (Json.lookup j "s3_ingestion_role_arn"))
    ; backtrack_window = Util.option_map (Json.lookup j "backtrack_window") Long.of_json
    ; enable_cloudwatch_logs_exports =
        LogTypeList.of_json
          (Util.of_option_exn (Json.lookup j "enable_cloudwatch_logs_exports"))
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; domain = Util.option_map (Json.lookup j "domain") String.of_json
    ; domain_i_a_m_role_name =
        Util.option_map (Json.lookup j "domain_i_a_m_role_name") String.of_json
    }
end

module DBSubnetGroupDoesNotCoverEnoughAZs = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBClusterMessage = struct
  type t =
    { d_b_cluster_identifier : String.t
    ; skip_final_snapshot : Boolean.t option
    ; final_d_b_snapshot_identifier : String.t option
    }

  let make ~d_b_cluster_identifier ?skip_final_snapshot ?final_d_b_snapshot_identifier ()
      =
    { d_b_cluster_identifier; skip_final_snapshot; final_d_b_snapshot_identifier }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; skip_final_snapshot =
          Util.option_bind (Xml.member "SkipFinalSnapshot" xml) Boolean.parse
      ; final_d_b_snapshot_identifier =
          Util.option_bind (Xml.member "FinalDBSnapshotIdentifier" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.final_d_b_snapshot_identifier (fun f ->
               Query.Pair ("FinalDBSnapshotIdentifier", String.to_query f))
         ; Util.option_map v.skip_final_snapshot (fun f ->
               Query.Pair ("SkipFinalSnapshot", Boolean.to_query f))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.final_d_b_snapshot_identifier (fun f ->
               "final_d_b_snapshot_identifier", String.to_json f)
         ; Util.option_map v.skip_final_snapshot (fun f ->
               "skip_final_snapshot", Boolean.to_json f)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; skip_final_snapshot =
        Util.option_map (Json.lookup j "skip_final_snapshot") Boolean.of_json
    ; final_d_b_snapshot_identifier =
        Util.option_map (Json.lookup j "final_d_b_snapshot_identifier") String.of_json
    }
end

module DomainNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidExportTaskStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeEngineDefaultParametersResult = struct
  type t = { engine_defaults : EngineDefaults.t }

  let make ~engine_defaults () = { engine_defaults }

  let parse xml =
    Some
      { engine_defaults =
          Xml.required
            "EngineDefaults"
            (Util.option_bind (Xml.member "EngineDefaults" xml) EngineDefaults.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("EngineDefaults", EngineDefaults.to_query v.engine_defaults))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("engine_defaults", EngineDefaults.to_json v.engine_defaults) ])

  let of_json j =
    { engine_defaults =
        EngineDefaults.of_json (Util.of_option_exn (Json.lookup j "engine_defaults"))
    }
end

module InvalidDBSecurityGroupStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module PointInTimeRestoreNotEnabledFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeEngineDefaultClusterParametersMessage = struct
  type t =
    { d_b_parameter_group_family : String.t
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ~d_b_parameter_group_family ?(filters = []) ?max_records ?marker () =
    { d_b_parameter_group_family; filters; max_records; marker }

  let parse xml =
    Some
      { d_b_parameter_group_family =
          Xml.required
            "DBParameterGroupFamily"
            (Util.option_bind (Xml.member "DBParameterGroupFamily" xml) String.parse)
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Some
             (Query.Pair
                ("DBParameterGroupFamily", String.to_query v.d_b_parameter_group_family))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Some ("d_b_parameter_group_family", String.to_json v.d_b_parameter_group_family)
         ])

  let of_json j =
    { d_b_parameter_group_family =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_parameter_group_family"))
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module ImportInstallationMediaMessage = struct
  type t =
    { custom_availability_zone_id : String.t
    ; engine : String.t
    ; engine_version : String.t
    ; engine_installation_media_path : String.t
    ; o_s_installation_media_path : String.t
    }

  let make
      ~custom_availability_zone_id
      ~engine
      ~engine_version
      ~engine_installation_media_path
      ~o_s_installation_media_path
      () =
    { custom_availability_zone_id
    ; engine
    ; engine_version
    ; engine_installation_media_path
    ; o_s_installation_media_path
    }

  let parse xml =
    Some
      { custom_availability_zone_id =
          Xml.required
            "CustomAvailabilityZoneId"
            (Util.option_bind (Xml.member "CustomAvailabilityZoneId" xml) String.parse)
      ; engine =
          Xml.required "Engine" (Util.option_bind (Xml.member "Engine" xml) String.parse)
      ; engine_version =
          Xml.required
            "EngineVersion"
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse)
      ; engine_installation_media_path =
          Xml.required
            "EngineInstallationMediaPath"
            (Util.option_bind (Xml.member "EngineInstallationMediaPath" xml) String.parse)
      ; o_s_installation_media_path =
          Xml.required
            "OSInstallationMediaPath"
            (Util.option_bind (Xml.member "OSInstallationMediaPath" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("OSInstallationMediaPath", String.to_query v.o_s_installation_media_path))
         ; Some
             (Query.Pair
                ( "EngineInstallationMediaPath"
                , String.to_query v.engine_installation_media_path ))
         ; Some (Query.Pair ("EngineVersion", String.to_query v.engine_version))
         ; Some (Query.Pair ("Engine", String.to_query v.engine))
         ; Some
             (Query.Pair
                ("CustomAvailabilityZoneId", String.to_query v.custom_availability_zone_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ("o_s_installation_media_path", String.to_json v.o_s_installation_media_path)
         ; Some
             ( "engine_installation_media_path"
             , String.to_json v.engine_installation_media_path )
         ; Some ("engine_version", String.to_json v.engine_version)
         ; Some ("engine", String.to_json v.engine)
         ; Some
             ("custom_availability_zone_id", String.to_json v.custom_availability_zone_id)
         ])

  let of_json j =
    { custom_availability_zone_id =
        String.of_json (Util.of_option_exn (Json.lookup j "custom_availability_zone_id"))
    ; engine = String.of_json (Util.of_option_exn (Json.lookup j "engine"))
    ; engine_version =
        String.of_json (Util.of_option_exn (Json.lookup j "engine_version"))
    ; engine_installation_media_path =
        String.of_json
          (Util.of_option_exn (Json.lookup j "engine_installation_media_path"))
    ; o_s_installation_media_path =
        String.of_json (Util.of_option_exn (Json.lookup j "o_s_installation_media_path"))
    }
end

module DBClusterSnapshotNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBSubnetQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module StorageTypeNotSupportedFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module RemoveRoleFromDBInstanceMessage = struct
  type t =
    { d_b_instance_identifier : String.t
    ; role_arn : String.t
    ; feature_name : String.t
    }

  let make ~d_b_instance_identifier ~role_arn ~feature_name () =
    { d_b_instance_identifier; role_arn; feature_name }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; role_arn =
          Xml.required
            "RoleArn"
            (Util.option_bind (Xml.member "RoleArn" xml) String.parse)
      ; feature_name =
          Xml.required
            "FeatureName"
            (Util.option_bind (Xml.member "FeatureName" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("FeatureName", String.to_query v.feature_name))
         ; Some (Query.Pair ("RoleArn", String.to_query v.role_arn))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("feature_name", String.to_json v.feature_name)
         ; Some ("role_arn", String.to_json v.role_arn)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; role_arn = String.of_json (Util.of_option_exn (Json.lookup j "role_arn"))
    ; feature_name = String.of_json (Util.of_option_exn (Json.lookup j "feature_name"))
    }
end

module ReservedDBInstanceMessage = struct
  type t =
    { marker : String.t option
    ; reserved_d_b_instances : ReservedDBInstanceList.t
    }

  let make ?marker ?(reserved_d_b_instances = []) () = { marker; reserved_d_b_instances }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; reserved_d_b_instances =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ReservedDBInstances" xml)
               ReservedDBInstanceList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "ReservedDBInstances.member"
                , ReservedDBInstanceList.to_query v.reserved_d_b_instances ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "reserved_d_b_instances"
             , ReservedDBInstanceList.to_json v.reserved_d_b_instances )
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; reserved_d_b_instances =
        ReservedDBInstanceList.of_json
          (Util.of_option_exn (Json.lookup j "reserved_d_b_instances"))
    }
end

module DBParameterGroupDetails = struct
  type t =
    { parameters : ParametersList.t
    ; marker : String.t option
    }

  let make ?(parameters = []) ?marker () = { parameters; marker }

  let parse xml =
    Some
      { parameters =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Parameters" xml) ParametersList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("parameters", ParametersList.to_json v.parameters)
         ])

  let of_json j =
    { parameters =
        ParametersList.of_json (Util.of_option_exn (Json.lookup j "parameters"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module CustomAvailabilityZoneQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteGlobalClusterResult = struct
  type t = { global_cluster : GlobalCluster.t option }

  let make ?global_cluster () = { global_cluster }

  let parse xml =
    Some
      { global_cluster =
          Util.option_bind (Xml.member "GlobalCluster" xml) GlobalCluster.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.global_cluster (fun f ->
               Query.Pair ("GlobalCluster", GlobalCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.global_cluster (fun f ->
               "global_cluster", GlobalCluster.to_json f)
         ])

  let of_json j =
    { global_cluster =
        Util.option_map (Json.lookup j "global_cluster") GlobalCluster.of_json
    }
end

module ModifyCertificatesMessage = struct
  type t =
    { certificate_identifier : String.t option
    ; remove_customer_override : Boolean.t option
    }

  let make ?certificate_identifier ?remove_customer_override () =
    { certificate_identifier; remove_customer_override }

  let parse xml =
    Some
      { certificate_identifier =
          Util.option_bind (Xml.member "CertificateIdentifier" xml) String.parse
      ; remove_customer_override =
          Util.option_bind (Xml.member "RemoveCustomerOverride" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.remove_customer_override (fun f ->
               Query.Pair ("RemoveCustomerOverride", Boolean.to_query f))
         ; Util.option_map v.certificate_identifier (fun f ->
               Query.Pair ("CertificateIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.remove_customer_override (fun f ->
               "remove_customer_override", Boolean.to_json f)
         ; Util.option_map v.certificate_identifier (fun f ->
               "certificate_identifier", String.to_json f)
         ])

  let of_json j =
    { certificate_identifier =
        Util.option_map (Json.lookup j "certificate_identifier") String.of_json
    ; remove_customer_override =
        Util.option_map (Json.lookup j "remove_customer_override") Boolean.of_json
    }
end

module ApplyPendingMaintenanceActionResult = struct
  type t =
    { resource_pending_maintenance_actions : ResourcePendingMaintenanceActions.t option }

  let make ?resource_pending_maintenance_actions () =
    { resource_pending_maintenance_actions }

  let parse xml =
    Some
      { resource_pending_maintenance_actions =
          Util.option_bind
            (Xml.member "ResourcePendingMaintenanceActions" xml)
            ResourcePendingMaintenanceActions.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.resource_pending_maintenance_actions (fun f ->
               Query.Pair
                 ( "ResourcePendingMaintenanceActions"
                 , ResourcePendingMaintenanceActions.to_query f ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.resource_pending_maintenance_actions (fun f ->
               ( "resource_pending_maintenance_actions"
               , ResourcePendingMaintenanceActions.to_json f ))
         ])

  let of_json j =
    { resource_pending_maintenance_actions =
        Util.option_map
          (Json.lookup j "resource_pending_maintenance_actions")
          ResourcePendingMaintenanceActions.of_json
    }
end

module StopDBClusterMessage = struct
  type t = { d_b_cluster_identifier : String.t }

  let make ~d_b_cluster_identifier () = { d_b_cluster_identifier }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier) ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    }
end

module InsufficientAvailableIPsInSubnetFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module SNSNoAuthorizationFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBClusterSnapshotResult = struct
  type t = { d_b_cluster_snapshot : DBClusterSnapshot.t option }

  let make ?d_b_cluster_snapshot () = { d_b_cluster_snapshot }

  let parse xml =
    Some
      { d_b_cluster_snapshot =
          Util.option_bind (Xml.member "DBClusterSnapshot" xml) DBClusterSnapshot.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_snapshot (fun f ->
               Query.Pair ("DBClusterSnapshot", DBClusterSnapshot.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_snapshot (fun f ->
               "d_b_cluster_snapshot", DBClusterSnapshot.to_json f)
         ])

  let of_json j =
    { d_b_cluster_snapshot =
        Util.option_map (Json.lookup j "d_b_cluster_snapshot") DBClusterSnapshot.of_json
    }
end

module CreateOptionGroupMessage = struct
  type t =
    { option_group_name : String.t
    ; engine_name : String.t
    ; major_engine_version : String.t
    ; option_group_description : String.t
    ; tags : TagList.t
    }

  let make
      ~option_group_name
      ~engine_name
      ~major_engine_version
      ~option_group_description
      ?(tags = [])
      () =
    { option_group_name
    ; engine_name
    ; major_engine_version
    ; option_group_description
    ; tags
    }

  let parse xml =
    Some
      { option_group_name =
          Xml.required
            "OptionGroupName"
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse)
      ; engine_name =
          Xml.required
            "EngineName"
            (Util.option_bind (Xml.member "EngineName" xml) String.parse)
      ; major_engine_version =
          Xml.required
            "MajorEngineVersion"
            (Util.option_bind (Xml.member "MajorEngineVersion" xml) String.parse)
      ; option_group_description =
          Xml.required
            "OptionGroupDescription"
            (Util.option_bind (Xml.member "OptionGroupDescription" xml) String.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Query.Pair
                ("OptionGroupDescription", String.to_query v.option_group_description))
         ; Some
             (Query.Pair ("MajorEngineVersion", String.to_query v.major_engine_version))
         ; Some (Query.Pair ("EngineName", String.to_query v.engine_name))
         ; Some (Query.Pair ("OptionGroupName", String.to_query v.option_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some ("option_group_description", String.to_json v.option_group_description)
         ; Some ("major_engine_version", String.to_json v.major_engine_version)
         ; Some ("engine_name", String.to_json v.engine_name)
         ; Some ("option_group_name", String.to_json v.option_group_name)
         ])

  let of_json j =
    { option_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "option_group_name"))
    ; engine_name = String.of_json (Util.of_option_exn (Json.lookup j "engine_name"))
    ; major_engine_version =
        String.of_json (Util.of_option_exn (Json.lookup j "major_engine_version"))
    ; option_group_description =
        String.of_json (Util.of_option_exn (Json.lookup j "option_group_description"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module InvalidVPCNetworkStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBProxyResponse = struct
  type t = { d_b_proxy : DBProxy.t option }

  let make ?d_b_proxy () = { d_b_proxy }

  let parse xml =
    Some { d_b_proxy = Util.option_bind (Xml.member "DBProxy" xml) DBProxy.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_proxy (fun f ->
               Query.Pair ("DBProxy", DBProxy.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_proxy (fun f -> "d_b_proxy", DBProxy.to_json f) ])

  let of_json j =
    { d_b_proxy = Util.option_map (Json.lookup j "d_b_proxy") DBProxy.of_json }
end

module DescribeDBClusterSnapshotAttributesResult = struct
  type t =
    { d_b_cluster_snapshot_attributes_result : DBClusterSnapshotAttributesResult.t option
    }

  let make ?d_b_cluster_snapshot_attributes_result () =
    { d_b_cluster_snapshot_attributes_result }

  let parse xml =
    Some
      { d_b_cluster_snapshot_attributes_result =
          Util.option_bind
            (Xml.member "DBClusterSnapshotAttributesResult" xml)
            DBClusterSnapshotAttributesResult.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_snapshot_attributes_result (fun f ->
               Query.Pair
                 ( "DBClusterSnapshotAttributesResult"
                 , DBClusterSnapshotAttributesResult.to_query f ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_snapshot_attributes_result (fun f ->
               ( "d_b_cluster_snapshot_attributes_result"
               , DBClusterSnapshotAttributesResult.to_json f ))
         ])

  let of_json j =
    { d_b_cluster_snapshot_attributes_result =
        Util.option_map
          (Json.lookup j "d_b_cluster_snapshot_attributes_result")
          DBClusterSnapshotAttributesResult.of_json
    }
end

module PendingMaintenanceActionsMessage = struct
  type t =
    { pending_maintenance_actions : PendingMaintenanceActions.t
    ; marker : String.t option
    }

  let make ?(pending_maintenance_actions = []) ?marker () =
    { pending_maintenance_actions; marker }

  let parse xml =
    Some
      { pending_maintenance_actions =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "PendingMaintenanceActions" xml)
               PendingMaintenanceActions.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair
                ( "PendingMaintenanceActions.member"
                , PendingMaintenanceActions.to_query v.pending_maintenance_actions ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some
             ( "pending_maintenance_actions"
             , PendingMaintenanceActions.to_json v.pending_maintenance_actions )
         ])

  let of_json j =
    { pending_maintenance_actions =
        PendingMaintenanceActions.of_json
          (Util.of_option_exn (Json.lookup j "pending_maintenance_actions"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module ModifyDBSnapshotMessage = struct
  type t =
    { d_b_snapshot_identifier : String.t
    ; engine_version : String.t option
    ; option_group_name : String.t option
    }

  let make ~d_b_snapshot_identifier ?engine_version ?option_group_name () =
    { d_b_snapshot_identifier; engine_version; option_group_name }

  let parse xml =
    Some
      { d_b_snapshot_identifier =
          Xml.required
            "DBSnapshotIdentifier"
            (Util.option_bind (Xml.member "DBSnapshotIdentifier" xml) String.parse)
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Some
             (Query.Pair
                ("DBSnapshotIdentifier", String.to_query v.d_b_snapshot_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Some ("d_b_snapshot_identifier", String.to_json v.d_b_snapshot_identifier)
         ])

  let of_json j =
    { d_b_snapshot_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_snapshot_identifier"))
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    }
end

module CustomAvailabilityZoneNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeDBProxyTargetGroupsResponse = struct
  type t =
    { target_groups : TargetGroupList.t
    ; marker : String.t option
    }

  let make ?(target_groups = []) ?marker () = { target_groups; marker }

  let parse xml =
    Some
      { target_groups =
          Util.of_option
            []
            (Util.option_bind (Xml.member "TargetGroups" xml) TargetGroupList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair ("TargetGroups.member", TargetGroupList.to_query v.target_groups))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("target_groups", TargetGroupList.to_json v.target_groups)
         ])

  let of_json j =
    { target_groups =
        TargetGroupList.of_json (Util.of_option_exn (Json.lookup j "target_groups"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module RestoreDBClusterFromS3Result = struct
  type t = { d_b_cluster : DBCluster.t option }

  let make ?d_b_cluster () = { d_b_cluster }

  let parse xml =
    Some { d_b_cluster = Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f ->
               Query.Pair ("DBCluster", DBCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f -> "d_b_cluster", DBCluster.to_json f) ])

  let of_json j =
    { d_b_cluster = Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json }
end

module RegisterDBProxyTargetsResponse = struct
  type t = { d_b_proxy_targets : TargetList.t }

  let make ?(d_b_proxy_targets = []) () = { d_b_proxy_targets }

  let parse xml =
    Some
      { d_b_proxy_targets =
          Util.of_option
            []
            (Util.option_bind (Xml.member "DBProxyTargets" xml) TargetList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("DBProxyTargets.member", TargetList.to_query v.d_b_proxy_targets))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_proxy_targets", TargetList.to_json v.d_b_proxy_targets) ])

  let of_json j =
    { d_b_proxy_targets =
        TargetList.of_json (Util.of_option_exn (Json.lookup j "d_b_proxy_targets"))
    }
end

module AuthorizeDBSecurityGroupIngressMessage = struct
  type t =
    { d_b_security_group_name : String.t
    ; c_i_d_r_i_p : String.t option
    ; e_c2_security_group_name : String.t option
    ; e_c2_security_group_id : String.t option
    ; e_c2_security_group_owner_id : String.t option
    }

  let make
      ~d_b_security_group_name
      ?c_i_d_r_i_p
      ?e_c2_security_group_name
      ?e_c2_security_group_id
      ?e_c2_security_group_owner_id
      () =
    { d_b_security_group_name
    ; c_i_d_r_i_p
    ; e_c2_security_group_name
    ; e_c2_security_group_id
    ; e_c2_security_group_owner_id
    }

  let parse xml =
    Some
      { d_b_security_group_name =
          Xml.required
            "DBSecurityGroupName"
            (Util.option_bind (Xml.member "DBSecurityGroupName" xml) String.parse)
      ; c_i_d_r_i_p = Util.option_bind (Xml.member "CIDRIP" xml) String.parse
      ; e_c2_security_group_name =
          Util.option_bind (Xml.member "EC2SecurityGroupName" xml) String.parse
      ; e_c2_security_group_id =
          Util.option_bind (Xml.member "EC2SecurityGroupId" xml) String.parse
      ; e_c2_security_group_owner_id =
          Util.option_bind (Xml.member "EC2SecurityGroupOwnerId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.e_c2_security_group_owner_id (fun f ->
               Query.Pair ("EC2SecurityGroupOwnerId", String.to_query f))
         ; Util.option_map v.e_c2_security_group_id (fun f ->
               Query.Pair ("EC2SecurityGroupId", String.to_query f))
         ; Util.option_map v.e_c2_security_group_name (fun f ->
               Query.Pair ("EC2SecurityGroupName", String.to_query f))
         ; Util.option_map v.c_i_d_r_i_p (fun f ->
               Query.Pair ("CIDRIP", String.to_query f))
         ; Some
             (Query.Pair ("DBSecurityGroupName", String.to_query v.d_b_security_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.e_c2_security_group_owner_id (fun f ->
               "e_c2_security_group_owner_id", String.to_json f)
         ; Util.option_map v.e_c2_security_group_id (fun f ->
               "e_c2_security_group_id", String.to_json f)
         ; Util.option_map v.e_c2_security_group_name (fun f ->
               "e_c2_security_group_name", String.to_json f)
         ; Util.option_map v.c_i_d_r_i_p (fun f -> "c_i_d_r_i_p", String.to_json f)
         ; Some ("d_b_security_group_name", String.to_json v.d_b_security_group_name)
         ])

  let of_json j =
    { d_b_security_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_security_group_name"))
    ; c_i_d_r_i_p = Util.option_map (Json.lookup j "c_i_d_r_i_p") String.of_json
    ; e_c2_security_group_name =
        Util.option_map (Json.lookup j "e_c2_security_group_name") String.of_json
    ; e_c2_security_group_id =
        Util.option_map (Json.lookup j "e_c2_security_group_id") String.of_json
    ; e_c2_security_group_owner_id =
        Util.option_map (Json.lookup j "e_c2_security_group_owner_id") String.of_json
    }
end

module DBClusterEndpointQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBInstanceAutomatedBackupResult = struct
  type t = { d_b_instance_automated_backup : DBInstanceAutomatedBackup.t option }

  let make ?d_b_instance_automated_backup () = { d_b_instance_automated_backup }

  let parse xml =
    Some
      { d_b_instance_automated_backup =
          Util.option_bind
            (Xml.member "DBInstanceAutomatedBackup" xml)
            DBInstanceAutomatedBackup.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance_automated_backup (fun f ->
               Query.Pair
                 ("DBInstanceAutomatedBackup", DBInstanceAutomatedBackup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance_automated_backup (fun f ->
               "d_b_instance_automated_backup", DBInstanceAutomatedBackup.to_json f)
         ])

  let of_json j =
    { d_b_instance_automated_backup =
        Util.option_map
          (Json.lookup j "d_b_instance_automated_backup")
          DBInstanceAutomatedBackup.of_json
    }
end

module DBEngineVersionMessage = struct
  type t =
    { marker : String.t option
    ; d_b_engine_versions : DBEngineVersionList.t
    }

  let make ?marker ?(d_b_engine_versions = []) () = { marker; d_b_engine_versions }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; d_b_engine_versions =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBEngineVersions" xml)
               DBEngineVersionList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBEngineVersions.member"
                , DBEngineVersionList.to_query v.d_b_engine_versions ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_engine_versions", DBEngineVersionList.to_json v.d_b_engine_versions)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; d_b_engine_versions =
        DBEngineVersionList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_engine_versions"))
    }
end

module InvalidDBInstanceAutomatedBackupStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DownloadDBLogFilePortionDetails = struct
  type t =
    { log_file_data : String.t option
    ; marker : String.t option
    ; additional_data_pending : Boolean.t option
    }

  let make ?log_file_data ?marker ?additional_data_pending () =
    { log_file_data; marker; additional_data_pending }

  let parse xml =
    Some
      { log_file_data = Util.option_bind (Xml.member "LogFileData" xml) String.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; additional_data_pending =
          Util.option_bind (Xml.member "AdditionalDataPending" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.additional_data_pending (fun f ->
               Query.Pair ("AdditionalDataPending", Boolean.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.log_file_data (fun f ->
               Query.Pair ("LogFileData", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.additional_data_pending (fun f ->
               "additional_data_pending", Boolean.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.log_file_data (fun f -> "log_file_data", String.to_json f)
         ])

  let of_json j =
    { log_file_data = Util.option_map (Json.lookup j "log_file_data") String.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; additional_data_pending =
        Util.option_map (Json.lookup j "additional_data_pending") Boolean.of_json
    }
end

module DBUpgradeDependencyFailureFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module RestoreDBClusterToPointInTimeMessage = struct
  type t =
    { d_b_cluster_identifier : String.t
    ; restore_type : String.t option
    ; source_d_b_cluster_identifier : String.t
    ; restore_to_time : DateTime.t option
    ; use_latest_restorable_time : Boolean.t option
    ; port : Integer.t option
    ; d_b_subnet_group_name : String.t option
    ; option_group_name : String.t option
    ; vpc_security_group_ids : VpcSecurityGroupIdList.t
    ; tags : TagList.t
    ; kms_key_id : String.t option
    ; enable_i_a_m_database_authentication : Boolean.t option
    ; backtrack_window : Long.t option
    ; enable_cloudwatch_logs_exports : LogTypeList.t
    ; d_b_cluster_parameter_group_name : String.t option
    ; deletion_protection : Boolean.t option
    ; copy_tags_to_snapshot : Boolean.t option
    ; domain : String.t option
    ; domain_i_a_m_role_name : String.t option
    }

  let make
      ~d_b_cluster_identifier
      ?restore_type
      ~source_d_b_cluster_identifier
      ?restore_to_time
      ?use_latest_restorable_time
      ?port
      ?d_b_subnet_group_name
      ?option_group_name
      ?(vpc_security_group_ids = [])
      ?(tags = [])
      ?kms_key_id
      ?enable_i_a_m_database_authentication
      ?backtrack_window
      ?(enable_cloudwatch_logs_exports = [])
      ?d_b_cluster_parameter_group_name
      ?deletion_protection
      ?copy_tags_to_snapshot
      ?domain
      ?domain_i_a_m_role_name
      () =
    { d_b_cluster_identifier
    ; restore_type
    ; source_d_b_cluster_identifier
    ; restore_to_time
    ; use_latest_restorable_time
    ; port
    ; d_b_subnet_group_name
    ; option_group_name
    ; vpc_security_group_ids
    ; tags
    ; kms_key_id
    ; enable_i_a_m_database_authentication
    ; backtrack_window
    ; enable_cloudwatch_logs_exports
    ; d_b_cluster_parameter_group_name
    ; deletion_protection
    ; copy_tags_to_snapshot
    ; domain
    ; domain_i_a_m_role_name
    }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; restore_type = Util.option_bind (Xml.member "RestoreType" xml) String.parse
      ; source_d_b_cluster_identifier =
          Xml.required
            "SourceDBClusterIdentifier"
            (Util.option_bind (Xml.member "SourceDBClusterIdentifier" xml) String.parse)
      ; restore_to_time = Util.option_bind (Xml.member "RestoreToTime" xml) DateTime.parse
      ; use_latest_restorable_time =
          Util.option_bind (Xml.member "UseLatestRestorableTime" xml) Boolean.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupIds" xml)
               VpcSecurityGroupIdList.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; enable_i_a_m_database_authentication =
          Util.option_bind
            (Xml.member "EnableIAMDatabaseAuthentication" xml)
            Boolean.parse
      ; backtrack_window = Util.option_bind (Xml.member "BacktrackWindow" xml) Long.parse
      ; enable_cloudwatch_logs_exports =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EnableCloudwatchLogsExports" xml)
               LogTypeList.parse)
      ; d_b_cluster_parameter_group_name =
          Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; domain = Util.option_bind (Xml.member "Domain" xml) String.parse
      ; domain_i_a_m_role_name =
          Util.option_bind (Xml.member "DomainIAMRoleName" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.domain_i_a_m_role_name (fun f ->
               Query.Pair ("DomainIAMRoleName", String.to_query f))
         ; Util.option_map v.domain (fun f -> Query.Pair ("Domain", String.to_query f))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               Query.Pair ("DBClusterParameterGroupName", String.to_query f))
         ; Some
             (Query.Pair
                ( "EnableCloudwatchLogsExports.member"
                , LogTypeList.to_query v.enable_cloudwatch_logs_exports ))
         ; Util.option_map v.backtrack_window (fun f ->
               Query.Pair ("BacktrackWindow", Long.to_query f))
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.use_latest_restorable_time (fun f ->
               Query.Pair ("UseLatestRestorableTime", Boolean.to_query f))
         ; Util.option_map v.restore_to_time (fun f ->
               Query.Pair ("RestoreToTime", DateTime.to_query f))
         ; Some
             (Query.Pair
                ( "SourceDBClusterIdentifier"
                , String.to_query v.source_d_b_cluster_identifier ))
         ; Util.option_map v.restore_type (fun f ->
               Query.Pair ("RestoreType", String.to_query f))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.domain_i_a_m_role_name (fun f ->
               "domain_i_a_m_role_name", String.to_json f)
         ; Util.option_map v.domain (fun f -> "domain", String.to_json f)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               "d_b_cluster_parameter_group_name", String.to_json f)
         ; Some
             ( "enable_cloudwatch_logs_exports"
             , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
         ; Util.option_map v.backtrack_window (fun f ->
               "backtrack_window", Long.to_json f)
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               "enable_i_a_m_database_authentication", Boolean.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Some
             ( "vpc_security_group_ids"
             , VpcSecurityGroupIdList.to_json v.vpc_security_group_ids )
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.use_latest_restorable_time (fun f ->
               "use_latest_restorable_time", Boolean.to_json f)
         ; Util.option_map v.restore_to_time (fun f ->
               "restore_to_time", DateTime.to_json f)
         ; Some
             ( "source_d_b_cluster_identifier"
             , String.to_json v.source_d_b_cluster_identifier )
         ; Util.option_map v.restore_type (fun f -> "restore_type", String.to_json f)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; restore_type = Util.option_map (Json.lookup j "restore_type") String.of_json
    ; source_d_b_cluster_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "source_d_b_cluster_identifier"))
    ; restore_to_time = Util.option_map (Json.lookup j "restore_to_time") DateTime.of_json
    ; use_latest_restorable_time =
        Util.option_map (Json.lookup j "use_latest_restorable_time") Boolean.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; vpc_security_group_ids =
        VpcSecurityGroupIdList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; enable_i_a_m_database_authentication =
        Util.option_map
          (Json.lookup j "enable_i_a_m_database_authentication")
          Boolean.of_json
    ; backtrack_window = Util.option_map (Json.lookup j "backtrack_window") Long.of_json
    ; enable_cloudwatch_logs_exports =
        LogTypeList.of_json
          (Util.of_option_exn (Json.lookup j "enable_cloudwatch_logs_exports"))
    ; d_b_cluster_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name") String.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; domain = Util.option_map (Json.lookup j "domain") String.of_json
    ; domain_i_a_m_role_name =
        Util.option_map (Json.lookup j "domain_i_a_m_role_name") String.of_json
    }
end

module DBLogFileNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module EventCategoriesMessage = struct
  type t = { event_categories_map_list : EventCategoriesMapList.t }

  let make ?(event_categories_map_list = []) () = { event_categories_map_list }

  let parse xml =
    Some
      { event_categories_map_list =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EventCategoriesMapList" xml)
               EventCategoriesMapList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "EventCategoriesMapList.member"
                , EventCategoriesMapList.to_query v.event_categories_map_list ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "event_categories_map_list"
             , EventCategoriesMapList.to_json v.event_categories_map_list )
         ])

  let of_json j =
    { event_categories_map_list =
        EventCategoriesMapList.of_json
          (Util.of_option_exn (Json.lookup j "event_categories_map_list"))
    }
end

module DescribeDBSnapshotsMessage = struct
  type t =
    { d_b_instance_identifier : String.t option
    ; d_b_snapshot_identifier : String.t option
    ; snapshot_type : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    ; include_shared : Boolean.t option
    ; include_public : Boolean.t option
    ; dbi_resource_id : String.t option
    }

  let make
      ?d_b_instance_identifier
      ?d_b_snapshot_identifier
      ?snapshot_type
      ?(filters = [])
      ?max_records
      ?marker
      ?include_shared
      ?include_public
      ?dbi_resource_id
      () =
    { d_b_instance_identifier
    ; d_b_snapshot_identifier
    ; snapshot_type
    ; filters
    ; max_records
    ; marker
    ; include_shared
    ; include_public
    ; dbi_resource_id
    }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse
      ; d_b_snapshot_identifier =
          Util.option_bind (Xml.member "DBSnapshotIdentifier" xml) String.parse
      ; snapshot_type = Util.option_bind (Xml.member "SnapshotType" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; include_shared = Util.option_bind (Xml.member "IncludeShared" xml) Boolean.parse
      ; include_public = Util.option_bind (Xml.member "IncludePublic" xml) Boolean.parse
      ; dbi_resource_id = Util.option_bind (Xml.member "DbiResourceId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.dbi_resource_id (fun f ->
               Query.Pair ("DbiResourceId", String.to_query f))
         ; Util.option_map v.include_public (fun f ->
               Query.Pair ("IncludePublic", Boolean.to_query f))
         ; Util.option_map v.include_shared (fun f ->
               Query.Pair ("IncludeShared", Boolean.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.snapshot_type (fun f ->
               Query.Pair ("SnapshotType", String.to_query f))
         ; Util.option_map v.d_b_snapshot_identifier (fun f ->
               Query.Pair ("DBSnapshotIdentifier", String.to_query f))
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               Query.Pair ("DBInstanceIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.dbi_resource_id (fun f ->
               "dbi_resource_id", String.to_json f)
         ; Util.option_map v.include_public (fun f -> "include_public", Boolean.to_json f)
         ; Util.option_map v.include_shared (fun f -> "include_shared", Boolean.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.snapshot_type (fun f -> "snapshot_type", String.to_json f)
         ; Util.option_map v.d_b_snapshot_identifier (fun f ->
               "d_b_snapshot_identifier", String.to_json f)
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               "d_b_instance_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_instance_identifier =
        Util.option_map (Json.lookup j "d_b_instance_identifier") String.of_json
    ; d_b_snapshot_identifier =
        Util.option_map (Json.lookup j "d_b_snapshot_identifier") String.of_json
    ; snapshot_type = Util.option_map (Json.lookup j "snapshot_type") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; include_shared = Util.option_map (Json.lookup j "include_shared") Boolean.of_json
    ; include_public = Util.option_map (Json.lookup j "include_public") Boolean.of_json
    ; dbi_resource_id = Util.option_map (Json.lookup j "dbi_resource_id") String.of_json
    }
end

module InvalidExportSourceStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBClusterCapacityInfo = struct
  type t =
    { d_b_cluster_identifier : String.t option
    ; pending_capacity : Integer.t option
    ; current_capacity : Integer.t option
    ; seconds_before_timeout : Integer.t option
    ; timeout_action : String.t option
    }

  let make
      ?d_b_cluster_identifier
      ?pending_capacity
      ?current_capacity
      ?seconds_before_timeout
      ?timeout_action
      () =
    { d_b_cluster_identifier
    ; pending_capacity
    ; current_capacity
    ; seconds_before_timeout
    ; timeout_action
    }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse
      ; pending_capacity =
          Util.option_bind (Xml.member "PendingCapacity" xml) Integer.parse
      ; current_capacity =
          Util.option_bind (Xml.member "CurrentCapacity" xml) Integer.parse
      ; seconds_before_timeout =
          Util.option_bind (Xml.member "SecondsBeforeTimeout" xml) Integer.parse
      ; timeout_action = Util.option_bind (Xml.member "TimeoutAction" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.timeout_action (fun f ->
               Query.Pair ("TimeoutAction", String.to_query f))
         ; Util.option_map v.seconds_before_timeout (fun f ->
               Query.Pair ("SecondsBeforeTimeout", Integer.to_query f))
         ; Util.option_map v.current_capacity (fun f ->
               Query.Pair ("CurrentCapacity", Integer.to_query f))
         ; Util.option_map v.pending_capacity (fun f ->
               Query.Pair ("PendingCapacity", Integer.to_query f))
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               Query.Pair ("DBClusterIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.timeout_action (fun f -> "timeout_action", String.to_json f)
         ; Util.option_map v.seconds_before_timeout (fun f ->
               "seconds_before_timeout", Integer.to_json f)
         ; Util.option_map v.current_capacity (fun f ->
               "current_capacity", Integer.to_json f)
         ; Util.option_map v.pending_capacity (fun f ->
               "pending_capacity", Integer.to_json f)
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               "d_b_cluster_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_identifier") String.of_json
    ; pending_capacity =
        Util.option_map (Json.lookup j "pending_capacity") Integer.of_json
    ; current_capacity =
        Util.option_map (Json.lookup j "current_capacity") Integer.of_json
    ; seconds_before_timeout =
        Util.option_map (Json.lookup j "seconds_before_timeout") Integer.of_json
    ; timeout_action = Util.option_map (Json.lookup j "timeout_action") String.of_json
    }
end

module CopyDBClusterSnapshotResult = struct
  type t = { d_b_cluster_snapshot : DBClusterSnapshot.t option }

  let make ?d_b_cluster_snapshot () = { d_b_cluster_snapshot }

  let parse xml =
    Some
      { d_b_cluster_snapshot =
          Util.option_bind (Xml.member "DBClusterSnapshot" xml) DBClusterSnapshot.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_snapshot (fun f ->
               Query.Pair ("DBClusterSnapshot", DBClusterSnapshot.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_snapshot (fun f ->
               "d_b_cluster_snapshot", DBClusterSnapshot.to_json f)
         ])

  let of_json j =
    { d_b_cluster_snapshot =
        Util.option_map (Json.lookup j "d_b_cluster_snapshot") DBClusterSnapshot.of_json
    }
end

module CopyDBClusterSnapshotMessage = struct
  type t =
    { source_d_b_cluster_snapshot_identifier : String.t
    ; target_d_b_cluster_snapshot_identifier : String.t
    ; kms_key_id : String.t option
    ; pre_signed_url : String.t option
    ; copy_tags : Boolean.t option
    ; tags : TagList.t
    }

  let make
      ~source_d_b_cluster_snapshot_identifier
      ~target_d_b_cluster_snapshot_identifier
      ?kms_key_id
      ?pre_signed_url
      ?copy_tags
      ?(tags = [])
      () =
    { source_d_b_cluster_snapshot_identifier
    ; target_d_b_cluster_snapshot_identifier
    ; kms_key_id
    ; pre_signed_url
    ; copy_tags
    ; tags
    }

  let parse xml =
    Some
      { source_d_b_cluster_snapshot_identifier =
          Xml.required
            "SourceDBClusterSnapshotIdentifier"
            (Util.option_bind
               (Xml.member "SourceDBClusterSnapshotIdentifier" xml)
               String.parse)
      ; target_d_b_cluster_snapshot_identifier =
          Xml.required
            "TargetDBClusterSnapshotIdentifier"
            (Util.option_bind
               (Xml.member "TargetDBClusterSnapshotIdentifier" xml)
               String.parse)
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; pre_signed_url = Util.option_bind (Xml.member "PreSignedUrl" xml) String.parse
      ; copy_tags = Util.option_bind (Xml.member "CopyTags" xml) Boolean.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.copy_tags (fun f ->
               Query.Pair ("CopyTags", Boolean.to_query f))
         ; Util.option_map v.pre_signed_url (fun f ->
               Query.Pair ("PreSignedUrl", String.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Some
             (Query.Pair
                ( "TargetDBClusterSnapshotIdentifier"
                , String.to_query v.target_d_b_cluster_snapshot_identifier ))
         ; Some
             (Query.Pair
                ( "SourceDBClusterSnapshotIdentifier"
                , String.to_query v.source_d_b_cluster_snapshot_identifier ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.copy_tags (fun f -> "copy_tags", Boolean.to_json f)
         ; Util.option_map v.pre_signed_url (fun f -> "pre_signed_url", String.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Some
             ( "target_d_b_cluster_snapshot_identifier"
             , String.to_json v.target_d_b_cluster_snapshot_identifier )
         ; Some
             ( "source_d_b_cluster_snapshot_identifier"
             , String.to_json v.source_d_b_cluster_snapshot_identifier )
         ])

  let of_json j =
    { source_d_b_cluster_snapshot_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "source_d_b_cluster_snapshot_identifier"))
    ; target_d_b_cluster_snapshot_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "target_d_b_cluster_snapshot_identifier"))
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; pre_signed_url = Util.option_map (Json.lookup j "pre_signed_url") String.of_json
    ; copy_tags = Util.option_map (Json.lookup j "copy_tags") Boolean.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module DescribeDBClustersMessage = struct
  type t =
    { d_b_cluster_identifier : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    ; include_shared : Boolean.t option
    }

  let make ?d_b_cluster_identifier ?(filters = []) ?max_records ?marker ?include_shared ()
      =
    { d_b_cluster_identifier; filters; max_records; marker; include_shared }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; include_shared = Util.option_bind (Xml.member "IncludeShared" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.include_shared (fun f ->
               Query.Pair ("IncludeShared", Boolean.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               Query.Pair ("DBClusterIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.include_shared (fun f -> "include_shared", Boolean.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               "d_b_cluster_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_identifier") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; include_shared = Util.option_map (Json.lookup j "include_shared") Boolean.of_json
    }
end

module DBInstanceRoleQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBSubnetGroupQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBClusterMessage = struct
  type t =
    { availability_zones : AvailabilityZones.t
    ; backup_retention_period : Integer.t option
    ; character_set_name : String.t option
    ; database_name : String.t option
    ; d_b_cluster_identifier : String.t
    ; d_b_cluster_parameter_group_name : String.t option
    ; vpc_security_group_ids : VpcSecurityGroupIdList.t
    ; d_b_subnet_group_name : String.t option
    ; engine : String.t
    ; engine_version : String.t option
    ; port : Integer.t option
    ; master_username : String.t option
    ; master_user_password : String.t option
    ; option_group_name : String.t option
    ; preferred_backup_window : String.t option
    ; preferred_maintenance_window : String.t option
    ; replication_source_identifier : String.t option
    ; tags : TagList.t
    ; storage_encrypted : Boolean.t option
    ; kms_key_id : String.t option
    ; pre_signed_url : String.t option
    ; enable_i_a_m_database_authentication : Boolean.t option
    ; backtrack_window : Long.t option
    ; enable_cloudwatch_logs_exports : LogTypeList.t
    ; engine_mode : String.t option
    ; scaling_configuration : ScalingConfiguration.t option
    ; deletion_protection : Boolean.t option
    ; global_cluster_identifier : String.t option
    ; enable_http_endpoint : Boolean.t option
    ; copy_tags_to_snapshot : Boolean.t option
    ; domain : String.t option
    ; domain_i_a_m_role_name : String.t option
    ; enable_global_write_forwarding : Boolean.t option
    }

  let make
      ?(availability_zones = [])
      ?backup_retention_period
      ?character_set_name
      ?database_name
      ~d_b_cluster_identifier
      ?d_b_cluster_parameter_group_name
      ?(vpc_security_group_ids = [])
      ?d_b_subnet_group_name
      ~engine
      ?engine_version
      ?port
      ?master_username
      ?master_user_password
      ?option_group_name
      ?preferred_backup_window
      ?preferred_maintenance_window
      ?replication_source_identifier
      ?(tags = [])
      ?storage_encrypted
      ?kms_key_id
      ?pre_signed_url
      ?enable_i_a_m_database_authentication
      ?backtrack_window
      ?(enable_cloudwatch_logs_exports = [])
      ?engine_mode
      ?scaling_configuration
      ?deletion_protection
      ?global_cluster_identifier
      ?enable_http_endpoint
      ?copy_tags_to_snapshot
      ?domain
      ?domain_i_a_m_role_name
      ?enable_global_write_forwarding
      () =
    { availability_zones
    ; backup_retention_period
    ; character_set_name
    ; database_name
    ; d_b_cluster_identifier
    ; d_b_cluster_parameter_group_name
    ; vpc_security_group_ids
    ; d_b_subnet_group_name
    ; engine
    ; engine_version
    ; port
    ; master_username
    ; master_user_password
    ; option_group_name
    ; preferred_backup_window
    ; preferred_maintenance_window
    ; replication_source_identifier
    ; tags
    ; storage_encrypted
    ; kms_key_id
    ; pre_signed_url
    ; enable_i_a_m_database_authentication
    ; backtrack_window
    ; enable_cloudwatch_logs_exports
    ; engine_mode
    ; scaling_configuration
    ; deletion_protection
    ; global_cluster_identifier
    ; enable_http_endpoint
    ; copy_tags_to_snapshot
    ; domain
    ; domain_i_a_m_role_name
    ; enable_global_write_forwarding
    }

  let parse xml =
    Some
      { availability_zones =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      ; backup_retention_period =
          Util.option_bind (Xml.member "BackupRetentionPeriod" xml) Integer.parse
      ; character_set_name =
          Util.option_bind (Xml.member "CharacterSetName" xml) String.parse
      ; database_name = Util.option_bind (Xml.member "DatabaseName" xml) String.parse
      ; d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; d_b_cluster_parameter_group_name =
          Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupIds" xml)
               VpcSecurityGroupIdList.parse)
      ; d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; engine =
          Xml.required "Engine" (Util.option_bind (Xml.member "Engine" xml) String.parse)
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; master_username = Util.option_bind (Xml.member "MasterUsername" xml) String.parse
      ; master_user_password =
          Util.option_bind (Xml.member "MasterUserPassword" xml) String.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; preferred_backup_window =
          Util.option_bind (Xml.member "PreferredBackupWindow" xml) String.parse
      ; preferred_maintenance_window =
          Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml) String.parse
      ; replication_source_identifier =
          Util.option_bind (Xml.member "ReplicationSourceIdentifier" xml) String.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; storage_encrypted =
          Util.option_bind (Xml.member "StorageEncrypted" xml) Boolean.parse
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; pre_signed_url = Util.option_bind (Xml.member "PreSignedUrl" xml) String.parse
      ; enable_i_a_m_database_authentication =
          Util.option_bind
            (Xml.member "EnableIAMDatabaseAuthentication" xml)
            Boolean.parse
      ; backtrack_window = Util.option_bind (Xml.member "BacktrackWindow" xml) Long.parse
      ; enable_cloudwatch_logs_exports =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EnableCloudwatchLogsExports" xml)
               LogTypeList.parse)
      ; engine_mode = Util.option_bind (Xml.member "EngineMode" xml) String.parse
      ; scaling_configuration =
          Util.option_bind
            (Xml.member "ScalingConfiguration" xml)
            ScalingConfiguration.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; global_cluster_identifier =
          Util.option_bind (Xml.member "GlobalClusterIdentifier" xml) String.parse
      ; enable_http_endpoint =
          Util.option_bind (Xml.member "EnableHttpEndpoint" xml) Boolean.parse
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; domain = Util.option_bind (Xml.member "Domain" xml) String.parse
      ; domain_i_a_m_role_name =
          Util.option_bind (Xml.member "DomainIAMRoleName" xml) String.parse
      ; enable_global_write_forwarding =
          Util.option_bind (Xml.member "EnableGlobalWriteForwarding" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.enable_global_write_forwarding (fun f ->
               Query.Pair ("EnableGlobalWriteForwarding", Boolean.to_query f))
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               Query.Pair ("DomainIAMRoleName", String.to_query f))
         ; Util.option_map v.domain (fun f -> Query.Pair ("Domain", String.to_query f))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Util.option_map v.enable_http_endpoint (fun f ->
               Query.Pair ("EnableHttpEndpoint", Boolean.to_query f))
         ; Util.option_map v.global_cluster_identifier (fun f ->
               Query.Pair ("GlobalClusterIdentifier", String.to_query f))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.scaling_configuration (fun f ->
               Query.Pair ("ScalingConfiguration", ScalingConfiguration.to_query f))
         ; Util.option_map v.engine_mode (fun f ->
               Query.Pair ("EngineMode", String.to_query f))
         ; Some
             (Query.Pair
                ( "EnableCloudwatchLogsExports.member"
                , LogTypeList.to_query v.enable_cloudwatch_logs_exports ))
         ; Util.option_map v.backtrack_window (fun f ->
               Query.Pair ("BacktrackWindow", Long.to_query f))
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
         ; Util.option_map v.pre_signed_url (fun f ->
               Query.Pair ("PreSignedUrl", String.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Util.option_map v.storage_encrypted (fun f ->
               Query.Pair ("StorageEncrypted", Boolean.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.replication_source_identifier (fun f ->
               Query.Pair ("ReplicationSourceIdentifier", String.to_query f))
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Util.option_map v.preferred_backup_window (fun f ->
               Query.Pair ("PreferredBackupWindow", String.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.master_user_password (fun f ->
               Query.Pair ("MasterUserPassword", String.to_query f))
         ; Util.option_map v.master_username (fun f ->
               Query.Pair ("MasterUsername", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Some (Query.Pair ("Engine", String.to_query v.engine))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               Query.Pair ("DBClusterParameterGroupName", String.to_query f))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ; Util.option_map v.database_name (fun f ->
               Query.Pair ("DatabaseName", String.to_query f))
         ; Util.option_map v.character_set_name (fun f ->
               Query.Pair ("CharacterSetName", String.to_query f))
         ; Util.option_map v.backup_retention_period (fun f ->
               Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
         ; Some
             (Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.enable_global_write_forwarding (fun f ->
               "enable_global_write_forwarding", Boolean.to_json f)
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               "domain_i_a_m_role_name", String.to_json f)
         ; Util.option_map v.domain (fun f -> "domain", String.to_json f)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Util.option_map v.enable_http_endpoint (fun f ->
               "enable_http_endpoint", Boolean.to_json f)
         ; Util.option_map v.global_cluster_identifier (fun f ->
               "global_cluster_identifier", String.to_json f)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.scaling_configuration (fun f ->
               "scaling_configuration", ScalingConfiguration.to_json f)
         ; Util.option_map v.engine_mode (fun f -> "engine_mode", String.to_json f)
         ; Some
             ( "enable_cloudwatch_logs_exports"
             , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
         ; Util.option_map v.backtrack_window (fun f ->
               "backtrack_window", Long.to_json f)
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               "enable_i_a_m_database_authentication", Boolean.to_json f)
         ; Util.option_map v.pre_signed_url (fun f -> "pre_signed_url", String.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Util.option_map v.storage_encrypted (fun f ->
               "storage_encrypted", Boolean.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.replication_source_identifier (fun f ->
               "replication_source_identifier", String.to_json f)
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               "preferred_maintenance_window", String.to_json f)
         ; Util.option_map v.preferred_backup_window (fun f ->
               "preferred_backup_window", String.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.master_user_password (fun f ->
               "master_user_password", String.to_json f)
         ; Util.option_map v.master_username (fun f ->
               "master_username", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Some ("engine", String.to_json v.engine)
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ; Some
             ( "vpc_security_group_ids"
             , VpcSecurityGroupIdList.to_json v.vpc_security_group_ids )
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               "d_b_cluster_parameter_group_name", String.to_json f)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ; Util.option_map v.database_name (fun f -> "database_name", String.to_json f)
         ; Util.option_map v.character_set_name (fun f ->
               "character_set_name", String.to_json f)
         ; Util.option_map v.backup_retention_period (fun f ->
               "backup_retention_period", Integer.to_json f)
         ; Some ("availability_zones", AvailabilityZones.to_json v.availability_zones)
         ])

  let of_json j =
    { availability_zones =
        AvailabilityZones.of_json
          (Util.of_option_exn (Json.lookup j "availability_zones"))
    ; backup_retention_period =
        Util.option_map (Json.lookup j "backup_retention_period") Integer.of_json
    ; character_set_name =
        Util.option_map (Json.lookup j "character_set_name") String.of_json
    ; database_name = Util.option_map (Json.lookup j "database_name") String.of_json
    ; d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; d_b_cluster_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name") String.of_json
    ; vpc_security_group_ids =
        VpcSecurityGroupIdList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; engine = String.of_json (Util.of_option_exn (Json.lookup j "engine"))
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; master_username = Util.option_map (Json.lookup j "master_username") String.of_json
    ; master_user_password =
        Util.option_map (Json.lookup j "master_user_password") String.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; preferred_backup_window =
        Util.option_map (Json.lookup j "preferred_backup_window") String.of_json
    ; preferred_maintenance_window =
        Util.option_map (Json.lookup j "preferred_maintenance_window") String.of_json
    ; replication_source_identifier =
        Util.option_map (Json.lookup j "replication_source_identifier") String.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; storage_encrypted =
        Util.option_map (Json.lookup j "storage_encrypted") Boolean.of_json
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; pre_signed_url = Util.option_map (Json.lookup j "pre_signed_url") String.of_json
    ; enable_i_a_m_database_authentication =
        Util.option_map
          (Json.lookup j "enable_i_a_m_database_authentication")
          Boolean.of_json
    ; backtrack_window = Util.option_map (Json.lookup j "backtrack_window") Long.of_json
    ; enable_cloudwatch_logs_exports =
        LogTypeList.of_json
          (Util.of_option_exn (Json.lookup j "enable_cloudwatch_logs_exports"))
    ; engine_mode = Util.option_map (Json.lookup j "engine_mode") String.of_json
    ; scaling_configuration =
        Util.option_map
          (Json.lookup j "scaling_configuration")
          ScalingConfiguration.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; global_cluster_identifier =
        Util.option_map (Json.lookup j "global_cluster_identifier") String.of_json
    ; enable_http_endpoint =
        Util.option_map (Json.lookup j "enable_http_endpoint") Boolean.of_json
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; domain = Util.option_map (Json.lookup j "domain") String.of_json
    ; domain_i_a_m_role_name =
        Util.option_map (Json.lookup j "domain_i_a_m_role_name") String.of_json
    ; enable_global_write_forwarding =
        Util.option_map (Json.lookup j "enable_global_write_forwarding") Boolean.of_json
    }
end

module DBInstanceAutomatedBackupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyDBClusterParameterGroupMessage = struct
  type t =
    { d_b_cluster_parameter_group_name : String.t
    ; parameters : ParametersList.t
    }

  let make ~d_b_cluster_parameter_group_name ~parameters () =
    { d_b_cluster_parameter_group_name; parameters }

  let parse xml =
    Some
      { d_b_cluster_parameter_group_name =
          Xml.required
            "DBClusterParameterGroupName"
            (Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse)
      ; parameters =
          Xml.required
            "Parameters"
            (Util.option_bind (Xml.member "Parameters" xml) ParametersList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
         ; Some
             (Query.Pair
                ( "DBClusterParameterGroupName"
                , String.to_query v.d_b_cluster_parameter_group_name ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("parameters", ParametersList.to_json v.parameters)
         ; Some
             ( "d_b_cluster_parameter_group_name"
             , String.to_json v.d_b_cluster_parameter_group_name )
         ])

  let of_json j =
    { d_b_cluster_parameter_group_name =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_parameter_group_name"))
    ; parameters =
        ParametersList.of_json (Util.of_option_exn (Json.lookup j "parameters"))
    }
end

module ModifyOptionGroupResult = struct
  type t = { option_group : OptionGroup.t option }

  let make ?option_group () = { option_group }

  let parse xml =
    Some
      { option_group = Util.option_bind (Xml.member "OptionGroup" xml) OptionGroup.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.option_group (fun f ->
               Query.Pair ("OptionGroup", OptionGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.option_group (fun f -> "option_group", OptionGroup.to_json f)
         ])

  let of_json j =
    { option_group = Util.option_map (Json.lookup j "option_group") OptionGroup.of_json }
end

module InvalidDBSubnetGroupFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyDBInstanceMessage = struct
  type t =
    { d_b_instance_identifier : String.t
    ; allocated_storage : Integer.t option
    ; d_b_instance_class : String.t option
    ; d_b_subnet_group_name : String.t option
    ; d_b_security_groups : DBSecurityGroupNameList.t
    ; vpc_security_group_ids : VpcSecurityGroupIdList.t
    ; apply_immediately : Boolean.t option
    ; master_user_password : String.t option
    ; d_b_parameter_group_name : String.t option
    ; backup_retention_period : Integer.t option
    ; preferred_backup_window : String.t option
    ; preferred_maintenance_window : String.t option
    ; multi_a_z : Boolean.t option
    ; engine_version : String.t option
    ; allow_major_version_upgrade : Boolean.t option
    ; auto_minor_version_upgrade : Boolean.t option
    ; license_model : String.t option
    ; iops : Integer.t option
    ; option_group_name : String.t option
    ; new_d_b_instance_identifier : String.t option
    ; storage_type : String.t option
    ; tde_credential_arn : String.t option
    ; tde_credential_password : String.t option
    ; c_a_certificate_identifier : String.t option
    ; domain : String.t option
    ; copy_tags_to_snapshot : Boolean.t option
    ; monitoring_interval : Integer.t option
    ; d_b_port_number : Integer.t option
    ; publicly_accessible : Boolean.t option
    ; monitoring_role_arn : String.t option
    ; domain_i_a_m_role_name : String.t option
    ; promotion_tier : Integer.t option
    ; enable_i_a_m_database_authentication : Boolean.t option
    ; enable_performance_insights : Boolean.t option
    ; performance_insights_k_m_s_key_id : String.t option
    ; performance_insights_retention_period : Integer.t option
    ; cloudwatch_logs_export_configuration : CloudwatchLogsExportConfiguration.t option
    ; processor_features : ProcessorFeatureList.t
    ; use_default_processor_features : Boolean.t option
    ; deletion_protection : Boolean.t option
    ; max_allocated_storage : Integer.t option
    ; certificate_rotation_restart : Boolean.t option
    ; replica_mode : ReplicaMode.t option
    }

  let make
      ~d_b_instance_identifier
      ?allocated_storage
      ?d_b_instance_class
      ?d_b_subnet_group_name
      ?(d_b_security_groups = [])
      ?(vpc_security_group_ids = [])
      ?apply_immediately
      ?master_user_password
      ?d_b_parameter_group_name
      ?backup_retention_period
      ?preferred_backup_window
      ?preferred_maintenance_window
      ?multi_a_z
      ?engine_version
      ?allow_major_version_upgrade
      ?auto_minor_version_upgrade
      ?license_model
      ?iops
      ?option_group_name
      ?new_d_b_instance_identifier
      ?storage_type
      ?tde_credential_arn
      ?tde_credential_password
      ?c_a_certificate_identifier
      ?domain
      ?copy_tags_to_snapshot
      ?monitoring_interval
      ?d_b_port_number
      ?publicly_accessible
      ?monitoring_role_arn
      ?domain_i_a_m_role_name
      ?promotion_tier
      ?enable_i_a_m_database_authentication
      ?enable_performance_insights
      ?performance_insights_k_m_s_key_id
      ?performance_insights_retention_period
      ?cloudwatch_logs_export_configuration
      ?(processor_features = [])
      ?use_default_processor_features
      ?deletion_protection
      ?max_allocated_storage
      ?certificate_rotation_restart
      ?replica_mode
      () =
    { d_b_instance_identifier
    ; allocated_storage
    ; d_b_instance_class
    ; d_b_subnet_group_name
    ; d_b_security_groups
    ; vpc_security_group_ids
    ; apply_immediately
    ; master_user_password
    ; d_b_parameter_group_name
    ; backup_retention_period
    ; preferred_backup_window
    ; preferred_maintenance_window
    ; multi_a_z
    ; engine_version
    ; allow_major_version_upgrade
    ; auto_minor_version_upgrade
    ; license_model
    ; iops
    ; option_group_name
    ; new_d_b_instance_identifier
    ; storage_type
    ; tde_credential_arn
    ; tde_credential_password
    ; c_a_certificate_identifier
    ; domain
    ; copy_tags_to_snapshot
    ; monitoring_interval
    ; d_b_port_number
    ; publicly_accessible
    ; monitoring_role_arn
    ; domain_i_a_m_role_name
    ; promotion_tier
    ; enable_i_a_m_database_authentication
    ; enable_performance_insights
    ; performance_insights_k_m_s_key_id
    ; performance_insights_retention_period
    ; cloudwatch_logs_export_configuration
    ; processor_features
    ; use_default_processor_features
    ; deletion_protection
    ; max_allocated_storage
    ; certificate_rotation_restart
    ; replica_mode
    }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; allocated_storage =
          Util.option_bind (Xml.member "AllocatedStorage" xml) Integer.parse
      ; d_b_instance_class =
          Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse
      ; d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; d_b_security_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBSecurityGroups" xml)
               DBSecurityGroupNameList.parse)
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupIds" xml)
               VpcSecurityGroupIdList.parse)
      ; apply_immediately =
          Util.option_bind (Xml.member "ApplyImmediately" xml) Boolean.parse
      ; master_user_password =
          Util.option_bind (Xml.member "MasterUserPassword" xml) String.parse
      ; d_b_parameter_group_name =
          Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse
      ; backup_retention_period =
          Util.option_bind (Xml.member "BackupRetentionPeriod" xml) Integer.parse
      ; preferred_backup_window =
          Util.option_bind (Xml.member "PreferredBackupWindow" xml) String.parse
      ; preferred_maintenance_window =
          Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml) String.parse
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; allow_major_version_upgrade =
          Util.option_bind (Xml.member "AllowMajorVersionUpgrade" xml) Boolean.parse
      ; auto_minor_version_upgrade =
          Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
      ; license_model = Util.option_bind (Xml.member "LicenseModel" xml) String.parse
      ; iops = Util.option_bind (Xml.member "Iops" xml) Integer.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; new_d_b_instance_identifier =
          Util.option_bind (Xml.member "NewDBInstanceIdentifier" xml) String.parse
      ; storage_type = Util.option_bind (Xml.member "StorageType" xml) String.parse
      ; tde_credential_arn =
          Util.option_bind (Xml.member "TdeCredentialArn" xml) String.parse
      ; tde_credential_password =
          Util.option_bind (Xml.member "TdeCredentialPassword" xml) String.parse
      ; c_a_certificate_identifier =
          Util.option_bind (Xml.member "CACertificateIdentifier" xml) String.parse
      ; domain = Util.option_bind (Xml.member "Domain" xml) String.parse
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; monitoring_interval =
          Util.option_bind (Xml.member "MonitoringInterval" xml) Integer.parse
      ; d_b_port_number = Util.option_bind (Xml.member "DBPortNumber" xml) Integer.parse
      ; publicly_accessible =
          Util.option_bind (Xml.member "PubliclyAccessible" xml) Boolean.parse
      ; monitoring_role_arn =
          Util.option_bind (Xml.member "MonitoringRoleArn" xml) String.parse
      ; domain_i_a_m_role_name =
          Util.option_bind (Xml.member "DomainIAMRoleName" xml) String.parse
      ; promotion_tier = Util.option_bind (Xml.member "PromotionTier" xml) Integer.parse
      ; enable_i_a_m_database_authentication =
          Util.option_bind
            (Xml.member "EnableIAMDatabaseAuthentication" xml)
            Boolean.parse
      ; enable_performance_insights =
          Util.option_bind (Xml.member "EnablePerformanceInsights" xml) Boolean.parse
      ; performance_insights_k_m_s_key_id =
          Util.option_bind (Xml.member "PerformanceInsightsKMSKeyId" xml) String.parse
      ; performance_insights_retention_period =
          Util.option_bind
            (Xml.member "PerformanceInsightsRetentionPeriod" xml)
            Integer.parse
      ; cloudwatch_logs_export_configuration =
          Util.option_bind
            (Xml.member "CloudwatchLogsExportConfiguration" xml)
            CloudwatchLogsExportConfiguration.parse
      ; processor_features =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ProcessorFeatures" xml)
               ProcessorFeatureList.parse)
      ; use_default_processor_features =
          Util.option_bind (Xml.member "UseDefaultProcessorFeatures" xml) Boolean.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; max_allocated_storage =
          Util.option_bind (Xml.member "MaxAllocatedStorage" xml) Integer.parse
      ; certificate_rotation_restart =
          Util.option_bind (Xml.member "CertificateRotationRestart" xml) Boolean.parse
      ; replica_mode = Util.option_bind (Xml.member "ReplicaMode" xml) ReplicaMode.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.replica_mode (fun f ->
               Query.Pair ("ReplicaMode", ReplicaMode.to_query f))
         ; Util.option_map v.certificate_rotation_restart (fun f ->
               Query.Pair ("CertificateRotationRestart", Boolean.to_query f))
         ; Util.option_map v.max_allocated_storage (fun f ->
               Query.Pair ("MaxAllocatedStorage", Integer.to_query f))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.use_default_processor_features (fun f ->
               Query.Pair ("UseDefaultProcessorFeatures", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "ProcessorFeatures.member"
                , ProcessorFeatureList.to_query v.processor_features ))
         ; Util.option_map v.cloudwatch_logs_export_configuration (fun f ->
               Query.Pair
                 ( "CloudwatchLogsExportConfiguration"
                 , CloudwatchLogsExportConfiguration.to_query f ))
         ; Util.option_map v.performance_insights_retention_period (fun f ->
               Query.Pair ("PerformanceInsightsRetentionPeriod", Integer.to_query f))
         ; Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
               Query.Pair ("PerformanceInsightsKMSKeyId", String.to_query f))
         ; Util.option_map v.enable_performance_insights (fun f ->
               Query.Pair ("EnablePerformanceInsights", Boolean.to_query f))
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
         ; Util.option_map v.promotion_tier (fun f ->
               Query.Pair ("PromotionTier", Integer.to_query f))
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               Query.Pair ("DomainIAMRoleName", String.to_query f))
         ; Util.option_map v.monitoring_role_arn (fun f ->
               Query.Pair ("MonitoringRoleArn", String.to_query f))
         ; Util.option_map v.publicly_accessible (fun f ->
               Query.Pair ("PubliclyAccessible", Boolean.to_query f))
         ; Util.option_map v.d_b_port_number (fun f ->
               Query.Pair ("DBPortNumber", Integer.to_query f))
         ; Util.option_map v.monitoring_interval (fun f ->
               Query.Pair ("MonitoringInterval", Integer.to_query f))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Util.option_map v.domain (fun f -> Query.Pair ("Domain", String.to_query f))
         ; Util.option_map v.c_a_certificate_identifier (fun f ->
               Query.Pair ("CACertificateIdentifier", String.to_query f))
         ; Util.option_map v.tde_credential_password (fun f ->
               Query.Pair ("TdeCredentialPassword", String.to_query f))
         ; Util.option_map v.tde_credential_arn (fun f ->
               Query.Pair ("TdeCredentialArn", String.to_query f))
         ; Util.option_map v.storage_type (fun f ->
               Query.Pair ("StorageType", String.to_query f))
         ; Util.option_map v.new_d_b_instance_identifier (fun f ->
               Query.Pair ("NewDBInstanceIdentifier", String.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.iops (fun f -> Query.Pair ("Iops", Integer.to_query f))
         ; Util.option_map v.license_model (fun f ->
               Query.Pair ("LicenseModel", String.to_query f))
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Util.option_map v.allow_major_version_upgrade (fun f ->
               Query.Pair ("AllowMajorVersionUpgrade", Boolean.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Util.option_map v.preferred_backup_window (fun f ->
               Query.Pair ("PreferredBackupWindow", String.to_query f))
         ; Util.option_map v.backup_retention_period (fun f ->
               Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               Query.Pair ("DBParameterGroupName", String.to_query f))
         ; Util.option_map v.master_user_password (fun f ->
               Query.Pair ("MasterUserPassword", String.to_query f))
         ; Util.option_map v.apply_immediately (fun f ->
               Query.Pair ("ApplyImmediately", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
         ; Some
             (Query.Pair
                ( "DBSecurityGroups.member"
                , DBSecurityGroupNameList.to_query v.d_b_security_groups ))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ; Util.option_map v.d_b_instance_class (fun f ->
               Query.Pair ("DBInstanceClass", String.to_query f))
         ; Util.option_map v.allocated_storage (fun f ->
               Query.Pair ("AllocatedStorage", Integer.to_query f))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.replica_mode (fun f -> "replica_mode", ReplicaMode.to_json f)
         ; Util.option_map v.certificate_rotation_restart (fun f ->
               "certificate_rotation_restart", Boolean.to_json f)
         ; Util.option_map v.max_allocated_storage (fun f ->
               "max_allocated_storage", Integer.to_json f)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.use_default_processor_features (fun f ->
               "use_default_processor_features", Boolean.to_json f)
         ; Some ("processor_features", ProcessorFeatureList.to_json v.processor_features)
         ; Util.option_map v.cloudwatch_logs_export_configuration (fun f ->
               ( "cloudwatch_logs_export_configuration"
               , CloudwatchLogsExportConfiguration.to_json f ))
         ; Util.option_map v.performance_insights_retention_period (fun f ->
               "performance_insights_retention_period", Integer.to_json f)
         ; Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
               "performance_insights_k_m_s_key_id", String.to_json f)
         ; Util.option_map v.enable_performance_insights (fun f ->
               "enable_performance_insights", Boolean.to_json f)
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               "enable_i_a_m_database_authentication", Boolean.to_json f)
         ; Util.option_map v.promotion_tier (fun f -> "promotion_tier", Integer.to_json f)
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               "domain_i_a_m_role_name", String.to_json f)
         ; Util.option_map v.monitoring_role_arn (fun f ->
               "monitoring_role_arn", String.to_json f)
         ; Util.option_map v.publicly_accessible (fun f ->
               "publicly_accessible", Boolean.to_json f)
         ; Util.option_map v.d_b_port_number (fun f ->
               "d_b_port_number", Integer.to_json f)
         ; Util.option_map v.monitoring_interval (fun f ->
               "monitoring_interval", Integer.to_json f)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Util.option_map v.domain (fun f -> "domain", String.to_json f)
         ; Util.option_map v.c_a_certificate_identifier (fun f ->
               "c_a_certificate_identifier", String.to_json f)
         ; Util.option_map v.tde_credential_password (fun f ->
               "tde_credential_password", String.to_json f)
         ; Util.option_map v.tde_credential_arn (fun f ->
               "tde_credential_arn", String.to_json f)
         ; Util.option_map v.storage_type (fun f -> "storage_type", String.to_json f)
         ; Util.option_map v.new_d_b_instance_identifier (fun f ->
               "new_d_b_instance_identifier", String.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
         ; Util.option_map v.license_model (fun f -> "license_model", String.to_json f)
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               "auto_minor_version_upgrade", Boolean.to_json f)
         ; Util.option_map v.allow_major_version_upgrade (fun f ->
               "allow_major_version_upgrade", Boolean.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               "preferred_maintenance_window", String.to_json f)
         ; Util.option_map v.preferred_backup_window (fun f ->
               "preferred_backup_window", String.to_json f)
         ; Util.option_map v.backup_retention_period (fun f ->
               "backup_retention_period", Integer.to_json f)
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               "d_b_parameter_group_name", String.to_json f)
         ; Util.option_map v.master_user_password (fun f ->
               "master_user_password", String.to_json f)
         ; Util.option_map v.apply_immediately (fun f ->
               "apply_immediately", Boolean.to_json f)
         ; Some
             ( "vpc_security_group_ids"
             , VpcSecurityGroupIdList.to_json v.vpc_security_group_ids )
         ; Some
             ("d_b_security_groups", DBSecurityGroupNameList.to_json v.d_b_security_groups)
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ; Util.option_map v.d_b_instance_class (fun f ->
               "d_b_instance_class", String.to_json f)
         ; Util.option_map v.allocated_storage (fun f ->
               "allocated_storage", Integer.to_json f)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; allocated_storage =
        Util.option_map (Json.lookup j "allocated_storage") Integer.of_json
    ; d_b_instance_class =
        Util.option_map (Json.lookup j "d_b_instance_class") String.of_json
    ; d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; d_b_security_groups =
        DBSecurityGroupNameList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_security_groups"))
    ; vpc_security_group_ids =
        VpcSecurityGroupIdList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; apply_immediately =
        Util.option_map (Json.lookup j "apply_immediately") Boolean.of_json
    ; master_user_password =
        Util.option_map (Json.lookup j "master_user_password") String.of_json
    ; d_b_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_parameter_group_name") String.of_json
    ; backup_retention_period =
        Util.option_map (Json.lookup j "backup_retention_period") Integer.of_json
    ; preferred_backup_window =
        Util.option_map (Json.lookup j "preferred_backup_window") String.of_json
    ; preferred_maintenance_window =
        Util.option_map (Json.lookup j "preferred_maintenance_window") String.of_json
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; allow_major_version_upgrade =
        Util.option_map (Json.lookup j "allow_major_version_upgrade") Boolean.of_json
    ; auto_minor_version_upgrade =
        Util.option_map (Json.lookup j "auto_minor_version_upgrade") Boolean.of_json
    ; license_model = Util.option_map (Json.lookup j "license_model") String.of_json
    ; iops = Util.option_map (Json.lookup j "iops") Integer.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; new_d_b_instance_identifier =
        Util.option_map (Json.lookup j "new_d_b_instance_identifier") String.of_json
    ; storage_type = Util.option_map (Json.lookup j "storage_type") String.of_json
    ; tde_credential_arn =
        Util.option_map (Json.lookup j "tde_credential_arn") String.of_json
    ; tde_credential_password =
        Util.option_map (Json.lookup j "tde_credential_password") String.of_json
    ; c_a_certificate_identifier =
        Util.option_map (Json.lookup j "c_a_certificate_identifier") String.of_json
    ; domain = Util.option_map (Json.lookup j "domain") String.of_json
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; monitoring_interval =
        Util.option_map (Json.lookup j "monitoring_interval") Integer.of_json
    ; d_b_port_number = Util.option_map (Json.lookup j "d_b_port_number") Integer.of_json
    ; publicly_accessible =
        Util.option_map (Json.lookup j "publicly_accessible") Boolean.of_json
    ; monitoring_role_arn =
        Util.option_map (Json.lookup j "monitoring_role_arn") String.of_json
    ; domain_i_a_m_role_name =
        Util.option_map (Json.lookup j "domain_i_a_m_role_name") String.of_json
    ; promotion_tier = Util.option_map (Json.lookup j "promotion_tier") Integer.of_json
    ; enable_i_a_m_database_authentication =
        Util.option_map
          (Json.lookup j "enable_i_a_m_database_authentication")
          Boolean.of_json
    ; enable_performance_insights =
        Util.option_map (Json.lookup j "enable_performance_insights") Boolean.of_json
    ; performance_insights_k_m_s_key_id =
        Util.option_map (Json.lookup j "performance_insights_k_m_s_key_id") String.of_json
    ; performance_insights_retention_period =
        Util.option_map
          (Json.lookup j "performance_insights_retention_period")
          Integer.of_json
    ; cloudwatch_logs_export_configuration =
        Util.option_map
          (Json.lookup j "cloudwatch_logs_export_configuration")
          CloudwatchLogsExportConfiguration.of_json
    ; processor_features =
        ProcessorFeatureList.of_json
          (Util.of_option_exn (Json.lookup j "processor_features"))
    ; use_default_processor_features =
        Util.option_map (Json.lookup j "use_default_processor_features") Boolean.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; max_allocated_storage =
        Util.option_map (Json.lookup j "max_allocated_storage") Integer.of_json
    ; certificate_rotation_restart =
        Util.option_map (Json.lookup j "certificate_rotation_restart") Boolean.of_json
    ; replica_mode = Util.option_map (Json.lookup j "replica_mode") ReplicaMode.of_json
    }
end

module DescribeReservedDBInstancesOfferingsMessage = struct
  type t =
    { reserved_d_b_instances_offering_id : String.t option
    ; d_b_instance_class : String.t option
    ; duration : String.t option
    ; product_description : String.t option
    ; offering_type : String.t option
    ; multi_a_z : Boolean.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ?reserved_d_b_instances_offering_id
      ?d_b_instance_class
      ?duration
      ?product_description
      ?offering_type
      ?multi_a_z
      ?(filters = [])
      ?max_records
      ?marker
      () =
    { reserved_d_b_instances_offering_id
    ; d_b_instance_class
    ; duration
    ; product_description
    ; offering_type
    ; multi_a_z
    ; filters
    ; max_records
    ; marker
    }

  let parse xml =
    Some
      { reserved_d_b_instances_offering_id =
          Util.option_bind (Xml.member "ReservedDBInstancesOfferingId" xml) String.parse
      ; d_b_instance_class =
          Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse
      ; duration = Util.option_bind (Xml.member "Duration" xml) String.parse
      ; product_description =
          Util.option_bind (Xml.member "ProductDescription" xml) String.parse
      ; offering_type = Util.option_bind (Xml.member "OfferingType" xml) String.parse
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Util.option_map v.offering_type (fun f ->
               Query.Pair ("OfferingType", String.to_query f))
         ; Util.option_map v.product_description (fun f ->
               Query.Pair ("ProductDescription", String.to_query f))
         ; Util.option_map v.duration (fun f ->
               Query.Pair ("Duration", String.to_query f))
         ; Util.option_map v.d_b_instance_class (fun f ->
               Query.Pair ("DBInstanceClass", String.to_query f))
         ; Util.option_map v.reserved_d_b_instances_offering_id (fun f ->
               Query.Pair ("ReservedDBInstancesOfferingId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Util.option_map v.offering_type (fun f -> "offering_type", String.to_json f)
         ; Util.option_map v.product_description (fun f ->
               "product_description", String.to_json f)
         ; Util.option_map v.duration (fun f -> "duration", String.to_json f)
         ; Util.option_map v.d_b_instance_class (fun f ->
               "d_b_instance_class", String.to_json f)
         ; Util.option_map v.reserved_d_b_instances_offering_id (fun f ->
               "reserved_d_b_instances_offering_id", String.to_json f)
         ])

  let of_json j =
    { reserved_d_b_instances_offering_id =
        Util.option_map
          (Json.lookup j "reserved_d_b_instances_offering_id")
          String.of_json
    ; d_b_instance_class =
        Util.option_map (Json.lookup j "d_b_instance_class") String.of_json
    ; duration = Util.option_map (Json.lookup j "duration") String.of_json
    ; product_description =
        Util.option_map (Json.lookup j "product_description") String.of_json
    ; offering_type = Util.option_map (Json.lookup j "offering_type") String.of_json
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module OptionGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ResetDBClusterParameterGroupMessage = struct
  type t =
    { d_b_cluster_parameter_group_name : String.t
    ; reset_all_parameters : Boolean.t option
    ; parameters : ParametersList.t
    }

  let make ~d_b_cluster_parameter_group_name ?reset_all_parameters ?(parameters = []) () =
    { d_b_cluster_parameter_group_name; reset_all_parameters; parameters }

  let parse xml =
    Some
      { d_b_cluster_parameter_group_name =
          Xml.required
            "DBClusterParameterGroupName"
            (Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse)
      ; reset_all_parameters =
          Util.option_bind (Xml.member "ResetAllParameters" xml) Boolean.parse
      ; parameters =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Parameters" xml) ParametersList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
         ; Util.option_map v.reset_all_parameters (fun f ->
               Query.Pair ("ResetAllParameters", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "DBClusterParameterGroupName"
                , String.to_query v.d_b_cluster_parameter_group_name ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("parameters", ParametersList.to_json v.parameters)
         ; Util.option_map v.reset_all_parameters (fun f ->
               "reset_all_parameters", Boolean.to_json f)
         ; Some
             ( "d_b_cluster_parameter_group_name"
             , String.to_json v.d_b_cluster_parameter_group_name )
         ])

  let of_json j =
    { d_b_cluster_parameter_group_name =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_parameter_group_name"))
    ; reset_all_parameters =
        Util.option_map (Json.lookup j "reset_all_parameters") Boolean.of_json
    ; parameters =
        ParametersList.of_json (Util.of_option_exn (Json.lookup j "parameters"))
    }
end

module InvalidDBClusterEndpointStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module RegisterDBProxyTargetsRequest = struct
  type t =
    { d_b_proxy_name : String.t
    ; target_group_name : String.t option
    ; d_b_instance_identifiers : StringList.t
    ; d_b_cluster_identifiers : StringList.t
    }

  let make
      ~d_b_proxy_name
      ?target_group_name
      ?(d_b_instance_identifiers = [])
      ?(d_b_cluster_identifiers = [])
      () =
    { d_b_proxy_name
    ; target_group_name
    ; d_b_instance_identifiers
    ; d_b_cluster_identifiers
    }

  let parse xml =
    Some
      { d_b_proxy_name =
          Xml.required
            "DBProxyName"
            (Util.option_bind (Xml.member "DBProxyName" xml) String.parse)
      ; target_group_name =
          Util.option_bind (Xml.member "TargetGroupName" xml) String.parse
      ; d_b_instance_identifiers =
          Util.of_option
            []
            (Util.option_bind (Xml.member "DBInstanceIdentifiers" xml) StringList.parse)
      ; d_b_cluster_identifiers =
          Util.of_option
            []
            (Util.option_bind (Xml.member "DBClusterIdentifiers" xml) StringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBClusterIdentifiers.member"
                , StringList.to_query v.d_b_cluster_identifiers ))
         ; Some
             (Query.Pair
                ( "DBInstanceIdentifiers.member"
                , StringList.to_query v.d_b_instance_identifiers ))
         ; Util.option_map v.target_group_name (fun f ->
               Query.Pair ("TargetGroupName", String.to_query f))
         ; Some (Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_cluster_identifiers", StringList.to_json v.d_b_cluster_identifiers)
         ; Some ("d_b_instance_identifiers", StringList.to_json v.d_b_instance_identifiers)
         ; Util.option_map v.target_group_name (fun f ->
               "target_group_name", String.to_json f)
         ; Some ("d_b_proxy_name", String.to_json v.d_b_proxy_name)
         ])

  let of_json j =
    { d_b_proxy_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_proxy_name"))
    ; target_group_name =
        Util.option_map (Json.lookup j "target_group_name") String.of_json
    ; d_b_instance_identifiers =
        StringList.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifiers"))
    ; d_b_cluster_identifiers =
        StringList.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifiers"))
    }
end

module DBParameterGroupQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyDBInstanceResult = struct
  type t = { d_b_instance : DBInstance.t option }

  let make ?d_b_instance () = { d_b_instance }

  let parse xml =
    Some
      { d_b_instance = Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f ->
               Query.Pair ("DBInstance", DBInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f -> "d_b_instance", DBInstance.to_json f)
         ])

  let of_json j =
    { d_b_instance = Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json }
end

module DescribeEngineDefaultClusterParametersResult = struct
  type t = { engine_defaults : EngineDefaults.t option }

  let make ?engine_defaults () = { engine_defaults }

  let parse xml =
    Some
      { engine_defaults =
          Util.option_bind (Xml.member "EngineDefaults" xml) EngineDefaults.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.engine_defaults (fun f ->
               Query.Pair ("EngineDefaults", EngineDefaults.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.engine_defaults (fun f ->
               "engine_defaults", EngineDefaults.to_json f)
         ])

  let of_json j =
    { engine_defaults =
        Util.option_map (Json.lookup j "engine_defaults") EngineDefaults.of_json
    }
end

module CreateDBSnapshotMessage = struct
  type t =
    { d_b_snapshot_identifier : String.t
    ; d_b_instance_identifier : String.t
    ; tags : TagList.t
    }

  let make ~d_b_snapshot_identifier ~d_b_instance_identifier ?(tags = []) () =
    { d_b_snapshot_identifier; d_b_instance_identifier; tags }

  let parse xml =
    Some
      { d_b_snapshot_identifier =
          Xml.required
            "DBSnapshotIdentifier"
            (Util.option_bind (Xml.member "DBSnapshotIdentifier" xml) String.parse)
      ; d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ; Some
             (Query.Pair
                ("DBSnapshotIdentifier", String.to_query v.d_b_snapshot_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ; Some ("d_b_snapshot_identifier", String.to_json v.d_b_snapshot_identifier)
         ])

  let of_json j =
    { d_b_snapshot_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_snapshot_identifier"))
    ; d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module DeregisterDBProxyTargetsRequest = struct
  type t =
    { d_b_proxy_name : String.t
    ; target_group_name : String.t option
    ; d_b_instance_identifiers : StringList.t
    ; d_b_cluster_identifiers : StringList.t
    }

  let make
      ~d_b_proxy_name
      ?target_group_name
      ?(d_b_instance_identifiers = [])
      ?(d_b_cluster_identifiers = [])
      () =
    { d_b_proxy_name
    ; target_group_name
    ; d_b_instance_identifiers
    ; d_b_cluster_identifiers
    }

  let parse xml =
    Some
      { d_b_proxy_name =
          Xml.required
            "DBProxyName"
            (Util.option_bind (Xml.member "DBProxyName" xml) String.parse)
      ; target_group_name =
          Util.option_bind (Xml.member "TargetGroupName" xml) String.parse
      ; d_b_instance_identifiers =
          Util.of_option
            []
            (Util.option_bind (Xml.member "DBInstanceIdentifiers" xml) StringList.parse)
      ; d_b_cluster_identifiers =
          Util.of_option
            []
            (Util.option_bind (Xml.member "DBClusterIdentifiers" xml) StringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBClusterIdentifiers.member"
                , StringList.to_query v.d_b_cluster_identifiers ))
         ; Some
             (Query.Pair
                ( "DBInstanceIdentifiers.member"
                , StringList.to_query v.d_b_instance_identifiers ))
         ; Util.option_map v.target_group_name (fun f ->
               Query.Pair ("TargetGroupName", String.to_query f))
         ; Some (Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_cluster_identifiers", StringList.to_json v.d_b_cluster_identifiers)
         ; Some ("d_b_instance_identifiers", StringList.to_json v.d_b_instance_identifiers)
         ; Util.option_map v.target_group_name (fun f ->
               "target_group_name", String.to_json f)
         ; Some ("d_b_proxy_name", String.to_json v.d_b_proxy_name)
         ])

  let of_json j =
    { d_b_proxy_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_proxy_name"))
    ; target_group_name =
        Util.option_map (Json.lookup j "target_group_name") String.of_json
    ; d_b_instance_identifiers =
        StringList.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifiers"))
    ; d_b_cluster_identifiers =
        StringList.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifiers"))
    }
end

module CopyDBParameterGroupMessage = struct
  type t =
    { source_d_b_parameter_group_identifier : String.t
    ; target_d_b_parameter_group_identifier : String.t
    ; target_d_b_parameter_group_description : String.t
    ; tags : TagList.t
    }

  let make
      ~source_d_b_parameter_group_identifier
      ~target_d_b_parameter_group_identifier
      ~target_d_b_parameter_group_description
      ?(tags = [])
      () =
    { source_d_b_parameter_group_identifier
    ; target_d_b_parameter_group_identifier
    ; target_d_b_parameter_group_description
    ; tags
    }

  let parse xml =
    Some
      { source_d_b_parameter_group_identifier =
          Xml.required
            "SourceDBParameterGroupIdentifier"
            (Util.option_bind
               (Xml.member "SourceDBParameterGroupIdentifier" xml)
               String.parse)
      ; target_d_b_parameter_group_identifier =
          Xml.required
            "TargetDBParameterGroupIdentifier"
            (Util.option_bind
               (Xml.member "TargetDBParameterGroupIdentifier" xml)
               String.parse)
      ; target_d_b_parameter_group_description =
          Xml.required
            "TargetDBParameterGroupDescription"
            (Util.option_bind
               (Xml.member "TargetDBParameterGroupDescription" xml)
               String.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Query.Pair
                ( "TargetDBParameterGroupDescription"
                , String.to_query v.target_d_b_parameter_group_description ))
         ; Some
             (Query.Pair
                ( "TargetDBParameterGroupIdentifier"
                , String.to_query v.target_d_b_parameter_group_identifier ))
         ; Some
             (Query.Pair
                ( "SourceDBParameterGroupIdentifier"
                , String.to_query v.source_d_b_parameter_group_identifier ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some
             ( "target_d_b_parameter_group_description"
             , String.to_json v.target_d_b_parameter_group_description )
         ; Some
             ( "target_d_b_parameter_group_identifier"
             , String.to_json v.target_d_b_parameter_group_identifier )
         ; Some
             ( "source_d_b_parameter_group_identifier"
             , String.to_json v.source_d_b_parameter_group_identifier )
         ])

  let of_json j =
    { source_d_b_parameter_group_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "source_d_b_parameter_group_identifier"))
    ; target_d_b_parameter_group_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "target_d_b_parameter_group_identifier"))
    ; target_d_b_parameter_group_description =
        String.of_json
          (Util.of_option_exn (Json.lookup j "target_d_b_parameter_group_description"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module InsufficientDBInstanceCapacityFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyGlobalClusterResult = struct
  type t = { global_cluster : GlobalCluster.t option }

  let make ?global_cluster () = { global_cluster }

  let parse xml =
    Some
      { global_cluster =
          Util.option_bind (Xml.member "GlobalCluster" xml) GlobalCluster.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.global_cluster (fun f ->
               Query.Pair ("GlobalCluster", GlobalCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.global_cluster (fun f ->
               "global_cluster", GlobalCluster.to_json f)
         ])

  let of_json j =
    { global_cluster =
        Util.option_map (Json.lookup j "global_cluster") GlobalCluster.of_json
    }
end

module DescribeCertificatesMessage = struct
  type t =
    { certificate_identifier : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?certificate_identifier ?(filters = []) ?max_records ?marker () =
    { certificate_identifier; filters; max_records; marker }

  let parse xml =
    Some
      { certificate_identifier =
          Util.option_bind (Xml.member "CertificateIdentifier" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.certificate_identifier (fun f ->
               Query.Pair ("CertificateIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.certificate_identifier (fun f ->
               "certificate_identifier", String.to_json f)
         ])

  let of_json j =
    { certificate_identifier =
        Util.option_map (Json.lookup j "certificate_identifier") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module CreateDBSecurityGroupResult = struct
  type t = { d_b_security_group : DBSecurityGroup.t option }

  let make ?d_b_security_group () = { d_b_security_group }

  let parse xml =
    Some
      { d_b_security_group =
          Util.option_bind (Xml.member "DBSecurityGroup" xml) DBSecurityGroup.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_security_group (fun f ->
               Query.Pair ("DBSecurityGroup", DBSecurityGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_security_group (fun f ->
               "d_b_security_group", DBSecurityGroup.to_json f)
         ])

  let of_json j =
    { d_b_security_group =
        Util.option_map (Json.lookup j "d_b_security_group") DBSecurityGroup.of_json
    }
end

module DescribeDBSnapshotAttributesMessage = struct
  type t = { d_b_snapshot_identifier : String.t }

  let make ~d_b_snapshot_identifier () = { d_b_snapshot_identifier }

  let parse xml =
    Some
      { d_b_snapshot_identifier =
          Xml.required
            "DBSnapshotIdentifier"
            (Util.option_bind (Xml.member "DBSnapshotIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("DBSnapshotIdentifier", String.to_query v.d_b_snapshot_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_snapshot_identifier", String.to_json v.d_b_snapshot_identifier) ])

  let of_json j =
    { d_b_snapshot_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_snapshot_identifier"))
    }
end

module GlobalClustersMessage = struct
  type t =
    { marker : String.t option
    ; global_clusters : GlobalClusterList.t
    }

  let make ?marker ?(global_clusters = []) () = { marker; global_clusters }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; global_clusters =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GlobalClusters" xml) GlobalClusterList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("GlobalClusters.member", GlobalClusterList.to_query v.global_clusters))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("global_clusters", GlobalClusterList.to_json v.global_clusters)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; global_clusters =
        GlobalClusterList.of_json (Util.of_option_exn (Json.lookup j "global_clusters"))
    }
end

module RestoreDBClusterToPointInTimeResult = struct
  type t = { d_b_cluster : DBCluster.t option }

  let make ?d_b_cluster () = { d_b_cluster }

  let parse xml =
    Some { d_b_cluster = Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f ->
               Query.Pair ("DBCluster", DBCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f -> "d_b_cluster", DBCluster.to_json f) ])

  let of_json j =
    { d_b_cluster = Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json }
end

module DeregisterDBProxyTargetsResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidDBSubnetStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module RebootDBInstanceResult = struct
  type t = { d_b_instance : DBInstance.t option }

  let make ?d_b_instance () = { d_b_instance }

  let parse xml =
    Some
      { d_b_instance = Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f ->
               Query.Pair ("DBInstance", DBInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f -> "d_b_instance", DBInstance.to_json f)
         ])

  let of_json j =
    { d_b_instance = Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json }
end

module RemoveFromGlobalClusterMessage = struct
  type t =
    { global_cluster_identifier : String.t option
    ; db_cluster_identifier : String.t option
    }

  let make ?global_cluster_identifier ?db_cluster_identifier () =
    { global_cluster_identifier; db_cluster_identifier }

  let parse xml =
    Some
      { global_cluster_identifier =
          Util.option_bind (Xml.member "GlobalClusterIdentifier" xml) String.parse
      ; db_cluster_identifier =
          Util.option_bind (Xml.member "DbClusterIdentifier" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.db_cluster_identifier (fun f ->
               Query.Pair ("DbClusterIdentifier", String.to_query f))
         ; Util.option_map v.global_cluster_identifier (fun f ->
               Query.Pair ("GlobalClusterIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.db_cluster_identifier (fun f ->
               "db_cluster_identifier", String.to_json f)
         ; Util.option_map v.global_cluster_identifier (fun f ->
               "global_cluster_identifier", String.to_json f)
         ])

  let of_json j =
    { global_cluster_identifier =
        Util.option_map (Json.lookup j "global_cluster_identifier") String.of_json
    ; db_cluster_identifier =
        Util.option_map (Json.lookup j "db_cluster_identifier") String.of_json
    }
end

module DBParameterGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBSecurityGroupMessage = struct
  type t = { d_b_security_group_name : String.t }

  let make ~d_b_security_group_name () = { d_b_security_group_name }

  let parse xml =
    Some
      { d_b_security_group_name =
          Xml.required
            "DBSecurityGroupName"
            (Util.option_bind (Xml.member "DBSecurityGroupName" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("DBSecurityGroupName", String.to_query v.d_b_security_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_security_group_name", String.to_json v.d_b_security_group_name) ])

  let of_json j =
    { d_b_security_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_security_group_name"))
    }
end

module DBInstanceAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBInstanceAutomatedBackupMessage = struct
  type t = { dbi_resource_id : String.t }

  let make ~dbi_resource_id () = { dbi_resource_id }

  let parse xml =
    Some
      { dbi_resource_id =
          Xml.required
            "DbiResourceId"
            (Util.option_bind (Xml.member "DbiResourceId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("DbiResourceId", String.to_query v.dbi_resource_id)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("dbi_resource_id", String.to_json v.dbi_resource_id) ])

  let of_json j =
    { dbi_resource_id =
        String.of_json (Util.of_option_exn (Json.lookup j "dbi_resource_id"))
    }
end

module DescribeDBProxiesRequest = struct
  type t =
    { d_b_proxy_name : String.t option
    ; filters : FilterList.t
    ; marker : String.t option
    ; max_records : Integer.t option
    }

  let make ?d_b_proxy_name ?(filters = []) ?marker ?max_records () =
    { d_b_proxy_name; filters; marker; max_records }

  let parse xml =
    Some
      { d_b_proxy_name = Util.option_bind (Xml.member "DBProxyName" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.d_b_proxy_name (fun f ->
               Query.Pair ("DBProxyName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.d_b_proxy_name (fun f -> "d_b_proxy_name", String.to_json f)
         ])

  let of_json j =
    { d_b_proxy_name = Util.option_map (Json.lookup j "d_b_proxy_name") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    }
end

module ModifyDBClusterSnapshotAttributeMessage = struct
  type t =
    { d_b_cluster_snapshot_identifier : String.t
    ; attribute_name : String.t
    ; values_to_add : AttributeValueList.t
    ; values_to_remove : AttributeValueList.t
    }

  let make
      ~d_b_cluster_snapshot_identifier
      ~attribute_name
      ?(values_to_add = [])
      ?(values_to_remove = [])
      () =
    { d_b_cluster_snapshot_identifier; attribute_name; values_to_add; values_to_remove }

  let parse xml =
    Some
      { d_b_cluster_snapshot_identifier =
          Xml.required
            "DBClusterSnapshotIdentifier"
            (Util.option_bind (Xml.member "DBClusterSnapshotIdentifier" xml) String.parse)
      ; attribute_name =
          Xml.required
            "AttributeName"
            (Util.option_bind (Xml.member "AttributeName" xml) String.parse)
      ; values_to_add =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ValuesToAdd" xml) AttributeValueList.parse)
      ; values_to_remove =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ValuesToRemove" xml) AttributeValueList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("ValuesToRemove.member", AttributeValueList.to_query v.values_to_remove))
         ; Some
             (Query.Pair
                ("ValuesToAdd.member", AttributeValueList.to_query v.values_to_add))
         ; Some (Query.Pair ("AttributeName", String.to_query v.attribute_name))
         ; Some
             (Query.Pair
                ( "DBClusterSnapshotIdentifier"
                , String.to_query v.d_b_cluster_snapshot_identifier ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("values_to_remove", AttributeValueList.to_json v.values_to_remove)
         ; Some ("values_to_add", AttributeValueList.to_json v.values_to_add)
         ; Some ("attribute_name", String.to_json v.attribute_name)
         ; Some
             ( "d_b_cluster_snapshot_identifier"
             , String.to_json v.d_b_cluster_snapshot_identifier )
         ])

  let of_json j =
    { d_b_cluster_snapshot_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_snapshot_identifier"))
    ; attribute_name =
        String.of_json (Util.of_option_exn (Json.lookup j "attribute_name"))
    ; values_to_add =
        AttributeValueList.of_json (Util.of_option_exn (Json.lookup j "values_to_add"))
    ; values_to_remove =
        AttributeValueList.of_json (Util.of_option_exn (Json.lookup j "values_to_remove"))
    }
end

module SubscriptionNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module InstallationMediaNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeValidDBInstanceModificationsMessage = struct
  type t = { d_b_instance_identifier : String.t }

  let make ~d_b_instance_identifier () = { d_b_instance_identifier }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier) ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    }
end

module DBClusterBacktrackNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBClusterParameterGroupsMessage = struct
  type t =
    { marker : String.t option
    ; d_b_cluster_parameter_groups : DBClusterParameterGroupList.t
    }

  let make ?marker ?(d_b_cluster_parameter_groups = []) () =
    { marker; d_b_cluster_parameter_groups }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; d_b_cluster_parameter_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBClusterParameterGroups" xml)
               DBClusterParameterGroupList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBClusterParameterGroups.member"
                , DBClusterParameterGroupList.to_query v.d_b_cluster_parameter_groups ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "d_b_cluster_parameter_groups"
             , DBClusterParameterGroupList.to_json v.d_b_cluster_parameter_groups )
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; d_b_cluster_parameter_groups =
        DBClusterParameterGroupList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_parameter_groups"))
    }
end

module DBInstanceRoleAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeDBLogFilesResponse = struct
  type t =
    { describe_d_b_log_files : DescribeDBLogFilesList.t
    ; marker : String.t option
    }

  let make ?(describe_d_b_log_files = []) ?marker () = { describe_d_b_log_files; marker }

  let parse xml =
    Some
      { describe_d_b_log_files =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DescribeDBLogFiles" xml)
               DescribeDBLogFilesList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair
                ( "DescribeDBLogFiles.member"
                , DescribeDBLogFilesList.to_query v.describe_d_b_log_files ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some
             ( "describe_d_b_log_files"
             , DescribeDBLogFilesList.to_json v.describe_d_b_log_files )
         ])

  let of_json j =
    { describe_d_b_log_files =
        DescribeDBLogFilesList.of_json
          (Util.of_option_exn (Json.lookup j "describe_d_b_log_files"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module SNSTopicArnNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module OptionGroupQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeReservedDBInstancesMessage = struct
  type t =
    { reserved_d_b_instance_id : String.t option
    ; reserved_d_b_instances_offering_id : String.t option
    ; d_b_instance_class : String.t option
    ; duration : String.t option
    ; product_description : String.t option
    ; offering_type : String.t option
    ; multi_a_z : Boolean.t option
    ; lease_id : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ?reserved_d_b_instance_id
      ?reserved_d_b_instances_offering_id
      ?d_b_instance_class
      ?duration
      ?product_description
      ?offering_type
      ?multi_a_z
      ?lease_id
      ?(filters = [])
      ?max_records
      ?marker
      () =
    { reserved_d_b_instance_id
    ; reserved_d_b_instances_offering_id
    ; d_b_instance_class
    ; duration
    ; product_description
    ; offering_type
    ; multi_a_z
    ; lease_id
    ; filters
    ; max_records
    ; marker
    }

  let parse xml =
    Some
      { reserved_d_b_instance_id =
          Util.option_bind (Xml.member "ReservedDBInstanceId" xml) String.parse
      ; reserved_d_b_instances_offering_id =
          Util.option_bind (Xml.member "ReservedDBInstancesOfferingId" xml) String.parse
      ; d_b_instance_class =
          Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse
      ; duration = Util.option_bind (Xml.member "Duration" xml) String.parse
      ; product_description =
          Util.option_bind (Xml.member "ProductDescription" xml) String.parse
      ; offering_type = Util.option_bind (Xml.member "OfferingType" xml) String.parse
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; lease_id = Util.option_bind (Xml.member "LeaseId" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.lease_id (fun f -> Query.Pair ("LeaseId", String.to_query f))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Util.option_map v.offering_type (fun f ->
               Query.Pair ("OfferingType", String.to_query f))
         ; Util.option_map v.product_description (fun f ->
               Query.Pair ("ProductDescription", String.to_query f))
         ; Util.option_map v.duration (fun f ->
               Query.Pair ("Duration", String.to_query f))
         ; Util.option_map v.d_b_instance_class (fun f ->
               Query.Pair ("DBInstanceClass", String.to_query f))
         ; Util.option_map v.reserved_d_b_instances_offering_id (fun f ->
               Query.Pair ("ReservedDBInstancesOfferingId", String.to_query f))
         ; Util.option_map v.reserved_d_b_instance_id (fun f ->
               Query.Pair ("ReservedDBInstanceId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.lease_id (fun f -> "lease_id", String.to_json f)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Util.option_map v.offering_type (fun f -> "offering_type", String.to_json f)
         ; Util.option_map v.product_description (fun f ->
               "product_description", String.to_json f)
         ; Util.option_map v.duration (fun f -> "duration", String.to_json f)
         ; Util.option_map v.d_b_instance_class (fun f ->
               "d_b_instance_class", String.to_json f)
         ; Util.option_map v.reserved_d_b_instances_offering_id (fun f ->
               "reserved_d_b_instances_offering_id", String.to_json f)
         ; Util.option_map v.reserved_d_b_instance_id (fun f ->
               "reserved_d_b_instance_id", String.to_json f)
         ])

  let of_json j =
    { reserved_d_b_instance_id =
        Util.option_map (Json.lookup j "reserved_d_b_instance_id") String.of_json
    ; reserved_d_b_instances_offering_id =
        Util.option_map
          (Json.lookup j "reserved_d_b_instances_offering_id")
          String.of_json
    ; d_b_instance_class =
        Util.option_map (Json.lookup j "d_b_instance_class") String.of_json
    ; duration = Util.option_map (Json.lookup j "duration") String.of_json
    ; product_description =
        Util.option_map (Json.lookup j "product_description") String.of_json
    ; offering_type = Util.option_map (Json.lookup j "offering_type") String.of_json
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; lease_id = Util.option_map (Json.lookup j "lease_id") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module DBClusterParameterGroupDetails = struct
  type t =
    { parameters : ParametersList.t
    ; marker : String.t option
    }

  let make ?(parameters = []) ?marker () = { parameters; marker }

  let parse xml =
    Some
      { parameters =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Parameters" xml) ParametersList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("parameters", ParametersList.to_json v.parameters)
         ])

  let of_json j =
    { parameters =
        ParametersList.of_json (Util.of_option_exn (Json.lookup j "parameters"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module CopyOptionGroupResult = struct
  type t = { option_group : OptionGroup.t option }

  let make ?option_group () = { option_group }

  let parse xml =
    Some
      { option_group = Util.option_bind (Xml.member "OptionGroup" xml) OptionGroup.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.option_group (fun f ->
               Query.Pair ("OptionGroup", OptionGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.option_group (fun f -> "option_group", OptionGroup.to_json f)
         ])

  let of_json j =
    { option_group = Util.option_map (Json.lookup j "option_group") OptionGroup.of_json }
end

module RemoveSourceIdentifierFromSubscriptionResult = struct
  type t = { event_subscription : EventSubscription.t option }

  let make ?event_subscription () = { event_subscription }

  let parse xml =
    Some
      { event_subscription =
          Util.option_bind (Xml.member "EventSubscription" xml) EventSubscription.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.event_subscription (fun f ->
               Query.Pair ("EventSubscription", EventSubscription.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.event_subscription (fun f ->
               "event_subscription", EventSubscription.to_json f)
         ])

  let of_json j =
    { event_subscription =
        Util.option_map (Json.lookup j "event_subscription") EventSubscription.of_json
    }
end

module RestoreDBInstanceFromDBSnapshotMessage = struct
  type t =
    { d_b_instance_identifier : String.t
    ; d_b_snapshot_identifier : String.t
    ; d_b_instance_class : String.t option
    ; port : Integer.t option
    ; availability_zone : String.t option
    ; d_b_subnet_group_name : String.t option
    ; multi_a_z : Boolean.t option
    ; publicly_accessible : Boolean.t option
    ; auto_minor_version_upgrade : Boolean.t option
    ; license_model : String.t option
    ; d_b_name : String.t option
    ; engine : String.t option
    ; iops : Integer.t option
    ; option_group_name : String.t option
    ; tags : TagList.t
    ; storage_type : String.t option
    ; tde_credential_arn : String.t option
    ; tde_credential_password : String.t option
    ; vpc_security_group_ids : VpcSecurityGroupIdList.t
    ; domain : String.t option
    ; copy_tags_to_snapshot : Boolean.t option
    ; domain_i_a_m_role_name : String.t option
    ; enable_i_a_m_database_authentication : Boolean.t option
    ; enable_cloudwatch_logs_exports : LogTypeList.t
    ; processor_features : ProcessorFeatureList.t
    ; use_default_processor_features : Boolean.t option
    ; d_b_parameter_group_name : String.t option
    ; deletion_protection : Boolean.t option
    }

  let make
      ~d_b_instance_identifier
      ~d_b_snapshot_identifier
      ?d_b_instance_class
      ?port
      ?availability_zone
      ?d_b_subnet_group_name
      ?multi_a_z
      ?publicly_accessible
      ?auto_minor_version_upgrade
      ?license_model
      ?d_b_name
      ?engine
      ?iops
      ?option_group_name
      ?(tags = [])
      ?storage_type
      ?tde_credential_arn
      ?tde_credential_password
      ?(vpc_security_group_ids = [])
      ?domain
      ?copy_tags_to_snapshot
      ?domain_i_a_m_role_name
      ?enable_i_a_m_database_authentication
      ?(enable_cloudwatch_logs_exports = [])
      ?(processor_features = [])
      ?use_default_processor_features
      ?d_b_parameter_group_name
      ?deletion_protection
      () =
    { d_b_instance_identifier
    ; d_b_snapshot_identifier
    ; d_b_instance_class
    ; port
    ; availability_zone
    ; d_b_subnet_group_name
    ; multi_a_z
    ; publicly_accessible
    ; auto_minor_version_upgrade
    ; license_model
    ; d_b_name
    ; engine
    ; iops
    ; option_group_name
    ; tags
    ; storage_type
    ; tde_credential_arn
    ; tde_credential_password
    ; vpc_security_group_ids
    ; domain
    ; copy_tags_to_snapshot
    ; domain_i_a_m_role_name
    ; enable_i_a_m_database_authentication
    ; enable_cloudwatch_logs_exports
    ; processor_features
    ; use_default_processor_features
    ; d_b_parameter_group_name
    ; deletion_protection
    }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; d_b_snapshot_identifier =
          Xml.required
            "DBSnapshotIdentifier"
            (Util.option_bind (Xml.member "DBSnapshotIdentifier" xml) String.parse)
      ; d_b_instance_class =
          Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; availability_zone =
          Util.option_bind (Xml.member "AvailabilityZone" xml) String.parse
      ; d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; publicly_accessible =
          Util.option_bind (Xml.member "PubliclyAccessible" xml) Boolean.parse
      ; auto_minor_version_upgrade =
          Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
      ; license_model = Util.option_bind (Xml.member "LicenseModel" xml) String.parse
      ; d_b_name = Util.option_bind (Xml.member "DBName" xml) String.parse
      ; engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; iops = Util.option_bind (Xml.member "Iops" xml) Integer.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; storage_type = Util.option_bind (Xml.member "StorageType" xml) String.parse
      ; tde_credential_arn =
          Util.option_bind (Xml.member "TdeCredentialArn" xml) String.parse
      ; tde_credential_password =
          Util.option_bind (Xml.member "TdeCredentialPassword" xml) String.parse
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupIds" xml)
               VpcSecurityGroupIdList.parse)
      ; domain = Util.option_bind (Xml.member "Domain" xml) String.parse
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; domain_i_a_m_role_name =
          Util.option_bind (Xml.member "DomainIAMRoleName" xml) String.parse
      ; enable_i_a_m_database_authentication =
          Util.option_bind
            (Xml.member "EnableIAMDatabaseAuthentication" xml)
            Boolean.parse
      ; enable_cloudwatch_logs_exports =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EnableCloudwatchLogsExports" xml)
               LogTypeList.parse)
      ; processor_features =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ProcessorFeatures" xml)
               ProcessorFeatureList.parse)
      ; use_default_processor_features =
          Util.option_bind (Xml.member "UseDefaultProcessorFeatures" xml) Boolean.parse
      ; d_b_parameter_group_name =
          Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               Query.Pair ("DBParameterGroupName", String.to_query f))
         ; Util.option_map v.use_default_processor_features (fun f ->
               Query.Pair ("UseDefaultProcessorFeatures", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "ProcessorFeatures.member"
                , ProcessorFeatureList.to_query v.processor_features ))
         ; Some
             (Query.Pair
                ( "EnableCloudwatchLogsExports.member"
                , LogTypeList.to_query v.enable_cloudwatch_logs_exports ))
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               Query.Pair ("DomainIAMRoleName", String.to_query f))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Util.option_map v.domain (fun f -> Query.Pair ("Domain", String.to_query f))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
         ; Util.option_map v.tde_credential_password (fun f ->
               Query.Pair ("TdeCredentialPassword", String.to_query f))
         ; Util.option_map v.tde_credential_arn (fun f ->
               Query.Pair ("TdeCredentialArn", String.to_query f))
         ; Util.option_map v.storage_type (fun f ->
               Query.Pair ("StorageType", String.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.iops (fun f -> Query.Pair ("Iops", Integer.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ; Util.option_map v.d_b_name (fun f -> Query.Pair ("DBName", String.to_query f))
         ; Util.option_map v.license_model (fun f ->
               Query.Pair ("LicenseModel", String.to_query f))
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Util.option_map v.publicly_accessible (fun f ->
               Query.Pair ("PubliclyAccessible", Boolean.to_query f))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ; Util.option_map v.availability_zone (fun f ->
               Query.Pair ("AvailabilityZone", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.d_b_instance_class (fun f ->
               Query.Pair ("DBInstanceClass", String.to_query f))
         ; Some
             (Query.Pair
                ("DBSnapshotIdentifier", String.to_query v.d_b_snapshot_identifier))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               "d_b_parameter_group_name", String.to_json f)
         ; Util.option_map v.use_default_processor_features (fun f ->
               "use_default_processor_features", Boolean.to_json f)
         ; Some ("processor_features", ProcessorFeatureList.to_json v.processor_features)
         ; Some
             ( "enable_cloudwatch_logs_exports"
             , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               "enable_i_a_m_database_authentication", Boolean.to_json f)
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               "domain_i_a_m_role_name", String.to_json f)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Util.option_map v.domain (fun f -> "domain", String.to_json f)
         ; Some
             ( "vpc_security_group_ids"
             , VpcSecurityGroupIdList.to_json v.vpc_security_group_ids )
         ; Util.option_map v.tde_credential_password (fun f ->
               "tde_credential_password", String.to_json f)
         ; Util.option_map v.tde_credential_arn (fun f ->
               "tde_credential_arn", String.to_json f)
         ; Util.option_map v.storage_type (fun f -> "storage_type", String.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ; Util.option_map v.d_b_name (fun f -> "d_b_name", String.to_json f)
         ; Util.option_map v.license_model (fun f -> "license_model", String.to_json f)
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               "auto_minor_version_upgrade", Boolean.to_json f)
         ; Util.option_map v.publicly_accessible (fun f ->
               "publicly_accessible", Boolean.to_json f)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ; Util.option_map v.availability_zone (fun f ->
               "availability_zone", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.d_b_instance_class (fun f ->
               "d_b_instance_class", String.to_json f)
         ; Some ("d_b_snapshot_identifier", String.to_json v.d_b_snapshot_identifier)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; d_b_snapshot_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_snapshot_identifier"))
    ; d_b_instance_class =
        Util.option_map (Json.lookup j "d_b_instance_class") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; availability_zone =
        Util.option_map (Json.lookup j "availability_zone") String.of_json
    ; d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; publicly_accessible =
        Util.option_map (Json.lookup j "publicly_accessible") Boolean.of_json
    ; auto_minor_version_upgrade =
        Util.option_map (Json.lookup j "auto_minor_version_upgrade") Boolean.of_json
    ; license_model = Util.option_map (Json.lookup j "license_model") String.of_json
    ; d_b_name = Util.option_map (Json.lookup j "d_b_name") String.of_json
    ; engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; iops = Util.option_map (Json.lookup j "iops") Integer.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; storage_type = Util.option_map (Json.lookup j "storage_type") String.of_json
    ; tde_credential_arn =
        Util.option_map (Json.lookup j "tde_credential_arn") String.of_json
    ; tde_credential_password =
        Util.option_map (Json.lookup j "tde_credential_password") String.of_json
    ; vpc_security_group_ids =
        VpcSecurityGroupIdList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; domain = Util.option_map (Json.lookup j "domain") String.of_json
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; domain_i_a_m_role_name =
        Util.option_map (Json.lookup j "domain_i_a_m_role_name") String.of_json
    ; enable_i_a_m_database_authentication =
        Util.option_map
          (Json.lookup j "enable_i_a_m_database_authentication")
          Boolean.of_json
    ; enable_cloudwatch_logs_exports =
        LogTypeList.of_json
          (Util.of_option_exn (Json.lookup j "enable_cloudwatch_logs_exports"))
    ; processor_features =
        ProcessorFeatureList.of_json
          (Util.of_option_exn (Json.lookup j "processor_features"))
    ; use_default_processor_features =
        Util.option_map (Json.lookup j "use_default_processor_features") Boolean.of_json
    ; d_b_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_parameter_group_name") String.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    }
end

module DBClusterRoleQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBSecurityGroupMessage = struct
  type t =
    { d_b_security_group_name : String.t
    ; d_b_security_group_description : String.t
    ; tags : TagList.t
    }

  let make ~d_b_security_group_name ~d_b_security_group_description ?(tags = []) () =
    { d_b_security_group_name; d_b_security_group_description; tags }

  let parse xml =
    Some
      { d_b_security_group_name =
          Xml.required
            "DBSecurityGroupName"
            (Util.option_bind (Xml.member "DBSecurityGroupName" xml) String.parse)
      ; d_b_security_group_description =
          Xml.required
            "DBSecurityGroupDescription"
            (Util.option_bind (Xml.member "DBSecurityGroupDescription" xml) String.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Query.Pair
                ( "DBSecurityGroupDescription"
                , String.to_query v.d_b_security_group_description ))
         ; Some
             (Query.Pair ("DBSecurityGroupName", String.to_query v.d_b_security_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some
             ( "d_b_security_group_description"
             , String.to_json v.d_b_security_group_description )
         ; Some ("d_b_security_group_name", String.to_json v.d_b_security_group_name)
         ])

  let of_json j =
    { d_b_security_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_security_group_name"))
    ; d_b_security_group_description =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_security_group_description"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module PurchaseReservedDBInstancesOfferingResult = struct
  type t = { reserved_d_b_instance : ReservedDBInstance.t option }

  let make ?reserved_d_b_instance () = { reserved_d_b_instance }

  let parse xml =
    Some
      { reserved_d_b_instance =
          Util.option_bind (Xml.member "ReservedDBInstance" xml) ReservedDBInstance.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.reserved_d_b_instance (fun f ->
               Query.Pair ("ReservedDBInstance", ReservedDBInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.reserved_d_b_instance (fun f ->
               "reserved_d_b_instance", ReservedDBInstance.to_json f)
         ])

  let of_json j =
    { reserved_d_b_instance =
        Util.option_map (Json.lookup j "reserved_d_b_instance") ReservedDBInstance.of_json
    }
end

module RestoreDBInstanceToPointInTimeResult = struct
  type t = { d_b_instance : DBInstance.t option }

  let make ?d_b_instance () = { d_b_instance }

  let parse xml =
    Some
      { d_b_instance = Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f ->
               Query.Pair ("DBInstance", DBInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f -> "d_b_instance", DBInstance.to_json f)
         ])

  let of_json j =
    { d_b_instance = Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json }
end

module ApplyPendingMaintenanceActionMessage = struct
  type t =
    { resource_identifier : String.t
    ; apply_action : String.t
    ; opt_in_type : String.t
    }

  let make ~resource_identifier ~apply_action ~opt_in_type () =
    { resource_identifier; apply_action; opt_in_type }

  let parse xml =
    Some
      { resource_identifier =
          Xml.required
            "ResourceIdentifier"
            (Util.option_bind (Xml.member "ResourceIdentifier" xml) String.parse)
      ; apply_action =
          Xml.required
            "ApplyAction"
            (Util.option_bind (Xml.member "ApplyAction" xml) String.parse)
      ; opt_in_type =
          Xml.required
            "OptInType"
            (Util.option_bind (Xml.member "OptInType" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("OptInType", String.to_query v.opt_in_type))
         ; Some (Query.Pair ("ApplyAction", String.to_query v.apply_action))
         ; Some (Query.Pair ("ResourceIdentifier", String.to_query v.resource_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("opt_in_type", String.to_json v.opt_in_type)
         ; Some ("apply_action", String.to_json v.apply_action)
         ; Some ("resource_identifier", String.to_json v.resource_identifier)
         ])

  let of_json j =
    { resource_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "resource_identifier"))
    ; apply_action = String.of_json (Util.of_option_exn (Json.lookup j "apply_action"))
    ; opt_in_type = String.of_json (Util.of_option_exn (Json.lookup j "opt_in_type"))
    }
end

module CreateDBClusterParameterGroupMessage = struct
  type t =
    { d_b_cluster_parameter_group_name : String.t
    ; d_b_parameter_group_family : String.t
    ; description : String.t
    ; tags : TagList.t
    }

  let make
      ~d_b_cluster_parameter_group_name
      ~d_b_parameter_group_family
      ~description
      ?(tags = [])
      () =
    { d_b_cluster_parameter_group_name; d_b_parameter_group_family; description; tags }

  let parse xml =
    Some
      { d_b_cluster_parameter_group_name =
          Xml.required
            "DBClusterParameterGroupName"
            (Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse)
      ; d_b_parameter_group_family =
          Xml.required
            "DBParameterGroupFamily"
            (Util.option_bind (Xml.member "DBParameterGroupFamily" xml) String.parse)
      ; description =
          Xml.required
            "Description"
            (Util.option_bind (Xml.member "Description" xml) String.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some (Query.Pair ("Description", String.to_query v.description))
         ; Some
             (Query.Pair
                ("DBParameterGroupFamily", String.to_query v.d_b_parameter_group_family))
         ; Some
             (Query.Pair
                ( "DBClusterParameterGroupName"
                , String.to_query v.d_b_cluster_parameter_group_name ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some ("description", String.to_json v.description)
         ; Some ("d_b_parameter_group_family", String.to_json v.d_b_parameter_group_family)
         ; Some
             ( "d_b_cluster_parameter_group_name"
             , String.to_json v.d_b_cluster_parameter_group_name )
         ])

  let of_json j =
    { d_b_cluster_parameter_group_name =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_parameter_group_name"))
    ; d_b_parameter_group_family =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_parameter_group_family"))
    ; description = String.of_json (Util.of_option_exn (Json.lookup j "description"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module DeleteOptionGroupMessage = struct
  type t = { option_group_name : String.t }

  let make ~option_group_name () = { option_group_name }

  let parse xml =
    Some
      { option_group_name =
          Xml.required
            "OptionGroupName"
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("OptionGroupName", String.to_query v.option_group_name)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("option_group_name", String.to_json v.option_group_name) ])

  let of_json j =
    { option_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "option_group_name"))
    }
end

module DescribeDBEngineVersionsMessage = struct
  type t =
    { engine : String.t option
    ; engine_version : String.t option
    ; d_b_parameter_group_family : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    ; default_only : Boolean.t option
    ; list_supported_character_sets : Boolean.t option
    ; list_supported_timezones : Boolean.t option
    ; include_all : Boolean.t option
    }

  let make
      ?engine
      ?engine_version
      ?d_b_parameter_group_family
      ?(filters = [])
      ?max_records
      ?marker
      ?default_only
      ?list_supported_character_sets
      ?list_supported_timezones
      ?include_all
      () =
    { engine
    ; engine_version
    ; d_b_parameter_group_family
    ; filters
    ; max_records
    ; marker
    ; default_only
    ; list_supported_character_sets
    ; list_supported_timezones
    ; include_all
    }

  let parse xml =
    Some
      { engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; d_b_parameter_group_family =
          Util.option_bind (Xml.member "DBParameterGroupFamily" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; default_only = Util.option_bind (Xml.member "DefaultOnly" xml) Boolean.parse
      ; list_supported_character_sets =
          Util.option_bind (Xml.member "ListSupportedCharacterSets" xml) Boolean.parse
      ; list_supported_timezones =
          Util.option_bind (Xml.member "ListSupportedTimezones" xml) Boolean.parse
      ; include_all = Util.option_bind (Xml.member "IncludeAll" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.include_all (fun f ->
               Query.Pair ("IncludeAll", Boolean.to_query f))
         ; Util.option_map v.list_supported_timezones (fun f ->
               Query.Pair ("ListSupportedTimezones", Boolean.to_query f))
         ; Util.option_map v.list_supported_character_sets (fun f ->
               Query.Pair ("ListSupportedCharacterSets", Boolean.to_query f))
         ; Util.option_map v.default_only (fun f ->
               Query.Pair ("DefaultOnly", Boolean.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.d_b_parameter_group_family (fun f ->
               Query.Pair ("DBParameterGroupFamily", String.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.include_all (fun f -> "include_all", Boolean.to_json f)
         ; Util.option_map v.list_supported_timezones (fun f ->
               "list_supported_timezones", Boolean.to_json f)
         ; Util.option_map v.list_supported_character_sets (fun f ->
               "list_supported_character_sets", Boolean.to_json f)
         ; Util.option_map v.default_only (fun f -> "default_only", Boolean.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.d_b_parameter_group_family (fun f ->
               "d_b_parameter_group_family", String.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ])

  let of_json j =
    { engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; d_b_parameter_group_family =
        Util.option_map (Json.lookup j "d_b_parameter_group_family") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; default_only = Util.option_map (Json.lookup j "default_only") Boolean.of_json
    ; list_supported_character_sets =
        Util.option_map (Json.lookup j "list_supported_character_sets") Boolean.of_json
    ; list_supported_timezones =
        Util.option_map (Json.lookup j "list_supported_timezones") Boolean.of_json
    ; include_all = Util.option_map (Json.lookup j "include_all") Boolean.of_json
    }
end

module RestoreDBInstanceFromS3Result = struct
  type t = { d_b_instance : DBInstance.t option }

  let make ?d_b_instance () = { d_b_instance }

  let parse xml =
    Some
      { d_b_instance = Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f ->
               Query.Pair ("DBInstance", DBInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f -> "d_b_instance", DBInstance.to_json f)
         ])

  let of_json j =
    { d_b_instance = Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json }
end

module SubscriptionAlreadyExistFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CertificateMessage = struct
  type t =
    { certificates : CertificateList.t
    ; marker : String.t option
    }

  let make ?(certificates = []) ?marker () = { certificates; marker }

  let parse xml =
    Some
      { certificates =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Certificates" xml) CertificateList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair ("Certificates.member", CertificateList.to_query v.certificates))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("certificates", CertificateList.to_json v.certificates)
         ])

  let of_json j =
    { certificates =
        CertificateList.of_json (Util.of_option_exn (Json.lookup j "certificates"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module CreateDBInstanceReadReplicaMessage = struct
  type t =
    { d_b_instance_identifier : String.t
    ; source_d_b_instance_identifier : String.t
    ; d_b_instance_class : String.t option
    ; availability_zone : String.t option
    ; port : Integer.t option
    ; multi_a_z : Boolean.t option
    ; auto_minor_version_upgrade : Boolean.t option
    ; iops : Integer.t option
    ; option_group_name : String.t option
    ; d_b_parameter_group_name : String.t option
    ; publicly_accessible : Boolean.t option
    ; tags : TagList.t
    ; d_b_subnet_group_name : String.t option
    ; vpc_security_group_ids : VpcSecurityGroupIdList.t
    ; storage_type : String.t option
    ; copy_tags_to_snapshot : Boolean.t option
    ; monitoring_interval : Integer.t option
    ; monitoring_role_arn : String.t option
    ; kms_key_id : String.t option
    ; pre_signed_url : String.t option
    ; enable_i_a_m_database_authentication : Boolean.t option
    ; enable_performance_insights : Boolean.t option
    ; performance_insights_k_m_s_key_id : String.t option
    ; performance_insights_retention_period : Integer.t option
    ; enable_cloudwatch_logs_exports : LogTypeList.t
    ; processor_features : ProcessorFeatureList.t
    ; use_default_processor_features : Boolean.t option
    ; deletion_protection : Boolean.t option
    ; domain : String.t option
    ; domain_i_a_m_role_name : String.t option
    ; replica_mode : ReplicaMode.t option
    ; max_allocated_storage : Integer.t option
    }

  let make
      ~d_b_instance_identifier
      ~source_d_b_instance_identifier
      ?d_b_instance_class
      ?availability_zone
      ?port
      ?multi_a_z
      ?auto_minor_version_upgrade
      ?iops
      ?option_group_name
      ?d_b_parameter_group_name
      ?publicly_accessible
      ?(tags = [])
      ?d_b_subnet_group_name
      ?(vpc_security_group_ids = [])
      ?storage_type
      ?copy_tags_to_snapshot
      ?monitoring_interval
      ?monitoring_role_arn
      ?kms_key_id
      ?pre_signed_url
      ?enable_i_a_m_database_authentication
      ?enable_performance_insights
      ?performance_insights_k_m_s_key_id
      ?performance_insights_retention_period
      ?(enable_cloudwatch_logs_exports = [])
      ?(processor_features = [])
      ?use_default_processor_features
      ?deletion_protection
      ?domain
      ?domain_i_a_m_role_name
      ?replica_mode
      ?max_allocated_storage
      () =
    { d_b_instance_identifier
    ; source_d_b_instance_identifier
    ; d_b_instance_class
    ; availability_zone
    ; port
    ; multi_a_z
    ; auto_minor_version_upgrade
    ; iops
    ; option_group_name
    ; d_b_parameter_group_name
    ; publicly_accessible
    ; tags
    ; d_b_subnet_group_name
    ; vpc_security_group_ids
    ; storage_type
    ; copy_tags_to_snapshot
    ; monitoring_interval
    ; monitoring_role_arn
    ; kms_key_id
    ; pre_signed_url
    ; enable_i_a_m_database_authentication
    ; enable_performance_insights
    ; performance_insights_k_m_s_key_id
    ; performance_insights_retention_period
    ; enable_cloudwatch_logs_exports
    ; processor_features
    ; use_default_processor_features
    ; deletion_protection
    ; domain
    ; domain_i_a_m_role_name
    ; replica_mode
    ; max_allocated_storage
    }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; source_d_b_instance_identifier =
          Xml.required
            "SourceDBInstanceIdentifier"
            (Util.option_bind (Xml.member "SourceDBInstanceIdentifier" xml) String.parse)
      ; d_b_instance_class =
          Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse
      ; availability_zone =
          Util.option_bind (Xml.member "AvailabilityZone" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; auto_minor_version_upgrade =
          Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
      ; iops = Util.option_bind (Xml.member "Iops" xml) Integer.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; d_b_parameter_group_name =
          Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse
      ; publicly_accessible =
          Util.option_bind (Xml.member "PubliclyAccessible" xml) Boolean.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupIds" xml)
               VpcSecurityGroupIdList.parse)
      ; storage_type = Util.option_bind (Xml.member "StorageType" xml) String.parse
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; monitoring_interval =
          Util.option_bind (Xml.member "MonitoringInterval" xml) Integer.parse
      ; monitoring_role_arn =
          Util.option_bind (Xml.member "MonitoringRoleArn" xml) String.parse
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; pre_signed_url = Util.option_bind (Xml.member "PreSignedUrl" xml) String.parse
      ; enable_i_a_m_database_authentication =
          Util.option_bind
            (Xml.member "EnableIAMDatabaseAuthentication" xml)
            Boolean.parse
      ; enable_performance_insights =
          Util.option_bind (Xml.member "EnablePerformanceInsights" xml) Boolean.parse
      ; performance_insights_k_m_s_key_id =
          Util.option_bind (Xml.member "PerformanceInsightsKMSKeyId" xml) String.parse
      ; performance_insights_retention_period =
          Util.option_bind
            (Xml.member "PerformanceInsightsRetentionPeriod" xml)
            Integer.parse
      ; enable_cloudwatch_logs_exports =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EnableCloudwatchLogsExports" xml)
               LogTypeList.parse)
      ; processor_features =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ProcessorFeatures" xml)
               ProcessorFeatureList.parse)
      ; use_default_processor_features =
          Util.option_bind (Xml.member "UseDefaultProcessorFeatures" xml) Boolean.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; domain = Util.option_bind (Xml.member "Domain" xml) String.parse
      ; domain_i_a_m_role_name =
          Util.option_bind (Xml.member "DomainIAMRoleName" xml) String.parse
      ; replica_mode = Util.option_bind (Xml.member "ReplicaMode" xml) ReplicaMode.parse
      ; max_allocated_storage =
          Util.option_bind (Xml.member "MaxAllocatedStorage" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_allocated_storage (fun f ->
               Query.Pair ("MaxAllocatedStorage", Integer.to_query f))
         ; Util.option_map v.replica_mode (fun f ->
               Query.Pair ("ReplicaMode", ReplicaMode.to_query f))
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               Query.Pair ("DomainIAMRoleName", String.to_query f))
         ; Util.option_map v.domain (fun f -> Query.Pair ("Domain", String.to_query f))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.use_default_processor_features (fun f ->
               Query.Pair ("UseDefaultProcessorFeatures", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "ProcessorFeatures.member"
                , ProcessorFeatureList.to_query v.processor_features ))
         ; Some
             (Query.Pair
                ( "EnableCloudwatchLogsExports.member"
                , LogTypeList.to_query v.enable_cloudwatch_logs_exports ))
         ; Util.option_map v.performance_insights_retention_period (fun f ->
               Query.Pair ("PerformanceInsightsRetentionPeriod", Integer.to_query f))
         ; Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
               Query.Pair ("PerformanceInsightsKMSKeyId", String.to_query f))
         ; Util.option_map v.enable_performance_insights (fun f ->
               Query.Pair ("EnablePerformanceInsights", Boolean.to_query f))
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
         ; Util.option_map v.pre_signed_url (fun f ->
               Query.Pair ("PreSignedUrl", String.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Util.option_map v.monitoring_role_arn (fun f ->
               Query.Pair ("MonitoringRoleArn", String.to_query f))
         ; Util.option_map v.monitoring_interval (fun f ->
               Query.Pair ("MonitoringInterval", Integer.to_query f))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Util.option_map v.storage_type (fun f ->
               Query.Pair ("StorageType", String.to_query f))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.publicly_accessible (fun f ->
               Query.Pair ("PubliclyAccessible", Boolean.to_query f))
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               Query.Pair ("DBParameterGroupName", String.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.iops (fun f -> Query.Pair ("Iops", Integer.to_query f))
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.availability_zone (fun f ->
               Query.Pair ("AvailabilityZone", String.to_query f))
         ; Util.option_map v.d_b_instance_class (fun f ->
               Query.Pair ("DBInstanceClass", String.to_query f))
         ; Some
             (Query.Pair
                ( "SourceDBInstanceIdentifier"
                , String.to_query v.source_d_b_instance_identifier ))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_allocated_storage (fun f ->
               "max_allocated_storage", Integer.to_json f)
         ; Util.option_map v.replica_mode (fun f -> "replica_mode", ReplicaMode.to_json f)
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               "domain_i_a_m_role_name", String.to_json f)
         ; Util.option_map v.domain (fun f -> "domain", String.to_json f)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.use_default_processor_features (fun f ->
               "use_default_processor_features", Boolean.to_json f)
         ; Some ("processor_features", ProcessorFeatureList.to_json v.processor_features)
         ; Some
             ( "enable_cloudwatch_logs_exports"
             , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
         ; Util.option_map v.performance_insights_retention_period (fun f ->
               "performance_insights_retention_period", Integer.to_json f)
         ; Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
               "performance_insights_k_m_s_key_id", String.to_json f)
         ; Util.option_map v.enable_performance_insights (fun f ->
               "enable_performance_insights", Boolean.to_json f)
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               "enable_i_a_m_database_authentication", Boolean.to_json f)
         ; Util.option_map v.pre_signed_url (fun f -> "pre_signed_url", String.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Util.option_map v.monitoring_role_arn (fun f ->
               "monitoring_role_arn", String.to_json f)
         ; Util.option_map v.monitoring_interval (fun f ->
               "monitoring_interval", Integer.to_json f)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Util.option_map v.storage_type (fun f -> "storage_type", String.to_json f)
         ; Some
             ( "vpc_security_group_ids"
             , VpcSecurityGroupIdList.to_json v.vpc_security_group_ids )
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.publicly_accessible (fun f ->
               "publicly_accessible", Boolean.to_json f)
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               "d_b_parameter_group_name", String.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               "auto_minor_version_upgrade", Boolean.to_json f)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.availability_zone (fun f ->
               "availability_zone", String.to_json f)
         ; Util.option_map v.d_b_instance_class (fun f ->
               "d_b_instance_class", String.to_json f)
         ; Some
             ( "source_d_b_instance_identifier"
             , String.to_json v.source_d_b_instance_identifier )
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; source_d_b_instance_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "source_d_b_instance_identifier"))
    ; d_b_instance_class =
        Util.option_map (Json.lookup j "d_b_instance_class") String.of_json
    ; availability_zone =
        Util.option_map (Json.lookup j "availability_zone") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; auto_minor_version_upgrade =
        Util.option_map (Json.lookup j "auto_minor_version_upgrade") Boolean.of_json
    ; iops = Util.option_map (Json.lookup j "iops") Integer.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; d_b_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_parameter_group_name") String.of_json
    ; publicly_accessible =
        Util.option_map (Json.lookup j "publicly_accessible") Boolean.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; vpc_security_group_ids =
        VpcSecurityGroupIdList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; storage_type = Util.option_map (Json.lookup j "storage_type") String.of_json
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; monitoring_interval =
        Util.option_map (Json.lookup j "monitoring_interval") Integer.of_json
    ; monitoring_role_arn =
        Util.option_map (Json.lookup j "monitoring_role_arn") String.of_json
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; pre_signed_url = Util.option_map (Json.lookup j "pre_signed_url") String.of_json
    ; enable_i_a_m_database_authentication =
        Util.option_map
          (Json.lookup j "enable_i_a_m_database_authentication")
          Boolean.of_json
    ; enable_performance_insights =
        Util.option_map (Json.lookup j "enable_performance_insights") Boolean.of_json
    ; performance_insights_k_m_s_key_id =
        Util.option_map (Json.lookup j "performance_insights_k_m_s_key_id") String.of_json
    ; performance_insights_retention_period =
        Util.option_map
          (Json.lookup j "performance_insights_retention_period")
          Integer.of_json
    ; enable_cloudwatch_logs_exports =
        LogTypeList.of_json
          (Util.of_option_exn (Json.lookup j "enable_cloudwatch_logs_exports"))
    ; processor_features =
        ProcessorFeatureList.of_json
          (Util.of_option_exn (Json.lookup j "processor_features"))
    ; use_default_processor_features =
        Util.option_map (Json.lookup j "use_default_processor_features") Boolean.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; domain = Util.option_map (Json.lookup j "domain") String.of_json
    ; domain_i_a_m_role_name =
        Util.option_map (Json.lookup j "domain_i_a_m_role_name") String.of_json
    ; replica_mode = Util.option_map (Json.lookup j "replica_mode") ReplicaMode.of_json
    ; max_allocated_storage =
        Util.option_map (Json.lookup j "max_allocated_storage") Integer.of_json
    }
end

module AuthorizeDBSecurityGroupIngressResult = struct
  type t = { d_b_security_group : DBSecurityGroup.t option }

  let make ?d_b_security_group () = { d_b_security_group }

  let parse xml =
    Some
      { d_b_security_group =
          Util.option_bind (Xml.member "DBSecurityGroup" xml) DBSecurityGroup.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_security_group (fun f ->
               Query.Pair ("DBSecurityGroup", DBSecurityGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_security_group (fun f ->
               "d_b_security_group", DBSecurityGroup.to_json f)
         ])

  let of_json j =
    { d_b_security_group =
        Util.option_map (Json.lookup j "d_b_security_group") DBSecurityGroup.of_json
    }
end

module SourceNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateCustomAvailabilityZoneMessage = struct
  type t =
    { custom_availability_zone_name : String.t
    ; existing_vpn_id : String.t option
    ; new_vpn_tunnel_name : String.t option
    ; vpn_tunnel_originator_i_p : String.t option
    }

  let make
      ~custom_availability_zone_name
      ?existing_vpn_id
      ?new_vpn_tunnel_name
      ?vpn_tunnel_originator_i_p
      () =
    { custom_availability_zone_name
    ; existing_vpn_id
    ; new_vpn_tunnel_name
    ; vpn_tunnel_originator_i_p
    }

  let parse xml =
    Some
      { custom_availability_zone_name =
          Xml.required
            "CustomAvailabilityZoneName"
            (Util.option_bind (Xml.member "CustomAvailabilityZoneName" xml) String.parse)
      ; existing_vpn_id = Util.option_bind (Xml.member "ExistingVpnId" xml) String.parse
      ; new_vpn_tunnel_name =
          Util.option_bind (Xml.member "NewVpnTunnelName" xml) String.parse
      ; vpn_tunnel_originator_i_p =
          Util.option_bind (Xml.member "VpnTunnelOriginatorIP" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.vpn_tunnel_originator_i_p (fun f ->
               Query.Pair ("VpnTunnelOriginatorIP", String.to_query f))
         ; Util.option_map v.new_vpn_tunnel_name (fun f ->
               Query.Pair ("NewVpnTunnelName", String.to_query f))
         ; Util.option_map v.existing_vpn_id (fun f ->
               Query.Pair ("ExistingVpnId", String.to_query f))
         ; Some
             (Query.Pair
                ( "CustomAvailabilityZoneName"
                , String.to_query v.custom_availability_zone_name ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.vpn_tunnel_originator_i_p (fun f ->
               "vpn_tunnel_originator_i_p", String.to_json f)
         ; Util.option_map v.new_vpn_tunnel_name (fun f ->
               "new_vpn_tunnel_name", String.to_json f)
         ; Util.option_map v.existing_vpn_id (fun f ->
               "existing_vpn_id", String.to_json f)
         ; Some
             ( "custom_availability_zone_name"
             , String.to_json v.custom_availability_zone_name )
         ])

  let of_json j =
    { custom_availability_zone_name =
        String.of_json
          (Util.of_option_exn (Json.lookup j "custom_availability_zone_name"))
    ; existing_vpn_id = Util.option_map (Json.lookup j "existing_vpn_id") String.of_json
    ; new_vpn_tunnel_name =
        Util.option_map (Json.lookup j "new_vpn_tunnel_name") String.of_json
    ; vpn_tunnel_originator_i_p =
        Util.option_map (Json.lookup j "vpn_tunnel_originator_i_p") String.of_json
    }
end

module ExportTaskAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module OptionGroups = struct
  type t =
    { option_groups_list : OptionGroupsList.t
    ; marker : String.t option
    }

  let make ?(option_groups_list = []) ?marker () = { option_groups_list; marker }

  let parse xml =
    Some
      { option_groups_list =
          Util.of_option
            []
            (Util.option_bind (Xml.member "OptionGroupsList" xml) OptionGroupsList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair
                ("OptionGroupsList.member", OptionGroupsList.to_query v.option_groups_list))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("option_groups_list", OptionGroupsList.to_json v.option_groups_list)
         ])

  let of_json j =
    { option_groups_list =
        OptionGroupsList.of_json (Util.of_option_exn (Json.lookup j "option_groups_list"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module DescribeDBParameterGroupsMessage = struct
  type t =
    { d_b_parameter_group_name : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?d_b_parameter_group_name ?(filters = []) ?max_records ?marker () =
    { d_b_parameter_group_name; filters; max_records; marker }

  let parse xml =
    Some
      { d_b_parameter_group_name =
          Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               Query.Pair ("DBParameterGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               "d_b_parameter_group_name", String.to_json f)
         ])

  let of_json j =
    { d_b_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_parameter_group_name") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module InsufficientDBClusterCapacityFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module PromoteReadReplicaDBClusterMessage = struct
  type t = { d_b_cluster_identifier : String.t }

  let make ~d_b_cluster_identifier () = { d_b_cluster_identifier }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier) ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    }
end

module DBSubnetGroupAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBInstanceResult = struct
  type t = { d_b_instance : DBInstance.t option }

  let make ?d_b_instance () = { d_b_instance }

  let parse xml =
    Some
      { d_b_instance = Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f ->
               Query.Pair ("DBInstance", DBInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f -> "d_b_instance", DBInstance.to_json f)
         ])

  let of_json j =
    { d_b_instance = Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json }
end

module DescribeDBClusterSnapshotsMessage = struct
  type t =
    { d_b_cluster_identifier : String.t option
    ; d_b_cluster_snapshot_identifier : String.t option
    ; snapshot_type : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    ; include_shared : Boolean.t option
    ; include_public : Boolean.t option
    }

  let make
      ?d_b_cluster_identifier
      ?d_b_cluster_snapshot_identifier
      ?snapshot_type
      ?(filters = [])
      ?max_records
      ?marker
      ?include_shared
      ?include_public
      () =
    { d_b_cluster_identifier
    ; d_b_cluster_snapshot_identifier
    ; snapshot_type
    ; filters
    ; max_records
    ; marker
    ; include_shared
    ; include_public
    }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse
      ; d_b_cluster_snapshot_identifier =
          Util.option_bind (Xml.member "DBClusterSnapshotIdentifier" xml) String.parse
      ; snapshot_type = Util.option_bind (Xml.member "SnapshotType" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; include_shared = Util.option_bind (Xml.member "IncludeShared" xml) Boolean.parse
      ; include_public = Util.option_bind (Xml.member "IncludePublic" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.include_public (fun f ->
               Query.Pair ("IncludePublic", Boolean.to_query f))
         ; Util.option_map v.include_shared (fun f ->
               Query.Pair ("IncludeShared", Boolean.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.snapshot_type (fun f ->
               Query.Pair ("SnapshotType", String.to_query f))
         ; Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
               Query.Pair ("DBClusterSnapshotIdentifier", String.to_query f))
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               Query.Pair ("DBClusterIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.include_public (fun f -> "include_public", Boolean.to_json f)
         ; Util.option_map v.include_shared (fun f -> "include_shared", Boolean.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.snapshot_type (fun f -> "snapshot_type", String.to_json f)
         ; Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
               "d_b_cluster_snapshot_identifier", String.to_json f)
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               "d_b_cluster_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_identifier") String.of_json
    ; d_b_cluster_snapshot_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_snapshot_identifier") String.of_json
    ; snapshot_type = Util.option_map (Json.lookup j "snapshot_type") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; include_shared = Util.option_map (Json.lookup j "include_shared") Boolean.of_json
    ; include_public = Util.option_map (Json.lookup j "include_public") Boolean.of_json
    }
end

module ModifyDBProxyRequest = struct
  type t =
    { d_b_proxy_name : String.t
    ; new_d_b_proxy_name : String.t option
    ; auth : UserAuthConfigList.t
    ; require_t_l_s : Boolean.t option
    ; idle_client_timeout : Integer.t option
    ; debug_logging : Boolean.t option
    ; role_arn : String.t option
    ; security_groups : StringList.t
    }

  let make
      ~d_b_proxy_name
      ?new_d_b_proxy_name
      ?(auth = [])
      ?require_t_l_s
      ?idle_client_timeout
      ?debug_logging
      ?role_arn
      ?(security_groups = [])
      () =
    { d_b_proxy_name
    ; new_d_b_proxy_name
    ; auth
    ; require_t_l_s
    ; idle_client_timeout
    ; debug_logging
    ; role_arn
    ; security_groups
    }

  let parse xml =
    Some
      { d_b_proxy_name =
          Xml.required
            "DBProxyName"
            (Util.option_bind (Xml.member "DBProxyName" xml) String.parse)
      ; new_d_b_proxy_name =
          Util.option_bind (Xml.member "NewDBProxyName" xml) String.parse
      ; auth =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Auth" xml) UserAuthConfigList.parse)
      ; require_t_l_s = Util.option_bind (Xml.member "RequireTLS" xml) Boolean.parse
      ; idle_client_timeout =
          Util.option_bind (Xml.member "IdleClientTimeout" xml) Integer.parse
      ; debug_logging = Util.option_bind (Xml.member "DebugLogging" xml) Boolean.parse
      ; role_arn = Util.option_bind (Xml.member "RoleArn" xml) String.parse
      ; security_groups =
          Util.of_option
            []
            (Util.option_bind (Xml.member "SecurityGroups" xml) StringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("SecurityGroups.member", StringList.to_query v.security_groups))
         ; Util.option_map v.role_arn (fun f -> Query.Pair ("RoleArn", String.to_query f))
         ; Util.option_map v.debug_logging (fun f ->
               Query.Pair ("DebugLogging", Boolean.to_query f))
         ; Util.option_map v.idle_client_timeout (fun f ->
               Query.Pair ("IdleClientTimeout", Integer.to_query f))
         ; Util.option_map v.require_t_l_s (fun f ->
               Query.Pair ("RequireTLS", Boolean.to_query f))
         ; Some (Query.Pair ("Auth.member", UserAuthConfigList.to_query v.auth))
         ; Util.option_map v.new_d_b_proxy_name (fun f ->
               Query.Pair ("NewDBProxyName", String.to_query f))
         ; Some (Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("security_groups", StringList.to_json v.security_groups)
         ; Util.option_map v.role_arn (fun f -> "role_arn", String.to_json f)
         ; Util.option_map v.debug_logging (fun f -> "debug_logging", Boolean.to_json f)
         ; Util.option_map v.idle_client_timeout (fun f ->
               "idle_client_timeout", Integer.to_json f)
         ; Util.option_map v.require_t_l_s (fun f -> "require_t_l_s", Boolean.to_json f)
         ; Some ("auth", UserAuthConfigList.to_json v.auth)
         ; Util.option_map v.new_d_b_proxy_name (fun f ->
               "new_d_b_proxy_name", String.to_json f)
         ; Some ("d_b_proxy_name", String.to_json v.d_b_proxy_name)
         ])

  let of_json j =
    { d_b_proxy_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_proxy_name"))
    ; new_d_b_proxy_name =
        Util.option_map (Json.lookup j "new_d_b_proxy_name") String.of_json
    ; auth = UserAuthConfigList.of_json (Util.of_option_exn (Json.lookup j "auth"))
    ; require_t_l_s = Util.option_map (Json.lookup j "require_t_l_s") Boolean.of_json
    ; idle_client_timeout =
        Util.option_map (Json.lookup j "idle_client_timeout") Integer.of_json
    ; debug_logging = Util.option_map (Json.lookup j "debug_logging") Boolean.of_json
    ; role_arn = Util.option_map (Json.lookup j "role_arn") String.of_json
    ; security_groups =
        StringList.of_json (Util.of_option_exn (Json.lookup j "security_groups"))
    }
end

module ModifyEventSubscriptionMessage = struct
  type t =
    { subscription_name : String.t
    ; sns_topic_arn : String.t option
    ; source_type : String.t option
    ; event_categories : EventCategoriesList.t
    ; enabled : Boolean.t option
    }

  let make
      ~subscription_name
      ?sns_topic_arn
      ?source_type
      ?(event_categories = [])
      ?enabled
      () =
    { subscription_name; sns_topic_arn; source_type; event_categories; enabled }

  let parse xml =
    Some
      { subscription_name =
          Xml.required
            "SubscriptionName"
            (Util.option_bind (Xml.member "SubscriptionName" xml) String.parse)
      ; sns_topic_arn = Util.option_bind (Xml.member "SnsTopicArn" xml) String.parse
      ; source_type = Util.option_bind (Xml.member "SourceType" xml) String.parse
      ; event_categories =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EventCategories" xml)
               EventCategoriesList.parse)
      ; enabled = Util.option_bind (Xml.member "Enabled" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.enabled (fun f -> Query.Pair ("Enabled", Boolean.to_query f))
         ; Some
             (Query.Pair
                ("EventCategories.member", EventCategoriesList.to_query v.event_categories))
         ; Util.option_map v.source_type (fun f ->
               Query.Pair ("SourceType", String.to_query f))
         ; Util.option_map v.sns_topic_arn (fun f ->
               Query.Pair ("SnsTopicArn", String.to_query f))
         ; Some (Query.Pair ("SubscriptionName", String.to_query v.subscription_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.enabled (fun f -> "enabled", Boolean.to_json f)
         ; Some ("event_categories", EventCategoriesList.to_json v.event_categories)
         ; Util.option_map v.source_type (fun f -> "source_type", String.to_json f)
         ; Util.option_map v.sns_topic_arn (fun f -> "sns_topic_arn", String.to_json f)
         ; Some ("subscription_name", String.to_json v.subscription_name)
         ])

  let of_json j =
    { subscription_name =
        String.of_json (Util.of_option_exn (Json.lookup j "subscription_name"))
    ; sns_topic_arn = Util.option_map (Json.lookup j "sns_topic_arn") String.of_json
    ; source_type = Util.option_map (Json.lookup j "source_type") String.of_json
    ; event_categories =
        EventCategoriesList.of_json
          (Util.of_option_exn (Json.lookup j "event_categories"))
    ; enabled = Util.option_map (Json.lookup j "enabled") Boolean.of_json
    }
end

module SourceRegionMessage = struct
  type t =
    { marker : String.t option
    ; source_regions : SourceRegionList.t
    }

  let make ?marker ?(source_regions = []) () = { marker; source_regions }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; source_regions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "SourceRegions" xml) SourceRegionList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("SourceRegions.member", SourceRegionList.to_query v.source_regions))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("source_regions", SourceRegionList.to_json v.source_regions)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; source_regions =
        SourceRegionList.of_json (Util.of_option_exn (Json.lookup j "source_regions"))
    }
end

module PurchaseReservedDBInstancesOfferingMessage = struct
  type t =
    { reserved_d_b_instances_offering_id : String.t
    ; reserved_d_b_instance_id : String.t option
    ; d_b_instance_count : Integer.t option
    ; tags : TagList.t
    }

  let make
      ~reserved_d_b_instances_offering_id
      ?reserved_d_b_instance_id
      ?d_b_instance_count
      ?(tags = [])
      () =
    { reserved_d_b_instances_offering_id
    ; reserved_d_b_instance_id
    ; d_b_instance_count
    ; tags
    }

  let parse xml =
    Some
      { reserved_d_b_instances_offering_id =
          Xml.required
            "ReservedDBInstancesOfferingId"
            (Util.option_bind
               (Xml.member "ReservedDBInstancesOfferingId" xml)
               String.parse)
      ; reserved_d_b_instance_id =
          Util.option_bind (Xml.member "ReservedDBInstanceId" xml) String.parse
      ; d_b_instance_count =
          Util.option_bind (Xml.member "DBInstanceCount" xml) Integer.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.d_b_instance_count (fun f ->
               Query.Pair ("DBInstanceCount", Integer.to_query f))
         ; Util.option_map v.reserved_d_b_instance_id (fun f ->
               Query.Pair ("ReservedDBInstanceId", String.to_query f))
         ; Some
             (Query.Pair
                ( "ReservedDBInstancesOfferingId"
                , String.to_query v.reserved_d_b_instances_offering_id ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.d_b_instance_count (fun f ->
               "d_b_instance_count", Integer.to_json f)
         ; Util.option_map v.reserved_d_b_instance_id (fun f ->
               "reserved_d_b_instance_id", String.to_json f)
         ; Some
             ( "reserved_d_b_instances_offering_id"
             , String.to_json v.reserved_d_b_instances_offering_id )
         ])

  let of_json j =
    { reserved_d_b_instances_offering_id =
        String.of_json
          (Util.of_option_exn (Json.lookup j "reserved_d_b_instances_offering_id"))
    ; reserved_d_b_instance_id =
        Util.option_map (Json.lookup j "reserved_d_b_instance_id") String.of_json
    ; d_b_instance_count =
        Util.option_map (Json.lookup j "d_b_instance_count") Integer.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module ExportTaskNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBParameterGroupMessage = struct
  type t =
    { d_b_parameter_group_name : String.t
    ; d_b_parameter_group_family : String.t
    ; description : String.t
    ; tags : TagList.t
    }

  let make
      ~d_b_parameter_group_name
      ~d_b_parameter_group_family
      ~description
      ?(tags = [])
      () =
    { d_b_parameter_group_name; d_b_parameter_group_family; description; tags }

  let parse xml =
    Some
      { d_b_parameter_group_name =
          Xml.required
            "DBParameterGroupName"
            (Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse)
      ; d_b_parameter_group_family =
          Xml.required
            "DBParameterGroupFamily"
            (Util.option_bind (Xml.member "DBParameterGroupFamily" xml) String.parse)
      ; description =
          Xml.required
            "Description"
            (Util.option_bind (Xml.member "Description" xml) String.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some (Query.Pair ("Description", String.to_query v.description))
         ; Some
             (Query.Pair
                ("DBParameterGroupFamily", String.to_query v.d_b_parameter_group_family))
         ; Some
             (Query.Pair
                ("DBParameterGroupName", String.to_query v.d_b_parameter_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some ("description", String.to_json v.description)
         ; Some ("d_b_parameter_group_family", String.to_json v.d_b_parameter_group_family)
         ; Some ("d_b_parameter_group_name", String.to_json v.d_b_parameter_group_name)
         ])

  let of_json j =
    { d_b_parameter_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_parameter_group_name"))
    ; d_b_parameter_group_family =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_parameter_group_family"))
    ; description = String.of_json (Util.of_option_exn (Json.lookup j "description"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module DescribeDBParametersMessage = struct
  type t =
    { d_b_parameter_group_name : String.t
    ; source : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ~d_b_parameter_group_name ?source ?(filters = []) ?max_records ?marker () =
    { d_b_parameter_group_name; source; filters; max_records; marker }

  let parse xml =
    Some
      { d_b_parameter_group_name =
          Xml.required
            "DBParameterGroupName"
            (Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse)
      ; source = Util.option_bind (Xml.member "Source" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.source (fun f -> Query.Pair ("Source", String.to_query f))
         ; Some
             (Query.Pair
                ("DBParameterGroupName", String.to_query v.d_b_parameter_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.source (fun f -> "source", String.to_json f)
         ; Some ("d_b_parameter_group_name", String.to_json v.d_b_parameter_group_name)
         ])

  let of_json j =
    { d_b_parameter_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_parameter_group_name"))
    ; source = Util.option_map (Json.lookup j "source") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module StartDBClusterMessage = struct
  type t = { d_b_cluster_identifier : String.t }

  let make ~d_b_cluster_identifier () = { d_b_cluster_identifier }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier) ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    }
end

module DBParameterGroupNameMessage = struct
  type t = { d_b_parameter_group_name : String.t option }

  let make ?d_b_parameter_group_name () = { d_b_parameter_group_name }

  let parse xml =
    Some
      { d_b_parameter_group_name =
          Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_parameter_group_name (fun f ->
               Query.Pair ("DBParameterGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_parameter_group_name (fun f ->
               "d_b_parameter_group_name", String.to_json f)
         ])

  let of_json j =
    { d_b_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_parameter_group_name") String.of_json
    }
end

module DBClusterSnapshotMessage = struct
  type t =
    { marker : String.t option
    ; d_b_cluster_snapshots : DBClusterSnapshotList.t
    }

  let make ?marker ?(d_b_cluster_snapshots = []) () = { marker; d_b_cluster_snapshots }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; d_b_cluster_snapshots =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBClusterSnapshots" xml)
               DBClusterSnapshotList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBClusterSnapshots.member"
                , DBClusterSnapshotList.to_query v.d_b_cluster_snapshots ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "d_b_cluster_snapshots"
             , DBClusterSnapshotList.to_json v.d_b_cluster_snapshots )
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; d_b_cluster_snapshots =
        DBClusterSnapshotList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_snapshots"))
    }
end

module ListTagsForResourceMessage = struct
  type t =
    { resource_name : String.t
    ; filters : FilterList.t
    }

  let make ~resource_name ?(filters = []) () = { resource_name; filters }

  let parse xml =
    Some
      { resource_name =
          Xml.required
            "ResourceName"
            (Util.option_bind (Xml.member "ResourceName" xml) String.parse)
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Some (Query.Pair ("ResourceName", String.to_query v.resource_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("filters", FilterList.to_json v.filters)
         ; Some ("resource_name", String.to_json v.resource_name)
         ])

  let of_json j =
    { resource_name = String.of_json (Util.of_option_exn (Json.lookup j "resource_name"))
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    }
end

module DescribeEngineDefaultParametersMessage = struct
  type t =
    { d_b_parameter_group_family : String.t
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ~d_b_parameter_group_family ?(filters = []) ?max_records ?marker () =
    { d_b_parameter_group_family; filters; max_records; marker }

  let parse xml =
    Some
      { d_b_parameter_group_family =
          Xml.required
            "DBParameterGroupFamily"
            (Util.option_bind (Xml.member "DBParameterGroupFamily" xml) String.parse)
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Some
             (Query.Pair
                ("DBParameterGroupFamily", String.to_query v.d_b_parameter_group_family))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Some ("d_b_parameter_group_family", String.to_json v.d_b_parameter_group_family)
         ])

  let of_json j =
    { d_b_parameter_group_family =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_parameter_group_family"))
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module InstanceQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBSubnetGroupResult = struct
  type t = { d_b_subnet_group : DBSubnetGroup.t option }

  let make ?d_b_subnet_group () = { d_b_subnet_group }

  let parse xml =
    Some
      { d_b_subnet_group =
          Util.option_bind (Xml.member "DBSubnetGroup" xml) DBSubnetGroup.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_subnet_group (fun f ->
               Query.Pair ("DBSubnetGroup", DBSubnetGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_subnet_group (fun f ->
               "d_b_subnet_group", DBSubnetGroup.to_json f)
         ])

  let of_json j =
    { d_b_subnet_group =
        Util.option_map (Json.lookup j "d_b_subnet_group") DBSubnetGroup.of_json
    }
end

module ModifyDBClusterResult = struct
  type t = { d_b_cluster : DBCluster.t option }

  let make ?d_b_cluster () = { d_b_cluster }

  let parse xml =
    Some { d_b_cluster = Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f ->
               Query.Pair ("DBCluster", DBCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f -> "d_b_cluster", DBCluster.to_json f) ])

  let of_json j =
    { d_b_cluster = Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json }
end

module DescribeDBClusterEndpointsMessage = struct
  type t =
    { d_b_cluster_identifier : String.t option
    ; d_b_cluster_endpoint_identifier : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ?d_b_cluster_identifier
      ?d_b_cluster_endpoint_identifier
      ?(filters = [])
      ?max_records
      ?marker
      () =
    { d_b_cluster_identifier
    ; d_b_cluster_endpoint_identifier
    ; filters
    ; max_records
    ; marker
    }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse
      ; d_b_cluster_endpoint_identifier =
          Util.option_bind (Xml.member "DBClusterEndpointIdentifier" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.d_b_cluster_endpoint_identifier (fun f ->
               Query.Pair ("DBClusterEndpointIdentifier", String.to_query f))
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               Query.Pair ("DBClusterIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.d_b_cluster_endpoint_identifier (fun f ->
               "d_b_cluster_endpoint_identifier", String.to_json f)
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               "d_b_cluster_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_identifier") String.of_json
    ; d_b_cluster_endpoint_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_endpoint_identifier") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module IamRoleMissingPermissionsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyEventSubscriptionResult = struct
  type t = { event_subscription : EventSubscription.t option }

  let make ?event_subscription () = { event_subscription }

  let parse xml =
    Some
      { event_subscription =
          Util.option_bind (Xml.member "EventSubscription" xml) EventSubscription.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.event_subscription (fun f ->
               Query.Pair ("EventSubscription", EventSubscription.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.event_subscription (fun f ->
               "event_subscription", EventSubscription.to_json f)
         ])

  let of_json j =
    { event_subscription =
        Util.option_map (Json.lookup j "event_subscription") EventSubscription.of_json
    }
end

module DBParameterGroupAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBProxyResponse = struct
  type t = { d_b_proxy : DBProxy.t option }

  let make ?d_b_proxy () = { d_b_proxy }

  let parse xml =
    Some { d_b_proxy = Util.option_bind (Xml.member "DBProxy" xml) DBProxy.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_proxy (fun f ->
               Query.Pair ("DBProxy", DBProxy.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_proxy (fun f -> "d_b_proxy", DBProxy.to_json f) ])

  let of_json j =
    { d_b_proxy = Util.option_map (Json.lookup j "d_b_proxy") DBProxy.of_json }
end

module DeleteDBClusterResult = struct
  type t = { d_b_cluster : DBCluster.t option }

  let make ?d_b_cluster () = { d_b_cluster }

  let parse xml =
    Some { d_b_cluster = Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f ->
               Query.Pair ("DBCluster", DBCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f -> "d_b_cluster", DBCluster.to_json f) ])

  let of_json j =
    { d_b_cluster = Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json }
end

module DescribeDBInstanceAutomatedBackupsMessage = struct
  type t =
    { dbi_resource_id : String.t option
    ; d_b_instance_identifier : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ?dbi_resource_id
      ?d_b_instance_identifier
      ?(filters = [])
      ?max_records
      ?marker
      () =
    { dbi_resource_id; d_b_instance_identifier; filters; max_records; marker }

  let parse xml =
    Some
      { dbi_resource_id = Util.option_bind (Xml.member "DbiResourceId" xml) String.parse
      ; d_b_instance_identifier =
          Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               Query.Pair ("DBInstanceIdentifier", String.to_query f))
         ; Util.option_map v.dbi_resource_id (fun f ->
               Query.Pair ("DbiResourceId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               "d_b_instance_identifier", String.to_json f)
         ; Util.option_map v.dbi_resource_id (fun f ->
               "dbi_resource_id", String.to_json f)
         ])

  let of_json j =
    { dbi_resource_id = Util.option_map (Json.lookup j "dbi_resource_id") String.of_json
    ; d_b_instance_identifier =
        Util.option_map (Json.lookup j "d_b_instance_identifier") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module InvalidDBClusterSnapshotStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBClusterRoleNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeDBSecurityGroupsMessage = struct
  type t =
    { d_b_security_group_name : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?d_b_security_group_name ?(filters = []) ?max_records ?marker () =
    { d_b_security_group_name; filters; max_records; marker }

  let parse xml =
    Some
      { d_b_security_group_name =
          Util.option_bind (Xml.member "DBSecurityGroupName" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.d_b_security_group_name (fun f ->
               Query.Pair ("DBSecurityGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.d_b_security_group_name (fun f ->
               "d_b_security_group_name", String.to_json f)
         ])

  let of_json j =
    { d_b_security_group_name =
        Util.option_map (Json.lookup j "d_b_security_group_name") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module SharedSnapshotQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBSnapshotResult = struct
  type t = { d_b_snapshot : DBSnapshot.t option }

  let make ?d_b_snapshot () = { d_b_snapshot }

  let parse xml =
    Some
      { d_b_snapshot = Util.option_bind (Xml.member "DBSnapshot" xml) DBSnapshot.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot (fun f ->
               Query.Pair ("DBSnapshot", DBSnapshot.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot (fun f -> "d_b_snapshot", DBSnapshot.to_json f)
         ])

  let of_json j =
    { d_b_snapshot = Util.option_map (Json.lookup j "d_b_snapshot") DBSnapshot.of_json }
end

module ModifyDBProxyTargetGroupRequest = struct
  type t =
    { target_group_name : String.t
    ; d_b_proxy_name : String.t
    ; connection_pool_config : ConnectionPoolConfiguration.t option
    ; new_name : String.t option
    }

  let make ~target_group_name ~d_b_proxy_name ?connection_pool_config ?new_name () =
    { target_group_name; d_b_proxy_name; connection_pool_config; new_name }

  let parse xml =
    Some
      { target_group_name =
          Xml.required
            "TargetGroupName"
            (Util.option_bind (Xml.member "TargetGroupName" xml) String.parse)
      ; d_b_proxy_name =
          Xml.required
            "DBProxyName"
            (Util.option_bind (Xml.member "DBProxyName" xml) String.parse)
      ; connection_pool_config =
          Util.option_bind
            (Xml.member "ConnectionPoolConfig" xml)
            ConnectionPoolConfiguration.parse
      ; new_name = Util.option_bind (Xml.member "NewName" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.new_name (fun f -> Query.Pair ("NewName", String.to_query f))
         ; Util.option_map v.connection_pool_config (fun f ->
               Query.Pair ("ConnectionPoolConfig", ConnectionPoolConfiguration.to_query f))
         ; Some (Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name))
         ; Some (Query.Pair ("TargetGroupName", String.to_query v.target_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.new_name (fun f -> "new_name", String.to_json f)
         ; Util.option_map v.connection_pool_config (fun f ->
               "connection_pool_config", ConnectionPoolConfiguration.to_json f)
         ; Some ("d_b_proxy_name", String.to_json v.d_b_proxy_name)
         ; Some ("target_group_name", String.to_json v.target_group_name)
         ])

  let of_json j =
    { target_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "target_group_name"))
    ; d_b_proxy_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_proxy_name"))
    ; connection_pool_config =
        Util.option_map
          (Json.lookup j "connection_pool_config")
          ConnectionPoolConfiguration.of_json
    ; new_name = Util.option_map (Json.lookup j "new_name") String.of_json
    }
end

module DBProxyQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeDBSubnetGroupsMessage = struct
  type t =
    { d_b_subnet_group_name : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?d_b_subnet_group_name ?(filters = []) ?max_records ?marker () =
    { d_b_subnet_group_name; filters; max_records; marker }

  let parse xml =
    Some
      { d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ])

  let of_json j =
    { d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module ReservedDBInstancesOfferingNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidExportOnlyFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeCustomAvailabilityZonesMessage = struct
  type t =
    { custom_availability_zone_id : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?custom_availability_zone_id ?(filters = []) ?max_records ?marker () =
    { custom_availability_zone_id; filters; max_records; marker }

  let parse xml =
    Some
      { custom_availability_zone_id =
          Util.option_bind (Xml.member "CustomAvailabilityZoneId" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.custom_availability_zone_id (fun f ->
               Query.Pair ("CustomAvailabilityZoneId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.custom_availability_zone_id (fun f ->
               "custom_availability_zone_id", String.to_json f)
         ])

  let of_json j =
    { custom_availability_zone_id =
        Util.option_map (Json.lookup j "custom_availability_zone_id") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module DescribeDBClusterSnapshotAttributesMessage = struct
  type t = { d_b_cluster_snapshot_identifier : String.t }

  let make ~d_b_cluster_snapshot_identifier () = { d_b_cluster_snapshot_identifier }

  let parse xml =
    Some
      { d_b_cluster_snapshot_identifier =
          Xml.required
            "DBClusterSnapshotIdentifier"
            (Util.option_bind (Xml.member "DBClusterSnapshotIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBClusterSnapshotIdentifier"
                , String.to_query v.d_b_cluster_snapshot_identifier ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "d_b_cluster_snapshot_identifier"
             , String.to_json v.d_b_cluster_snapshot_identifier )
         ])

  let of_json j =
    { d_b_cluster_snapshot_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_snapshot_identifier"))
    }
end

module DescribeDBClusterBacktracksMessage = struct
  type t =
    { d_b_cluster_identifier : String.t
    ; backtrack_identifier : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ~d_b_cluster_identifier
      ?backtrack_identifier
      ?(filters = [])
      ?max_records
      ?marker
      () =
    { d_b_cluster_identifier; backtrack_identifier; filters; max_records; marker }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; backtrack_identifier =
          Util.option_bind (Xml.member "BacktrackIdentifier" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.backtrack_identifier (fun f ->
               Query.Pair ("BacktrackIdentifier", String.to_query f))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.backtrack_identifier (fun f ->
               "backtrack_identifier", String.to_json f)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; backtrack_identifier =
        Util.option_map (Json.lookup j "backtrack_identifier") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module TagListMessage = struct
  type t = { tag_list : TagList.t }

  let make ?(tag_list = []) () = { tag_list }

  let parse xml =
    Some
      { tag_list =
          Util.of_option [] (Util.option_bind (Xml.member "TagList" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("TagList.member", TagList.to_query v.tag_list)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("tag_list", TagList.to_json v.tag_list) ])

  let of_json j =
    { tag_list = TagList.of_json (Util.of_option_exn (Json.lookup j "tag_list")) }
end

module RebootDBInstanceMessage = struct
  type t =
    { d_b_instance_identifier : String.t
    ; force_failover : Boolean.t option
    }

  let make ~d_b_instance_identifier ?force_failover () =
    { d_b_instance_identifier; force_failover }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; force_failover = Util.option_bind (Xml.member "ForceFailover" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.force_failover (fun f ->
               Query.Pair ("ForceFailover", Boolean.to_query f))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.force_failover (fun f -> "force_failover", Boolean.to_json f)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; force_failover = Util.option_map (Json.lookup j "force_failover") Boolean.of_json
    }
end

module AuthorizationAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyDBClusterMessage = struct
  type t =
    { d_b_cluster_identifier : String.t
    ; new_d_b_cluster_identifier : String.t option
    ; apply_immediately : Boolean.t option
    ; backup_retention_period : Integer.t option
    ; d_b_cluster_parameter_group_name : String.t option
    ; vpc_security_group_ids : VpcSecurityGroupIdList.t
    ; port : Integer.t option
    ; master_user_password : String.t option
    ; option_group_name : String.t option
    ; preferred_backup_window : String.t option
    ; preferred_maintenance_window : String.t option
    ; enable_i_a_m_database_authentication : Boolean.t option
    ; backtrack_window : Long.t option
    ; cloudwatch_logs_export_configuration : CloudwatchLogsExportConfiguration.t option
    ; engine_version : String.t option
    ; allow_major_version_upgrade : Boolean.t option
    ; d_b_instance_parameter_group_name : String.t option
    ; domain : String.t option
    ; domain_i_a_m_role_name : String.t option
    ; scaling_configuration : ScalingConfiguration.t option
    ; deletion_protection : Boolean.t option
    ; enable_http_endpoint : Boolean.t option
    ; copy_tags_to_snapshot : Boolean.t option
    ; enable_global_write_forwarding : Boolean.t option
    }

  let make
      ~d_b_cluster_identifier
      ?new_d_b_cluster_identifier
      ?apply_immediately
      ?backup_retention_period
      ?d_b_cluster_parameter_group_name
      ?(vpc_security_group_ids = [])
      ?port
      ?master_user_password
      ?option_group_name
      ?preferred_backup_window
      ?preferred_maintenance_window
      ?enable_i_a_m_database_authentication
      ?backtrack_window
      ?cloudwatch_logs_export_configuration
      ?engine_version
      ?allow_major_version_upgrade
      ?d_b_instance_parameter_group_name
      ?domain
      ?domain_i_a_m_role_name
      ?scaling_configuration
      ?deletion_protection
      ?enable_http_endpoint
      ?copy_tags_to_snapshot
      ?enable_global_write_forwarding
      () =
    { d_b_cluster_identifier
    ; new_d_b_cluster_identifier
    ; apply_immediately
    ; backup_retention_period
    ; d_b_cluster_parameter_group_name
    ; vpc_security_group_ids
    ; port
    ; master_user_password
    ; option_group_name
    ; preferred_backup_window
    ; preferred_maintenance_window
    ; enable_i_a_m_database_authentication
    ; backtrack_window
    ; cloudwatch_logs_export_configuration
    ; engine_version
    ; allow_major_version_upgrade
    ; d_b_instance_parameter_group_name
    ; domain
    ; domain_i_a_m_role_name
    ; scaling_configuration
    ; deletion_protection
    ; enable_http_endpoint
    ; copy_tags_to_snapshot
    ; enable_global_write_forwarding
    }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; new_d_b_cluster_identifier =
          Util.option_bind (Xml.member "NewDBClusterIdentifier" xml) String.parse
      ; apply_immediately =
          Util.option_bind (Xml.member "ApplyImmediately" xml) Boolean.parse
      ; backup_retention_period =
          Util.option_bind (Xml.member "BackupRetentionPeriod" xml) Integer.parse
      ; d_b_cluster_parameter_group_name =
          Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupIds" xml)
               VpcSecurityGroupIdList.parse)
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; master_user_password =
          Util.option_bind (Xml.member "MasterUserPassword" xml) String.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; preferred_backup_window =
          Util.option_bind (Xml.member "PreferredBackupWindow" xml) String.parse
      ; preferred_maintenance_window =
          Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml) String.parse
      ; enable_i_a_m_database_authentication =
          Util.option_bind
            (Xml.member "EnableIAMDatabaseAuthentication" xml)
            Boolean.parse
      ; backtrack_window = Util.option_bind (Xml.member "BacktrackWindow" xml) Long.parse
      ; cloudwatch_logs_export_configuration =
          Util.option_bind
            (Xml.member "CloudwatchLogsExportConfiguration" xml)
            CloudwatchLogsExportConfiguration.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; allow_major_version_upgrade =
          Util.option_bind (Xml.member "AllowMajorVersionUpgrade" xml) Boolean.parse
      ; d_b_instance_parameter_group_name =
          Util.option_bind (Xml.member "DBInstanceParameterGroupName" xml) String.parse
      ; domain = Util.option_bind (Xml.member "Domain" xml) String.parse
      ; domain_i_a_m_role_name =
          Util.option_bind (Xml.member "DomainIAMRoleName" xml) String.parse
      ; scaling_configuration =
          Util.option_bind
            (Xml.member "ScalingConfiguration" xml)
            ScalingConfiguration.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; enable_http_endpoint =
          Util.option_bind (Xml.member "EnableHttpEndpoint" xml) Boolean.parse
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; enable_global_write_forwarding =
          Util.option_bind (Xml.member "EnableGlobalWriteForwarding" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.enable_global_write_forwarding (fun f ->
               Query.Pair ("EnableGlobalWriteForwarding", Boolean.to_query f))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Util.option_map v.enable_http_endpoint (fun f ->
               Query.Pair ("EnableHttpEndpoint", Boolean.to_query f))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.scaling_configuration (fun f ->
               Query.Pair ("ScalingConfiguration", ScalingConfiguration.to_query f))
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               Query.Pair ("DomainIAMRoleName", String.to_query f))
         ; Util.option_map v.domain (fun f -> Query.Pair ("Domain", String.to_query f))
         ; Util.option_map v.d_b_instance_parameter_group_name (fun f ->
               Query.Pair ("DBInstanceParameterGroupName", String.to_query f))
         ; Util.option_map v.allow_major_version_upgrade (fun f ->
               Query.Pair ("AllowMajorVersionUpgrade", Boolean.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.cloudwatch_logs_export_configuration (fun f ->
               Query.Pair
                 ( "CloudwatchLogsExportConfiguration"
                 , CloudwatchLogsExportConfiguration.to_query f ))
         ; Util.option_map v.backtrack_window (fun f ->
               Query.Pair ("BacktrackWindow", Long.to_query f))
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Util.option_map v.preferred_backup_window (fun f ->
               Query.Pair ("PreferredBackupWindow", String.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.master_user_password (fun f ->
               Query.Pair ("MasterUserPassword", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               Query.Pair ("DBClusterParameterGroupName", String.to_query f))
         ; Util.option_map v.backup_retention_period (fun f ->
               Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
         ; Util.option_map v.apply_immediately (fun f ->
               Query.Pair ("ApplyImmediately", Boolean.to_query f))
         ; Util.option_map v.new_d_b_cluster_identifier (fun f ->
               Query.Pair ("NewDBClusterIdentifier", String.to_query f))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.enable_global_write_forwarding (fun f ->
               "enable_global_write_forwarding", Boolean.to_json f)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Util.option_map v.enable_http_endpoint (fun f ->
               "enable_http_endpoint", Boolean.to_json f)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.scaling_configuration (fun f ->
               "scaling_configuration", ScalingConfiguration.to_json f)
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               "domain_i_a_m_role_name", String.to_json f)
         ; Util.option_map v.domain (fun f -> "domain", String.to_json f)
         ; Util.option_map v.d_b_instance_parameter_group_name (fun f ->
               "d_b_instance_parameter_group_name", String.to_json f)
         ; Util.option_map v.allow_major_version_upgrade (fun f ->
               "allow_major_version_upgrade", Boolean.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.cloudwatch_logs_export_configuration (fun f ->
               ( "cloudwatch_logs_export_configuration"
               , CloudwatchLogsExportConfiguration.to_json f ))
         ; Util.option_map v.backtrack_window (fun f ->
               "backtrack_window", Long.to_json f)
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               "enable_i_a_m_database_authentication", Boolean.to_json f)
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               "preferred_maintenance_window", String.to_json f)
         ; Util.option_map v.preferred_backup_window (fun f ->
               "preferred_backup_window", String.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.master_user_password (fun f ->
               "master_user_password", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Some
             ( "vpc_security_group_ids"
             , VpcSecurityGroupIdList.to_json v.vpc_security_group_ids )
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               "d_b_cluster_parameter_group_name", String.to_json f)
         ; Util.option_map v.backup_retention_period (fun f ->
               "backup_retention_period", Integer.to_json f)
         ; Util.option_map v.apply_immediately (fun f ->
               "apply_immediately", Boolean.to_json f)
         ; Util.option_map v.new_d_b_cluster_identifier (fun f ->
               "new_d_b_cluster_identifier", String.to_json f)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; new_d_b_cluster_identifier =
        Util.option_map (Json.lookup j "new_d_b_cluster_identifier") String.of_json
    ; apply_immediately =
        Util.option_map (Json.lookup j "apply_immediately") Boolean.of_json
    ; backup_retention_period =
        Util.option_map (Json.lookup j "backup_retention_period") Integer.of_json
    ; d_b_cluster_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name") String.of_json
    ; vpc_security_group_ids =
        VpcSecurityGroupIdList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; master_user_password =
        Util.option_map (Json.lookup j "master_user_password") String.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; preferred_backup_window =
        Util.option_map (Json.lookup j "preferred_backup_window") String.of_json
    ; preferred_maintenance_window =
        Util.option_map (Json.lookup j "preferred_maintenance_window") String.of_json
    ; enable_i_a_m_database_authentication =
        Util.option_map
          (Json.lookup j "enable_i_a_m_database_authentication")
          Boolean.of_json
    ; backtrack_window = Util.option_map (Json.lookup j "backtrack_window") Long.of_json
    ; cloudwatch_logs_export_configuration =
        Util.option_map
          (Json.lookup j "cloudwatch_logs_export_configuration")
          CloudwatchLogsExportConfiguration.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; allow_major_version_upgrade =
        Util.option_map (Json.lookup j "allow_major_version_upgrade") Boolean.of_json
    ; d_b_instance_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_instance_parameter_group_name") String.of_json
    ; domain = Util.option_map (Json.lookup j "domain") String.of_json
    ; domain_i_a_m_role_name =
        Util.option_map (Json.lookup j "domain_i_a_m_role_name") String.of_json
    ; scaling_configuration =
        Util.option_map
          (Json.lookup j "scaling_configuration")
          ScalingConfiguration.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; enable_http_endpoint =
        Util.option_map (Json.lookup j "enable_http_endpoint") Boolean.of_json
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; enable_global_write_forwarding =
        Util.option_map (Json.lookup j "enable_global_write_forwarding") Boolean.of_json
    }
end

module DBClusterParameterGroupNameMessage = struct
  type t = { d_b_cluster_parameter_group_name : String.t option }

  let make ?d_b_cluster_parameter_group_name () = { d_b_cluster_parameter_group_name }

  let parse xml =
    Some
      { d_b_cluster_parameter_group_name =
          Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               Query.Pair ("DBClusterParameterGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               "d_b_cluster_parameter_group_name", String.to_json f)
         ])

  let of_json j =
    { d_b_cluster_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name") String.of_json
    }
end

module DBClusterSnapshotAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBClusterResult = struct
  type t = { d_b_cluster : DBCluster.t option }

  let make ?d_b_cluster () = { d_b_cluster }

  let parse xml =
    Some { d_b_cluster = Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f ->
               Query.Pair ("DBCluster", DBCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f -> "d_b_cluster", DBCluster.to_json f) ])

  let of_json j =
    { d_b_cluster = Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json }
end

module RestoreDBInstanceFromS3Message = struct
  type t =
    { d_b_name : String.t option
    ; d_b_instance_identifier : String.t
    ; allocated_storage : Integer.t option
    ; d_b_instance_class : String.t
    ; engine : String.t
    ; master_username : String.t option
    ; master_user_password : String.t option
    ; d_b_security_groups : DBSecurityGroupNameList.t
    ; vpc_security_group_ids : VpcSecurityGroupIdList.t
    ; availability_zone : String.t option
    ; d_b_subnet_group_name : String.t option
    ; preferred_maintenance_window : String.t option
    ; d_b_parameter_group_name : String.t option
    ; backup_retention_period : Integer.t option
    ; preferred_backup_window : String.t option
    ; port : Integer.t option
    ; multi_a_z : Boolean.t option
    ; engine_version : String.t option
    ; auto_minor_version_upgrade : Boolean.t option
    ; license_model : String.t option
    ; iops : Integer.t option
    ; option_group_name : String.t option
    ; publicly_accessible : Boolean.t option
    ; tags : TagList.t
    ; storage_type : String.t option
    ; storage_encrypted : Boolean.t option
    ; kms_key_id : String.t option
    ; copy_tags_to_snapshot : Boolean.t option
    ; monitoring_interval : Integer.t option
    ; monitoring_role_arn : String.t option
    ; enable_i_a_m_database_authentication : Boolean.t option
    ; source_engine : String.t
    ; source_engine_version : String.t
    ; s3_bucket_name : String.t
    ; s3_prefix : String.t option
    ; s3_ingestion_role_arn : String.t
    ; enable_performance_insights : Boolean.t option
    ; performance_insights_k_m_s_key_id : String.t option
    ; performance_insights_retention_period : Integer.t option
    ; enable_cloudwatch_logs_exports : LogTypeList.t
    ; processor_features : ProcessorFeatureList.t
    ; use_default_processor_features : Boolean.t option
    ; deletion_protection : Boolean.t option
    ; max_allocated_storage : Integer.t option
    }

  let make
      ?d_b_name
      ~d_b_instance_identifier
      ?allocated_storage
      ~d_b_instance_class
      ~engine
      ?master_username
      ?master_user_password
      ?(d_b_security_groups = [])
      ?(vpc_security_group_ids = [])
      ?availability_zone
      ?d_b_subnet_group_name
      ?preferred_maintenance_window
      ?d_b_parameter_group_name
      ?backup_retention_period
      ?preferred_backup_window
      ?port
      ?multi_a_z
      ?engine_version
      ?auto_minor_version_upgrade
      ?license_model
      ?iops
      ?option_group_name
      ?publicly_accessible
      ?(tags = [])
      ?storage_type
      ?storage_encrypted
      ?kms_key_id
      ?copy_tags_to_snapshot
      ?monitoring_interval
      ?monitoring_role_arn
      ?enable_i_a_m_database_authentication
      ~source_engine
      ~source_engine_version
      ~s3_bucket_name
      ?s3_prefix
      ~s3_ingestion_role_arn
      ?enable_performance_insights
      ?performance_insights_k_m_s_key_id
      ?performance_insights_retention_period
      ?(enable_cloudwatch_logs_exports = [])
      ?(processor_features = [])
      ?use_default_processor_features
      ?deletion_protection
      ?max_allocated_storage
      () =
    { d_b_name
    ; d_b_instance_identifier
    ; allocated_storage
    ; d_b_instance_class
    ; engine
    ; master_username
    ; master_user_password
    ; d_b_security_groups
    ; vpc_security_group_ids
    ; availability_zone
    ; d_b_subnet_group_name
    ; preferred_maintenance_window
    ; d_b_parameter_group_name
    ; backup_retention_period
    ; preferred_backup_window
    ; port
    ; multi_a_z
    ; engine_version
    ; auto_minor_version_upgrade
    ; license_model
    ; iops
    ; option_group_name
    ; publicly_accessible
    ; tags
    ; storage_type
    ; storage_encrypted
    ; kms_key_id
    ; copy_tags_to_snapshot
    ; monitoring_interval
    ; monitoring_role_arn
    ; enable_i_a_m_database_authentication
    ; source_engine
    ; source_engine_version
    ; s3_bucket_name
    ; s3_prefix
    ; s3_ingestion_role_arn
    ; enable_performance_insights
    ; performance_insights_k_m_s_key_id
    ; performance_insights_retention_period
    ; enable_cloudwatch_logs_exports
    ; processor_features
    ; use_default_processor_features
    ; deletion_protection
    ; max_allocated_storage
    }

  let parse xml =
    Some
      { d_b_name = Util.option_bind (Xml.member "DBName" xml) String.parse
      ; d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; allocated_storage =
          Util.option_bind (Xml.member "AllocatedStorage" xml) Integer.parse
      ; d_b_instance_class =
          Xml.required
            "DBInstanceClass"
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse)
      ; engine =
          Xml.required "Engine" (Util.option_bind (Xml.member "Engine" xml) String.parse)
      ; master_username = Util.option_bind (Xml.member "MasterUsername" xml) String.parse
      ; master_user_password =
          Util.option_bind (Xml.member "MasterUserPassword" xml) String.parse
      ; d_b_security_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBSecurityGroups" xml)
               DBSecurityGroupNameList.parse)
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupIds" xml)
               VpcSecurityGroupIdList.parse)
      ; availability_zone =
          Util.option_bind (Xml.member "AvailabilityZone" xml) String.parse
      ; d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; preferred_maintenance_window =
          Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml) String.parse
      ; d_b_parameter_group_name =
          Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse
      ; backup_retention_period =
          Util.option_bind (Xml.member "BackupRetentionPeriod" xml) Integer.parse
      ; preferred_backup_window =
          Util.option_bind (Xml.member "PreferredBackupWindow" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; auto_minor_version_upgrade =
          Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
      ; license_model = Util.option_bind (Xml.member "LicenseModel" xml) String.parse
      ; iops = Util.option_bind (Xml.member "Iops" xml) Integer.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; publicly_accessible =
          Util.option_bind (Xml.member "PubliclyAccessible" xml) Boolean.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; storage_type = Util.option_bind (Xml.member "StorageType" xml) String.parse
      ; storage_encrypted =
          Util.option_bind (Xml.member "StorageEncrypted" xml) Boolean.parse
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; monitoring_interval =
          Util.option_bind (Xml.member "MonitoringInterval" xml) Integer.parse
      ; monitoring_role_arn =
          Util.option_bind (Xml.member "MonitoringRoleArn" xml) String.parse
      ; enable_i_a_m_database_authentication =
          Util.option_bind
            (Xml.member "EnableIAMDatabaseAuthentication" xml)
            Boolean.parse
      ; source_engine =
          Xml.required
            "SourceEngine"
            (Util.option_bind (Xml.member "SourceEngine" xml) String.parse)
      ; source_engine_version =
          Xml.required
            "SourceEngineVersion"
            (Util.option_bind (Xml.member "SourceEngineVersion" xml) String.parse)
      ; s3_bucket_name =
          Xml.required
            "S3BucketName"
            (Util.option_bind (Xml.member "S3BucketName" xml) String.parse)
      ; s3_prefix = Util.option_bind (Xml.member "S3Prefix" xml) String.parse
      ; s3_ingestion_role_arn =
          Xml.required
            "S3IngestionRoleArn"
            (Util.option_bind (Xml.member "S3IngestionRoleArn" xml) String.parse)
      ; enable_performance_insights =
          Util.option_bind (Xml.member "EnablePerformanceInsights" xml) Boolean.parse
      ; performance_insights_k_m_s_key_id =
          Util.option_bind (Xml.member "PerformanceInsightsKMSKeyId" xml) String.parse
      ; performance_insights_retention_period =
          Util.option_bind
            (Xml.member "PerformanceInsightsRetentionPeriod" xml)
            Integer.parse
      ; enable_cloudwatch_logs_exports =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EnableCloudwatchLogsExports" xml)
               LogTypeList.parse)
      ; processor_features =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ProcessorFeatures" xml)
               ProcessorFeatureList.parse)
      ; use_default_processor_features =
          Util.option_bind (Xml.member "UseDefaultProcessorFeatures" xml) Boolean.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; max_allocated_storage =
          Util.option_bind (Xml.member "MaxAllocatedStorage" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_allocated_storage (fun f ->
               Query.Pair ("MaxAllocatedStorage", Integer.to_query f))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.use_default_processor_features (fun f ->
               Query.Pair ("UseDefaultProcessorFeatures", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "ProcessorFeatures.member"
                , ProcessorFeatureList.to_query v.processor_features ))
         ; Some
             (Query.Pair
                ( "EnableCloudwatchLogsExports.member"
                , LogTypeList.to_query v.enable_cloudwatch_logs_exports ))
         ; Util.option_map v.performance_insights_retention_period (fun f ->
               Query.Pair ("PerformanceInsightsRetentionPeriod", Integer.to_query f))
         ; Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
               Query.Pair ("PerformanceInsightsKMSKeyId", String.to_query f))
         ; Util.option_map v.enable_performance_insights (fun f ->
               Query.Pair ("EnablePerformanceInsights", Boolean.to_query f))
         ; Some
             (Query.Pair ("S3IngestionRoleArn", String.to_query v.s3_ingestion_role_arn))
         ; Util.option_map v.s3_prefix (fun f ->
               Query.Pair ("S3Prefix", String.to_query f))
         ; Some (Query.Pair ("S3BucketName", String.to_query v.s3_bucket_name))
         ; Some
             (Query.Pair ("SourceEngineVersion", String.to_query v.source_engine_version))
         ; Some (Query.Pair ("SourceEngine", String.to_query v.source_engine))
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
         ; Util.option_map v.monitoring_role_arn (fun f ->
               Query.Pair ("MonitoringRoleArn", String.to_query f))
         ; Util.option_map v.monitoring_interval (fun f ->
               Query.Pair ("MonitoringInterval", Integer.to_query f))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Util.option_map v.storage_encrypted (fun f ->
               Query.Pair ("StorageEncrypted", Boolean.to_query f))
         ; Util.option_map v.storage_type (fun f ->
               Query.Pair ("StorageType", String.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.publicly_accessible (fun f ->
               Query.Pair ("PubliclyAccessible", Boolean.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.iops (fun f -> Query.Pair ("Iops", Integer.to_query f))
         ; Util.option_map v.license_model (fun f ->
               Query.Pair ("LicenseModel", String.to_query f))
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.preferred_backup_window (fun f ->
               Query.Pair ("PreferredBackupWindow", String.to_query f))
         ; Util.option_map v.backup_retention_period (fun f ->
               Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               Query.Pair ("DBParameterGroupName", String.to_query f))
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ; Util.option_map v.availability_zone (fun f ->
               Query.Pair ("AvailabilityZone", String.to_query f))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
         ; Some
             (Query.Pair
                ( "DBSecurityGroups.member"
                , DBSecurityGroupNameList.to_query v.d_b_security_groups ))
         ; Util.option_map v.master_user_password (fun f ->
               Query.Pair ("MasterUserPassword", String.to_query f))
         ; Util.option_map v.master_username (fun f ->
               Query.Pair ("MasterUsername", String.to_query f))
         ; Some (Query.Pair ("Engine", String.to_query v.engine))
         ; Some (Query.Pair ("DBInstanceClass", String.to_query v.d_b_instance_class))
         ; Util.option_map v.allocated_storage (fun f ->
               Query.Pair ("AllocatedStorage", Integer.to_query f))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ; Util.option_map v.d_b_name (fun f -> Query.Pair ("DBName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_allocated_storage (fun f ->
               "max_allocated_storage", Integer.to_json f)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.use_default_processor_features (fun f ->
               "use_default_processor_features", Boolean.to_json f)
         ; Some ("processor_features", ProcessorFeatureList.to_json v.processor_features)
         ; Some
             ( "enable_cloudwatch_logs_exports"
             , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
         ; Util.option_map v.performance_insights_retention_period (fun f ->
               "performance_insights_retention_period", Integer.to_json f)
         ; Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
               "performance_insights_k_m_s_key_id", String.to_json f)
         ; Util.option_map v.enable_performance_insights (fun f ->
               "enable_performance_insights", Boolean.to_json f)
         ; Some ("s3_ingestion_role_arn", String.to_json v.s3_ingestion_role_arn)
         ; Util.option_map v.s3_prefix (fun f -> "s3_prefix", String.to_json f)
         ; Some ("s3_bucket_name", String.to_json v.s3_bucket_name)
         ; Some ("source_engine_version", String.to_json v.source_engine_version)
         ; Some ("source_engine", String.to_json v.source_engine)
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               "enable_i_a_m_database_authentication", Boolean.to_json f)
         ; Util.option_map v.monitoring_role_arn (fun f ->
               "monitoring_role_arn", String.to_json f)
         ; Util.option_map v.monitoring_interval (fun f ->
               "monitoring_interval", Integer.to_json f)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Util.option_map v.storage_encrypted (fun f ->
               "storage_encrypted", Boolean.to_json f)
         ; Util.option_map v.storage_type (fun f -> "storage_type", String.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.publicly_accessible (fun f ->
               "publicly_accessible", Boolean.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
         ; Util.option_map v.license_model (fun f -> "license_model", String.to_json f)
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               "auto_minor_version_upgrade", Boolean.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.preferred_backup_window (fun f ->
               "preferred_backup_window", String.to_json f)
         ; Util.option_map v.backup_retention_period (fun f ->
               "backup_retention_period", Integer.to_json f)
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               "d_b_parameter_group_name", String.to_json f)
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               "preferred_maintenance_window", String.to_json f)
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ; Util.option_map v.availability_zone (fun f ->
               "availability_zone", String.to_json f)
         ; Some
             ( "vpc_security_group_ids"
             , VpcSecurityGroupIdList.to_json v.vpc_security_group_ids )
         ; Some
             ("d_b_security_groups", DBSecurityGroupNameList.to_json v.d_b_security_groups)
         ; Util.option_map v.master_user_password (fun f ->
               "master_user_password", String.to_json f)
         ; Util.option_map v.master_username (fun f ->
               "master_username", String.to_json f)
         ; Some ("engine", String.to_json v.engine)
         ; Some ("d_b_instance_class", String.to_json v.d_b_instance_class)
         ; Util.option_map v.allocated_storage (fun f ->
               "allocated_storage", Integer.to_json f)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ; Util.option_map v.d_b_name (fun f -> "d_b_name", String.to_json f)
         ])

  let of_json j =
    { d_b_name = Util.option_map (Json.lookup j "d_b_name") String.of_json
    ; d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; allocated_storage =
        Util.option_map (Json.lookup j "allocated_storage") Integer.of_json
    ; d_b_instance_class =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_class"))
    ; engine = String.of_json (Util.of_option_exn (Json.lookup j "engine"))
    ; master_username = Util.option_map (Json.lookup j "master_username") String.of_json
    ; master_user_password =
        Util.option_map (Json.lookup j "master_user_password") String.of_json
    ; d_b_security_groups =
        DBSecurityGroupNameList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_security_groups"))
    ; vpc_security_group_ids =
        VpcSecurityGroupIdList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; availability_zone =
        Util.option_map (Json.lookup j "availability_zone") String.of_json
    ; d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; preferred_maintenance_window =
        Util.option_map (Json.lookup j "preferred_maintenance_window") String.of_json
    ; d_b_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_parameter_group_name") String.of_json
    ; backup_retention_period =
        Util.option_map (Json.lookup j "backup_retention_period") Integer.of_json
    ; preferred_backup_window =
        Util.option_map (Json.lookup j "preferred_backup_window") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; auto_minor_version_upgrade =
        Util.option_map (Json.lookup j "auto_minor_version_upgrade") Boolean.of_json
    ; license_model = Util.option_map (Json.lookup j "license_model") String.of_json
    ; iops = Util.option_map (Json.lookup j "iops") Integer.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; publicly_accessible =
        Util.option_map (Json.lookup j "publicly_accessible") Boolean.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; storage_type = Util.option_map (Json.lookup j "storage_type") String.of_json
    ; storage_encrypted =
        Util.option_map (Json.lookup j "storage_encrypted") Boolean.of_json
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; monitoring_interval =
        Util.option_map (Json.lookup j "monitoring_interval") Integer.of_json
    ; monitoring_role_arn =
        Util.option_map (Json.lookup j "monitoring_role_arn") String.of_json
    ; enable_i_a_m_database_authentication =
        Util.option_map
          (Json.lookup j "enable_i_a_m_database_authentication")
          Boolean.of_json
    ; source_engine = String.of_json (Util.of_option_exn (Json.lookup j "source_engine"))
    ; source_engine_version =
        String.of_json (Util.of_option_exn (Json.lookup j "source_engine_version"))
    ; s3_bucket_name =
        String.of_json (Util.of_option_exn (Json.lookup j "s3_bucket_name"))
    ; s3_prefix = Util.option_map (Json.lookup j "s3_prefix") String.of_json
    ; s3_ingestion_role_arn =
        String.of_json (Util.of_option_exn (Json.lookup j "s3_ingestion_role_arn"))
    ; enable_performance_insights =
        Util.option_map (Json.lookup j "enable_performance_insights") Boolean.of_json
    ; performance_insights_k_m_s_key_id =
        Util.option_map (Json.lookup j "performance_insights_k_m_s_key_id") String.of_json
    ; performance_insights_retention_period =
        Util.option_map
          (Json.lookup j "performance_insights_retention_period")
          Integer.of_json
    ; enable_cloudwatch_logs_exports =
        LogTypeList.of_json
          (Util.of_option_exn (Json.lookup j "enable_cloudwatch_logs_exports"))
    ; processor_features =
        ProcessorFeatureList.of_json
          (Util.of_option_exn (Json.lookup j "processor_features"))
    ; use_default_processor_features =
        Util.option_map (Json.lookup j "use_default_processor_features") Boolean.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; max_allocated_storage =
        Util.option_map (Json.lookup j "max_allocated_storage") Integer.of_json
    }
end

module DBClusterEndpointMessage = struct
  type t =
    { marker : String.t option
    ; d_b_cluster_endpoints : DBClusterEndpointList.t
    }

  let make ?marker ?(d_b_cluster_endpoints = []) () = { marker; d_b_cluster_endpoints }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; d_b_cluster_endpoints =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBClusterEndpoints" xml)
               DBClusterEndpointList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBClusterEndpoints.member"
                , DBClusterEndpointList.to_query v.d_b_cluster_endpoints ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "d_b_cluster_endpoints"
             , DBClusterEndpointList.to_json v.d_b_cluster_endpoints )
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; d_b_cluster_endpoints =
        DBClusterEndpointList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_endpoints"))
    }
end

module EventSubscriptionsMessage = struct
  type t =
    { marker : String.t option
    ; event_subscriptions_list : EventSubscriptionsList.t
    }

  let make ?marker ?(event_subscriptions_list = []) () =
    { marker; event_subscriptions_list }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; event_subscriptions_list =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EventSubscriptionsList" xml)
               EventSubscriptionsList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "EventSubscriptionsList.member"
                , EventSubscriptionsList.to_query v.event_subscriptions_list ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "event_subscriptions_list"
             , EventSubscriptionsList.to_json v.event_subscriptions_list )
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; event_subscriptions_list =
        EventSubscriptionsList.of_json
          (Util.of_option_exn (Json.lookup j "event_subscriptions_list"))
    }
end

module AddSourceIdentifierToSubscriptionMessage = struct
  type t =
    { subscription_name : String.t
    ; source_identifier : String.t
    }

  let make ~subscription_name ~source_identifier () =
    { subscription_name; source_identifier }

  let parse xml =
    Some
      { subscription_name =
          Xml.required
            "SubscriptionName"
            (Util.option_bind (Xml.member "SubscriptionName" xml) String.parse)
      ; source_identifier =
          Xml.required
            "SourceIdentifier"
            (Util.option_bind (Xml.member "SourceIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("SourceIdentifier", String.to_query v.source_identifier))
         ; Some (Query.Pair ("SubscriptionName", String.to_query v.subscription_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("source_identifier", String.to_json v.source_identifier)
         ; Some ("subscription_name", String.to_json v.subscription_name)
         ])

  let of_json j =
    { subscription_name =
        String.of_json (Util.of_option_exn (Json.lookup j "subscription_name"))
    ; source_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "source_identifier"))
    }
end

module GlobalClusterAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidSubnet = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyOptionGroupMessage = struct
  type t =
    { option_group_name : String.t
    ; options_to_include : OptionConfigurationList.t
    ; options_to_remove : OptionNamesList.t
    ; apply_immediately : Boolean.t option
    }

  let make
      ~option_group_name
      ?(options_to_include = [])
      ?(options_to_remove = [])
      ?apply_immediately
      () =
    { option_group_name; options_to_include; options_to_remove; apply_immediately }

  let parse xml =
    Some
      { option_group_name =
          Xml.required
            "OptionGroupName"
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse)
      ; options_to_include =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "OptionsToInclude" xml)
               OptionConfigurationList.parse)
      ; options_to_remove =
          Util.of_option
            []
            (Util.option_bind (Xml.member "OptionsToRemove" xml) OptionNamesList.parse)
      ; apply_immediately =
          Util.option_bind (Xml.member "ApplyImmediately" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.apply_immediately (fun f ->
               Query.Pair ("ApplyImmediately", Boolean.to_query f))
         ; Some
             (Query.Pair
                ("OptionsToRemove.member", OptionNamesList.to_query v.options_to_remove))
         ; Some
             (Query.Pair
                ( "OptionsToInclude.member"
                , OptionConfigurationList.to_query v.options_to_include ))
         ; Some (Query.Pair ("OptionGroupName", String.to_query v.option_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.apply_immediately (fun f ->
               "apply_immediately", Boolean.to_json f)
         ; Some ("options_to_remove", OptionNamesList.to_json v.options_to_remove)
         ; Some
             ("options_to_include", OptionConfigurationList.to_json v.options_to_include)
         ; Some ("option_group_name", String.to_json v.option_group_name)
         ])

  let of_json j =
    { option_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "option_group_name"))
    ; options_to_include =
        OptionConfigurationList.of_json
          (Util.of_option_exn (Json.lookup j "options_to_include"))
    ; options_to_remove =
        OptionNamesList.of_json (Util.of_option_exn (Json.lookup j "options_to_remove"))
    ; apply_immediately =
        Util.option_map (Json.lookup j "apply_immediately") Boolean.of_json
    }
end

module DBClusterEndpointAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module AddTagsToResourceMessage = struct
  type t =
    { resource_name : String.t
    ; tags : TagList.t
    }

  let make ~resource_name ~tags () = { resource_name; tags }

  let parse xml =
    Some
      { resource_name =
          Xml.required
            "ResourceName"
            (Util.option_bind (Xml.member "ResourceName" xml) String.parse)
      ; tags =
          Xml.required "Tags" (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some (Query.Pair ("ResourceName", String.to_query v.resource_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some ("resource_name", String.to_json v.resource_name)
         ])

  let of_json j =
    { resource_name = String.of_json (Util.of_option_exn (Json.lookup j "resource_name"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module InvalidOptionGroupStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyDBSubnetGroupResult = struct
  type t = { d_b_subnet_group : DBSubnetGroup.t option }

  let make ?d_b_subnet_group () = { d_b_subnet_group }

  let parse xml =
    Some
      { d_b_subnet_group =
          Util.option_bind (Xml.member "DBSubnetGroup" xml) DBSubnetGroup.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_subnet_group (fun f ->
               Query.Pair ("DBSubnetGroup", DBSubnetGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_subnet_group (fun f ->
               "d_b_subnet_group", DBSubnetGroup.to_json f)
         ])

  let of_json j =
    { d_b_subnet_group =
        Util.option_map (Json.lookup j "d_b_subnet_group") DBSubnetGroup.of_json
    }
end

module DescribeInstallationMediaMessage = struct
  type t =
    { installation_media_id : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?installation_media_id ?(filters = []) ?max_records ?marker () =
    { installation_media_id; filters; max_records; marker }

  let parse xml =
    Some
      { installation_media_id =
          Util.option_bind (Xml.member "InstallationMediaId" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.installation_media_id (fun f ->
               Query.Pair ("InstallationMediaId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.installation_media_id (fun f ->
               "installation_media_id", String.to_json f)
         ])

  let of_json j =
    { installation_media_id =
        Util.option_map (Json.lookup j "installation_media_id") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module DescribeSourceRegionsMessage = struct
  type t =
    { region_name : String.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    ; filters : FilterList.t
    }

  let make ?region_name ?max_records ?marker ?(filters = []) () =
    { region_name; max_records; marker; filters }

  let parse xml =
    Some
      { region_name = Util.option_bind (Xml.member "RegionName" xml) String.parse
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Util.option_map v.region_name (fun f ->
               Query.Pair ("RegionName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Util.option_map v.region_name (fun f -> "region_name", String.to_json f)
         ])

  let of_json j =
    { region_name = Util.option_map (Json.lookup j "region_name") String.of_json
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    }
end

module InvalidDBProxyStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyDBProxyResponse = struct
  type t = { d_b_proxy : DBProxy.t option }

  let make ?d_b_proxy () = { d_b_proxy }

  let parse xml =
    Some { d_b_proxy = Util.option_bind (Xml.member "DBProxy" xml) DBProxy.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_proxy (fun f ->
               Query.Pair ("DBProxy", DBProxy.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_proxy (fun f -> "d_b_proxy", DBProxy.to_json f) ])

  let of_json j =
    { d_b_proxy = Util.option_map (Json.lookup j "d_b_proxy") DBProxy.of_json }
end

module SubnetAlreadyInUse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module GlobalClusterQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ProvisionedIopsNotAvailableInAZFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module RestoreDBInstanceToPointInTimeMessage = struct
  type t =
    { source_d_b_instance_identifier : String.t option
    ; target_d_b_instance_identifier : String.t
    ; restore_time : DateTime.t option
    ; use_latest_restorable_time : Boolean.t option
    ; d_b_instance_class : String.t option
    ; port : Integer.t option
    ; availability_zone : String.t option
    ; d_b_subnet_group_name : String.t option
    ; multi_a_z : Boolean.t option
    ; publicly_accessible : Boolean.t option
    ; auto_minor_version_upgrade : Boolean.t option
    ; license_model : String.t option
    ; d_b_name : String.t option
    ; engine : String.t option
    ; iops : Integer.t option
    ; option_group_name : String.t option
    ; copy_tags_to_snapshot : Boolean.t option
    ; tags : TagList.t
    ; storage_type : String.t option
    ; tde_credential_arn : String.t option
    ; tde_credential_password : String.t option
    ; vpc_security_group_ids : VpcSecurityGroupIdList.t
    ; domain : String.t option
    ; domain_i_a_m_role_name : String.t option
    ; enable_i_a_m_database_authentication : Boolean.t option
    ; enable_cloudwatch_logs_exports : LogTypeList.t
    ; processor_features : ProcessorFeatureList.t
    ; use_default_processor_features : Boolean.t option
    ; d_b_parameter_group_name : String.t option
    ; deletion_protection : Boolean.t option
    ; source_dbi_resource_id : String.t option
    ; max_allocated_storage : Integer.t option
    }

  let make
      ?source_d_b_instance_identifier
      ~target_d_b_instance_identifier
      ?restore_time
      ?use_latest_restorable_time
      ?d_b_instance_class
      ?port
      ?availability_zone
      ?d_b_subnet_group_name
      ?multi_a_z
      ?publicly_accessible
      ?auto_minor_version_upgrade
      ?license_model
      ?d_b_name
      ?engine
      ?iops
      ?option_group_name
      ?copy_tags_to_snapshot
      ?(tags = [])
      ?storage_type
      ?tde_credential_arn
      ?tde_credential_password
      ?(vpc_security_group_ids = [])
      ?domain
      ?domain_i_a_m_role_name
      ?enable_i_a_m_database_authentication
      ?(enable_cloudwatch_logs_exports = [])
      ?(processor_features = [])
      ?use_default_processor_features
      ?d_b_parameter_group_name
      ?deletion_protection
      ?source_dbi_resource_id
      ?max_allocated_storage
      () =
    { source_d_b_instance_identifier
    ; target_d_b_instance_identifier
    ; restore_time
    ; use_latest_restorable_time
    ; d_b_instance_class
    ; port
    ; availability_zone
    ; d_b_subnet_group_name
    ; multi_a_z
    ; publicly_accessible
    ; auto_minor_version_upgrade
    ; license_model
    ; d_b_name
    ; engine
    ; iops
    ; option_group_name
    ; copy_tags_to_snapshot
    ; tags
    ; storage_type
    ; tde_credential_arn
    ; tde_credential_password
    ; vpc_security_group_ids
    ; domain
    ; domain_i_a_m_role_name
    ; enable_i_a_m_database_authentication
    ; enable_cloudwatch_logs_exports
    ; processor_features
    ; use_default_processor_features
    ; d_b_parameter_group_name
    ; deletion_protection
    ; source_dbi_resource_id
    ; max_allocated_storage
    }

  let parse xml =
    Some
      { source_d_b_instance_identifier =
          Util.option_bind (Xml.member "SourceDBInstanceIdentifier" xml) String.parse
      ; target_d_b_instance_identifier =
          Xml.required
            "TargetDBInstanceIdentifier"
            (Util.option_bind (Xml.member "TargetDBInstanceIdentifier" xml) String.parse)
      ; restore_time = Util.option_bind (Xml.member "RestoreTime" xml) DateTime.parse
      ; use_latest_restorable_time =
          Util.option_bind (Xml.member "UseLatestRestorableTime" xml) Boolean.parse
      ; d_b_instance_class =
          Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; availability_zone =
          Util.option_bind (Xml.member "AvailabilityZone" xml) String.parse
      ; d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; publicly_accessible =
          Util.option_bind (Xml.member "PubliclyAccessible" xml) Boolean.parse
      ; auto_minor_version_upgrade =
          Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
      ; license_model = Util.option_bind (Xml.member "LicenseModel" xml) String.parse
      ; d_b_name = Util.option_bind (Xml.member "DBName" xml) String.parse
      ; engine = Util.option_bind (Xml.member "Engine" xml) String.parse
      ; iops = Util.option_bind (Xml.member "Iops" xml) Integer.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; storage_type = Util.option_bind (Xml.member "StorageType" xml) String.parse
      ; tde_credential_arn =
          Util.option_bind (Xml.member "TdeCredentialArn" xml) String.parse
      ; tde_credential_password =
          Util.option_bind (Xml.member "TdeCredentialPassword" xml) String.parse
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupIds" xml)
               VpcSecurityGroupIdList.parse)
      ; domain = Util.option_bind (Xml.member "Domain" xml) String.parse
      ; domain_i_a_m_role_name =
          Util.option_bind (Xml.member "DomainIAMRoleName" xml) String.parse
      ; enable_i_a_m_database_authentication =
          Util.option_bind
            (Xml.member "EnableIAMDatabaseAuthentication" xml)
            Boolean.parse
      ; enable_cloudwatch_logs_exports =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EnableCloudwatchLogsExports" xml)
               LogTypeList.parse)
      ; processor_features =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ProcessorFeatures" xml)
               ProcessorFeatureList.parse)
      ; use_default_processor_features =
          Util.option_bind (Xml.member "UseDefaultProcessorFeatures" xml) Boolean.parse
      ; d_b_parameter_group_name =
          Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; source_dbi_resource_id =
          Util.option_bind (Xml.member "SourceDbiResourceId" xml) String.parse
      ; max_allocated_storage =
          Util.option_bind (Xml.member "MaxAllocatedStorage" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_allocated_storage (fun f ->
               Query.Pair ("MaxAllocatedStorage", Integer.to_query f))
         ; Util.option_map v.source_dbi_resource_id (fun f ->
               Query.Pair ("SourceDbiResourceId", String.to_query f))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               Query.Pair ("DBParameterGroupName", String.to_query f))
         ; Util.option_map v.use_default_processor_features (fun f ->
               Query.Pair ("UseDefaultProcessorFeatures", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "ProcessorFeatures.member"
                , ProcessorFeatureList.to_query v.processor_features ))
         ; Some
             (Query.Pair
                ( "EnableCloudwatchLogsExports.member"
                , LogTypeList.to_query v.enable_cloudwatch_logs_exports ))
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               Query.Pair ("DomainIAMRoleName", String.to_query f))
         ; Util.option_map v.domain (fun f -> Query.Pair ("Domain", String.to_query f))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
         ; Util.option_map v.tde_credential_password (fun f ->
               Query.Pair ("TdeCredentialPassword", String.to_query f))
         ; Util.option_map v.tde_credential_arn (fun f ->
               Query.Pair ("TdeCredentialArn", String.to_query f))
         ; Util.option_map v.storage_type (fun f ->
               Query.Pair ("StorageType", String.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.iops (fun f -> Query.Pair ("Iops", Integer.to_query f))
         ; Util.option_map v.engine (fun f -> Query.Pair ("Engine", String.to_query f))
         ; Util.option_map v.d_b_name (fun f -> Query.Pair ("DBName", String.to_query f))
         ; Util.option_map v.license_model (fun f ->
               Query.Pair ("LicenseModel", String.to_query f))
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Util.option_map v.publicly_accessible (fun f ->
               Query.Pair ("PubliclyAccessible", Boolean.to_query f))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ; Util.option_map v.availability_zone (fun f ->
               Query.Pair ("AvailabilityZone", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.d_b_instance_class (fun f ->
               Query.Pair ("DBInstanceClass", String.to_query f))
         ; Util.option_map v.use_latest_restorable_time (fun f ->
               Query.Pair ("UseLatestRestorableTime", Boolean.to_query f))
         ; Util.option_map v.restore_time (fun f ->
               Query.Pair ("RestoreTime", DateTime.to_query f))
         ; Some
             (Query.Pair
                ( "TargetDBInstanceIdentifier"
                , String.to_query v.target_d_b_instance_identifier ))
         ; Util.option_map v.source_d_b_instance_identifier (fun f ->
               Query.Pair ("SourceDBInstanceIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_allocated_storage (fun f ->
               "max_allocated_storage", Integer.to_json f)
         ; Util.option_map v.source_dbi_resource_id (fun f ->
               "source_dbi_resource_id", String.to_json f)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               "d_b_parameter_group_name", String.to_json f)
         ; Util.option_map v.use_default_processor_features (fun f ->
               "use_default_processor_features", Boolean.to_json f)
         ; Some ("processor_features", ProcessorFeatureList.to_json v.processor_features)
         ; Some
             ( "enable_cloudwatch_logs_exports"
             , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               "enable_i_a_m_database_authentication", Boolean.to_json f)
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               "domain_i_a_m_role_name", String.to_json f)
         ; Util.option_map v.domain (fun f -> "domain", String.to_json f)
         ; Some
             ( "vpc_security_group_ids"
             , VpcSecurityGroupIdList.to_json v.vpc_security_group_ids )
         ; Util.option_map v.tde_credential_password (fun f ->
               "tde_credential_password", String.to_json f)
         ; Util.option_map v.tde_credential_arn (fun f ->
               "tde_credential_arn", String.to_json f)
         ; Util.option_map v.storage_type (fun f -> "storage_type", String.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
         ; Util.option_map v.engine (fun f -> "engine", String.to_json f)
         ; Util.option_map v.d_b_name (fun f -> "d_b_name", String.to_json f)
         ; Util.option_map v.license_model (fun f -> "license_model", String.to_json f)
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               "auto_minor_version_upgrade", Boolean.to_json f)
         ; Util.option_map v.publicly_accessible (fun f ->
               "publicly_accessible", Boolean.to_json f)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ; Util.option_map v.availability_zone (fun f ->
               "availability_zone", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.d_b_instance_class (fun f ->
               "d_b_instance_class", String.to_json f)
         ; Util.option_map v.use_latest_restorable_time (fun f ->
               "use_latest_restorable_time", Boolean.to_json f)
         ; Util.option_map v.restore_time (fun f -> "restore_time", DateTime.to_json f)
         ; Some
             ( "target_d_b_instance_identifier"
             , String.to_json v.target_d_b_instance_identifier )
         ; Util.option_map v.source_d_b_instance_identifier (fun f ->
               "source_d_b_instance_identifier", String.to_json f)
         ])

  let of_json j =
    { source_d_b_instance_identifier =
        Util.option_map (Json.lookup j "source_d_b_instance_identifier") String.of_json
    ; target_d_b_instance_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "target_d_b_instance_identifier"))
    ; restore_time = Util.option_map (Json.lookup j "restore_time") DateTime.of_json
    ; use_latest_restorable_time =
        Util.option_map (Json.lookup j "use_latest_restorable_time") Boolean.of_json
    ; d_b_instance_class =
        Util.option_map (Json.lookup j "d_b_instance_class") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; availability_zone =
        Util.option_map (Json.lookup j "availability_zone") String.of_json
    ; d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; publicly_accessible =
        Util.option_map (Json.lookup j "publicly_accessible") Boolean.of_json
    ; auto_minor_version_upgrade =
        Util.option_map (Json.lookup j "auto_minor_version_upgrade") Boolean.of_json
    ; license_model = Util.option_map (Json.lookup j "license_model") String.of_json
    ; d_b_name = Util.option_map (Json.lookup j "d_b_name") String.of_json
    ; engine = Util.option_map (Json.lookup j "engine") String.of_json
    ; iops = Util.option_map (Json.lookup j "iops") Integer.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; storage_type = Util.option_map (Json.lookup j "storage_type") String.of_json
    ; tde_credential_arn =
        Util.option_map (Json.lookup j "tde_credential_arn") String.of_json
    ; tde_credential_password =
        Util.option_map (Json.lookup j "tde_credential_password") String.of_json
    ; vpc_security_group_ids =
        VpcSecurityGroupIdList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; domain = Util.option_map (Json.lookup j "domain") String.of_json
    ; domain_i_a_m_role_name =
        Util.option_map (Json.lookup j "domain_i_a_m_role_name") String.of_json
    ; enable_i_a_m_database_authentication =
        Util.option_map
          (Json.lookup j "enable_i_a_m_database_authentication")
          Boolean.of_json
    ; enable_cloudwatch_logs_exports =
        LogTypeList.of_json
          (Util.of_option_exn (Json.lookup j "enable_cloudwatch_logs_exports"))
    ; processor_features =
        ProcessorFeatureList.of_json
          (Util.of_option_exn (Json.lookup j "processor_features"))
    ; use_default_processor_features =
        Util.option_map (Json.lookup j "use_default_processor_features") Boolean.of_json
    ; d_b_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_parameter_group_name") String.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; source_dbi_resource_id =
        Util.option_map (Json.lookup j "source_dbi_resource_id") String.of_json
    ; max_allocated_storage =
        Util.option_map (Json.lookup j "max_allocated_storage") Integer.of_json
    }
end

module ModifyDBParameterGroupMessage = struct
  type t =
    { d_b_parameter_group_name : String.t
    ; parameters : ParametersList.t
    }

  let make ~d_b_parameter_group_name ~parameters () =
    { d_b_parameter_group_name; parameters }

  let parse xml =
    Some
      { d_b_parameter_group_name =
          Xml.required
            "DBParameterGroupName"
            (Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse)
      ; parameters =
          Xml.required
            "Parameters"
            (Util.option_bind (Xml.member "Parameters" xml) ParametersList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
         ; Some
             (Query.Pair
                ("DBParameterGroupName", String.to_query v.d_b_parameter_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("parameters", ParametersList.to_json v.parameters)
         ; Some ("d_b_parameter_group_name", String.to_json v.d_b_parameter_group_name)
         ])

  let of_json j =
    { d_b_parameter_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_parameter_group_name"))
    ; parameters =
        ParametersList.of_json (Util.of_option_exn (Json.lookup j "parameters"))
    }
end

module SubscriptionCategoryNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBSubnetGroupMessage = struct
  type t =
    { marker : String.t option
    ; d_b_subnet_groups : DBSubnetGroups.t
    }

  let make ?marker ?(d_b_subnet_groups = []) () = { marker; d_b_subnet_groups }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; d_b_subnet_groups =
          Util.of_option
            []
            (Util.option_bind (Xml.member "DBSubnetGroups" xml) DBSubnetGroups.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("DBSubnetGroups.member", DBSubnetGroups.to_query v.d_b_subnet_groups))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_subnet_groups", DBSubnetGroups.to_json v.d_b_subnet_groups)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; d_b_subnet_groups =
        DBSubnetGroups.of_json (Util.of_option_exn (Json.lookup j "d_b_subnet_groups"))
    }
end

module ResourceNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBClusterSnapshotMessage = struct
  type t =
    { d_b_cluster_snapshot_identifier : String.t
    ; d_b_cluster_identifier : String.t
    ; tags : TagList.t
    }

  let make ~d_b_cluster_snapshot_identifier ~d_b_cluster_identifier ?(tags = []) () =
    { d_b_cluster_snapshot_identifier; d_b_cluster_identifier; tags }

  let parse xml =
    Some
      { d_b_cluster_snapshot_identifier =
          Xml.required
            "DBClusterSnapshotIdentifier"
            (Util.option_bind (Xml.member "DBClusterSnapshotIdentifier" xml) String.parse)
      ; d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ; Some
             (Query.Pair
                ( "DBClusterSnapshotIdentifier"
                , String.to_query v.d_b_cluster_snapshot_identifier ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ; Some
             ( "d_b_cluster_snapshot_identifier"
             , String.to_json v.d_b_cluster_snapshot_identifier )
         ])

  let of_json j =
    { d_b_cluster_snapshot_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_snapshot_identifier"))
    ; d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module ReservedDBInstanceQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBParameterGroupResult = struct
  type t = { d_b_parameter_group : DBParameterGroup.t option }

  let make ?d_b_parameter_group () = { d_b_parameter_group }

  let parse xml =
    Some
      { d_b_parameter_group =
          Util.option_bind (Xml.member "DBParameterGroup" xml) DBParameterGroup.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_parameter_group (fun f ->
               Query.Pair ("DBParameterGroup", DBParameterGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_parameter_group (fun f ->
               "d_b_parameter_group", DBParameterGroup.to_json f)
         ])

  let of_json j =
    { d_b_parameter_group =
        Util.option_map (Json.lookup j "d_b_parameter_group") DBParameterGroup.of_json
    }
end

module ModifyGlobalClusterMessage = struct
  type t =
    { global_cluster_identifier : String.t option
    ; new_global_cluster_identifier : String.t option
    ; deletion_protection : Boolean.t option
    }

  let make
      ?global_cluster_identifier
      ?new_global_cluster_identifier
      ?deletion_protection
      () =
    { global_cluster_identifier; new_global_cluster_identifier; deletion_protection }

  let parse xml =
    Some
      { global_cluster_identifier =
          Util.option_bind (Xml.member "GlobalClusterIdentifier" xml) String.parse
      ; new_global_cluster_identifier =
          Util.option_bind (Xml.member "NewGlobalClusterIdentifier" xml) String.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.new_global_cluster_identifier (fun f ->
               Query.Pair ("NewGlobalClusterIdentifier", String.to_query f))
         ; Util.option_map v.global_cluster_identifier (fun f ->
               Query.Pair ("GlobalClusterIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.new_global_cluster_identifier (fun f ->
               "new_global_cluster_identifier", String.to_json f)
         ; Util.option_map v.global_cluster_identifier (fun f ->
               "global_cluster_identifier", String.to_json f)
         ])

  let of_json j =
    { global_cluster_identifier =
        Util.option_map (Json.lookup j "global_cluster_identifier") String.of_json
    ; new_global_cluster_identifier =
        Util.option_map (Json.lookup j "new_global_cluster_identifier") String.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    }
end

module DescribeExportTasksMessage = struct
  type t =
    { export_task_identifier : String.t option
    ; source_arn : String.t option
    ; filters : FilterList.t
    ; marker : String.t option
    ; max_records : Integer.t option
    }

  let make ?export_task_identifier ?source_arn ?(filters = []) ?marker ?max_records () =
    { export_task_identifier; source_arn; filters; marker; max_records }

  let parse xml =
    Some
      { export_task_identifier =
          Util.option_bind (Xml.member "ExportTaskIdentifier" xml) String.parse
      ; source_arn = Util.option_bind (Xml.member "SourceArn" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.source_arn (fun f ->
               Query.Pair ("SourceArn", String.to_query f))
         ; Util.option_map v.export_task_identifier (fun f ->
               Query.Pair ("ExportTaskIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.source_arn (fun f -> "source_arn", String.to_json f)
         ; Util.option_map v.export_task_identifier (fun f ->
               "export_task_identifier", String.to_json f)
         ])

  let of_json j =
    { export_task_identifier =
        Util.option_map (Json.lookup j "export_task_identifier") String.of_json
    ; source_arn = Util.option_map (Json.lookup j "source_arn") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    }
end

module InvalidDBSnapshotStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyDBClusterEndpointMessage = struct
  type t =
    { d_b_cluster_endpoint_identifier : String.t
    ; endpoint_type : String.t option
    ; static_members : StringList.t
    ; excluded_members : StringList.t
    }

  let make
      ~d_b_cluster_endpoint_identifier
      ?endpoint_type
      ?(static_members = [])
      ?(excluded_members = [])
      () =
    { d_b_cluster_endpoint_identifier; endpoint_type; static_members; excluded_members }

  let parse xml =
    Some
      { d_b_cluster_endpoint_identifier =
          Xml.required
            "DBClusterEndpointIdentifier"
            (Util.option_bind (Xml.member "DBClusterEndpointIdentifier" xml) String.parse)
      ; endpoint_type = Util.option_bind (Xml.member "EndpointType" xml) String.parse
      ; static_members =
          Util.of_option
            []
            (Util.option_bind (Xml.member "StaticMembers" xml) StringList.parse)
      ; excluded_members =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ExcludedMembers" xml) StringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("ExcludedMembers.member", StringList.to_query v.excluded_members))
         ; Some
             (Query.Pair ("StaticMembers.member", StringList.to_query v.static_members))
         ; Util.option_map v.endpoint_type (fun f ->
               Query.Pair ("EndpointType", String.to_query f))
         ; Some
             (Query.Pair
                ( "DBClusterEndpointIdentifier"
                , String.to_query v.d_b_cluster_endpoint_identifier ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("excluded_members", StringList.to_json v.excluded_members)
         ; Some ("static_members", StringList.to_json v.static_members)
         ; Util.option_map v.endpoint_type (fun f -> "endpoint_type", String.to_json f)
         ; Some
             ( "d_b_cluster_endpoint_identifier"
             , String.to_json v.d_b_cluster_endpoint_identifier )
         ])

  let of_json j =
    { d_b_cluster_endpoint_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_endpoint_identifier"))
    ; endpoint_type = Util.option_map (Json.lookup j "endpoint_type") String.of_json
    ; static_members =
        StringList.of_json (Util.of_option_exn (Json.lookup j "static_members"))
    ; excluded_members =
        StringList.of_json (Util.of_option_exn (Json.lookup j "excluded_members"))
    }
end

module InvalidDBInstanceStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyCertificatesResult = struct
  type t = { certificate : Certificate.t option }

  let make ?certificate () = { certificate }

  let parse xml =
    Some
      { certificate = Util.option_bind (Xml.member "Certificate" xml) Certificate.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.certificate (fun f ->
               Query.Pair ("Certificate", Certificate.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.certificate (fun f -> "certificate", Certificate.to_json f) ])

  let of_json j =
    { certificate = Util.option_map (Json.lookup j "certificate") Certificate.of_json }
end

module DownloadDBLogFilePortionMessage = struct
  type t =
    { d_b_instance_identifier : String.t
    ; log_file_name : String.t
    ; marker : String.t option
    ; number_of_lines : Integer.t option
    }

  let make ~d_b_instance_identifier ~log_file_name ?marker ?number_of_lines () =
    { d_b_instance_identifier; log_file_name; marker; number_of_lines }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; log_file_name =
          Xml.required
            "LogFileName"
            (Util.option_bind (Xml.member "LogFileName" xml) String.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; number_of_lines = Util.option_bind (Xml.member "NumberOfLines" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.number_of_lines (fun f ->
               Query.Pair ("NumberOfLines", Integer.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("LogFileName", String.to_query v.log_file_name))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.number_of_lines (fun f ->
               "number_of_lines", Integer.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("log_file_name", String.to_json v.log_file_name)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; log_file_name = String.of_json (Util.of_option_exn (Json.lookup j "log_file_name"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; number_of_lines = Util.option_map (Json.lookup j "number_of_lines") Integer.of_json
    }
end

module DBProxyTargetNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateDBClusterParameterGroupResult = struct
  type t = { d_b_cluster_parameter_group : DBClusterParameterGroup.t option }

  let make ?d_b_cluster_parameter_group () = { d_b_cluster_parameter_group }

  let parse xml =
    Some
      { d_b_cluster_parameter_group =
          Util.option_bind
            (Xml.member "DBClusterParameterGroup" xml)
            DBClusterParameterGroup.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_parameter_group (fun f ->
               Query.Pair ("DBClusterParameterGroup", DBClusterParameterGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_parameter_group (fun f ->
               "d_b_cluster_parameter_group", DBClusterParameterGroup.to_json f)
         ])

  let of_json j =
    { d_b_cluster_parameter_group =
        Util.option_map
          (Json.lookup j "d_b_cluster_parameter_group")
          DBClusterParameterGroup.of_json
    }
end

module DBClusterParameterGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBSnapshotMessage = struct
  type t =
    { marker : String.t option
    ; d_b_snapshots : DBSnapshotList.t
    }

  let make ?marker ?(d_b_snapshots = []) () = { marker; d_b_snapshots }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; d_b_snapshots =
          Util.of_option
            []
            (Util.option_bind (Xml.member "DBSnapshots" xml) DBSnapshotList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("DBSnapshots.member", DBSnapshotList.to_query v.d_b_snapshots))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_snapshots", DBSnapshotList.to_json v.d_b_snapshots)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; d_b_snapshots =
        DBSnapshotList.of_json (Util.of_option_exn (Json.lookup j "d_b_snapshots"))
    }
end

module DBSecurityGroupNotSupportedFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateEventSubscriptionResult = struct
  type t = { event_subscription : EventSubscription.t option }

  let make ?event_subscription () = { event_subscription }

  let parse xml =
    Some
      { event_subscription =
          Util.option_bind (Xml.member "EventSubscription" xml) EventSubscription.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.event_subscription (fun f ->
               Query.Pair ("EventSubscription", EventSubscription.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.event_subscription (fun f ->
               "event_subscription", EventSubscription.to_json f)
         ])

  let of_json j =
    { event_subscription =
        Util.option_map (Json.lookup j "event_subscription") EventSubscription.of_json
    }
end

module DescribeDBSnapshotAttributesResult = struct
  type t = { d_b_snapshot_attributes_result : DBSnapshotAttributesResult.t option }

  let make ?d_b_snapshot_attributes_result () = { d_b_snapshot_attributes_result }

  let parse xml =
    Some
      { d_b_snapshot_attributes_result =
          Util.option_bind
            (Xml.member "DBSnapshotAttributesResult" xml)
            DBSnapshotAttributesResult.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot_attributes_result (fun f ->
               Query.Pair
                 ("DBSnapshotAttributesResult", DBSnapshotAttributesResult.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot_attributes_result (fun f ->
               "d_b_snapshot_attributes_result", DBSnapshotAttributesResult.to_json f)
         ])

  let of_json j =
    { d_b_snapshot_attributes_result =
        Util.option_map
          (Json.lookup j "d_b_snapshot_attributes_result")
          DBSnapshotAttributesResult.of_json
    }
end

module DescribeDBProxiesResponse = struct
  type t =
    { d_b_proxies : DBProxyList.t
    ; marker : String.t option
    }

  let make ?(d_b_proxies = []) ?marker () = { d_b_proxies; marker }

  let parse xml =
    Some
      { d_b_proxies =
          Util.of_option
            []
            (Util.option_bind (Xml.member "DBProxies" xml) DBProxyList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("DBProxies.member", DBProxyList.to_query v.d_b_proxies))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("d_b_proxies", DBProxyList.to_json v.d_b_proxies)
         ])

  let of_json j =
    { d_b_proxies = DBProxyList.of_json (Util.of_option_exn (Json.lookup j "d_b_proxies"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module CreateEventSubscriptionMessage = struct
  type t =
    { subscription_name : String.t
    ; sns_topic_arn : String.t
    ; source_type : String.t option
    ; event_categories : EventCategoriesList.t
    ; source_ids : SourceIdsList.t
    ; enabled : Boolean.t option
    ; tags : TagList.t
    }

  let make
      ~subscription_name
      ~sns_topic_arn
      ?source_type
      ?(event_categories = [])
      ?(source_ids = [])
      ?enabled
      ?(tags = [])
      () =
    { subscription_name
    ; sns_topic_arn
    ; source_type
    ; event_categories
    ; source_ids
    ; enabled
    ; tags
    }

  let parse xml =
    Some
      { subscription_name =
          Xml.required
            "SubscriptionName"
            (Util.option_bind (Xml.member "SubscriptionName" xml) String.parse)
      ; sns_topic_arn =
          Xml.required
            "SnsTopicArn"
            (Util.option_bind (Xml.member "SnsTopicArn" xml) String.parse)
      ; source_type = Util.option_bind (Xml.member "SourceType" xml) String.parse
      ; event_categories =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EventCategories" xml)
               EventCategoriesList.parse)
      ; source_ids =
          Util.of_option
            []
            (Util.option_bind (Xml.member "SourceIds" xml) SourceIdsList.parse)
      ; enabled = Util.option_bind (Xml.member "Enabled" xml) Boolean.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.enabled (fun f -> Query.Pair ("Enabled", Boolean.to_query f))
         ; Some (Query.Pair ("SourceIds.member", SourceIdsList.to_query v.source_ids))
         ; Some
             (Query.Pair
                ("EventCategories.member", EventCategoriesList.to_query v.event_categories))
         ; Util.option_map v.source_type (fun f ->
               Query.Pair ("SourceType", String.to_query f))
         ; Some (Query.Pair ("SnsTopicArn", String.to_query v.sns_topic_arn))
         ; Some (Query.Pair ("SubscriptionName", String.to_query v.subscription_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.enabled (fun f -> "enabled", Boolean.to_json f)
         ; Some ("source_ids", SourceIdsList.to_json v.source_ids)
         ; Some ("event_categories", EventCategoriesList.to_json v.event_categories)
         ; Util.option_map v.source_type (fun f -> "source_type", String.to_json f)
         ; Some ("sns_topic_arn", String.to_json v.sns_topic_arn)
         ; Some ("subscription_name", String.to_json v.subscription_name)
         ])

  let of_json j =
    { subscription_name =
        String.of_json (Util.of_option_exn (Json.lookup j "subscription_name"))
    ; sns_topic_arn = String.of_json (Util.of_option_exn (Json.lookup j "sns_topic_arn"))
    ; source_type = Util.option_map (Json.lookup j "source_type") String.of_json
    ; event_categories =
        EventCategoriesList.of_json
          (Util.of_option_exn (Json.lookup j "event_categories"))
    ; source_ids = SourceIdsList.of_json (Util.of_option_exn (Json.lookup j "source_ids"))
    ; enabled = Util.option_map (Json.lookup j "enabled") Boolean.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module DBSecurityGroupAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBParameterGroupMessage = struct
  type t = { d_b_parameter_group_name : String.t }

  let make ~d_b_parameter_group_name () = { d_b_parameter_group_name }

  let parse xml =
    Some
      { d_b_parameter_group_name =
          Xml.required
            "DBParameterGroupName"
            (Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("DBParameterGroupName", String.to_query v.d_b_parameter_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_parameter_group_name", String.to_json v.d_b_parameter_group_name) ])

  let of_json j =
    { d_b_parameter_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_parameter_group_name"))
    }
end

module CustomAvailabilityZoneAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CopyDBClusterParameterGroupMessage = struct
  type t =
    { source_d_b_cluster_parameter_group_identifier : String.t
    ; target_d_b_cluster_parameter_group_identifier : String.t
    ; target_d_b_cluster_parameter_group_description : String.t
    ; tags : TagList.t
    }

  let make
      ~source_d_b_cluster_parameter_group_identifier
      ~target_d_b_cluster_parameter_group_identifier
      ~target_d_b_cluster_parameter_group_description
      ?(tags = [])
      () =
    { source_d_b_cluster_parameter_group_identifier
    ; target_d_b_cluster_parameter_group_identifier
    ; target_d_b_cluster_parameter_group_description
    ; tags
    }

  let parse xml =
    Some
      { source_d_b_cluster_parameter_group_identifier =
          Xml.required
            "SourceDBClusterParameterGroupIdentifier"
            (Util.option_bind
               (Xml.member "SourceDBClusterParameterGroupIdentifier" xml)
               String.parse)
      ; target_d_b_cluster_parameter_group_identifier =
          Xml.required
            "TargetDBClusterParameterGroupIdentifier"
            (Util.option_bind
               (Xml.member "TargetDBClusterParameterGroupIdentifier" xml)
               String.parse)
      ; target_d_b_cluster_parameter_group_description =
          Xml.required
            "TargetDBClusterParameterGroupDescription"
            (Util.option_bind
               (Xml.member "TargetDBClusterParameterGroupDescription" xml)
               String.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Query.Pair
                ( "TargetDBClusterParameterGroupDescription"
                , String.to_query v.target_d_b_cluster_parameter_group_description ))
         ; Some
             (Query.Pair
                ( "TargetDBClusterParameterGroupIdentifier"
                , String.to_query v.target_d_b_cluster_parameter_group_identifier ))
         ; Some
             (Query.Pair
                ( "SourceDBClusterParameterGroupIdentifier"
                , String.to_query v.source_d_b_cluster_parameter_group_identifier ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some
             ( "target_d_b_cluster_parameter_group_description"
             , String.to_json v.target_d_b_cluster_parameter_group_description )
         ; Some
             ( "target_d_b_cluster_parameter_group_identifier"
             , String.to_json v.target_d_b_cluster_parameter_group_identifier )
         ; Some
             ( "source_d_b_cluster_parameter_group_identifier"
             , String.to_json v.source_d_b_cluster_parameter_group_identifier )
         ])

  let of_json j =
    { source_d_b_cluster_parameter_group_identifier =
        String.of_json
          (Util.of_option_exn
             (Json.lookup j "source_d_b_cluster_parameter_group_identifier"))
    ; target_d_b_cluster_parameter_group_identifier =
        String.of_json
          (Util.of_option_exn
             (Json.lookup j "target_d_b_cluster_parameter_group_identifier"))
    ; target_d_b_cluster_parameter_group_description =
        String.of_json
          (Util.of_option_exn
             (Json.lookup j "target_d_b_cluster_parameter_group_description"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module StartDBInstanceMessage = struct
  type t = { d_b_instance_identifier : String.t }

  let make ~d_b_instance_identifier () = { d_b_instance_identifier }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier) ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    }
end

module RestoreDBClusterFromSnapshotResult = struct
  type t = { d_b_cluster : DBCluster.t option }

  let make ?d_b_cluster () = { d_b_cluster }

  let parse xml =
    Some { d_b_cluster = Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f ->
               Query.Pair ("DBCluster", DBCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f -> "d_b_cluster", DBCluster.to_json f) ])

  let of_json j =
    { d_b_cluster = Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json }
end

module RestoreDBInstanceFromDBSnapshotResult = struct
  type t = { d_b_instance : DBInstance.t option }

  let make ?d_b_instance () = { d_b_instance }

  let parse xml =
    Some
      { d_b_instance = Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f ->
               Query.Pair ("DBInstance", DBInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f -> "d_b_instance", DBInstance.to_json f)
         ])

  let of_json j =
    { d_b_instance = Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json }
end

module RestoreDBClusterFromSnapshotMessage = struct
  type t =
    { availability_zones : AvailabilityZones.t
    ; d_b_cluster_identifier : String.t
    ; snapshot_identifier : String.t
    ; engine : String.t
    ; engine_version : String.t option
    ; port : Integer.t option
    ; d_b_subnet_group_name : String.t option
    ; database_name : String.t option
    ; option_group_name : String.t option
    ; vpc_security_group_ids : VpcSecurityGroupIdList.t
    ; tags : TagList.t
    ; kms_key_id : String.t option
    ; enable_i_a_m_database_authentication : Boolean.t option
    ; backtrack_window : Long.t option
    ; enable_cloudwatch_logs_exports : LogTypeList.t
    ; engine_mode : String.t option
    ; scaling_configuration : ScalingConfiguration.t option
    ; d_b_cluster_parameter_group_name : String.t option
    ; deletion_protection : Boolean.t option
    ; copy_tags_to_snapshot : Boolean.t option
    ; domain : String.t option
    ; domain_i_a_m_role_name : String.t option
    }

  let make
      ?(availability_zones = [])
      ~d_b_cluster_identifier
      ~snapshot_identifier
      ~engine
      ?engine_version
      ?port
      ?d_b_subnet_group_name
      ?database_name
      ?option_group_name
      ?(vpc_security_group_ids = [])
      ?(tags = [])
      ?kms_key_id
      ?enable_i_a_m_database_authentication
      ?backtrack_window
      ?(enable_cloudwatch_logs_exports = [])
      ?engine_mode
      ?scaling_configuration
      ?d_b_cluster_parameter_group_name
      ?deletion_protection
      ?copy_tags_to_snapshot
      ?domain
      ?domain_i_a_m_role_name
      () =
    { availability_zones
    ; d_b_cluster_identifier
    ; snapshot_identifier
    ; engine
    ; engine_version
    ; port
    ; d_b_subnet_group_name
    ; database_name
    ; option_group_name
    ; vpc_security_group_ids
    ; tags
    ; kms_key_id
    ; enable_i_a_m_database_authentication
    ; backtrack_window
    ; enable_cloudwatch_logs_exports
    ; engine_mode
    ; scaling_configuration
    ; d_b_cluster_parameter_group_name
    ; deletion_protection
    ; copy_tags_to_snapshot
    ; domain
    ; domain_i_a_m_role_name
    }

  let parse xml =
    Some
      { availability_zones =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      ; d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; snapshot_identifier =
          Xml.required
            "SnapshotIdentifier"
            (Util.option_bind (Xml.member "SnapshotIdentifier" xml) String.parse)
      ; engine =
          Xml.required "Engine" (Util.option_bind (Xml.member "Engine" xml) String.parse)
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; database_name = Util.option_bind (Xml.member "DatabaseName" xml) String.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupIds" xml)
               VpcSecurityGroupIdList.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; enable_i_a_m_database_authentication =
          Util.option_bind
            (Xml.member "EnableIAMDatabaseAuthentication" xml)
            Boolean.parse
      ; backtrack_window = Util.option_bind (Xml.member "BacktrackWindow" xml) Long.parse
      ; enable_cloudwatch_logs_exports =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EnableCloudwatchLogsExports" xml)
               LogTypeList.parse)
      ; engine_mode = Util.option_bind (Xml.member "EngineMode" xml) String.parse
      ; scaling_configuration =
          Util.option_bind
            (Xml.member "ScalingConfiguration" xml)
            ScalingConfiguration.parse
      ; d_b_cluster_parameter_group_name =
          Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; domain = Util.option_bind (Xml.member "Domain" xml) String.parse
      ; domain_i_a_m_role_name =
          Util.option_bind (Xml.member "DomainIAMRoleName" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.domain_i_a_m_role_name (fun f ->
               Query.Pair ("DomainIAMRoleName", String.to_query f))
         ; Util.option_map v.domain (fun f -> Query.Pair ("Domain", String.to_query f))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               Query.Pair ("DBClusterParameterGroupName", String.to_query f))
         ; Util.option_map v.scaling_configuration (fun f ->
               Query.Pair ("ScalingConfiguration", ScalingConfiguration.to_query f))
         ; Util.option_map v.engine_mode (fun f ->
               Query.Pair ("EngineMode", String.to_query f))
         ; Some
             (Query.Pair
                ( "EnableCloudwatchLogsExports.member"
                , LogTypeList.to_query v.enable_cloudwatch_logs_exports ))
         ; Util.option_map v.backtrack_window (fun f ->
               Query.Pair ("BacktrackWindow", Long.to_query f))
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.database_name (fun f ->
               Query.Pair ("DatabaseName", String.to_query f))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Some (Query.Pair ("Engine", String.to_query v.engine))
         ; Some (Query.Pair ("SnapshotIdentifier", String.to_query v.snapshot_identifier))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ; Some
             (Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.domain_i_a_m_role_name (fun f ->
               "domain_i_a_m_role_name", String.to_json f)
         ; Util.option_map v.domain (fun f -> "domain", String.to_json f)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               "d_b_cluster_parameter_group_name", String.to_json f)
         ; Util.option_map v.scaling_configuration (fun f ->
               "scaling_configuration", ScalingConfiguration.to_json f)
         ; Util.option_map v.engine_mode (fun f -> "engine_mode", String.to_json f)
         ; Some
             ( "enable_cloudwatch_logs_exports"
             , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
         ; Util.option_map v.backtrack_window (fun f ->
               "backtrack_window", Long.to_json f)
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               "enable_i_a_m_database_authentication", Boolean.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Some
             ( "vpc_security_group_ids"
             , VpcSecurityGroupIdList.to_json v.vpc_security_group_ids )
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.database_name (fun f -> "database_name", String.to_json f)
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Some ("engine", String.to_json v.engine)
         ; Some ("snapshot_identifier", String.to_json v.snapshot_identifier)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ; Some ("availability_zones", AvailabilityZones.to_json v.availability_zones)
         ])

  let of_json j =
    { availability_zones =
        AvailabilityZones.of_json
          (Util.of_option_exn (Json.lookup j "availability_zones"))
    ; d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; snapshot_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "snapshot_identifier"))
    ; engine = String.of_json (Util.of_option_exn (Json.lookup j "engine"))
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; database_name = Util.option_map (Json.lookup j "database_name") String.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; vpc_security_group_ids =
        VpcSecurityGroupIdList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; enable_i_a_m_database_authentication =
        Util.option_map
          (Json.lookup j "enable_i_a_m_database_authentication")
          Boolean.of_json
    ; backtrack_window = Util.option_map (Json.lookup j "backtrack_window") Long.of_json
    ; enable_cloudwatch_logs_exports =
        LogTypeList.of_json
          (Util.of_option_exn (Json.lookup j "enable_cloudwatch_logs_exports"))
    ; engine_mode = Util.option_map (Json.lookup j "engine_mode") String.of_json
    ; scaling_configuration =
        Util.option_map
          (Json.lookup j "scaling_configuration")
          ScalingConfiguration.of_json
    ; d_b_cluster_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name") String.of_json
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; domain = Util.option_map (Json.lookup j "domain") String.of_json
    ; domain_i_a_m_role_name =
        Util.option_map (Json.lookup j "domain_i_a_m_role_name") String.of_json
    }
end

module DescribeDBInstancesMessage = struct
  type t =
    { d_b_instance_identifier : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?d_b_instance_identifier ?(filters = []) ?max_records ?marker () =
    { d_b_instance_identifier; filters; max_records; marker }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               Query.Pair ("DBInstanceIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.d_b_instance_identifier (fun f ->
               "d_b_instance_identifier", String.to_json f)
         ])

  let of_json j =
    { d_b_instance_identifier =
        Util.option_map (Json.lookup j "d_b_instance_identifier") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module StopDBClusterResult = struct
  type t = { d_b_cluster : DBCluster.t option }

  let make ?d_b_cluster () = { d_b_cluster }

  let parse xml =
    Some { d_b_cluster = Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f ->
               Query.Pair ("DBCluster", DBCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f -> "d_b_cluster", DBCluster.to_json f) ])

  let of_json j =
    { d_b_cluster = Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json }
end

module CreateCustomAvailabilityZoneResult = struct
  type t = { custom_availability_zone : CustomAvailabilityZone.t option }

  let make ?custom_availability_zone () = { custom_availability_zone }

  let parse xml =
    Some
      { custom_availability_zone =
          Util.option_bind
            (Xml.member "CustomAvailabilityZone" xml)
            CustomAvailabilityZone.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.custom_availability_zone (fun f ->
               Query.Pair ("CustomAvailabilityZone", CustomAvailabilityZone.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.custom_availability_zone (fun f ->
               "custom_availability_zone", CustomAvailabilityZone.to_json f)
         ])

  let of_json j =
    { custom_availability_zone =
        Util.option_map
          (Json.lookup j "custom_availability_zone")
          CustomAvailabilityZone.of_json
    }
end

module DescribeDBProxyTargetGroupsRequest = struct
  type t =
    { d_b_proxy_name : String.t
    ; target_group_name : String.t option
    ; filters : FilterList.t
    ; marker : String.t option
    ; max_records : Integer.t option
    }

  let make ~d_b_proxy_name ?target_group_name ?(filters = []) ?marker ?max_records () =
    { d_b_proxy_name; target_group_name; filters; marker; max_records }

  let parse xml =
    Some
      { d_b_proxy_name =
          Xml.required
            "DBProxyName"
            (Util.option_bind (Xml.member "DBProxyName" xml) String.parse)
      ; target_group_name =
          Util.option_bind (Xml.member "TargetGroupName" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.target_group_name (fun f ->
               Query.Pair ("TargetGroupName", String.to_query f))
         ; Some (Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.target_group_name (fun f ->
               "target_group_name", String.to_json f)
         ; Some ("d_b_proxy_name", String.to_json v.d_b_proxy_name)
         ])

  let of_json j =
    { d_b_proxy_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_proxy_name"))
    ; target_group_name =
        Util.option_map (Json.lookup j "target_group_name") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    }
end

module DeleteEventSubscriptionResult = struct
  type t = { event_subscription : EventSubscription.t option }

  let make ?event_subscription () = { event_subscription }

  let parse xml =
    Some
      { event_subscription =
          Util.option_bind (Xml.member "EventSubscription" xml) EventSubscription.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.event_subscription (fun f ->
               Query.Pair ("EventSubscription", EventSubscription.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.event_subscription (fun f ->
               "event_subscription", EventSubscription.to_json f)
         ])

  let of_json j =
    { event_subscription =
        Util.option_map (Json.lookup j "event_subscription") EventSubscription.of_json
    }
end

module GlobalClusterNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module OptionGroupAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidEventSubscriptionStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBInstanceMessage = struct
  type t =
    { marker : String.t option
    ; d_b_instances : DBInstanceList.t
    }

  let make ?marker ?(d_b_instances = []) () = { marker; d_b_instances }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; d_b_instances =
          Util.of_option
            []
            (Util.option_bind (Xml.member "DBInstances" xml) DBInstanceList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("DBInstances.member", DBInstanceList.to_query v.d_b_instances))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_instances", DBInstanceList.to_json v.d_b_instances)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; d_b_instances =
        DBInstanceList.of_json (Util.of_option_exn (Json.lookup j "d_b_instances"))
    }
end

module CopyDBSnapshotResult = struct
  type t = { d_b_snapshot : DBSnapshot.t option }

  let make ?d_b_snapshot () = { d_b_snapshot }

  let parse xml =
    Some
      { d_b_snapshot = Util.option_bind (Xml.member "DBSnapshot" xml) DBSnapshot.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot (fun f ->
               Query.Pair ("DBSnapshot", DBSnapshot.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot (fun f -> "d_b_snapshot", DBSnapshot.to_json f)
         ])

  let of_json j =
    { d_b_snapshot = Util.option_map (Json.lookup j "d_b_snapshot") DBSnapshot.of_json }
end

module CreateOptionGroupResult = struct
  type t = { option_group : OptionGroup.t option }

  let make ?option_group () = { option_group }

  let parse xml =
    Some
      { option_group = Util.option_bind (Xml.member "OptionGroup" xml) OptionGroup.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.option_group (fun f ->
               Query.Pair ("OptionGroup", OptionGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.option_group (fun f -> "option_group", OptionGroup.to_json f)
         ])

  let of_json j =
    { option_group = Util.option_map (Json.lookup j "option_group") OptionGroup.of_json }
end

module InstallationMediaMessage = struct
  type t =
    { marker : String.t option
    ; installation_media : InstallationMediaList.t
    }

  let make ?marker ?(installation_media = []) () = { marker; installation_media }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; installation_media =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InstallationMedia" xml)
               InstallationMediaList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "InstallationMedia.member"
                , InstallationMediaList.to_query v.installation_media ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("installation_media", InstallationMediaList.to_json v.installation_media)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; installation_media =
        InstallationMediaList.of_json
          (Util.of_option_exn (Json.lookup j "installation_media"))
    }
end

module DBProxyTargetAlreadyRegisteredFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBSubnetGroupNotAllowedFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ExportTasksMessage = struct
  type t =
    { marker : String.t option
    ; export_tasks : ExportTasksList.t
    }

  let make ?marker ?(export_tasks = []) () = { marker; export_tasks }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; export_tasks =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ExportTasks" xml) ExportTasksList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("ExportTasks.member", ExportTasksList.to_query v.export_tasks))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("export_tasks", ExportTasksList.to_json v.export_tasks)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; export_tasks =
        ExportTasksList.of_json (Util.of_option_exn (Json.lookup j "export_tasks"))
    }
end

module ModifyCurrentDBClusterCapacityMessage = struct
  type t =
    { d_b_cluster_identifier : String.t
    ; capacity : Integer.t option
    ; seconds_before_timeout : Integer.t option
    ; timeout_action : String.t option
    }

  let make ~d_b_cluster_identifier ?capacity ?seconds_before_timeout ?timeout_action () =
    { d_b_cluster_identifier; capacity; seconds_before_timeout; timeout_action }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; capacity = Util.option_bind (Xml.member "Capacity" xml) Integer.parse
      ; seconds_before_timeout =
          Util.option_bind (Xml.member "SecondsBeforeTimeout" xml) Integer.parse
      ; timeout_action = Util.option_bind (Xml.member "TimeoutAction" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.timeout_action (fun f ->
               Query.Pair ("TimeoutAction", String.to_query f))
         ; Util.option_map v.seconds_before_timeout (fun f ->
               Query.Pair ("SecondsBeforeTimeout", Integer.to_query f))
         ; Util.option_map v.capacity (fun f ->
               Query.Pair ("Capacity", Integer.to_query f))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.timeout_action (fun f -> "timeout_action", String.to_json f)
         ; Util.option_map v.seconds_before_timeout (fun f ->
               "seconds_before_timeout", Integer.to_json f)
         ; Util.option_map v.capacity (fun f -> "capacity", Integer.to_json f)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; capacity = Util.option_map (Json.lookup j "capacity") Integer.of_json
    ; seconds_before_timeout =
        Util.option_map (Json.lookup j "seconds_before_timeout") Integer.of_json
    ; timeout_action = Util.option_map (Json.lookup j "timeout_action") String.of_json
    }
end

module AccountAttributesMessage = struct
  type t = { account_quotas : AccountQuotaList.t }

  let make ?(account_quotas = []) () = { account_quotas }

  let parse xml =
    Some
      { account_quotas =
          Util.of_option
            []
            (Util.option_bind (Xml.member "AccountQuotas" xml) AccountQuotaList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("AccountQuotas.member", AccountQuotaList.to_query v.account_quotas))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("account_quotas", AccountQuotaList.to_json v.account_quotas) ])

  let of_json j =
    { account_quotas =
        AccountQuotaList.of_json (Util.of_option_exn (Json.lookup j "account_quotas"))
    }
end

module DBSecurityGroupQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeDBProxyTargetsRequest = struct
  type t =
    { d_b_proxy_name : String.t
    ; target_group_name : String.t option
    ; filters : FilterList.t
    ; marker : String.t option
    ; max_records : Integer.t option
    }

  let make ~d_b_proxy_name ?target_group_name ?(filters = []) ?marker ?max_records () =
    { d_b_proxy_name; target_group_name; filters; marker; max_records }

  let parse xml =
    Some
      { d_b_proxy_name =
          Xml.required
            "DBProxyName"
            (Util.option_bind (Xml.member "DBProxyName" xml) String.parse)
      ; target_group_name =
          Util.option_bind (Xml.member "TargetGroupName" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.target_group_name (fun f ->
               Query.Pair ("TargetGroupName", String.to_query f))
         ; Some (Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.target_group_name (fun f ->
               "target_group_name", String.to_json f)
         ; Some ("d_b_proxy_name", String.to_json v.d_b_proxy_name)
         ])

  let of_json j =
    { d_b_proxy_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_proxy_name"))
    ; target_group_name =
        Util.option_map (Json.lookup j "target_group_name") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    }
end

module StorageQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBClusterEndpointMessage = struct
  type t = { d_b_cluster_endpoint_identifier : String.t }

  let make ~d_b_cluster_endpoint_identifier () = { d_b_cluster_endpoint_identifier }

  let parse xml =
    Some
      { d_b_cluster_endpoint_identifier =
          Xml.required
            "DBClusterEndpointIdentifier"
            (Util.option_bind (Xml.member "DBClusterEndpointIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBClusterEndpointIdentifier"
                , String.to_query v.d_b_cluster_endpoint_identifier ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "d_b_cluster_endpoint_identifier"
             , String.to_json v.d_b_cluster_endpoint_identifier )
         ])

  let of_json j =
    { d_b_cluster_endpoint_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_endpoint_identifier"))
    }
end

module ResetDBParameterGroupMessage = struct
  type t =
    { d_b_parameter_group_name : String.t
    ; reset_all_parameters : Boolean.t option
    ; parameters : ParametersList.t
    }

  let make ~d_b_parameter_group_name ?reset_all_parameters ?(parameters = []) () =
    { d_b_parameter_group_name; reset_all_parameters; parameters }

  let parse xml =
    Some
      { d_b_parameter_group_name =
          Xml.required
            "DBParameterGroupName"
            (Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse)
      ; reset_all_parameters =
          Util.option_bind (Xml.member "ResetAllParameters" xml) Boolean.parse
      ; parameters =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Parameters" xml) ParametersList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
         ; Util.option_map v.reset_all_parameters (fun f ->
               Query.Pair ("ResetAllParameters", Boolean.to_query f))
         ; Some
             (Query.Pair
                ("DBParameterGroupName", String.to_query v.d_b_parameter_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("parameters", ParametersList.to_json v.parameters)
         ; Util.option_map v.reset_all_parameters (fun f ->
               "reset_all_parameters", Boolean.to_json f)
         ; Some ("d_b_parameter_group_name", String.to_json v.d_b_parameter_group_name)
         ])

  let of_json j =
    { d_b_parameter_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_parameter_group_name"))
    ; reset_all_parameters =
        Util.option_map (Json.lookup j "reset_all_parameters") Boolean.of_json
    ; parameters =
        ParametersList.of_json (Util.of_option_exn (Json.lookup j "parameters"))
    }
end

module DescribeOptionGroupsMessage = struct
  type t =
    { option_group_name : String.t option
    ; filters : FilterList.t
    ; marker : String.t option
    ; max_records : Integer.t option
    ; engine_name : String.t option
    ; major_engine_version : String.t option
    }

  let make
      ?option_group_name
      ?(filters = [])
      ?marker
      ?max_records
      ?engine_name
      ?major_engine_version
      () =
    { option_group_name; filters; marker; max_records; engine_name; major_engine_version }

  let parse xml =
    Some
      { option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; engine_name = Util.option_bind (Xml.member "EngineName" xml) String.parse
      ; major_engine_version =
          Util.option_bind (Xml.member "MajorEngineVersion" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.major_engine_version (fun f ->
               Query.Pair ("MajorEngineVersion", String.to_query f))
         ; Util.option_map v.engine_name (fun f ->
               Query.Pair ("EngineName", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.major_engine_version (fun f ->
               "major_engine_version", String.to_json f)
         ; Util.option_map v.engine_name (fun f -> "engine_name", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ])

  let of_json j =
    { option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; engine_name = Util.option_map (Json.lookup j "engine_name") String.of_json
    ; major_engine_version =
        Util.option_map (Json.lookup j "major_engine_version") String.of_json
    }
end

module InvalidDBParameterGroupStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBProxyAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteEventSubscriptionMessage = struct
  type t = { subscription_name : String.t }

  let make ~subscription_name () = { subscription_name }

  let parse xml =
    Some
      { subscription_name =
          Xml.required
            "SubscriptionName"
            (Util.option_bind (Xml.member "SubscriptionName" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("SubscriptionName", String.to_query v.subscription_name)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("subscription_name", String.to_json v.subscription_name) ])

  let of_json j =
    { subscription_name =
        String.of_json (Util.of_option_exn (Json.lookup j "subscription_name"))
    }
end

module DBInstanceAutomatedBackupMessage = struct
  type t =
    { marker : String.t option
    ; d_b_instance_automated_backups : DBInstanceAutomatedBackupList.t
    }

  let make ?marker ?(d_b_instance_automated_backups = []) () =
    { marker; d_b_instance_automated_backups }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; d_b_instance_automated_backups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBInstanceAutomatedBackups" xml)
               DBInstanceAutomatedBackupList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBInstanceAutomatedBackups.member"
                , DBInstanceAutomatedBackupList.to_query v.d_b_instance_automated_backups
                ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "d_b_instance_automated_backups"
             , DBInstanceAutomatedBackupList.to_json v.d_b_instance_automated_backups )
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; d_b_instance_automated_backups =
        DBInstanceAutomatedBackupList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_instance_automated_backups"))
    }
end

module DeleteCustomAvailabilityZoneResult = struct
  type t = { custom_availability_zone : CustomAvailabilityZone.t option }

  let make ?custom_availability_zone () = { custom_availability_zone }

  let parse xml =
    Some
      { custom_availability_zone =
          Util.option_bind
            (Xml.member "CustomAvailabilityZone" xml)
            CustomAvailabilityZone.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.custom_availability_zone (fun f ->
               Query.Pair ("CustomAvailabilityZone", CustomAvailabilityZone.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.custom_availability_zone (fun f ->
               "custom_availability_zone", CustomAvailabilityZone.to_json f)
         ])

  let of_json j =
    { custom_availability_zone =
        Util.option_map
          (Json.lookup j "custom_availability_zone")
          CustomAvailabilityZone.of_json
    }
end

module StartActivityStreamResponse = struct
  type t =
    { kms_key_id : String.t option
    ; kinesis_stream_name : String.t option
    ; status : ActivityStreamStatus.t option
    ; mode : ActivityStreamMode.t option
    ; apply_immediately : Boolean.t option
    }

  let make ?kms_key_id ?kinesis_stream_name ?status ?mode ?apply_immediately () =
    { kms_key_id; kinesis_stream_name; status; mode; apply_immediately }

  let parse xml =
    Some
      { kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; kinesis_stream_name =
          Util.option_bind (Xml.member "KinesisStreamName" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) ActivityStreamStatus.parse
      ; mode = Util.option_bind (Xml.member "Mode" xml) ActivityStreamMode.parse
      ; apply_immediately =
          Util.option_bind (Xml.member "ApplyImmediately" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.apply_immediately (fun f ->
               Query.Pair ("ApplyImmediately", Boolean.to_query f))
         ; Util.option_map v.mode (fun f ->
               Query.Pair ("Mode", ActivityStreamMode.to_query f))
         ; Util.option_map v.status (fun f ->
               Query.Pair ("Status", ActivityStreamStatus.to_query f))
         ; Util.option_map v.kinesis_stream_name (fun f ->
               Query.Pair ("KinesisStreamName", String.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.apply_immediately (fun f ->
               "apply_immediately", Boolean.to_json f)
         ; Util.option_map v.mode (fun f -> "mode", ActivityStreamMode.to_json f)
         ; Util.option_map v.status (fun f -> "status", ActivityStreamStatus.to_json f)
         ; Util.option_map v.kinesis_stream_name (fun f ->
               "kinesis_stream_name", String.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ])

  let of_json j =
    { kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; kinesis_stream_name =
        Util.option_map (Json.lookup j "kinesis_stream_name") String.of_json
    ; status = Util.option_map (Json.lookup j "status") ActivityStreamStatus.of_json
    ; mode = Util.option_map (Json.lookup j "mode") ActivityStreamMode.of_json
    ; apply_immediately =
        Util.option_map (Json.lookup j "apply_immediately") Boolean.of_json
    }
end

module DBProxyTargetGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyDBClusterSnapshotAttributeResult = struct
  type t =
    { d_b_cluster_snapshot_attributes_result : DBClusterSnapshotAttributesResult.t option
    }

  let make ?d_b_cluster_snapshot_attributes_result () =
    { d_b_cluster_snapshot_attributes_result }

  let parse xml =
    Some
      { d_b_cluster_snapshot_attributes_result =
          Util.option_bind
            (Xml.member "DBClusterSnapshotAttributesResult" xml)
            DBClusterSnapshotAttributesResult.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_snapshot_attributes_result (fun f ->
               Query.Pair
                 ( "DBClusterSnapshotAttributesResult"
                 , DBClusterSnapshotAttributesResult.to_query f ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster_snapshot_attributes_result (fun f ->
               ( "d_b_cluster_snapshot_attributes_result"
               , DBClusterSnapshotAttributesResult.to_json f ))
         ])

  let of_json j =
    { d_b_cluster_snapshot_attributes_result =
        Util.option_map
          (Json.lookup j "d_b_cluster_snapshot_attributes_result")
          DBClusterSnapshotAttributesResult.of_json
    }
end

module RevokeDBSecurityGroupIngressMessage = struct
  type t =
    { d_b_security_group_name : String.t
    ; c_i_d_r_i_p : String.t option
    ; e_c2_security_group_name : String.t option
    ; e_c2_security_group_id : String.t option
    ; e_c2_security_group_owner_id : String.t option
    }

  let make
      ~d_b_security_group_name
      ?c_i_d_r_i_p
      ?e_c2_security_group_name
      ?e_c2_security_group_id
      ?e_c2_security_group_owner_id
      () =
    { d_b_security_group_name
    ; c_i_d_r_i_p
    ; e_c2_security_group_name
    ; e_c2_security_group_id
    ; e_c2_security_group_owner_id
    }

  let parse xml =
    Some
      { d_b_security_group_name =
          Xml.required
            "DBSecurityGroupName"
            (Util.option_bind (Xml.member "DBSecurityGroupName" xml) String.parse)
      ; c_i_d_r_i_p = Util.option_bind (Xml.member "CIDRIP" xml) String.parse
      ; e_c2_security_group_name =
          Util.option_bind (Xml.member "EC2SecurityGroupName" xml) String.parse
      ; e_c2_security_group_id =
          Util.option_bind (Xml.member "EC2SecurityGroupId" xml) String.parse
      ; e_c2_security_group_owner_id =
          Util.option_bind (Xml.member "EC2SecurityGroupOwnerId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.e_c2_security_group_owner_id (fun f ->
               Query.Pair ("EC2SecurityGroupOwnerId", String.to_query f))
         ; Util.option_map v.e_c2_security_group_id (fun f ->
               Query.Pair ("EC2SecurityGroupId", String.to_query f))
         ; Util.option_map v.e_c2_security_group_name (fun f ->
               Query.Pair ("EC2SecurityGroupName", String.to_query f))
         ; Util.option_map v.c_i_d_r_i_p (fun f ->
               Query.Pair ("CIDRIP", String.to_query f))
         ; Some
             (Query.Pair ("DBSecurityGroupName", String.to_query v.d_b_security_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.e_c2_security_group_owner_id (fun f ->
               "e_c2_security_group_owner_id", String.to_json f)
         ; Util.option_map v.e_c2_security_group_id (fun f ->
               "e_c2_security_group_id", String.to_json f)
         ; Util.option_map v.e_c2_security_group_name (fun f ->
               "e_c2_security_group_name", String.to_json f)
         ; Util.option_map v.c_i_d_r_i_p (fun f -> "c_i_d_r_i_p", String.to_json f)
         ; Some ("d_b_security_group_name", String.to_json v.d_b_security_group_name)
         ])

  let of_json j =
    { d_b_security_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_security_group_name"))
    ; c_i_d_r_i_p = Util.option_map (Json.lookup j "c_i_d_r_i_p") String.of_json
    ; e_c2_security_group_name =
        Util.option_map (Json.lookup j "e_c2_security_group_name") String.of_json
    ; e_c2_security_group_id =
        Util.option_map (Json.lookup j "e_c2_security_group_id") String.of_json
    ; e_c2_security_group_owner_id =
        Util.option_map (Json.lookup j "e_c2_security_group_owner_id") String.of_json
    }
end

module StopActivityStreamRequest = struct
  type t =
    { resource_arn : String.t
    ; apply_immediately : Boolean.t option
    }

  let make ~resource_arn ?apply_immediately () = { resource_arn; apply_immediately }

  let parse xml =
    Some
      { resource_arn =
          Xml.required
            "ResourceArn"
            (Util.option_bind (Xml.member "ResourceArn" xml) String.parse)
      ; apply_immediately =
          Util.option_bind (Xml.member "ApplyImmediately" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.apply_immediately (fun f ->
               Query.Pair ("ApplyImmediately", Boolean.to_query f))
         ; Some (Query.Pair ("ResourceArn", String.to_query v.resource_arn))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.apply_immediately (fun f ->
               "apply_immediately", Boolean.to_json f)
         ; Some ("resource_arn", String.to_json v.resource_arn)
         ])

  let of_json j =
    { resource_arn = String.of_json (Util.of_option_exn (Json.lookup j "resource_arn"))
    ; apply_immediately =
        Util.option_map (Json.lookup j "apply_immediately") Boolean.of_json
    }
end

module InvalidGlobalClusterStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ReservedDBInstanceAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CopyDBSnapshotMessage = struct
  type t =
    { source_d_b_snapshot_identifier : String.t
    ; target_d_b_snapshot_identifier : String.t
    ; kms_key_id : String.t option
    ; tags : TagList.t
    ; copy_tags : Boolean.t option
    ; pre_signed_url : String.t option
    ; option_group_name : String.t option
    ; target_custom_availability_zone : String.t option
    }

  let make
      ~source_d_b_snapshot_identifier
      ~target_d_b_snapshot_identifier
      ?kms_key_id
      ?(tags = [])
      ?copy_tags
      ?pre_signed_url
      ?option_group_name
      ?target_custom_availability_zone
      () =
    { source_d_b_snapshot_identifier
    ; target_d_b_snapshot_identifier
    ; kms_key_id
    ; tags
    ; copy_tags
    ; pre_signed_url
    ; option_group_name
    ; target_custom_availability_zone
    }

  let parse xml =
    Some
      { source_d_b_snapshot_identifier =
          Xml.required
            "SourceDBSnapshotIdentifier"
            (Util.option_bind (Xml.member "SourceDBSnapshotIdentifier" xml) String.parse)
      ; target_d_b_snapshot_identifier =
          Xml.required
            "TargetDBSnapshotIdentifier"
            (Util.option_bind (Xml.member "TargetDBSnapshotIdentifier" xml) String.parse)
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; copy_tags = Util.option_bind (Xml.member "CopyTags" xml) Boolean.parse
      ; pre_signed_url = Util.option_bind (Xml.member "PreSignedUrl" xml) String.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; target_custom_availability_zone =
          Util.option_bind (Xml.member "TargetCustomAvailabilityZone" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.target_custom_availability_zone (fun f ->
               Query.Pair ("TargetCustomAvailabilityZone", String.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.pre_signed_url (fun f ->
               Query.Pair ("PreSignedUrl", String.to_query f))
         ; Util.option_map v.copy_tags (fun f ->
               Query.Pair ("CopyTags", Boolean.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Some
             (Query.Pair
                ( "TargetDBSnapshotIdentifier"
                , String.to_query v.target_d_b_snapshot_identifier ))
         ; Some
             (Query.Pair
                ( "SourceDBSnapshotIdentifier"
                , String.to_query v.source_d_b_snapshot_identifier ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.target_custom_availability_zone (fun f ->
               "target_custom_availability_zone", String.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.pre_signed_url (fun f -> "pre_signed_url", String.to_json f)
         ; Util.option_map v.copy_tags (fun f -> "copy_tags", Boolean.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Some
             ( "target_d_b_snapshot_identifier"
             , String.to_json v.target_d_b_snapshot_identifier )
         ; Some
             ( "source_d_b_snapshot_identifier"
             , String.to_json v.source_d_b_snapshot_identifier )
         ])

  let of_json j =
    { source_d_b_snapshot_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "source_d_b_snapshot_identifier"))
    ; target_d_b_snapshot_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "target_d_b_snapshot_identifier"))
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; copy_tags = Util.option_map (Json.lookup j "copy_tags") Boolean.of_json
    ; pre_signed_url = Util.option_map (Json.lookup j "pre_signed_url") String.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; target_custom_availability_zone =
        Util.option_map (Json.lookup j "target_custom_availability_zone") String.of_json
    }
end

module RevokeDBSecurityGroupIngressResult = struct
  type t = { d_b_security_group : DBSecurityGroup.t option }

  let make ?d_b_security_group () = { d_b_security_group }

  let parse xml =
    Some
      { d_b_security_group =
          Util.option_bind (Xml.member "DBSecurityGroup" xml) DBSecurityGroup.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_security_group (fun f ->
               Query.Pair ("DBSecurityGroup", DBSecurityGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_security_group (fun f ->
               "d_b_security_group", DBSecurityGroup.to_json f)
         ])

  let of_json j =
    { d_b_security_group =
        Util.option_map (Json.lookup j "d_b_security_group") DBSecurityGroup.of_json
    }
end

module DBClusterEndpointNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeEventsMessage = struct
  type t =
    { source_identifier : String.t option
    ; source_type : SourceType.t option
    ; start_time : DateTime.t option
    ; end_time : DateTime.t option
    ; duration : Integer.t option
    ; event_categories : EventCategoriesList.t
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ?source_identifier
      ?source_type
      ?start_time
      ?end_time
      ?duration
      ?(event_categories = [])
      ?(filters = [])
      ?max_records
      ?marker
      () =
    { source_identifier
    ; source_type
    ; start_time
    ; end_time
    ; duration
    ; event_categories
    ; filters
    ; max_records
    ; marker
    }

  let parse xml =
    Some
      { source_identifier =
          Util.option_bind (Xml.member "SourceIdentifier" xml) String.parse
      ; source_type = Util.option_bind (Xml.member "SourceType" xml) SourceType.parse
      ; start_time = Util.option_bind (Xml.member "StartTime" xml) DateTime.parse
      ; end_time = Util.option_bind (Xml.member "EndTime" xml) DateTime.parse
      ; duration = Util.option_bind (Xml.member "Duration" xml) Integer.parse
      ; event_categories =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EventCategories" xml)
               EventCategoriesList.parse)
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Some
             (Query.Pair
                ("EventCategories.member", EventCategoriesList.to_query v.event_categories))
         ; Util.option_map v.duration (fun f ->
               Query.Pair ("Duration", Integer.to_query f))
         ; Util.option_map v.end_time (fun f ->
               Query.Pair ("EndTime", DateTime.to_query f))
         ; Util.option_map v.start_time (fun f ->
               Query.Pair ("StartTime", DateTime.to_query f))
         ; Util.option_map v.source_type (fun f ->
               Query.Pair ("SourceType", SourceType.to_query f))
         ; Util.option_map v.source_identifier (fun f ->
               Query.Pair ("SourceIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Some ("event_categories", EventCategoriesList.to_json v.event_categories)
         ; Util.option_map v.duration (fun f -> "duration", Integer.to_json f)
         ; Util.option_map v.end_time (fun f -> "end_time", DateTime.to_json f)
         ; Util.option_map v.start_time (fun f -> "start_time", DateTime.to_json f)
         ; Util.option_map v.source_type (fun f -> "source_type", SourceType.to_json f)
         ; Util.option_map v.source_identifier (fun f ->
               "source_identifier", String.to_json f)
         ])

  let of_json j =
    { source_identifier =
        Util.option_map (Json.lookup j "source_identifier") String.of_json
    ; source_type = Util.option_map (Json.lookup j "source_type") SourceType.of_json
    ; start_time = Util.option_map (Json.lookup j "start_time") DateTime.of_json
    ; end_time = Util.option_map (Json.lookup j "end_time") DateTime.of_json
    ; duration = Util.option_map (Json.lookup j "duration") Integer.of_json
    ; event_categories =
        EventCategoriesList.of_json
          (Util.of_option_exn (Json.lookup j "event_categories"))
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module InvalidDBClusterStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CertificateNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ReservedDBInstanceNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module StopDBInstanceResult = struct
  type t = { d_b_instance : DBInstance.t option }

  let make ?d_b_instance () = { d_b_instance }

  let parse xml =
    Some
      { d_b_instance = Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f ->
               Query.Pair ("DBInstance", DBInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f -> "d_b_instance", DBInstance.to_json f)
         ])

  let of_json j =
    { d_b_instance = Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json }
end

module AddRoleToDBInstanceMessage = struct
  type t =
    { d_b_instance_identifier : String.t
    ; role_arn : String.t
    ; feature_name : String.t
    }

  let make ~d_b_instance_identifier ~role_arn ~feature_name () =
    { d_b_instance_identifier; role_arn; feature_name }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; role_arn =
          Xml.required
            "RoleArn"
            (Util.option_bind (Xml.member "RoleArn" xml) String.parse)
      ; feature_name =
          Xml.required
            "FeatureName"
            (Util.option_bind (Xml.member "FeatureName" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("FeatureName", String.to_query v.feature_name))
         ; Some (Query.Pair ("RoleArn", String.to_query v.role_arn))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("feature_name", String.to_json v.feature_name)
         ; Some ("role_arn", String.to_json v.role_arn)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; role_arn = String.of_json (Util.of_option_exn (Json.lookup j "role_arn"))
    ; feature_name = String.of_json (Util.of_option_exn (Json.lookup j "feature_name"))
    }
end

module ModifyDBSnapshotAttributeMessage = struct
  type t =
    { d_b_snapshot_identifier : String.t
    ; attribute_name : String.t
    ; values_to_add : AttributeValueList.t
    ; values_to_remove : AttributeValueList.t
    }

  let make
      ~d_b_snapshot_identifier
      ~attribute_name
      ?(values_to_add = [])
      ?(values_to_remove = [])
      () =
    { d_b_snapshot_identifier; attribute_name; values_to_add; values_to_remove }

  let parse xml =
    Some
      { d_b_snapshot_identifier =
          Xml.required
            "DBSnapshotIdentifier"
            (Util.option_bind (Xml.member "DBSnapshotIdentifier" xml) String.parse)
      ; attribute_name =
          Xml.required
            "AttributeName"
            (Util.option_bind (Xml.member "AttributeName" xml) String.parse)
      ; values_to_add =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ValuesToAdd" xml) AttributeValueList.parse)
      ; values_to_remove =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ValuesToRemove" xml) AttributeValueList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("ValuesToRemove.member", AttributeValueList.to_query v.values_to_remove))
         ; Some
             (Query.Pair
                ("ValuesToAdd.member", AttributeValueList.to_query v.values_to_add))
         ; Some (Query.Pair ("AttributeName", String.to_query v.attribute_name))
         ; Some
             (Query.Pair
                ("DBSnapshotIdentifier", String.to_query v.d_b_snapshot_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("values_to_remove", AttributeValueList.to_json v.values_to_remove)
         ; Some ("values_to_add", AttributeValueList.to_json v.values_to_add)
         ; Some ("attribute_name", String.to_json v.attribute_name)
         ; Some ("d_b_snapshot_identifier", String.to_json v.d_b_snapshot_identifier)
         ])

  let of_json j =
    { d_b_snapshot_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_snapshot_identifier"))
    ; attribute_name =
        String.of_json (Util.of_option_exn (Json.lookup j "attribute_name"))
    ; values_to_add =
        AttributeValueList.of_json (Util.of_option_exn (Json.lookup j "values_to_add"))
    ; values_to_remove =
        AttributeValueList.of_json (Util.of_option_exn (Json.lookup j "values_to_remove"))
    }
end

module DescribeDBClusterParametersMessage = struct
  type t =
    { d_b_cluster_parameter_group_name : String.t
    ; source : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ~d_b_cluster_parameter_group_name
      ?source
      ?(filters = [])
      ?max_records
      ?marker
      () =
    { d_b_cluster_parameter_group_name; source; filters; max_records; marker }

  let parse xml =
    Some
      { d_b_cluster_parameter_group_name =
          Xml.required
            "DBClusterParameterGroupName"
            (Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse)
      ; source = Util.option_bind (Xml.member "Source" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.source (fun f -> Query.Pair ("Source", String.to_query f))
         ; Some
             (Query.Pair
                ( "DBClusterParameterGroupName"
                , String.to_query v.d_b_cluster_parameter_group_name ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.source (fun f -> "source", String.to_json f)
         ; Some
             ( "d_b_cluster_parameter_group_name"
             , String.to_json v.d_b_cluster_parameter_group_name )
         ])

  let of_json j =
    { d_b_cluster_parameter_group_name =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_parameter_group_name"))
    ; source = Util.option_map (Json.lookup j "source") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module CreateDBProxyRequest = struct
  type t =
    { d_b_proxy_name : String.t
    ; engine_family : EngineFamily.t
    ; auth : UserAuthConfigList.t
    ; role_arn : String.t
    ; vpc_subnet_ids : StringList.t
    ; vpc_security_group_ids : StringList.t
    ; require_t_l_s : Boolean.t option
    ; idle_client_timeout : Integer.t option
    ; debug_logging : Boolean.t option
    ; tags : TagList.t
    }

  let make
      ~d_b_proxy_name
      ~engine_family
      ~auth
      ~role_arn
      ~vpc_subnet_ids
      ?(vpc_security_group_ids = [])
      ?require_t_l_s
      ?idle_client_timeout
      ?debug_logging
      ?(tags = [])
      () =
    { d_b_proxy_name
    ; engine_family
    ; auth
    ; role_arn
    ; vpc_subnet_ids
    ; vpc_security_group_ids
    ; require_t_l_s
    ; idle_client_timeout
    ; debug_logging
    ; tags
    }

  let parse xml =
    Some
      { d_b_proxy_name =
          Xml.required
            "DBProxyName"
            (Util.option_bind (Xml.member "DBProxyName" xml) String.parse)
      ; engine_family =
          Xml.required
            "EngineFamily"
            (Util.option_bind (Xml.member "EngineFamily" xml) EngineFamily.parse)
      ; auth =
          Xml.required
            "Auth"
            (Util.option_bind (Xml.member "Auth" xml) UserAuthConfigList.parse)
      ; role_arn =
          Xml.required
            "RoleArn"
            (Util.option_bind (Xml.member "RoleArn" xml) String.parse)
      ; vpc_subnet_ids =
          Xml.required
            "VpcSubnetIds"
            (Util.option_bind (Xml.member "VpcSubnetIds" xml) StringList.parse)
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind (Xml.member "VpcSecurityGroupIds" xml) StringList.parse)
      ; require_t_l_s = Util.option_bind (Xml.member "RequireTLS" xml) Boolean.parse
      ; idle_client_timeout =
          Util.option_bind (Xml.member "IdleClientTimeout" xml) Integer.parse
      ; debug_logging = Util.option_bind (Xml.member "DebugLogging" xml) Boolean.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.debug_logging (fun f ->
               Query.Pair ("DebugLogging", Boolean.to_query f))
         ; Util.option_map v.idle_client_timeout (fun f ->
               Query.Pair ("IdleClientTimeout", Integer.to_query f))
         ; Util.option_map v.require_t_l_s (fun f ->
               Query.Pair ("RequireTLS", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , StringList.to_query v.vpc_security_group_ids ))
         ; Some (Query.Pair ("VpcSubnetIds.member", StringList.to_query v.vpc_subnet_ids))
         ; Some (Query.Pair ("RoleArn", String.to_query v.role_arn))
         ; Some (Query.Pair ("Auth.member", UserAuthConfigList.to_query v.auth))
         ; Some (Query.Pair ("EngineFamily", EngineFamily.to_query v.engine_family))
         ; Some (Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.debug_logging (fun f -> "debug_logging", Boolean.to_json f)
         ; Util.option_map v.idle_client_timeout (fun f ->
               "idle_client_timeout", Integer.to_json f)
         ; Util.option_map v.require_t_l_s (fun f -> "require_t_l_s", Boolean.to_json f)
         ; Some ("vpc_security_group_ids", StringList.to_json v.vpc_security_group_ids)
         ; Some ("vpc_subnet_ids", StringList.to_json v.vpc_subnet_ids)
         ; Some ("role_arn", String.to_json v.role_arn)
         ; Some ("auth", UserAuthConfigList.to_json v.auth)
         ; Some ("engine_family", EngineFamily.to_json v.engine_family)
         ; Some ("d_b_proxy_name", String.to_json v.d_b_proxy_name)
         ])

  let of_json j =
    { d_b_proxy_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_proxy_name"))
    ; engine_family =
        EngineFamily.of_json (Util.of_option_exn (Json.lookup j "engine_family"))
    ; auth = UserAuthConfigList.of_json (Util.of_option_exn (Json.lookup j "auth"))
    ; role_arn = String.of_json (Util.of_option_exn (Json.lookup j "role_arn"))
    ; vpc_subnet_ids =
        StringList.of_json (Util.of_option_exn (Json.lookup j "vpc_subnet_ids"))
    ; vpc_security_group_ids =
        StringList.of_json (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; require_t_l_s = Util.option_map (Json.lookup j "require_t_l_s") Boolean.of_json
    ; idle_client_timeout =
        Util.option_map (Json.lookup j "idle_client_timeout") Integer.of_json
    ; debug_logging = Util.option_map (Json.lookup j "debug_logging") Boolean.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module DeleteCustomAvailabilityZoneMessage = struct
  type t = { custom_availability_zone_id : String.t }

  let make ~custom_availability_zone_id () = { custom_availability_zone_id }

  let parse xml =
    Some
      { custom_availability_zone_id =
          Xml.required
            "CustomAvailabilityZoneId"
            (Util.option_bind (Xml.member "CustomAvailabilityZoneId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("CustomAvailabilityZoneId", String.to_query v.custom_availability_zone_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ("custom_availability_zone_id", String.to_json v.custom_availability_zone_id)
         ])

  let of_json j =
    { custom_availability_zone_id =
        String.of_json (Util.of_option_exn (Json.lookup j "custom_availability_zone_id"))
    }
end

module RemoveRoleFromDBClusterMessage = struct
  type t =
    { d_b_cluster_identifier : String.t
    ; role_arn : String.t
    ; feature_name : String.t option
    }

  let make ~d_b_cluster_identifier ~role_arn ?feature_name () =
    { d_b_cluster_identifier; role_arn; feature_name }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; role_arn =
          Xml.required
            "RoleArn"
            (Util.option_bind (Xml.member "RoleArn" xml) String.parse)
      ; feature_name = Util.option_bind (Xml.member "FeatureName" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.feature_name (fun f ->
               Query.Pair ("FeatureName", String.to_query f))
         ; Some (Query.Pair ("RoleArn", String.to_query v.role_arn))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.feature_name (fun f -> "feature_name", String.to_json f)
         ; Some ("role_arn", String.to_json v.role_arn)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; role_arn = String.of_json (Util.of_option_exn (Json.lookup j "role_arn"))
    ; feature_name = Util.option_map (Json.lookup j "feature_name") String.of_json
    }
end

module InvalidDBClusterCapacityFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module PromoteReadReplicaDBClusterResult = struct
  type t = { d_b_cluster : DBCluster.t option }

  let make ?d_b_cluster () = { d_b_cluster }

  let parse xml =
    Some { d_b_cluster = Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f ->
               Query.Pair ("DBCluster", DBCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f -> "d_b_cluster", DBCluster.to_json f) ])

  let of_json j =
    { d_b_cluster = Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json }
end

module SnapshotQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBInstanceRoleNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyDBProxyTargetGroupResponse = struct
  type t = { d_b_proxy_target_group : DBProxyTargetGroup.t option }

  let make ?d_b_proxy_target_group () = { d_b_proxy_target_group }

  let parse xml =
    Some
      { d_b_proxy_target_group =
          Util.option_bind (Xml.member "DBProxyTargetGroup" xml) DBProxyTargetGroup.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_proxy_target_group (fun f ->
               Query.Pair ("DBProxyTargetGroup", DBProxyTargetGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_proxy_target_group (fun f ->
               "d_b_proxy_target_group", DBProxyTargetGroup.to_json f)
         ])

  let of_json j =
    { d_b_proxy_target_group =
        Util.option_map
          (Json.lookup j "d_b_proxy_target_group")
          DBProxyTargetGroup.of_json
    }
end

module DBClusterNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribePendingMaintenanceActionsMessage = struct
  type t =
    { resource_identifier : String.t option
    ; filters : FilterList.t
    ; marker : String.t option
    ; max_records : Integer.t option
    }

  let make ?resource_identifier ?(filters = []) ?marker ?max_records () =
    { resource_identifier; filters; marker; max_records }

  let parse xml =
    Some
      { resource_identifier =
          Util.option_bind (Xml.member "ResourceIdentifier" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.resource_identifier (fun f ->
               Query.Pair ("ResourceIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.resource_identifier (fun f ->
               "resource_identifier", String.to_json f)
         ])

  let of_json j =
    { resource_identifier =
        Util.option_map (Json.lookup j "resource_identifier") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    }
end

module DescribeDBClusterParameterGroupsMessage = struct
  type t =
    { d_b_cluster_parameter_group_name : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?d_b_cluster_parameter_group_name ?(filters = []) ?max_records ?marker () =
    { d_b_cluster_parameter_group_name; filters; max_records; marker }

  let parse xml =
    Some
      { d_b_cluster_parameter_group_name =
          Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               Query.Pair ("DBClusterParameterGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
               "d_b_cluster_parameter_group_name", String.to_json f)
         ])

  let of_json j =
    { d_b_cluster_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module PromoteReadReplicaResult = struct
  type t = { d_b_instance : DBInstance.t option }

  let make ?d_b_instance () = { d_b_instance }

  let parse xml =
    Some
      { d_b_instance = Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f ->
               Query.Pair ("DBInstance", DBInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_instance (fun f -> "d_b_instance", DBInstance.to_json f)
         ])

  let of_json j =
    { d_b_instance = Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json }
end

module CancelExportTaskMessage = struct
  type t = { export_task_identifier : String.t }

  let make ~export_task_identifier () = { export_task_identifier }

  let parse xml =
    Some
      { export_task_identifier =
          Xml.required
            "ExportTaskIdentifier"
            (Util.option_bind (Xml.member "ExportTaskIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("ExportTaskIdentifier", String.to_query v.export_task_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("export_task_identifier", String.to_json v.export_task_identifier) ])

  let of_json j =
    { export_task_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "export_task_identifier"))
    }
end

module CreateDBInstanceMessage = struct
  type t =
    { d_b_name : String.t option
    ; d_b_instance_identifier : String.t
    ; allocated_storage : Integer.t option
    ; d_b_instance_class : String.t
    ; engine : String.t
    ; master_username : String.t option
    ; master_user_password : String.t option
    ; d_b_security_groups : DBSecurityGroupNameList.t
    ; vpc_security_group_ids : VpcSecurityGroupIdList.t
    ; availability_zone : String.t option
    ; d_b_subnet_group_name : String.t option
    ; preferred_maintenance_window : String.t option
    ; d_b_parameter_group_name : String.t option
    ; backup_retention_period : Integer.t option
    ; preferred_backup_window : String.t option
    ; port : Integer.t option
    ; multi_a_z : Boolean.t option
    ; engine_version : String.t option
    ; auto_minor_version_upgrade : Boolean.t option
    ; license_model : String.t option
    ; iops : Integer.t option
    ; option_group_name : String.t option
    ; character_set_name : String.t option
    ; nchar_character_set_name : String.t option
    ; publicly_accessible : Boolean.t option
    ; tags : TagList.t
    ; d_b_cluster_identifier : String.t option
    ; storage_type : String.t option
    ; tde_credential_arn : String.t option
    ; tde_credential_password : String.t option
    ; storage_encrypted : Boolean.t option
    ; kms_key_id : String.t option
    ; domain : String.t option
    ; copy_tags_to_snapshot : Boolean.t option
    ; monitoring_interval : Integer.t option
    ; monitoring_role_arn : String.t option
    ; domain_i_a_m_role_name : String.t option
    ; promotion_tier : Integer.t option
    ; timezone : String.t option
    ; enable_i_a_m_database_authentication : Boolean.t option
    ; enable_performance_insights : Boolean.t option
    ; performance_insights_k_m_s_key_id : String.t option
    ; performance_insights_retention_period : Integer.t option
    ; enable_cloudwatch_logs_exports : LogTypeList.t
    ; processor_features : ProcessorFeatureList.t
    ; deletion_protection : Boolean.t option
    ; max_allocated_storage : Integer.t option
    }

  let make
      ?d_b_name
      ~d_b_instance_identifier
      ?allocated_storage
      ~d_b_instance_class
      ~engine
      ?master_username
      ?master_user_password
      ?(d_b_security_groups = [])
      ?(vpc_security_group_ids = [])
      ?availability_zone
      ?d_b_subnet_group_name
      ?preferred_maintenance_window
      ?d_b_parameter_group_name
      ?backup_retention_period
      ?preferred_backup_window
      ?port
      ?multi_a_z
      ?engine_version
      ?auto_minor_version_upgrade
      ?license_model
      ?iops
      ?option_group_name
      ?character_set_name
      ?nchar_character_set_name
      ?publicly_accessible
      ?(tags = [])
      ?d_b_cluster_identifier
      ?storage_type
      ?tde_credential_arn
      ?tde_credential_password
      ?storage_encrypted
      ?kms_key_id
      ?domain
      ?copy_tags_to_snapshot
      ?monitoring_interval
      ?monitoring_role_arn
      ?domain_i_a_m_role_name
      ?promotion_tier
      ?timezone
      ?enable_i_a_m_database_authentication
      ?enable_performance_insights
      ?performance_insights_k_m_s_key_id
      ?performance_insights_retention_period
      ?(enable_cloudwatch_logs_exports = [])
      ?(processor_features = [])
      ?deletion_protection
      ?max_allocated_storage
      () =
    { d_b_name
    ; d_b_instance_identifier
    ; allocated_storage
    ; d_b_instance_class
    ; engine
    ; master_username
    ; master_user_password
    ; d_b_security_groups
    ; vpc_security_group_ids
    ; availability_zone
    ; d_b_subnet_group_name
    ; preferred_maintenance_window
    ; d_b_parameter_group_name
    ; backup_retention_period
    ; preferred_backup_window
    ; port
    ; multi_a_z
    ; engine_version
    ; auto_minor_version_upgrade
    ; license_model
    ; iops
    ; option_group_name
    ; character_set_name
    ; nchar_character_set_name
    ; publicly_accessible
    ; tags
    ; d_b_cluster_identifier
    ; storage_type
    ; tde_credential_arn
    ; tde_credential_password
    ; storage_encrypted
    ; kms_key_id
    ; domain
    ; copy_tags_to_snapshot
    ; monitoring_interval
    ; monitoring_role_arn
    ; domain_i_a_m_role_name
    ; promotion_tier
    ; timezone
    ; enable_i_a_m_database_authentication
    ; enable_performance_insights
    ; performance_insights_k_m_s_key_id
    ; performance_insights_retention_period
    ; enable_cloudwatch_logs_exports
    ; processor_features
    ; deletion_protection
    ; max_allocated_storage
    }

  let parse xml =
    Some
      { d_b_name = Util.option_bind (Xml.member "DBName" xml) String.parse
      ; d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; allocated_storage =
          Util.option_bind (Xml.member "AllocatedStorage" xml) Integer.parse
      ; d_b_instance_class =
          Xml.required
            "DBInstanceClass"
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse)
      ; engine =
          Xml.required "Engine" (Util.option_bind (Xml.member "Engine" xml) String.parse)
      ; master_username = Util.option_bind (Xml.member "MasterUsername" xml) String.parse
      ; master_user_password =
          Util.option_bind (Xml.member "MasterUserPassword" xml) String.parse
      ; d_b_security_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBSecurityGroups" xml)
               DBSecurityGroupNameList.parse)
      ; vpc_security_group_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "VpcSecurityGroupIds" xml)
               VpcSecurityGroupIdList.parse)
      ; availability_zone =
          Util.option_bind (Xml.member "AvailabilityZone" xml) String.parse
      ; d_b_subnet_group_name =
          Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse
      ; preferred_maintenance_window =
          Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml) String.parse
      ; d_b_parameter_group_name =
          Util.option_bind (Xml.member "DBParameterGroupName" xml) String.parse
      ; backup_retention_period =
          Util.option_bind (Xml.member "BackupRetentionPeriod" xml) Integer.parse
      ; preferred_backup_window =
          Util.option_bind (Xml.member "PreferredBackupWindow" xml) String.parse
      ; port = Util.option_bind (Xml.member "Port" xml) Integer.parse
      ; multi_a_z = Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; auto_minor_version_upgrade =
          Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
      ; license_model = Util.option_bind (Xml.member "LicenseModel" xml) String.parse
      ; iops = Util.option_bind (Xml.member "Iops" xml) Integer.parse
      ; option_group_name =
          Util.option_bind (Xml.member "OptionGroupName" xml) String.parse
      ; character_set_name =
          Util.option_bind (Xml.member "CharacterSetName" xml) String.parse
      ; nchar_character_set_name =
          Util.option_bind (Xml.member "NcharCharacterSetName" xml) String.parse
      ; publicly_accessible =
          Util.option_bind (Xml.member "PubliclyAccessible" xml) Boolean.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; d_b_cluster_identifier =
          Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse
      ; storage_type = Util.option_bind (Xml.member "StorageType" xml) String.parse
      ; tde_credential_arn =
          Util.option_bind (Xml.member "TdeCredentialArn" xml) String.parse
      ; tde_credential_password =
          Util.option_bind (Xml.member "TdeCredentialPassword" xml) String.parse
      ; storage_encrypted =
          Util.option_bind (Xml.member "StorageEncrypted" xml) Boolean.parse
      ; kms_key_id = Util.option_bind (Xml.member "KmsKeyId" xml) String.parse
      ; domain = Util.option_bind (Xml.member "Domain" xml) String.parse
      ; copy_tags_to_snapshot =
          Util.option_bind (Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
      ; monitoring_interval =
          Util.option_bind (Xml.member "MonitoringInterval" xml) Integer.parse
      ; monitoring_role_arn =
          Util.option_bind (Xml.member "MonitoringRoleArn" xml) String.parse
      ; domain_i_a_m_role_name =
          Util.option_bind (Xml.member "DomainIAMRoleName" xml) String.parse
      ; promotion_tier = Util.option_bind (Xml.member "PromotionTier" xml) Integer.parse
      ; timezone = Util.option_bind (Xml.member "Timezone" xml) String.parse
      ; enable_i_a_m_database_authentication =
          Util.option_bind
            (Xml.member "EnableIAMDatabaseAuthentication" xml)
            Boolean.parse
      ; enable_performance_insights =
          Util.option_bind (Xml.member "EnablePerformanceInsights" xml) Boolean.parse
      ; performance_insights_k_m_s_key_id =
          Util.option_bind (Xml.member "PerformanceInsightsKMSKeyId" xml) String.parse
      ; performance_insights_retention_period =
          Util.option_bind
            (Xml.member "PerformanceInsightsRetentionPeriod" xml)
            Integer.parse
      ; enable_cloudwatch_logs_exports =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EnableCloudwatchLogsExports" xml)
               LogTypeList.parse)
      ; processor_features =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ProcessorFeatures" xml)
               ProcessorFeatureList.parse)
      ; deletion_protection =
          Util.option_bind (Xml.member "DeletionProtection" xml) Boolean.parse
      ; max_allocated_storage =
          Util.option_bind (Xml.member "MaxAllocatedStorage" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.max_allocated_storage (fun f ->
               Query.Pair ("MaxAllocatedStorage", Integer.to_query f))
         ; Util.option_map v.deletion_protection (fun f ->
               Query.Pair ("DeletionProtection", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "ProcessorFeatures.member"
                , ProcessorFeatureList.to_query v.processor_features ))
         ; Some
             (Query.Pair
                ( "EnableCloudwatchLogsExports.member"
                , LogTypeList.to_query v.enable_cloudwatch_logs_exports ))
         ; Util.option_map v.performance_insights_retention_period (fun f ->
               Query.Pair ("PerformanceInsightsRetentionPeriod", Integer.to_query f))
         ; Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
               Query.Pair ("PerformanceInsightsKMSKeyId", String.to_query f))
         ; Util.option_map v.enable_performance_insights (fun f ->
               Query.Pair ("EnablePerformanceInsights", Boolean.to_query f))
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
         ; Util.option_map v.timezone (fun f ->
               Query.Pair ("Timezone", String.to_query f))
         ; Util.option_map v.promotion_tier (fun f ->
               Query.Pair ("PromotionTier", Integer.to_query f))
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               Query.Pair ("DomainIAMRoleName", String.to_query f))
         ; Util.option_map v.monitoring_role_arn (fun f ->
               Query.Pair ("MonitoringRoleArn", String.to_query f))
         ; Util.option_map v.monitoring_interval (fun f ->
               Query.Pair ("MonitoringInterval", Integer.to_query f))
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
         ; Util.option_map v.domain (fun f -> Query.Pair ("Domain", String.to_query f))
         ; Util.option_map v.kms_key_id (fun f ->
               Query.Pair ("KmsKeyId", String.to_query f))
         ; Util.option_map v.storage_encrypted (fun f ->
               Query.Pair ("StorageEncrypted", Boolean.to_query f))
         ; Util.option_map v.tde_credential_password (fun f ->
               Query.Pair ("TdeCredentialPassword", String.to_query f))
         ; Util.option_map v.tde_credential_arn (fun f ->
               Query.Pair ("TdeCredentialArn", String.to_query f))
         ; Util.option_map v.storage_type (fun f ->
               Query.Pair ("StorageType", String.to_query f))
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               Query.Pair ("DBClusterIdentifier", String.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.publicly_accessible (fun f ->
               Query.Pair ("PubliclyAccessible", Boolean.to_query f))
         ; Util.option_map v.nchar_character_set_name (fun f ->
               Query.Pair ("NcharCharacterSetName", String.to_query f))
         ; Util.option_map v.character_set_name (fun f ->
               Query.Pair ("CharacterSetName", String.to_query f))
         ; Util.option_map v.option_group_name (fun f ->
               Query.Pair ("OptionGroupName", String.to_query f))
         ; Util.option_map v.iops (fun f -> Query.Pair ("Iops", Integer.to_query f))
         ; Util.option_map v.license_model (fun f ->
               Query.Pair ("LicenseModel", String.to_query f))
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Util.option_map v.multi_a_z (fun f ->
               Query.Pair ("MultiAZ", Boolean.to_query f))
         ; Util.option_map v.port (fun f -> Query.Pair ("Port", Integer.to_query f))
         ; Util.option_map v.preferred_backup_window (fun f ->
               Query.Pair ("PreferredBackupWindow", String.to_query f))
         ; Util.option_map v.backup_retention_period (fun f ->
               Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               Query.Pair ("DBParameterGroupName", String.to_query f))
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               Query.Pair ("DBSubnetGroupName", String.to_query f))
         ; Util.option_map v.availability_zone (fun f ->
               Query.Pair ("AvailabilityZone", String.to_query f))
         ; Some
             (Query.Pair
                ( "VpcSecurityGroupIds.member"
                , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
         ; Some
             (Query.Pair
                ( "DBSecurityGroups.member"
                , DBSecurityGroupNameList.to_query v.d_b_security_groups ))
         ; Util.option_map v.master_user_password (fun f ->
               Query.Pair ("MasterUserPassword", String.to_query f))
         ; Util.option_map v.master_username (fun f ->
               Query.Pair ("MasterUsername", String.to_query f))
         ; Some (Query.Pair ("Engine", String.to_query v.engine))
         ; Some (Query.Pair ("DBInstanceClass", String.to_query v.d_b_instance_class))
         ; Util.option_map v.allocated_storage (fun f ->
               Query.Pair ("AllocatedStorage", Integer.to_query f))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ; Util.option_map v.d_b_name (fun f -> Query.Pair ("DBName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.max_allocated_storage (fun f ->
               "max_allocated_storage", Integer.to_json f)
         ; Util.option_map v.deletion_protection (fun f ->
               "deletion_protection", Boolean.to_json f)
         ; Some ("processor_features", ProcessorFeatureList.to_json v.processor_features)
         ; Some
             ( "enable_cloudwatch_logs_exports"
             , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
         ; Util.option_map v.performance_insights_retention_period (fun f ->
               "performance_insights_retention_period", Integer.to_json f)
         ; Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
               "performance_insights_k_m_s_key_id", String.to_json f)
         ; Util.option_map v.enable_performance_insights (fun f ->
               "enable_performance_insights", Boolean.to_json f)
         ; Util.option_map v.enable_i_a_m_database_authentication (fun f ->
               "enable_i_a_m_database_authentication", Boolean.to_json f)
         ; Util.option_map v.timezone (fun f -> "timezone", String.to_json f)
         ; Util.option_map v.promotion_tier (fun f -> "promotion_tier", Integer.to_json f)
         ; Util.option_map v.domain_i_a_m_role_name (fun f ->
               "domain_i_a_m_role_name", String.to_json f)
         ; Util.option_map v.monitoring_role_arn (fun f ->
               "monitoring_role_arn", String.to_json f)
         ; Util.option_map v.monitoring_interval (fun f ->
               "monitoring_interval", Integer.to_json f)
         ; Util.option_map v.copy_tags_to_snapshot (fun f ->
               "copy_tags_to_snapshot", Boolean.to_json f)
         ; Util.option_map v.domain (fun f -> "domain", String.to_json f)
         ; Util.option_map v.kms_key_id (fun f -> "kms_key_id", String.to_json f)
         ; Util.option_map v.storage_encrypted (fun f ->
               "storage_encrypted", Boolean.to_json f)
         ; Util.option_map v.tde_credential_password (fun f ->
               "tde_credential_password", String.to_json f)
         ; Util.option_map v.tde_credential_arn (fun f ->
               "tde_credential_arn", String.to_json f)
         ; Util.option_map v.storage_type (fun f -> "storage_type", String.to_json f)
         ; Util.option_map v.d_b_cluster_identifier (fun f ->
               "d_b_cluster_identifier", String.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.publicly_accessible (fun f ->
               "publicly_accessible", Boolean.to_json f)
         ; Util.option_map v.nchar_character_set_name (fun f ->
               "nchar_character_set_name", String.to_json f)
         ; Util.option_map v.character_set_name (fun f ->
               "character_set_name", String.to_json f)
         ; Util.option_map v.option_group_name (fun f ->
               "option_group_name", String.to_json f)
         ; Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
         ; Util.option_map v.license_model (fun f -> "license_model", String.to_json f)
         ; Util.option_map v.auto_minor_version_upgrade (fun f ->
               "auto_minor_version_upgrade", Boolean.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Util.option_map v.multi_a_z (fun f -> "multi_a_z", Boolean.to_json f)
         ; Util.option_map v.port (fun f -> "port", Integer.to_json f)
         ; Util.option_map v.preferred_backup_window (fun f ->
               "preferred_backup_window", String.to_json f)
         ; Util.option_map v.backup_retention_period (fun f ->
               "backup_retention_period", Integer.to_json f)
         ; Util.option_map v.d_b_parameter_group_name (fun f ->
               "d_b_parameter_group_name", String.to_json f)
         ; Util.option_map v.preferred_maintenance_window (fun f ->
               "preferred_maintenance_window", String.to_json f)
         ; Util.option_map v.d_b_subnet_group_name (fun f ->
               "d_b_subnet_group_name", String.to_json f)
         ; Util.option_map v.availability_zone (fun f ->
               "availability_zone", String.to_json f)
         ; Some
             ( "vpc_security_group_ids"
             , VpcSecurityGroupIdList.to_json v.vpc_security_group_ids )
         ; Some
             ("d_b_security_groups", DBSecurityGroupNameList.to_json v.d_b_security_groups)
         ; Util.option_map v.master_user_password (fun f ->
               "master_user_password", String.to_json f)
         ; Util.option_map v.master_username (fun f ->
               "master_username", String.to_json f)
         ; Some ("engine", String.to_json v.engine)
         ; Some ("d_b_instance_class", String.to_json v.d_b_instance_class)
         ; Util.option_map v.allocated_storage (fun f ->
               "allocated_storage", Integer.to_json f)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ; Util.option_map v.d_b_name (fun f -> "d_b_name", String.to_json f)
         ])

  let of_json j =
    { d_b_name = Util.option_map (Json.lookup j "d_b_name") String.of_json
    ; d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; allocated_storage =
        Util.option_map (Json.lookup j "allocated_storage") Integer.of_json
    ; d_b_instance_class =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_class"))
    ; engine = String.of_json (Util.of_option_exn (Json.lookup j "engine"))
    ; master_username = Util.option_map (Json.lookup j "master_username") String.of_json
    ; master_user_password =
        Util.option_map (Json.lookup j "master_user_password") String.of_json
    ; d_b_security_groups =
        DBSecurityGroupNameList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_security_groups"))
    ; vpc_security_group_ids =
        VpcSecurityGroupIdList.of_json
          (Util.of_option_exn (Json.lookup j "vpc_security_group_ids"))
    ; availability_zone =
        Util.option_map (Json.lookup j "availability_zone") String.of_json
    ; d_b_subnet_group_name =
        Util.option_map (Json.lookup j "d_b_subnet_group_name") String.of_json
    ; preferred_maintenance_window =
        Util.option_map (Json.lookup j "preferred_maintenance_window") String.of_json
    ; d_b_parameter_group_name =
        Util.option_map (Json.lookup j "d_b_parameter_group_name") String.of_json
    ; backup_retention_period =
        Util.option_map (Json.lookup j "backup_retention_period") Integer.of_json
    ; preferred_backup_window =
        Util.option_map (Json.lookup j "preferred_backup_window") String.of_json
    ; port = Util.option_map (Json.lookup j "port") Integer.of_json
    ; multi_a_z = Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; auto_minor_version_upgrade =
        Util.option_map (Json.lookup j "auto_minor_version_upgrade") Boolean.of_json
    ; license_model = Util.option_map (Json.lookup j "license_model") String.of_json
    ; iops = Util.option_map (Json.lookup j "iops") Integer.of_json
    ; option_group_name =
        Util.option_map (Json.lookup j "option_group_name") String.of_json
    ; character_set_name =
        Util.option_map (Json.lookup j "character_set_name") String.of_json
    ; nchar_character_set_name =
        Util.option_map (Json.lookup j "nchar_character_set_name") String.of_json
    ; publicly_accessible =
        Util.option_map (Json.lookup j "publicly_accessible") Boolean.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; d_b_cluster_identifier =
        Util.option_map (Json.lookup j "d_b_cluster_identifier") String.of_json
    ; storage_type = Util.option_map (Json.lookup j "storage_type") String.of_json
    ; tde_credential_arn =
        Util.option_map (Json.lookup j "tde_credential_arn") String.of_json
    ; tde_credential_password =
        Util.option_map (Json.lookup j "tde_credential_password") String.of_json
    ; storage_encrypted =
        Util.option_map (Json.lookup j "storage_encrypted") Boolean.of_json
    ; kms_key_id = Util.option_map (Json.lookup j "kms_key_id") String.of_json
    ; domain = Util.option_map (Json.lookup j "domain") String.of_json
    ; copy_tags_to_snapshot =
        Util.option_map (Json.lookup j "copy_tags_to_snapshot") Boolean.of_json
    ; monitoring_interval =
        Util.option_map (Json.lookup j "monitoring_interval") Integer.of_json
    ; monitoring_role_arn =
        Util.option_map (Json.lookup j "monitoring_role_arn") String.of_json
    ; domain_i_a_m_role_name =
        Util.option_map (Json.lookup j "domain_i_a_m_role_name") String.of_json
    ; promotion_tier = Util.option_map (Json.lookup j "promotion_tier") Integer.of_json
    ; timezone = Util.option_map (Json.lookup j "timezone") String.of_json
    ; enable_i_a_m_database_authentication =
        Util.option_map
          (Json.lookup j "enable_i_a_m_database_authentication")
          Boolean.of_json
    ; enable_performance_insights =
        Util.option_map (Json.lookup j "enable_performance_insights") Boolean.of_json
    ; performance_insights_k_m_s_key_id =
        Util.option_map (Json.lookup j "performance_insights_k_m_s_key_id") String.of_json
    ; performance_insights_retention_period =
        Util.option_map
          (Json.lookup j "performance_insights_retention_period")
          Integer.of_json
    ; enable_cloudwatch_logs_exports =
        LogTypeList.of_json
          (Util.of_option_exn (Json.lookup j "enable_cloudwatch_logs_exports"))
    ; processor_features =
        ProcessorFeatureList.of_json
          (Util.of_option_exn (Json.lookup j "processor_features"))
    ; deletion_protection =
        Util.option_map (Json.lookup j "deletion_protection") Boolean.of_json
    ; max_allocated_storage =
        Util.option_map (Json.lookup j "max_allocated_storage") Integer.of_json
    }
end

module StartExportTaskMessage = struct
  type t =
    { export_task_identifier : String.t
    ; source_arn : String.t
    ; s3_bucket_name : String.t
    ; iam_role_arn : String.t
    ; kms_key_id : String.t
    ; s3_prefix : String.t option
    ; export_only : StringList.t
    }

  let make
      ~export_task_identifier
      ~source_arn
      ~s3_bucket_name
      ~iam_role_arn
      ~kms_key_id
      ?s3_prefix
      ?(export_only = [])
      () =
    { export_task_identifier
    ; source_arn
    ; s3_bucket_name
    ; iam_role_arn
    ; kms_key_id
    ; s3_prefix
    ; export_only
    }

  let parse xml =
    Some
      { export_task_identifier =
          Xml.required
            "ExportTaskIdentifier"
            (Util.option_bind (Xml.member "ExportTaskIdentifier" xml) String.parse)
      ; source_arn =
          Xml.required
            "SourceArn"
            (Util.option_bind (Xml.member "SourceArn" xml) String.parse)
      ; s3_bucket_name =
          Xml.required
            "S3BucketName"
            (Util.option_bind (Xml.member "S3BucketName" xml) String.parse)
      ; iam_role_arn =
          Xml.required
            "IamRoleArn"
            (Util.option_bind (Xml.member "IamRoleArn" xml) String.parse)
      ; kms_key_id =
          Xml.required
            "KmsKeyId"
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse)
      ; s3_prefix = Util.option_bind (Xml.member "S3Prefix" xml) String.parse
      ; export_only =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ExportOnly" xml) StringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("ExportOnly.member", StringList.to_query v.export_only))
         ; Util.option_map v.s3_prefix (fun f ->
               Query.Pair ("S3Prefix", String.to_query f))
         ; Some (Query.Pair ("KmsKeyId", String.to_query v.kms_key_id))
         ; Some (Query.Pair ("IamRoleArn", String.to_query v.iam_role_arn))
         ; Some (Query.Pair ("S3BucketName", String.to_query v.s3_bucket_name))
         ; Some (Query.Pair ("SourceArn", String.to_query v.source_arn))
         ; Some
             (Query.Pair ("ExportTaskIdentifier", String.to_query v.export_task_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("export_only", StringList.to_json v.export_only)
         ; Util.option_map v.s3_prefix (fun f -> "s3_prefix", String.to_json f)
         ; Some ("kms_key_id", String.to_json v.kms_key_id)
         ; Some ("iam_role_arn", String.to_json v.iam_role_arn)
         ; Some ("s3_bucket_name", String.to_json v.s3_bucket_name)
         ; Some ("source_arn", String.to_json v.source_arn)
         ; Some ("export_task_identifier", String.to_json v.export_task_identifier)
         ])

  let of_json j =
    { export_task_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "export_task_identifier"))
    ; source_arn = String.of_json (Util.of_option_exn (Json.lookup j "source_arn"))
    ; s3_bucket_name =
        String.of_json (Util.of_option_exn (Json.lookup j "s3_bucket_name"))
    ; iam_role_arn = String.of_json (Util.of_option_exn (Json.lookup j "iam_role_arn"))
    ; kms_key_id = String.of_json (Util.of_option_exn (Json.lookup j "kms_key_id"))
    ; s3_prefix = Util.option_map (Json.lookup j "s3_prefix") String.of_json
    ; export_only = StringList.of_json (Util.of_option_exn (Json.lookup j "export_only"))
    }
end

module DBSnapshotNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyDBSnapshotResult = struct
  type t = { d_b_snapshot : DBSnapshot.t option }

  let make ?d_b_snapshot () = { d_b_snapshot }

  let parse xml =
    Some
      { d_b_snapshot = Util.option_bind (Xml.member "DBSnapshot" xml) DBSnapshot.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot (fun f ->
               Query.Pair ("DBSnapshot", DBSnapshot.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot (fun f -> "d_b_snapshot", DBSnapshot.to_json f)
         ])

  let of_json j =
    { d_b_snapshot = Util.option_map (Json.lookup j "d_b_snapshot") DBSnapshot.of_json }
end

module DBClusterAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidRestoreFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteGlobalClusterMessage = struct
  type t = { global_cluster_identifier : String.t }

  let make ~global_cluster_identifier () = { global_cluster_identifier }

  let parse xml =
    Some
      { global_cluster_identifier =
          Xml.required
            "GlobalClusterIdentifier"
            (Util.option_bind (Xml.member "GlobalClusterIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("GlobalClusterIdentifier", String.to_query v.global_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("global_cluster_identifier", String.to_json v.global_cluster_identifier)
         ])

  let of_json j =
    { global_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "global_cluster_identifier"))
    }
end

module FailoverDBClusterMessage = struct
  type t =
    { d_b_cluster_identifier : String.t
    ; target_d_b_instance_identifier : String.t option
    }

  let make ~d_b_cluster_identifier ?target_d_b_instance_identifier () =
    { d_b_cluster_identifier; target_d_b_instance_identifier }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; target_d_b_instance_identifier =
          Util.option_bind (Xml.member "TargetDBInstanceIdentifier" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.target_d_b_instance_identifier (fun f ->
               Query.Pair ("TargetDBInstanceIdentifier", String.to_query f))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.target_d_b_instance_identifier (fun f ->
               "target_d_b_instance_identifier", String.to_json f)
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; target_d_b_instance_identifier =
        Util.option_map (Json.lookup j "target_d_b_instance_identifier") String.of_json
    }
end

module StartActivityStreamRequest = struct
  type t =
    { resource_arn : String.t
    ; mode : ActivityStreamMode.t
    ; kms_key_id : String.t
    ; apply_immediately : Boolean.t option
    }

  let make ~resource_arn ~mode ~kms_key_id ?apply_immediately () =
    { resource_arn; mode; kms_key_id; apply_immediately }

  let parse xml =
    Some
      { resource_arn =
          Xml.required
            "ResourceArn"
            (Util.option_bind (Xml.member "ResourceArn" xml) String.parse)
      ; mode =
          Xml.required
            "Mode"
            (Util.option_bind (Xml.member "Mode" xml) ActivityStreamMode.parse)
      ; kms_key_id =
          Xml.required
            "KmsKeyId"
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse)
      ; apply_immediately =
          Util.option_bind (Xml.member "ApplyImmediately" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.apply_immediately (fun f ->
               Query.Pair ("ApplyImmediately", Boolean.to_query f))
         ; Some (Query.Pair ("KmsKeyId", String.to_query v.kms_key_id))
         ; Some (Query.Pair ("Mode", ActivityStreamMode.to_query v.mode))
         ; Some (Query.Pair ("ResourceArn", String.to_query v.resource_arn))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.apply_immediately (fun f ->
               "apply_immediately", Boolean.to_json f)
         ; Some ("kms_key_id", String.to_json v.kms_key_id)
         ; Some ("mode", ActivityStreamMode.to_json v.mode)
         ; Some ("resource_arn", String.to_json v.resource_arn)
         ])

  let of_json j =
    { resource_arn = String.of_json (Util.of_option_exn (Json.lookup j "resource_arn"))
    ; mode = ActivityStreamMode.of_json (Util.of_option_exn (Json.lookup j "mode"))
    ; kms_key_id = String.of_json (Util.of_option_exn (Json.lookup j "kms_key_id"))
    ; apply_immediately =
        Util.option_map (Json.lookup j "apply_immediately") Boolean.of_json
    }
end

module DBParameterGroupsMessage = struct
  type t =
    { marker : String.t option
    ; d_b_parameter_groups : DBParameterGroupList.t
    }

  let make ?marker ?(d_b_parameter_groups = []) () = { marker; d_b_parameter_groups }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; d_b_parameter_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBParameterGroups" xml)
               DBParameterGroupList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBParameterGroups.member"
                , DBParameterGroupList.to_query v.d_b_parameter_groups ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ("d_b_parameter_groups", DBParameterGroupList.to_json v.d_b_parameter_groups)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; d_b_parameter_groups =
        DBParameterGroupList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_parameter_groups"))
    }
end

module InstallationMediaAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateGlobalClusterResult = struct
  type t = { global_cluster : GlobalCluster.t option }

  let make ?global_cluster () = { global_cluster }

  let parse xml =
    Some
      { global_cluster =
          Util.option_bind (Xml.member "GlobalCluster" xml) GlobalCluster.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.global_cluster (fun f ->
               Query.Pair ("GlobalCluster", GlobalCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.global_cluster (fun f ->
               "global_cluster", GlobalCluster.to_json f)
         ])

  let of_json j =
    { global_cluster =
        Util.option_map (Json.lookup j "global_cluster") GlobalCluster.of_json
    }
end

module CopyDBParameterGroupResult = struct
  type t = { d_b_parameter_group : DBParameterGroup.t option }

  let make ?d_b_parameter_group () = { d_b_parameter_group }

  let parse xml =
    Some
      { d_b_parameter_group =
          Util.option_bind (Xml.member "DBParameterGroup" xml) DBParameterGroup.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_parameter_group (fun f ->
               Query.Pair ("DBParameterGroup", DBParameterGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_parameter_group (fun f ->
               "d_b_parameter_group", DBParameterGroup.to_json f)
         ])

  let of_json j =
    { d_b_parameter_group =
        Util.option_map (Json.lookup j "d_b_parameter_group") DBParameterGroup.of_json
    }
end

module DescribeOptionGroupOptionsMessage = struct
  type t =
    { engine_name : String.t
    ; major_engine_version : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ~engine_name ?major_engine_version ?(filters = []) ?max_records ?marker () =
    { engine_name; major_engine_version; filters; max_records; marker }

  let parse xml =
    Some
      { engine_name =
          Xml.required
            "EngineName"
            (Util.option_bind (Xml.member "EngineName" xml) String.parse)
      ; major_engine_version =
          Util.option_bind (Xml.member "MajorEngineVersion" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.major_engine_version (fun f ->
               Query.Pair ("MajorEngineVersion", String.to_query f))
         ; Some (Query.Pair ("EngineName", String.to_query v.engine_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.major_engine_version (fun f ->
               "major_engine_version", String.to_json f)
         ; Some ("engine_name", String.to_json v.engine_name)
         ])

  let of_json j =
    { engine_name = String.of_json (Util.of_option_exn (Json.lookup j "engine_name"))
    ; major_engine_version =
        Util.option_map (Json.lookup j "major_engine_version") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module EventsMessage = struct
  type t =
    { marker : String.t option
    ; events : EventList.t
    }

  let make ?marker ?(events = []) () = { marker; events }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; events =
          Util.of_option [] (Util.option_bind (Xml.member "Events" xml) EventList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Events.member", EventList.to_query v.events))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("events", EventList.to_json v.events)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; events = EventList.of_json (Util.of_option_exn (Json.lookup j "events"))
    }
end

module ModifyDBSubnetGroupMessage = struct
  type t =
    { d_b_subnet_group_name : String.t
    ; d_b_subnet_group_description : String.t option
    ; subnet_ids : SubnetIdentifierList.t
    }

  let make ~d_b_subnet_group_name ?d_b_subnet_group_description ~subnet_ids () =
    { d_b_subnet_group_name; d_b_subnet_group_description; subnet_ids }

  let parse xml =
    Some
      { d_b_subnet_group_name =
          Xml.required
            "DBSubnetGroupName"
            (Util.option_bind (Xml.member "DBSubnetGroupName" xml) String.parse)
      ; d_b_subnet_group_description =
          Util.option_bind (Xml.member "DBSubnetGroupDescription" xml) String.parse
      ; subnet_ids =
          Xml.required
            "SubnetIds"
            (Util.option_bind (Xml.member "SubnetIds" xml) SubnetIdentifierList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("SubnetIds.member", SubnetIdentifierList.to_query v.subnet_ids))
         ; Util.option_map v.d_b_subnet_group_description (fun f ->
               Query.Pair ("DBSubnetGroupDescription", String.to_query f))
         ; Some
             (Query.Pair ("DBSubnetGroupName", String.to_query v.d_b_subnet_group_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("subnet_ids", SubnetIdentifierList.to_json v.subnet_ids)
         ; Util.option_map v.d_b_subnet_group_description (fun f ->
               "d_b_subnet_group_description", String.to_json f)
         ; Some ("d_b_subnet_group_name", String.to_json v.d_b_subnet_group_name)
         ])

  let of_json j =
    { d_b_subnet_group_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_subnet_group_name"))
    ; d_b_subnet_group_description =
        Util.option_map (Json.lookup j "d_b_subnet_group_description") String.of_json
    ; subnet_ids =
        SubnetIdentifierList.of_json (Util.of_option_exn (Json.lookup j "subnet_ids"))
    }
end

module DBSecurityGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBSnapshotResult = struct
  type t = { d_b_snapshot : DBSnapshot.t option }

  let make ?d_b_snapshot () = { d_b_snapshot }

  let parse xml =
    Some
      { d_b_snapshot = Util.option_bind (Xml.member "DBSnapshot" xml) DBSnapshot.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot (fun f ->
               Query.Pair ("DBSnapshot", DBSnapshot.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot (fun f -> "d_b_snapshot", DBSnapshot.to_json f)
         ])

  let of_json j =
    { d_b_snapshot = Util.option_map (Json.lookup j "d_b_snapshot") DBSnapshot.of_json }
end

module SNSInvalidTopicFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBClusterParameterGroupMessage = struct
  type t = { d_b_cluster_parameter_group_name : String.t }

  let make ~d_b_cluster_parameter_group_name () = { d_b_cluster_parameter_group_name }

  let parse xml =
    Some
      { d_b_cluster_parameter_group_name =
          Xml.required
            "DBClusterParameterGroupName"
            (Util.option_bind (Xml.member "DBClusterParameterGroupName" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBClusterParameterGroupName"
                , String.to_query v.d_b_cluster_parameter_group_name ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "d_b_cluster_parameter_group_name"
             , String.to_json v.d_b_cluster_parameter_group_name )
         ])

  let of_json j =
    { d_b_cluster_parameter_group_name =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_parameter_group_name"))
    }
end

module OrderableDBInstanceOptionsMessage = struct
  type t =
    { orderable_d_b_instance_options : OrderableDBInstanceOptionsList.t
    ; marker : String.t option
    }

  let make ?(orderable_d_b_instance_options = []) ?marker () =
    { orderable_d_b_instance_options; marker }

  let parse xml =
    Some
      { orderable_d_b_instance_options =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "OrderableDBInstanceOptions" xml)
               OrderableDBInstanceOptionsList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair
                ( "OrderableDBInstanceOptions.member"
                , OrderableDBInstanceOptionsList.to_query v.orderable_d_b_instance_options
                ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some
             ( "orderable_d_b_instance_options"
             , OrderableDBInstanceOptionsList.to_json v.orderable_d_b_instance_options )
         ])

  let of_json j =
    { orderable_d_b_instance_options =
        OrderableDBInstanceOptionsList.of_json
          (Util.of_option_exn (Json.lookup j "orderable_d_b_instance_options"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module StopDBInstanceMessage = struct
  type t =
    { d_b_instance_identifier : String.t
    ; d_b_snapshot_identifier : String.t option
    }

  let make ~d_b_instance_identifier ?d_b_snapshot_identifier () =
    { d_b_instance_identifier; d_b_snapshot_identifier }

  let parse xml =
    Some
      { d_b_instance_identifier =
          Xml.required
            "DBInstanceIdentifier"
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml) String.parse)
      ; d_b_snapshot_identifier =
          Util.option_bind (Xml.member "DBSnapshotIdentifier" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot_identifier (fun f ->
               Query.Pair ("DBSnapshotIdentifier", String.to_query f))
         ; Some
             (Query.Pair
                ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot_identifier (fun f ->
               "d_b_snapshot_identifier", String.to_json f)
         ; Some ("d_b_instance_identifier", String.to_json v.d_b_instance_identifier)
         ])

  let of_json j =
    { d_b_instance_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_instance_identifier"))
    ; d_b_snapshot_identifier =
        Util.option_map (Json.lookup j "d_b_snapshot_identifier") String.of_json
    }
end

module DeleteInstallationMediaMessage = struct
  type t = { installation_media_id : String.t }

  let make ~installation_media_id () = { installation_media_id }

  let parse xml =
    Some
      { installation_media_id =
          Xml.required
            "InstallationMediaId"
            (Util.option_bind (Xml.member "InstallationMediaId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("InstallationMediaId", String.to_query v.installation_media_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("installation_media_id", String.to_json v.installation_media_id) ])

  let of_json j =
    { installation_media_id =
        String.of_json (Util.of_option_exn (Json.lookup j "installation_media_id"))
    }
end

module RemoveTagsFromResourceMessage = struct
  type t =
    { resource_name : String.t
    ; tag_keys : KeyList.t
    }

  let make ~resource_name ~tag_keys () = { resource_name; tag_keys }

  let parse xml =
    Some
      { resource_name =
          Xml.required
            "ResourceName"
            (Util.option_bind (Xml.member "ResourceName" xml) String.parse)
      ; tag_keys =
          Xml.required
            "TagKeys"
            (Util.option_bind (Xml.member "TagKeys" xml) KeyList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("TagKeys.member", KeyList.to_query v.tag_keys))
         ; Some (Query.Pair ("ResourceName", String.to_query v.resource_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tag_keys", KeyList.to_json v.tag_keys)
         ; Some ("resource_name", String.to_json v.resource_name)
         ])

  let of_json j =
    { resource_name = String.of_json (Util.of_option_exn (Json.lookup j "resource_name"))
    ; tag_keys = KeyList.of_json (Util.of_option_exn (Json.lookup j "tag_keys"))
    }
end

module ModifyDBSnapshotAttributeResult = struct
  type t = { d_b_snapshot_attributes_result : DBSnapshotAttributesResult.t option }

  let make ?d_b_snapshot_attributes_result () = { d_b_snapshot_attributes_result }

  let parse xml =
    Some
      { d_b_snapshot_attributes_result =
          Util.option_bind
            (Xml.member "DBSnapshotAttributesResult" xml)
            DBSnapshotAttributesResult.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot_attributes_result (fun f ->
               Query.Pair
                 ("DBSnapshotAttributesResult", DBSnapshotAttributesResult.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_snapshot_attributes_result (fun f ->
               "d_b_snapshot_attributes_result", DBSnapshotAttributesResult.to_json f)
         ])

  let of_json j =
    { d_b_snapshot_attributes_result =
        Util.option_map
          (Json.lookup j "d_b_snapshot_attributes_result")
          DBSnapshotAttributesResult.of_json
    }
end

module DescribeDBProxyTargetsResponse = struct
  type t =
    { targets : TargetList.t
    ; marker : String.t option
    }

  let make ?(targets = []) ?marker () = { targets; marker }

  let parse xml =
    Some
      { targets =
          Util.of_option [] (Util.option_bind (Xml.member "Targets" xml) TargetList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("Targets.member", TargetList.to_query v.targets))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("targets", TargetList.to_json v.targets)
         ])

  let of_json j =
    { targets = TargetList.of_json (Util.of_option_exn (Json.lookup j "targets"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module DBProxyNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module KMSKeyNotAccessibleFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteDBProxyRequest = struct
  type t = { d_b_proxy_name : String.t }

  let make ~d_b_proxy_name () = { d_b_proxy_name }

  let parse xml =
    Some
      { d_b_proxy_name =
          Xml.required
            "DBProxyName"
            (Util.option_bind (Xml.member "DBProxyName" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("d_b_proxy_name", String.to_json v.d_b_proxy_name) ])

  let of_json j =
    { d_b_proxy_name =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_proxy_name"))
    }
end

module DescribeEventCategoriesMessage = struct
  type t =
    { source_type : String.t option
    ; filters : FilterList.t
    }

  let make ?source_type ?(filters = []) () = { source_type; filters }

  let parse xml =
    Some
      { source_type = Util.option_bind (Xml.member "SourceType" xml) String.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.source_type (fun f ->
               Query.Pair ("SourceType", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.source_type (fun f -> "source_type", String.to_json f)
         ])

  let of_json j =
    { source_type = Util.option_map (Json.lookup j "source_type") String.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    }
end

module AuthorizationQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DBSubnetGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeOrderableDBInstanceOptionsMessage = struct
  type t =
    { engine : String.t
    ; engine_version : String.t option
    ; d_b_instance_class : String.t option
    ; license_model : String.t option
    ; availability_zone_group : String.t option
    ; vpc : Boolean.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ~engine
      ?engine_version
      ?d_b_instance_class
      ?license_model
      ?availability_zone_group
      ?vpc
      ?(filters = [])
      ?max_records
      ?marker
      () =
    { engine
    ; engine_version
    ; d_b_instance_class
    ; license_model
    ; availability_zone_group
    ; vpc
    ; filters
    ; max_records
    ; marker
    }

  let parse xml =
    Some
      { engine =
          Xml.required "Engine" (Util.option_bind (Xml.member "Engine" xml) String.parse)
      ; engine_version = Util.option_bind (Xml.member "EngineVersion" xml) String.parse
      ; d_b_instance_class =
          Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse
      ; license_model = Util.option_bind (Xml.member "LicenseModel" xml) String.parse
      ; availability_zone_group =
          Util.option_bind (Xml.member "AvailabilityZoneGroup" xml) String.parse
      ; vpc = Util.option_bind (Xml.member "Vpc" xml) Boolean.parse
      ; filters =
          Util.of_option [] (Util.option_bind (Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Util.option_map v.vpc (fun f -> Query.Pair ("Vpc", Boolean.to_query f))
         ; Util.option_map v.availability_zone_group (fun f ->
               Query.Pair ("AvailabilityZoneGroup", String.to_query f))
         ; Util.option_map v.license_model (fun f ->
               Query.Pair ("LicenseModel", String.to_query f))
         ; Util.option_map v.d_b_instance_class (fun f ->
               Query.Pair ("DBInstanceClass", String.to_query f))
         ; Util.option_map v.engine_version (fun f ->
               Query.Pair ("EngineVersion", String.to_query f))
         ; Some (Query.Pair ("Engine", String.to_query v.engine))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Some ("filters", FilterList.to_json v.filters)
         ; Util.option_map v.vpc (fun f -> "vpc", Boolean.to_json f)
         ; Util.option_map v.availability_zone_group (fun f ->
               "availability_zone_group", String.to_json f)
         ; Util.option_map v.license_model (fun f -> "license_model", String.to_json f)
         ; Util.option_map v.d_b_instance_class (fun f ->
               "d_b_instance_class", String.to_json f)
         ; Util.option_map v.engine_version (fun f -> "engine_version", String.to_json f)
         ; Some ("engine", String.to_json v.engine)
         ])

  let of_json j =
    { engine = String.of_json (Util.of_option_exn (Json.lookup j "engine"))
    ; engine_version = Util.option_map (Json.lookup j "engine_version") String.of_json
    ; d_b_instance_class =
        Util.option_map (Json.lookup j "d_b_instance_class") String.of_json
    ; license_model = Util.option_map (Json.lookup j "license_model") String.of_json
    ; availability_zone_group =
        Util.option_map (Json.lookup j "availability_zone_group") String.of_json
    ; vpc = Util.option_map (Json.lookup j "vpc") Boolean.of_json
    ; filters = FilterList.of_json (Util.of_option_exn (Json.lookup j "filters"))
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module CreateDBClusterEndpointMessage = struct
  type t =
    { d_b_cluster_identifier : String.t
    ; d_b_cluster_endpoint_identifier : String.t
    ; endpoint_type : String.t
    ; static_members : StringList.t
    ; excluded_members : StringList.t
    ; tags : TagList.t
    }

  let make
      ~d_b_cluster_identifier
      ~d_b_cluster_endpoint_identifier
      ~endpoint_type
      ?(static_members = [])
      ?(excluded_members = [])
      ?(tags = [])
      () =
    { d_b_cluster_identifier
    ; d_b_cluster_endpoint_identifier
    ; endpoint_type
    ; static_members
    ; excluded_members
    ; tags
    }

  let parse xml =
    Some
      { d_b_cluster_identifier =
          Xml.required
            "DBClusterIdentifier"
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml) String.parse)
      ; d_b_cluster_endpoint_identifier =
          Xml.required
            "DBClusterEndpointIdentifier"
            (Util.option_bind (Xml.member "DBClusterEndpointIdentifier" xml) String.parse)
      ; endpoint_type =
          Xml.required
            "EndpointType"
            (Util.option_bind (Xml.member "EndpointType" xml) String.parse)
      ; static_members =
          Util.of_option
            []
            (Util.option_bind (Xml.member "StaticMembers" xml) StringList.parse)
      ; excluded_members =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ExcludedMembers" xml) StringList.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Query.Pair ("ExcludedMembers.member", StringList.to_query v.excluded_members))
         ; Some
             (Query.Pair ("StaticMembers.member", StringList.to_query v.static_members))
         ; Some (Query.Pair ("EndpointType", String.to_query v.endpoint_type))
         ; Some
             (Query.Pair
                ( "DBClusterEndpointIdentifier"
                , String.to_query v.d_b_cluster_endpoint_identifier ))
         ; Some
             (Query.Pair ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some ("excluded_members", StringList.to_json v.excluded_members)
         ; Some ("static_members", StringList.to_json v.static_members)
         ; Some ("endpoint_type", String.to_json v.endpoint_type)
         ; Some
             ( "d_b_cluster_endpoint_identifier"
             , String.to_json v.d_b_cluster_endpoint_identifier )
         ; Some ("d_b_cluster_identifier", String.to_json v.d_b_cluster_identifier)
         ])

  let of_json j =
    { d_b_cluster_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier"))
    ; d_b_cluster_endpoint_identifier =
        String.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_endpoint_identifier"))
    ; endpoint_type = String.of_json (Util.of_option_exn (Json.lookup j "endpoint_type"))
    ; static_members =
        StringList.of_json (Util.of_option_exn (Json.lookup j "static_members"))
    ; excluded_members =
        StringList.of_json (Util.of_option_exn (Json.lookup j "excluded_members"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module EventSubscriptionQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module BackupPolicyNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module StartDBClusterResult = struct
  type t = { d_b_cluster : DBCluster.t option }

  let make ?d_b_cluster () = { d_b_cluster }

  let parse xml =
    Some { d_b_cluster = Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f ->
               Query.Pair ("DBCluster", DBCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.d_b_cluster (fun f -> "d_b_cluster", DBCluster.to_json f) ])

  let of_json j =
    { d_b_cluster = Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json }
end

module ReservedDBInstancesOfferingMessage = struct
  type t =
    { marker : String.t option
    ; reserved_d_b_instances_offerings : ReservedDBInstancesOfferingList.t
    }

  let make ?marker ?(reserved_d_b_instances_offerings = []) () =
    { marker; reserved_d_b_instances_offerings }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; reserved_d_b_instances_offerings =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "ReservedDBInstancesOfferings" xml)
               ReservedDBInstancesOfferingList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "ReservedDBInstancesOfferings.member"
                , ReservedDBInstancesOfferingList.to_query
                    v.reserved_d_b_instances_offerings ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "reserved_d_b_instances_offerings"
             , ReservedDBInstancesOfferingList.to_json v.reserved_d_b_instances_offerings
             )
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; reserved_d_b_instances_offerings =
        ReservedDBInstancesOfferingList.of_json
          (Util.of_option_exn (Json.lookup j "reserved_d_b_instances_offerings"))
    }
end

module DBClusterBacktrackMessage = struct
  type t =
    { marker : String.t option
    ; d_b_cluster_backtracks : DBClusterBacktrackList.t
    }

  let make ?marker ?(d_b_cluster_backtracks = []) () = { marker; d_b_cluster_backtracks }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; d_b_cluster_backtracks =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "DBClusterBacktracks" xml)
               DBClusterBacktrackList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBClusterBacktracks.member"
                , DBClusterBacktrackList.to_query v.d_b_cluster_backtracks ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "d_b_cluster_backtracks"
             , DBClusterBacktrackList.to_json v.d_b_cluster_backtracks )
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; d_b_cluster_backtracks =
        DBClusterBacktrackList.of_json
          (Util.of_option_exn (Json.lookup j "d_b_cluster_backtracks"))
    }
end

module DBClusterMessage = struct
  type t =
    { marker : String.t option
    ; d_b_clusters : DBClusterList.t
    }

  let make ?marker ?(d_b_clusters = []) () = { marker; d_b_clusters }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; d_b_clusters =
          Util.of_option
            []
            (Util.option_bind (Xml.member "DBClusters" xml) DBClusterList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("DBClusters.member", DBClusterList.to_query v.d_b_clusters))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_clusters", DBClusterList.to_json v.d_b_clusters)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; d_b_clusters =
        DBClusterList.of_json (Util.of_option_exn (Json.lookup j "d_b_clusters"))
    }
end

module RemoveSourceIdentifierFromSubscriptionMessage = struct
  type t =
    { subscription_name : String.t
    ; source_identifier : String.t
    }

  let make ~subscription_name ~source_identifier () =
    { subscription_name; source_identifier }

  let parse xml =
    Some
      { subscription_name =
          Xml.required
            "SubscriptionName"
            (Util.option_bind (Xml.member "SubscriptionName" xml) String.parse)
      ; source_identifier =
          Xml.required
            "SourceIdentifier"
            (Util.option_bind (Xml.member "SourceIdentifier" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("SourceIdentifier", String.to_query v.source_identifier))
         ; Some (Query.Pair ("SubscriptionName", String.to_query v.subscription_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("source_identifier", String.to_json v.source_identifier)
         ; Some ("subscription_name", String.to_json v.subscription_name)
         ])

  let of_json j =
    { subscription_name =
        String.of_json (Util.of_option_exn (Json.lookup j "subscription_name"))
    ; source_identifier =
        String.of_json (Util.of_option_exn (Json.lookup j "source_identifier"))
    }
end

module DBSecurityGroupMessage = struct
  type t =
    { marker : String.t option
    ; d_b_security_groups : DBSecurityGroups.t
    }

  let make ?marker ?(d_b_security_groups = []) () = { marker; d_b_security_groups }

  let parse xml =
    Some
      { marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; d_b_security_groups =
          Util.of_option
            []
            (Util.option_bind (Xml.member "DBSecurityGroups" xml) DBSecurityGroups.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "DBSecurityGroups.member"
                , DBSecurityGroups.to_query v.d_b_security_groups ))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("d_b_security_groups", DBSecurityGroups.to_json v.d_b_security_groups)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ])

  let of_json j =
    { marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; d_b_security_groups =
        DBSecurityGroups.of_json
          (Util.of_option_exn (Json.lookup j "d_b_security_groups"))
    }
end
