module H = Hashtbl
module E = Errormsg

type basictyp =
  | Short
  | Int
  | Bool
  | Long
  | LongLong
  | Ptr
  | Float
  | Double
  | LongDouble
  | Float16 (* _Float16 *)
  | Float32x (* _Float32x *)
  | Float64x (* _Float64x *)
  | Float128 (* _Float128 *)
  | Void
  | Fun
  | Str
[@@deriving enumerate, show { with_path = false }, yojson]

let basictyp_of_string (s: string): basictyp Ppx_deriving_yojson_runtime.error_or =
  basictyp_of_yojson (`List [ `String s ])

let basictyp_to_string (t: basictyp): string =
  match basictyp_to_yojson t with
  | `List [ `String s ] -> s
  | _ -> failwith "Unexpected yojson format for basictyp"

type basictypemeta = {
  c_type: string option;
  optional: bool;
}

let metaOfBasicType (t: basictyp): basictypemeta =
  match t with
  | Short -> {c_type = Some "short"; optional = false}
  | Int -> {c_type = Some "int"; optional = false}
  | Bool -> {c_type = Some "_Bool"; optional = false}
  | Long -> {c_type = Some "long"; optional = false}
  | LongLong -> {c_type = Some "long long"; optional = false}
  | Ptr -> {c_type = None; optional = false}
  | Float -> {c_type = Some "float"; optional = false}
  | Double -> {c_type = Some "double"; optional = false}
  | LongDouble -> {c_type = Some "long double"; optional = false}
  | Float16 -> {c_type = Some "_Float16"; optional = true}
  | Float32x -> {c_type = Some "_Float32x"; optional = true}
  | Float64x -> {c_type = Some "_Float64x"; optional = true}
  | Float128 -> {c_type = Some "_Float128"; optional = true}
  | Void -> {c_type = Some "void"; optional = false}
  | Fun -> {c_type = None; optional = false}
  | Str -> {c_type = None; optional = false}

let nameOfBasicType (t: basictyp): string = 
  show_basictyp t |> String.lowercase_ascii

let allBasicTyps: basictyp list = all_of_basictyp

type basictypinfo = {
  sizeof: int;
  alignof: int;
} [@@deriving show { with_path = false }, yojson]

type compilerver = {
  major: int;
  minor: int;
  patch: int;
} [@@deriving show { with_path = false }, yojson]

type modelmisc = {
  char_is_unsigned: bool; (* Whether "char" is unsigned *)
  little_endian: bool; (* whether the machine is little endian *)
  thread_is_keyword: bool; (* whether __thread is a keyword *)
  builtin_va_list: bool; (* whether __builtin_va_list is builtin (gccism) *)
  alignof_aligned: int;   (* Alignment of anything with the "aligned" attribute *)
  stdc_ver: int;
  size_type: string;
  wchar_type: string;
} [@@deriving show { with_path = false }, yojson]

let hashtbl_to_yojson (k_to_string: 'a -> string) (v_to_yojson: 'b -> Yojson.Safe.t) (h: ('a, 'b) H.t): Yojson.Safe.t =
  let lst = H.fold (fun k v acc -> (k_to_string k, v_to_yojson v) :: acc) h [] in
  `Assoc lst

let rec collect (x: ('a, 'b) result list): ('a list, 'b) result = match x with
  | [] -> Ok []
  | Ok x :: xs -> begin
      match collect xs with
      | Ok ys -> Ok (x :: ys)
      | Error e -> Error e
  end
  | Error e :: xs -> Error e 

let hashtbl_of_yojson (k_of_string: string -> 'a Ppx_deriving_yojson_runtime.error_or) (v_of_yojson: Yojson.Safe.t -> 'b Ppx_deriving_yojson_runtime.error_or) (json: Yojson.Safe.t): ('a, 'b) H.t Ppx_deriving_yojson_runtime.error_or =
  match json with
  | `Assoc lst -> begin
    match lst |> List.map (fun (k, v) ->
      match (k_of_string k, v_of_yojson v) with
      | (Ok k, Ok v) -> Ok (k, v)
      | (Error e, _) -> Error e
      | (_, Error e) -> Error e
    ) |> collect with 
    Ok lst -> Ok (lst |> List.to_seq |> H.of_seq)
  | Error e -> Error e
    end
  | _ -> Error "Expected an object for hashtbl"

type model = {
  typeinfo: (basictyp, basictypinfo) H.t [@to_yojson hashtbl_to_yojson basictyp_to_string basictypinfo_to_yojson] [@of_yojson hashtbl_of_yojson basictyp_of_string basictypinfo_of_yojson];
  misc: modelmisc;
  gcc_ver: compilerver;
  clang_ver: compilerver option;
} [@@deriving yojson]

let cver (major: int) (minor: int) (patch: int): compilerver = { major; minor; patch }


let strValOfMacro (md: (string, string) H.t) (t: string): string = H.find md t

let intValOfMacro (md: (string, string) H.t) (t: string): int =
  let remove_parens (s: string): string =
    if String.length s >= 2 && String.get s 0 = '(' && String.get s (String.length s - 1) = ')' then
      String.sub s 1 (String.length s - 2)
    else s in
      strValOfMacro md t |> remove_parens |> int_of_string

let parseStdcVer (s: string): int =
  try int_of_string (String.sub s 0 (String.length s - 1)) with Failure _ -> ignore (E.warn "Failed to parse __STDC_VERSION__: %s, assuming 199711L\n" s); 199711

let macroExists (md: (string, string) H.t) (t: string): bool = H.mem md t

let typeInfoFromMacroDefs (md: (string, string) H.t) (t: basictyp): basictypinfo option =
  let is_i386 = macroExists md "__i386__" in
  let is_x86_64 = macroExists md "__x86_64__" in
  let is_clang = macroExists md "__clang__" in
  let alignof_generic (t: int): int = if is_i386 && t < 16 then min 4 t else t in
  let essential (sizeof_macro: string) (alignof_f: int -> int): basictypinfo option = 
    let sizeof_t = intValOfMacro md sizeof_macro in
    Some { sizeof = sizeof_t; alignof = alignof_f sizeof_t } in
  let optional (existent_checking_macro: string) (sizeof: int) (alignof: int): basictypinfo option =
    if macroExists md existent_checking_macro then Some { sizeof; alignof } else None in
  let fixed (sizeof: int) (alignof: int): basictypinfo option = Some { sizeof; alignof } in
  let float_info ?(x: bool = false) (ident: int): (int * int * int) option =
    let s = string_of_int ident ^ (if x then "X" else "") in
    if not (macroExists md ("__FLT" ^ s ^ "_MANT_DIG__")) then
      None
    else
      let mant_dig = intValOfMacro md ("__FLT" ^ s ^ "_MANT_DIG__") in
      let max_exp = intValOfMacro md ("__FLT" ^ s ^ "_MAX_EXP__") in
      let min_exp = intValOfMacro md ("__FLT" ^ s ^ "_MIN_EXP__") in
      Some (mant_dig, max_exp, min_exp) in
  let floatx (ident: int): basictypinfo option =
    match float_info ~x:true ident with
    | Some x ->
      let ret =
        if Some x = float_info 32 then { sizeof = 4; alignof = alignof_generic 4 } else
        if Some x = float_info 64 then { sizeof = 8; alignof = alignof_generic 8 } else
        if Some x = float_info 128 then { sizeof = 16; alignof = alignof_generic 16 } else
        if (is_i386 || is_x86_64) && x = (64, 16384, -16381) then
          (* x86 extended precision float *)
          let sz = intValOfMacro md "__SIZEOF_FLOAT80__" in { sizeof = sz; alignof = alignof_generic sz }
        else
          E.s (E.bug "Failed to detect _Float%dx type from macros" ident)
        in
      Some ret
    | None -> None in

  match t with
  | Short -> essential "__SIZEOF_SHORT__" alignof_generic
  | Int -> essential "__SIZEOF_INT__" alignof_generic
  | Bool -> fixed 1 1
  | Long -> essential "__SIZEOF_LONG__" alignof_generic
  | LongLong -> essential "__SIZEOF_LONG_LONG__" alignof_generic
  | Ptr -> essential "__SIZEOF_POINTER__" alignof_generic
  | Float -> essential "__SIZEOF_FLOAT__" alignof_generic
  | Double -> essential "__SIZEOF_DOUBLE__" alignof_generic
  | LongDouble -> essential "__SIZEOF_LONG_DOUBLE__" alignof_generic
  | Float16 -> optional "__FLT16_MAX__" 2 2
  | Float32x -> floatx 32
  | Float64x -> floatx 64
  | Float128 -> optional "__FLT128_MAX__" 16 16
  | Void -> fixed 1 1
  | Fun -> fixed 1 (if is_clang (* clang uses 4-byte alignment for function pointers *) || not (is_i386 || is_x86_64) then 4 else 1)
  | Str -> fixed 0 1

let modelMiscFromMacroDefs (md: (string, string) H.t) (v: compilerver): modelmisc = {
  char_is_unsigned = macroExists md "__CHAR_UNSIGNED__";
  little_endian = (let o = strValOfMacro md "__BYTE_ORDER__" in o = "1234" || o = "__ORDER_LITTLE_ENDIAN__");
  thread_is_keyword = (v >= cver 3 3 0); (* TODO: verify this *)
  builtin_va_list = (v >= cver 2 96 0); (* TODO: verify this *)
  alignof_aligned = intValOfMacro md "__BIGGEST_ALIGNMENT__";
  stdc_ver = parseStdcVer (strValOfMacro md "__STDC_VERSION__");
  size_type = strValOfMacro md "__SIZE_TYPE__";
  wchar_type = strValOfMacro md "__WCHAR_TYPE__";
}

let modelFromMacroDefs (md: (string, string) H.t): model =
  if not (macroExists md "__STDC__") then
    E.s (E.error "Macro definitions not detected. Have you called cc with -Wp,-dD?\n");
  let gcc_ver = cver (intValOfMacro md "__GNUC__") (intValOfMacro md "__GNUC_MINOR__") (intValOfMacro md "__GNUC_PATCHLEVEL__") in
  let clang_ver = if macroExists md "__clang__" then Some (cver (intValOfMacro md "__clang_major__") (intValOfMacro md "__clang_minor__") (intValOfMacro md "__clang_patchlevel__")) else None in
  let misc = modelMiscFromMacroDefs md gcc_ver in
  let typeinfo = H.create 16 in
  let all_typeinfos = List.map (fun t -> (t, typeInfoFromMacroDefs md t)) allBasicTyps in
  List.iter (fun (t, info) -> match info with Some x -> H.add typeinfo t x | None -> ()) all_typeinfos;
  { typeinfo; misc; gcc_ver; clang_ver }

let uninitModel: model = {
  typeinfo = H.create 0;
  misc = {
    char_is_unsigned = false;
    little_endian = false;
    thread_is_keyword = false;
    builtin_va_list = false;
    alignof_aligned = 0;
    stdc_ver = 0;
    size_type = "";
    wchar_type = "";
  };
  gcc_ver = cver 0 0 0;
  clang_ver = None;
}

let gcc10x64Model: model = {
  typeinfo = (let h = H.create 16 in
    let add t sizeof alignof = H.add h t { sizeof; alignof } in
    add Str 0 1;
    add Int 4 4;
    add Float32x 8 8;
    add Short 2 2;
    add Float64x 16 16;
    add LongLong 8 8;
    add Ptr 8 8;
    add Float128 16 16;
    add Void 1 1;
    add Long 8 8;
    add Double 8 8;
    add LongDouble 16 16;
    add Fun 1 1;
    add Bool 1 1;
    add Float 4 4;
    h);
  
  misc = {
    char_is_unsigned = false;
    little_endian = true;
    thread_is_keyword = true;
    builtin_va_list = true;
    alignof_aligned = 16;
    stdc_ver = 201710;
    size_type = "long unsigned int";
    wchar_type = "int";
  };
  
  gcc_ver = cver 10 5 0;
  clang_ver = None;
}

let theModel: model ref = ref uninitModel

let sizeOf (k: basictyp): int = (H.find !theModel.typeinfo k).sizeof
let alignOf (k: basictyp): int = (H.find !theModel.typeinfo k).alignof

type modelsrc = 
| MMacroDefs (* model detected from macro defintions in preprocessor output via -Wp,-dD *)
| MFixed of model (* model specified by the user via CIL_MACHINE environment variable *)

let modelSource : modelsrc ref = ref MMacroDefs

let initModelFromMacroDefs (md: (string, string) H.t): unit =
  theModel := modelFromMacroDefs md
