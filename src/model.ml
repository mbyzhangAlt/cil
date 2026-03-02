open ModelCommon

module H = Hashtbl
module E = Errormsg

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
  let misc = modelMiscFromMacroDefs md gcc_ver in
  let typeinfo = H.create 16 in
  let all_typeinfos = List.map (fun t -> (t, typeInfoFromMacroDefs md t)) allBasicTyps in
  List.iter (fun (t, info) -> match info with Some x -> H.add typeinfo t x | None -> ()) all_typeinfos;
  { typeinfo; misc; gcc_ver }

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
