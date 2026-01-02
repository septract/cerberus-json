(* JSON serialization for Core AST - mirrors pp_core.ml structure *)
open Core
open Annot

(* Configuration module type - mirrors pp_core.ml CONFIG *)
(* Note: pp_core also has show_std for Astd annotations, but we don't serialize
   annotation contents in JSON, so we only need show_include for location filtering *)
module type CONFIG =
sig
  val show_include: bool
end

(* Output module type *)
module type JSON_CORE =
sig
  val json_file: ('a, 'b) generic_file -> Yojson.Safe.t
end

(* Main functor - mirrors pp_core.ml Make *)
module Make (Config: CONFIG) : JSON_CORE =
struct
open Config

(* Helper for creating JSON objects *)
let obj tag fields = `Assoc (("tag", `String tag) :: fields)
let obj_only tag = `Assoc [("tag", `String tag)]

(* Conditional output - mirrors pp_cond in pp_core.ml *)
let json_cond loc (json_fn : unit -> Yojson.Safe.t) : Yojson.Safe.t option =
  if show_include || Cerb_location.from_main_file loc then
    Some (json_fn ())
  else
    None

(* Helper to convert PPrint document to string *)
let pp_to_string doc = Pp_utils.to_plain_string doc

(* Symbols and identifiers - matches pp_symbol.ml to_string_pretty *)
let json_sym (sym : Symbol.sym) : Yojson.Safe.t =
  (* Must match pp_symbol.ml to_string_pretty for consistency *)
  let Symbol.Symbol (_, n, sd) = sym in
  let name = match sd with
    | Symbol.SD_Id name
    | Symbol.SD_ObjectAddress name
    | Symbol.SD_FunArgValue name -> name
    | Symbol.SD_CN_Id name -> name
    | Symbol.SD_unnamed_tag _ -> Printf.sprintf "__cerbty_unnamed_tag_%d" n
    (* All other cases fall through to a_N, matching pp_symbol.ml *)
    | Symbol.SD_None
    | Symbol.SD_Return
    | Symbol.SD_FunArg _ -> Printf.sprintf "a_%d" n
  in
  `Assoc [
    ("id", `Int n);
    ("name", `String name)
  ]

(* Symbols for object types (struct/union in BTy_loaded, BTy_object)
   Matches pp_symbol.ml to_string (NOT to_string_pretty)
   pp_core.ml pp_core_object_type uses: !^(Pp_symbol.to_string ident) *)
let json_object_type_sym (sym : Symbol.sym) : Yojson.Safe.t =
  let Symbol.Symbol (_, n, sd) = sym in
  let name = match sd with
    | Symbol.SD_Id str
    | Symbol.SD_ObjectAddress str
    | Symbol.SD_FunArgValue str -> Printf.sprintf "%s_%d" str n
    (* to_string uses a_N for all other cases including SD_unnamed_tag *)
    | _ -> Printf.sprintf "a_%d" n
  in
  `Assoc [
    ("id", `Int n);
    ("name", `String name)
  ]

let json_identifier (Symbol.Identifier (_, name)) : Yojson.Safe.t =
  `String name

let json_prefix (pref : Symbol.prefix) : Yojson.Safe.t =
  match pref with
  | Symbol.PrefSource (_, syms) ->
      obj "PrefSource" [("symbols", `List (List.map json_sym syms))]
  | Symbol.PrefFunArg (_, _, n) ->
      obj "PrefFunArg" [("index", `Int n)]
  | Symbol.PrefStringLiteral (_, _) ->
      obj_only "PrefStringLiteral"
  | Symbol.PrefCompoundLiteral (_, _) ->
      obj_only "PrefCompoundLiteral"
  | Symbol.PrefMalloc ->
      obj_only "PrefMalloc"
  | Symbol.PrefTemporaryLifetime (_, _) ->
      obj_only "PrefTemporaryLifetime"
  | Symbol.PrefOther str ->
      obj "PrefOther" [("name", `String str)]

(* Location *)
let json_loc (loc : Cerb_location.t) : Yojson.Safe.t =
  `String (pp_to_string (Cerb_location.pp_location loc))

(* Core object types - uses json_object_type_sym to match pp_core.ml pp_core_object_type *)
let rec json_core_object_type = function
  | OTy_integer -> obj_only "OTy_integer"
  | OTy_floating -> obj_only "OTy_floating"
  | OTy_pointer -> obj_only "OTy_pointer"
  | OTy_array oty -> obj "OTy_array" [("element", json_core_object_type oty)]
  | OTy_struct sym -> obj "OTy_struct" [("struct_tag", json_object_type_sym sym)]
  | OTy_union sym -> obj "OTy_union" [("union_tag", json_object_type_sym sym)]

(* Core base types *)
let rec json_core_base_type = function
  | BTy_unit -> obj_only "BTy_unit"
  | BTy_boolean -> obj_only "BTy_boolean"
  | BTy_ctype -> obj_only "BTy_ctype"
  | BTy_list bty -> obj "BTy_list" [("element", json_core_base_type bty)]
  | BTy_tuple btys -> obj "BTy_tuple" [("elements", `List (List.map json_core_base_type btys))]
  | BTy_storable -> obj_only "BTy_storable"
  | BTy_object oty -> obj "BTy_object" [("object_type", json_core_object_type oty)]
  | BTy_loaded oty -> obj "BTy_loaded" [("object_type", json_core_object_type oty)]

(* Names *)
let json_name = function
  | Sym sym -> obj "Sym" [("symbol", json_sym sym)]
  | Impl ic -> obj "Impl" [("constant", `String (Implementation.string_of_implementation_constant ic))]

(* Qualifiers *)
let json_qualifiers (q : Ctype.qualifiers) : Yojson.Safe.t =
  `Assoc [
    ("const", `Bool q.const);
    ("restrict", `Bool q.restrict);
    ("volatile", `Bool q.volatile)
  ]

(* Integer types - structured representation *)
let json_integer_type_struct (ity : Ctype.integerType) : Yojson.Safe.t =
  match ity with
  | Char -> obj_only "Char"
  | Bool -> obj_only "Bool"
  | Signed sik -> obj "Signed" [("kind",
      match sik with
      | Ichar -> `String "Ichar"
      | Short -> `String "Short"
      | Int_ -> `String "Int_"
      | Long -> `String "Long"
      | LongLong -> `String "LongLong"
      | IntN_t n -> obj "IntN_t" [("bits", `Int n)]
      | Int_leastN_t n -> obj "Int_leastN_t" [("bits", `Int n)]
      | Int_fastN_t n -> obj "Int_fastN_t" [("bits", `Int n)]
      | Intmax_t -> `String "Intmax_t"
      | Intptr_t -> `String "Intptr_t"
    )]
  | Unsigned ibty -> obj "Unsigned" [("kind",
      match ibty with
      | Ichar -> `String "Ichar"
      | Short -> `String "Short"
      | Int_ -> `String "Int_"
      | Long -> `String "Long"
      | LongLong -> `String "LongLong"
      | IntN_t n -> obj "IntN_t" [("bits", `Int n)]
      | Int_leastN_t n -> obj "Int_leastN_t" [("bits", `Int n)]
      | Int_fastN_t n -> obj "Int_fastN_t" [("bits", `Int n)]
      | Intmax_t -> `String "Intmax_t"
      | Intptr_t -> `String "Intptr_t"
    )]
  | Enum sym -> obj "Enum" [("enum_tag", json_sym sym)]
  | Size_t -> obj_only "Size_t"
  | Wchar_t -> obj_only "Wchar_t"
  | Wint_t -> obj_only "Wint_t"
  | Ptrdiff_t -> obj_only "Ptrdiff_t"
  | Ptraddr_t -> obj_only "Ptraddr_t"

(* Basic types *)
let json_basic_type (bty : Ctype.basicType) : Yojson.Safe.t =
  match bty with
  | Integer ity -> obj "Integer" [("int_type", json_integer_type_struct ity)]
  | Floating (RealFloating rfty) -> obj "Floating" [("float_type",
      match rfty with
      | Float -> `String "Float"
      | Double -> `String "Double"
      | LongDouble -> `String "LongDouble"
    )]

(* C types - structured representation *)
let rec json_ctype (ty : Ctype.ctype) : Yojson.Safe.t =
  let Ctype.Ctype (_, ty_) = ty in
  json_ctype_ ty_

and json_ctype_ (ty_ : Ctype.ctype_) : Yojson.Safe.t =
  match ty_ with
  | Void -> obj_only "Void"
  | Basic bty -> obj "Basic" [("basic_type", json_basic_type bty)]
  | Array (elem_ty, size_opt) -> obj "Array" [
      ("element_type", json_ctype elem_ty);
      ("size", match size_opt with Some n -> `Int (Nat_big_num.to_int n) | None -> `Null)
    ]
  | Function ((ret_quals, ret_ty), params, is_variadic) -> obj "Function" [
      ("return_type", json_ctype ret_ty);
      ("return_qualifiers", json_qualifiers ret_quals);
      ("params", `List (List.map (fun (quals, ty, _is_reg) ->
        `Assoc [("qualifiers", json_qualifiers quals); ("type", json_ctype ty)]
      ) params));
      ("variadic", `Bool is_variadic)
    ]
  | FunctionNoParams (ret_quals, ret_ty) -> obj "FunctionNoParams" [
      ("return_type", json_ctype ret_ty);
      ("return_qualifiers", json_qualifiers ret_quals)
    ]
  | Pointer (quals, pointee_ty) -> obj "Pointer" [
      ("qualifiers", json_qualifiers quals);
      ("pointee_type", json_ctype pointee_ty)
    ]
  | Atomic inner_ty -> obj "Atomic" [("inner_type", json_ctype inner_ty)]
  | Struct sym -> obj "Struct" [("struct_tag", json_sym sym)]
  | Union sym -> obj "Union" [("union_tag", json_sym sym)]
  | Byte -> obj_only "Byte"

(* Constructors *)
let json_ctor = function
  | Cnil bty -> obj "Cnil" [("type", json_core_base_type bty)]
  | Ccons -> obj_only "Ccons"
  | Ctuple -> obj_only "Ctuple"
  | Carray -> obj_only "Carray"
  | Civmax -> obj_only "Civmax"
  | Civmin -> obj_only "Civmin"
  | Civsizeof -> obj_only "Civsizeof"
  | Civalignof -> obj_only "Civalignof"
  | CivCOMPL -> obj_only "CivCOMPL"
  | CivAND -> obj_only "CivAND"
  | CivOR -> obj_only "CivOR"
  | CivXOR -> obj_only "CivXOR"
  | Cspecified -> obj_only "Cspecified"
  | Cunspecified -> obj_only "Cunspecified"
  | Cfvfromint -> obj_only "Cfvfromint"
  | Civfromfloat -> obj_only "Civfromfloat"
  | CivNULLcap is_signed -> obj "CivNULLcap" [("is_signed", `Bool is_signed)]

(* Polarity *)
let json_polarity = function
  | Pos -> `String "Pos"
  | Neg -> `String "Neg"

(* Memory order *)
let json_memory_order = function
  | Cmm_csem.NA -> `String "NA"
  | Cmm_csem.Seq_cst -> `String "Seq_cst"
  | Cmm_csem.Relaxed -> `String "Relaxed"
  | Cmm_csem.Release -> `String "Release"
  | Cmm_csem.Acquire -> `String "Acquire"
  | Cmm_csem.Consume -> `String "Consume"
  | Cmm_csem.Acq_rel -> `String "Acq_rel"

(* Linux memory order *)
let json_linux_memory_order = function
  | Linux.Once -> `String "Once"
  | Linux.LAcquire -> `String "LAcquire"
  | Linux.LRelease -> `String "LRelease"
  | Linux.Rmb -> `String "Rmb"
  | Linux.Wmb -> `String "Wmb"
  | Linux.Mb -> `String "Mb"
  | Linux.RbDep -> `String "RbDep"
  | Linux.RcuLock -> `String "RcuLock"
  | Linux.RcuUnlock -> `String "RcuUnlock"
  | Linux.SyncRcu -> `String "SyncRcu"

(* Kill kind *)
let json_kill_kind = function
  | Dynamic -> obj_only "Dynamic"
  | Static0 cty -> obj "Static" [("ctype", json_ctype cty)]

(* Binary operators *)
let json_binop = function
  | OpAdd -> `String "OpAdd"
  | OpSub -> `String "OpSub"
  | OpMul -> `String "OpMul"
  | OpDiv -> `String "OpDiv"
  | OpRem_t -> `String "OpRem_t"
  | OpRem_f -> `String "OpRem_f"
  | OpExp -> `String "OpExp"
  | OpEq -> `String "OpEq"
  | OpGt -> `String "OpGt"
  | OpLt -> `String "OpLt"
  | OpGe -> `String "OpGe"
  | OpLe -> `String "OpLe"
  | OpAnd -> `String "OpAnd"
  | OpOr -> `String "OpOr"

(* Integer operations *)
let json_iop = function
  | IOpAdd -> `String "IOpAdd"
  | IOpSub -> `String "IOpSub"
  | IOpMul -> `String "IOpMul"
  | IOpShl -> `String "IOpShl"
  | IOpShr -> `String "IOpShr"
  | IOpDiv -> `String "IOpDiv"
  | IOpRem_t -> `String "IOpRem_t"

(* Pointer values - structured serialization using case_ptrval interface *)
let json_pointer_value (pval : Impl_mem.pointer_value) : Yojson.Safe.t =
  Impl_mem.case_ptrval pval
    (* null pointer *)
    (fun cty -> `Assoc [
      ("tag", `String "PVnull");
      ("ctype", json_ctype cty)
    ])
    (* function pointer *)
    (fun sym_opt -> `Assoc [
      ("tag", `String "PVfunction");
      ("sym", match sym_opt with
        | Some sym -> json_sym sym
        | None -> `Null)
    ])
    (* concrete pointer *)
    (fun alloc_id_opt addr -> `Assoc [
      ("tag", `String "PVconcrete");
      ("alloc_id", match alloc_id_opt with
        | Some id -> `String (Nat_big_num.to_string id)
        | None -> `Null);
      ("addr", `String (Nat_big_num.to_string addr))
    ])

(* Values *)
let rec json_object_value = function
  | OVinteger ival ->
      obj "OVinteger" [("value", `String (pp_to_string (Impl_mem.pp_integer_value_for_core ival)))]
  | OVfloating fval ->
      obj "OVfloating" [("value",
        Impl_mem.case_fval fval
          (fun () -> `String "unspecified")
          (fun f ->
            (* Handle special float values that aren't valid JSON *)
            if f <> f then `String "NaN"  (* NaN is the only float not equal to itself *)
            else if f = infinity then `String "Infinity"
            else if f = neg_infinity then `String "-Infinity"
            else `Float f))]
  | OVpointer pval ->
      obj "OVpointer" [("value", json_pointer_value pval)]
  | OVarray lvals ->
      obj "OVarray" [("elements", `List (List.map json_loaded_value lvals))]
  | OVstruct (tag, members) ->
      obj "OVstruct" [
        ("struct_tag", json_sym tag);
        ("members", `List (List.map (fun (id, cty, mval) ->
          `Assoc [
            ("name", json_identifier id);
            ("ctype", json_ctype cty);
            ("value", `String (pp_to_string (Impl_mem.pp_mem_value mval)))
          ]) members))
      ]
  | OVunion (tag, id, mval) ->
      obj "OVunion" [
        ("union_tag", json_sym tag);
        ("member", json_identifier id);
        ("value", `String (pp_to_string (Impl_mem.pp_mem_value mval)))
      ]

and json_loaded_value = function
  | LVspecified oval -> obj "LVspecified" [("value", json_object_value oval)]
  | LVunspecified cty -> obj "LVunspecified" [("ctype", json_ctype cty)]

let rec json_value = function
  | Vobject oval -> obj "Vobject" [("value", json_object_value oval)]
  | Vloaded lval -> obj "Vloaded" [("value", json_loaded_value lval)]
  | Vunit -> obj_only "Vunit"
  | Vtrue -> obj_only "Vtrue"
  | Vfalse -> obj_only "Vfalse"
  | Vctype cty -> obj "Vctype" [("ctype", json_ctype cty)]
  | Vlist (bty, vals) ->
      obj "Vlist" [
        ("type", json_core_base_type bty);
        ("elements", `List (List.map json_value vals))
      ]
  | Vtuple vals ->
      obj "Vtuple" [("elements", `List (List.map json_value vals))]

(* Patterns *)
let rec json_pattern (Pattern (annots, pat_)) =
  let loc = match Annot.get_loc annots with
    | Some l -> json_loc l
    | None -> `Null
  in
  match pat_ with
  | CaseBase (sym_opt, bty) ->
      obj "CaseBase" [
        ("loc", loc);
        ("symbol", match sym_opt with Some s -> json_sym s | None -> `Null);
        ("type", json_core_base_type bty)
      ]
  | CaseCtor (ctor, pats) ->
      obj "CaseCtor" [
        ("loc", loc);
        ("constructor", json_ctor ctor);
        ("patterns", `List (List.map json_pattern pats))
      ]

(* Pure memop *)
let json_pure_memop (op : Mem_common.pure_memop) : Yojson.Safe.t =
  `String (pp_to_string (Pp_mem.pp_pure_memop op))

(* Pure expressions *)
let rec json_pexpr (Pexpr (annots, _, pe_)) =
  let loc = match Annot.get_loc annots with
    | Some l -> json_loc l
    | None -> `Null
  in
  let content = match pe_ with
    | PEsym sym ->
        obj "PEsym" [("symbol", json_sym sym)]
    | PEimpl ic ->
        obj "PEimpl" [("constant", `String (Implementation.string_of_implementation_constant ic))]
    | PEval v ->
        obj "PEval" [("value", json_value v)]
    | PEconstrained _ ->
        obj_only "PEconstrained"
    | PEundef (loc, ub) ->
        obj "PEundef" [
          ("loc", json_loc loc);
          ("ub", `String (Undefined.stringFromUndefined_behaviour ub))
        ]
    | PEerror (msg, pe) ->
        obj "PEerror" [("message", `String msg); ("expr", json_pexpr pe)]
    | PEctor (ctor, pes) ->
        obj "PEctor" [
          ("constructor", json_ctor ctor);
          ("args", `List (List.map json_pexpr pes))
        ]
    | PEcase (pe, branches) ->
        obj "PEcase" [
          ("scrutinee", json_pexpr pe);
          ("branches", `List (List.map (fun (pat, pe) ->
            `Assoc [("pattern", json_pattern pat); ("body", json_pexpr pe)]
          ) branches))
        ]
    | PEarray_shift (pe1, cty, pe2) ->
        obj "PEarray_shift" [
          ("ptr", json_pexpr pe1);
          ("ctype", json_ctype cty);
          ("index", json_pexpr pe2)
        ]
    | PEmember_shift (pe, tag, id) ->
        obj "PEmember_shift" [
          ("ptr", json_pexpr pe);
          ("struct_tag", json_sym tag);
          ("member", json_identifier id)
        ]
    | PEmemop (op, pes) ->
        obj "PEmemop" [
          ("op", json_pure_memop op);
          ("args", `List (List.map json_pexpr pes))
        ]
    | PEnot pe ->
        obj "PEnot" [("expr", json_pexpr pe)]
    | PEop (op, pe1, pe2) ->
        obj "PEop" [
          ("op", json_binop op);
          ("left", json_pexpr pe1);
          ("right", json_pexpr pe2)
        ]
    | PEconv_int (ity, pe) ->
        obj "PEconv_int" [
          ("type", json_integer_type_struct ity);
          ("expr", json_pexpr pe)
        ]
    | PEwrapI (ity, iop, pe1, pe2) ->
        obj "PEwrapI" [
          ("type", json_integer_type_struct ity);
          ("op", json_iop iop);
          ("left", json_pexpr pe1);
          ("right", json_pexpr pe2)
        ]
    | PEcatch_exceptional_condition (ity, iop, pe1, pe2) ->
        obj "PEcatch_exceptional_condition" [
          ("type", json_integer_type_struct ity);
          ("op", json_iop iop);
          ("left", json_pexpr pe1);
          ("right", json_pexpr pe2)
        ]
    | PEstruct (tag, members) ->
        obj "PEstruct" [
          ("struct_tag", json_sym tag);
          ("members", `List (List.map (fun (id, pe) ->
            `Assoc [("name", json_identifier id); ("value", json_pexpr pe)]
          ) members))
        ]
    | PEunion (tag, id, pe) ->
        obj "PEunion" [
          ("union_tag", json_sym tag);
          ("member", json_identifier id);
          ("value", json_pexpr pe)
        ]
    | PEcfunction pe ->
        obj "PEcfunction" [("expr", json_pexpr pe)]
    | PEmemberof (tag, id, pe) ->
        obj "PEmemberof" [
          ("member_tag", json_sym tag);
          ("member", json_identifier id);
          ("expr", json_pexpr pe)
        ]
    | PEcall (name, pes) ->
        obj "PEcall" [
          ("name", json_name name);
          ("args", `List (List.map json_pexpr pes))
        ]
    | PElet (pat, pe1, pe2) ->
        obj "PElet" [
          ("pattern", json_pattern pat);
          ("binding", json_pexpr pe1);
          ("body", json_pexpr pe2)
        ]
    | PEif (pe1, pe2, pe3) ->
        obj "PEif" [
          ("condition", json_pexpr pe1);
          ("then_branch", json_pexpr pe2);
          ("else_branch", json_pexpr pe3)
        ]
    | PEis_scalar pe -> obj "PEis_scalar" [("expr", json_pexpr pe)]
    | PEis_integer pe -> obj "PEis_integer" [("expr", json_pexpr pe)]
    | PEis_signed pe -> obj "PEis_signed" [("expr", json_pexpr pe)]
    | PEis_unsigned pe -> obj "PEis_unsigned" [("expr", json_pexpr pe)]
    | PEbmc_assume pe -> obj "PEbmc_assume" [("expr", json_pexpr pe)]
    | PEare_compatible (pe1, pe2) ->
        obj "PEare_compatible" [
          ("left", json_pexpr pe1);
          ("right", json_pexpr pe2)
        ]
  in
  `Assoc [("loc", loc); ("expr", content)]

(* Actions *)
let json_action_ act_ =
  match act_ with
  | Create (pe1, pe2, pref) ->
      obj "Create" [
        ("align", json_pexpr pe1);
        ("size", json_pexpr pe2);
        ("prefix", json_prefix pref)
      ]
  | CreateReadOnly (pe1, pe2, pe3, pref) ->
      obj "CreateReadOnly" [
        ("align", json_pexpr pe1);
        ("size", json_pexpr pe2);
        ("init", json_pexpr pe3);
        ("prefix", json_prefix pref)
      ]
  | Alloc0 (pe1, pe2, pref) ->
      obj "Alloc" [
        ("align", json_pexpr pe1);
        ("size", json_pexpr pe2);
        ("prefix", json_prefix pref)
      ]
  | Kill (kind, pe) ->
      obj "Kill" [
        ("kind", json_kill_kind kind);
        ("ptr", json_pexpr pe)
      ]
  | Store0 (locking, pe_ty, pe_ptr, pe_val, mo) ->
      obj "Store" [
        ("locking", `Bool locking);
        ("ctype", json_pexpr pe_ty);
        ("ptr", json_pexpr pe_ptr);
        ("value", json_pexpr pe_val);
        ("memory_order", json_memory_order mo)
      ]
  | Load0 (pe_ty, pe_ptr, mo) ->
      obj "Load" [
        ("ctype", json_pexpr pe_ty);
        ("ptr", json_pexpr pe_ptr);
        ("memory_order", json_memory_order mo)
      ]
  | SeqRMW (is_update, pe_ty, pe_ptr, sym, pe_val) ->
      obj "SeqRMW" [
        ("is_update", `Bool is_update);
        ("ctype", json_pexpr pe_ty);
        ("ptr", json_pexpr pe_ptr);
        ("symbol", json_sym sym);
        ("value", json_pexpr pe_val)
      ]
  | RMW0 (pe_ty, pe_ptr, pe_exp, pe_des, mo_succ, mo_fail) ->
      obj "RMW" [
        ("ctype", json_pexpr pe_ty);
        ("ptr", json_pexpr pe_ptr);
        ("expected", json_pexpr pe_exp);
        ("desired", json_pexpr pe_des);
        ("success_order", json_memory_order mo_succ);
        ("failure_order", json_memory_order mo_fail)
      ]
  | Fence0 mo ->
      obj "Fence" [("memory_order", json_memory_order mo)]
  | CompareExchangeStrong (pe_ty, pe_ptr, pe_exp, pe_des, mo_succ, mo_fail) ->
      obj "CompareExchangeStrong" [
        ("ctype", json_pexpr pe_ty);
        ("ptr", json_pexpr pe_ptr);
        ("expected", json_pexpr pe_exp);
        ("desired", json_pexpr pe_des);
        ("success_order", json_memory_order mo_succ);
        ("failure_order", json_memory_order mo_fail)
      ]
  | CompareExchangeWeak (pe_ty, pe_ptr, pe_exp, pe_des, mo_succ, mo_fail) ->
      obj "CompareExchangeWeak" [
        ("ctype", json_pexpr pe_ty);
        ("ptr", json_pexpr pe_ptr);
        ("expected", json_pexpr pe_exp);
        ("desired", json_pexpr pe_des);
        ("success_order", json_memory_order mo_succ);
        ("failure_order", json_memory_order mo_fail)
      ]
  | LinuxFence mo ->
      obj "LinuxFence" [("memory_order", json_linux_memory_order mo)]
  | LinuxLoad (pe_ty, pe_ptr, mo) ->
      obj "LinuxLoad" [
        ("ctype", json_pexpr pe_ty);
        ("ptr", json_pexpr pe_ptr);
        ("memory_order", json_linux_memory_order mo)
      ]
  | LinuxStore (pe_ty, pe_ptr, pe_val, mo) ->
      obj "LinuxStore" [
        ("ctype", json_pexpr pe_ty);
        ("ptr", json_pexpr pe_ptr);
        ("value", json_pexpr pe_val);
        ("memory_order", json_linux_memory_order mo)
      ]
  | LinuxRMW (pe_ty, pe_ptr, pe_val, mo) ->
      obj "LinuxRMW" [
        ("ctype", json_pexpr pe_ty);
        ("ptr", json_pexpr pe_ptr);
        ("value", json_pexpr pe_val);
        ("memory_order", json_linux_memory_order mo)
      ]

let json_action (Action (loc, _, act_)) =
  `Assoc [
    ("loc", json_loc loc);
    ("action", json_action_ act_)
  ]

let json_paction (Paction (pol, act)) =
  `Assoc [
    ("polarity", json_polarity pol);
    ("action", json_action act)
  ]

(* Memop *)
let json_memop (op : 'sym Mem_common.generic_memop) : Yojson.Safe.t =
  `String (pp_to_string (Pp_mem.pp_memop op))

(* Effectful expressions *)
let rec json_expr (Expr (annots, e_)) =
  let loc = match Annot.get_loc annots with
    | Some l -> json_loc l
    | None -> `Null
  in
  let content = match e_ with
    | Epure pe ->
        obj "Epure" [("expr", json_pexpr pe)]
    | Ememop (op, pes) ->
        obj "Ememop" [
          ("op", json_memop op);
          ("args", `List (List.map json_pexpr pes))
        ]
    | Eaction pact ->
        obj "Eaction" [("action", json_paction pact)]
    | Ecase (pe, branches) ->
        obj "Ecase" [
          ("scrutinee", json_pexpr pe);
          ("branches", `List (List.map (fun (pat, e) ->
            `Assoc [("pattern", json_pattern pat); ("body", json_expr e)]
          ) branches))
        ]
    | Elet (pat, pe, e) ->
        obj "Elet" [
          ("pattern", json_pattern pat);
          ("binding", json_pexpr pe);
          ("body", json_expr e)
        ]
    | Eif (pe, e1, e2) ->
        obj "Eif" [
          ("condition", json_pexpr pe);
          ("then_branch", json_expr e1);
          ("else_branch", json_expr e2)
        ]
    | Eccall (_, pe_ty, pe_fn, pes) ->
        obj "Eccall" [
          ("type", json_pexpr pe_ty);
          ("function", json_pexpr pe_fn);
          ("args", `List (List.map json_pexpr pes))
        ]
    | Eproc (_, name, pes) ->
        obj "Eproc" [
          ("name", json_name name);
          ("args", `List (List.map json_pexpr pes))
        ]
    | Eunseq es ->
        obj "Eunseq" [("exprs", `List (List.map json_expr es))]
    | Ewseq (pat, e1, e2) ->
        obj "Ewseq" [
          ("pattern", json_pattern pat);
          ("left", json_expr e1);
          ("right", json_expr e2)
        ]
    | Esseq (pat, e1, e2) ->
        obj "Esseq" [
          ("pattern", json_pattern pat);
          ("left", json_expr e1);
          ("right", json_expr e2)
        ]
    | Ebound e ->
        obj "Ebound" [("expr", json_expr e)]
    | End es ->
        obj "End" [("exprs", `List (List.map json_expr es))]
    | Esave ((sym, bty), args, e) ->
        obj "Esave" [
          ("label", json_sym sym);
          ("return_type", json_core_base_type bty);
          ("args", `List (List.map (fun (s, ((bt, _), pe)) ->
            `Assoc [
              ("symbol", json_sym s);
              ("type", json_core_base_type bt);
              ("value", json_pexpr pe)
            ]) args));
          ("body", json_expr e)
        ]
    | Erun (_, sym, pes) ->
        obj "Erun" [
          ("label", json_sym sym);
          ("args", `List (List.map json_pexpr pes))
        ]
    | Epar es ->
        obj "Epar" [("exprs", `List (List.map json_expr es))]
    | Ewait tid ->
        obj "Ewait" [("thread_id", `Int tid)]
    | Eannot (_, e) ->
        obj "Eannot" [("expr", json_expr e)]
    | Eexcluded (_, act) ->
        obj "Eexcluded" [("action", json_action act)]
  in
  `Assoc [("loc", loc); ("expr", content)]

(* Tag definitions *)
let json_tag_definition (td : Ctype.tag_definition) : Yojson.Safe.t =
  match td with
  | Ctype.StructDef (fields, flex) ->
      obj "StructDef" [
        ("fields", `List (List.map (fun (id, (_, _, _, cty)) ->
          `Assoc [("name", json_identifier id); ("ctype", json_ctype cty)]
        ) fields));
        ("flexible_array", match flex with
          | Some (Ctype.FlexibleArrayMember (_, id, _, cty)) ->
              `Assoc [("name", json_identifier id); ("ctype", json_ctype cty)]
          | None -> `Null)
      ]
  | Ctype.UnionDef fields ->
      obj "UnionDef" [
        ("fields", `List (List.map (fun (id, (_, _, _, cty)) ->
          `Assoc [("name", json_identifier id); ("ctype", json_ctype cty)]
        ) fields))
      ]

(* Function declarations - mirrors pp_fun_map in pp_core.ml *)
let json_fun_map_decl decl =
  match decl with
  | Fun (ret_ty, params, body) ->
      (* Fun always outputs - no location check, same as pp_core.ml *)
      Some (obj "Fun" [
        ("return_type", json_core_base_type ret_ty);
        ("params", `List (List.map (fun (sym, bty) ->
          `Assoc [("symbol", json_sym sym); ("type", json_core_base_type bty)]
        ) params));
        ("body", json_pexpr body)
      ])
  | ProcDecl (loc, ret_ty, param_tys) ->
      (* ProcDecl uses pp_cond loc in pp_core.ml *)
      json_cond loc (fun () ->
        obj "ProcDecl" [
          ("loc", json_loc loc);
          ("return_type", json_core_base_type ret_ty);
          ("param_types", `List (List.map json_core_base_type param_tys))
        ])
  | BuiltinDecl (loc, ret_ty, param_tys) ->
      (* BuiltinDecl uses pp_cond loc in pp_core.ml *)
      json_cond loc (fun () ->
        obj "BuiltinDecl" [
          ("loc", json_loc loc);
          ("return_type", json_core_base_type ret_ty);
          ("param_types", `List (List.map json_core_base_type param_tys))
        ])
  | Proc (loc, _mrk, ret_ty, params, body) ->
      (* Proc uses pp_cond loc in pp_core.ml *)
      json_cond loc (fun () ->
        obj "Proc" [
          ("loc", json_loc loc);
          ("return_type", json_core_base_type ret_ty);
          ("params", `List (List.map (fun (sym, bty) ->
            `Assoc [("symbol", json_sym sym); ("type", json_core_base_type bty)]
          ) params));
          ("body", json_expr body)
        ])

(* Function declaration without location filtering - for stdlib *)
let json_fun_map_decl_all decl =
  match decl with
  | Fun (ret_ty, params, body) ->
      obj "Fun" [
        ("return_type", json_core_base_type ret_ty);
        ("params", `List (List.map (fun (sym, bty) ->
          `Assoc [("symbol", json_sym sym); ("type", json_core_base_type bty)]
        ) params));
        ("body", json_pexpr body)
      ]
  | ProcDecl (loc, ret_ty, param_tys) ->
      obj "ProcDecl" [
        ("loc", json_loc loc);
        ("return_type", json_core_base_type ret_ty);
        ("param_types", `List (List.map json_core_base_type param_tys))
      ]
  | BuiltinDecl (loc, ret_ty, param_tys) ->
      obj "BuiltinDecl" [
        ("loc", json_loc loc);
        ("return_type", json_core_base_type ret_ty);
        ("param_types", `List (List.map json_core_base_type param_tys))
      ]
  | Proc (loc, _mrk, ret_ty, params, body) ->
      obj "Proc" [
        ("loc", json_loc loc);
        ("return_type", json_core_base_type ret_ty);
        ("params", `List (List.map (fun (sym, bty) ->
          `Assoc [("symbol", json_sym sym); ("type", json_core_base_type bty)]
        ) params));
        ("body", json_expr body)
      ]

(* Function map - mirrors pp_fun_map in pp_core.ml *)
(* Note: Pmap.fold iterates in key order, and pp_core uses acc ^^ new (append),
   so we prepend to accumulator and reverse at the end to get the same order *)
let json_fun_map funs =
  List.rev @@ Pmap.fold (fun sym decl acc ->
    match json_fun_map_decl decl with
    | Some json_decl ->
        `Assoc [
          ("symbol", json_sym sym);
          ("declaration", json_decl)
        ] :: acc
    | None -> acc
  ) funs []

(* Function map without filtering - for stdlib *)
let json_fun_map_all funs =
  List.rev @@ Pmap.fold (fun sym decl acc ->
    `Assoc [
      ("symbol", json_sym sym);
      ("declaration", json_fun_map_decl_all decl)
    ] :: acc
  ) funs []

(* Tag definitions - mirrors pp_tagDefinitions *)
let json_tag_definitions tagDefs =
  let tagDefs = Pmap.bindings_list tagDefs in
  List.filter_map (fun (sym, (loc, tagDef)) ->
    json_cond loc (fun () ->
      `Assoc [
        ("symbol", json_sym sym);
        ("loc", json_loc loc);
        ("definition", json_tag_definition tagDef)
      ])
  ) tagDefs

(* Global definitions - mirrors pp_globs which only outputs GlobalDef, not GlobalDecl *)
let json_glob_decl (sym, decl) =
  match decl with
  | GlobalDef ((bty, cty), e) ->
      Some (`Assoc [
        ("symbol", json_sym sym);
        ("tag", `String "GlobalDef");
        ("core_type", json_core_base_type bty);
        ("ctype", json_ctype cty);
        ("init", json_expr e)
      ])
  | GlobalDecl _ ->
      (* pp_globs skips GlobalDecl, so we do too *)
      None

(* Function info entry - for cfunction() expression *)
let json_funinfo_entry (sym, (loc, _attrs, ret_ty, params, is_variadic, has_proto)) =
  `Assoc [
    ("symbol", json_sym sym);
    ("loc", json_loc loc);
    ("return_type", json_ctype ret_ty);
    ("params", `List (List.map (fun (sym_opt, ty) ->
      `Assoc [
        ("symbol", match sym_opt with Some s -> json_sym s | None -> `Null);
        ("type", json_ctype ty)
      ]) params));
    ("is_variadic", `Bool is_variadic);
    ("has_proto", `Bool has_proto)
  ]

(* Function info map - needed for cfunction() evaluation *)
let json_funinfo funinfo =
  `List (List.map json_funinfo_entry (Pmap.bindings_list funinfo))

(* Top-level file - mirrors pp_file in pp_core.ml *)
let json_file (file : ('bty, 'a) generic_file) : Yojson.Safe.t =
  let main_json = match file.main with
    | Some sym -> json_sym sym
    | None -> `Null
  in
  let tagdefs_json = `List (json_tag_definitions file.tagDefs) in
  let stdlib_json = `List (json_fun_map_all file.stdlib) in
  let globs_json = `List (List.filter_map json_glob_decl file.globs) in
  let funs_json = `List (json_fun_map file.funs) in
  let funinfo_json = json_funinfo file.funinfo in
  `Assoc [
    ("main", main_json);
    ("tagDefs", tagdefs_json);
    ("stdlib", stdlib_json);
    ("globs", globs_json);
    ("funs", funs_json);
    ("funinfo", funinfo_json)
  ]

end (* Make *)

(* Module instances - mirror pp_core.ml Basic, All, etc. *)
module Basic = Make (struct
  let show_include = false
end)

module All = Make (struct
  let show_include = true
end)
