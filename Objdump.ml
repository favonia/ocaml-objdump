let pp_sep fmt () = Format.fprintf fmt ",@,"

let pp_list ?(start_with_sep=false) pp fmt =
  function
  | [] -> ()
  | l ->
    if start_with_sep then pp_sep fmt ();
    Format.pp_print_list ~pp_sep pp fmt l

let pp_rawfield fmt = Format.fprintf fmt "<%#nx>"

let rec special_scan_tags =
  Obj.[
    forcing_tag, pp_forcing;
    cont_tag, pp_cont;
    lazy_tag, pp_lazy;
    closure_tag, pp_closure;
    object_tag, pp_object;
    infix_tag, pp_infix;
    forward_tag, pp_forward;
  ]

and pp fmt r =
  if Obj.is_int r then
    Format.fprintf fmt "@[<hv 2>int(%i)@]" (Obj.obj r : int)
  else begin
    assert (Obj.is_block r);
    let tag = Obj.tag r in
    if tag >= Obj.no_scan_tag then
      pp_no_scan fmt r
    else if tag <= Obj.last_non_constant_constructor_tag then begin
      pp_variant fmt r
    end else
      match List.assoc_opt tag special_scan_tags with
      | Some pp -> pp fmt r
      | None -> pp_unknown fmt r
  end

and pp_variant fmt r =
  Format.fprintf fmt "@[<hv 2>variant%i(@,%a)@]" (Obj.tag r) (pp_list pp) (fields r)

and fields ?(start_at=0) r =
  List.init (Obj.size r - start_at) (fun i -> Obj.field r (i + start_at))

and raw_fields ?(start_at=0) ?end_at r =
  let end_at = Option.value ~default:(Obj.size r) end_at in
  List.init (end_at - start_at) (fun i -> Obj.raw_field r (i + start_at))

and pp_forcing fmt r =
  Format.fprintf fmt "@[<hv 2>forcing(%a)@]" (pp_list pp_rawfield) (raw_fields r)

and pp_cont fmt r =
  Format.fprintf fmt "@[<hv 2>cont(%a)@]" (pp_list pp_rawfield) (raw_fields r)

and pp_lazy fmt r =
  assert (Obj.size r = 1);
  Format.fprintf fmt "@[<hv 2>lazy(@,%a)@]" pp_closure (Obj.field r 0)

and pp_closure fmt r =
  let info = Obj.Closure.info r in
  Format.fprintf fmt "@[<hv 2>closure{@,code=%a,@,arity=%i%a,@,@[env=@;<0 2>@[<hv 1>(%a)@]@]}@]"
    pp_rawfield (Obj.raw_field r 0)
    info.arity
    (pp_list ~start_with_sep:true (fun fmt f -> Format.fprintf fmt "_=%a" pp_rawfield f)) (raw_fields ~start_at:2 ~end_at:info.start_env r)
    (pp_list pp) (fields ~start_at:info.start_env r)

and pp_object fmt r =
  assert (Obj.size r = 2);
  Format.fprintf fmt "@[<hv 2>object{@,class=%a,@,object-id=%a}@]"
    pp (Obj.field r 0) pp (Obj.field r 1)

and pp_infix fmt r =
  let offset = Obj.size r in
  let whole_r = Obj.add_offset r (Int32.of_int (- offset * (Nativeint.size / 8))) in
  (* this is safe because [pp_closure] will not recuresely call [pp] on opaque fields. *)
  Format.fprintf fmt "@[<hv 2>infix{@,offset=%i,@,@[whole=@;<0 2>@[%a@]}@]@]" offset pp_closure whole_r

and pp_forward fmt r =
  assert (Obj.size r = 1);
  Format.fprintf fmt "@[<hv 2>forward(@,%a)@]" pp (Obj.field r 0)

and pp_no_scan fmt r =
  let tag = Obj.tag r in
  if tag = Obj.string_tag then
    Format.fprintf fmt "@[<hv 2>string(@,\"%s\")@]" (String.escaped (Obj.obj r : string))
  else if tag = Obj.double_tag then
    Format.fprintf fmt "@[<hv 2>double(%f)@]" (Obj.obj r : float)
  else if tag = Obj.double_array_tag then
    Format.fprintf fmt "@[<hv 2>double_array(@,%a)@]" (pp_list Format.pp_print_float) (Array.to_list (Obj.obj r : float array))
  else if tag = Obj.abstract_tag then
    Format.fprintf fmt "@[<hv 2>abstract(%a)@]" (pp_list pp_rawfield) (raw_fields r)
  else if tag = Obj.custom_tag then
    Format.fprintf fmt "@[<hv 2>custom(%a)@]" (pp_list pp_rawfield) (raw_fields r)
  else
    pp_unknown fmt r

and pp_unknown fmt r =
  Format.fprintf fmt "@[<hv 2>unknown%i(%a)@]"
    (Obj.tag r) (pp_list pp_rawfield) (raw_fields r)

let pp fmt x = pp fmt (Obj.repr x)
