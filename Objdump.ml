let pp_list ?(start_with_sep=false) pp fmt =
  function
  | [] -> ()
  | l ->
    if start_with_sep then Format.fprintf fmt ",@,";
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@,") pp fmt l

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

and fields ?(start=0) r = List.init (Obj.size r - start) (fun i -> Obj.field r (i + start))

and pp_forcing fmt _r =
  Format.fprintf fmt "@[<hv 2>forcing(...)@]" (* TODO *)

and pp_cont fmt _r =
  Format.fprintf fmt "@[<hv 2>cont(...)@]" (* TODO *)

and pp_lazy fmt r =
  assert (Obj.size r = 1);
  Format.fprintf fmt "@[<hv 2>lazy(@,%a)@]" pp_closure (Obj.field r 0)

and pp_closure fmt r =
  let info = Obj.Closure.info r in
  Format.fprintf fmt "@[<hv 2>closure(@,<code>,@,@[<hv 1>{arity=%i,@,start_env=%i}@]%a)@]"
    info.arity info.start_env (pp_list ~start_with_sep:true pp) (fields ~start:info.start_env r)

and pp_object fmt r =
  assert (Obj.size r = 2);
  Format.fprintf fmt "@[<hv 2>object(@,%a)@]" (pp_list pp) (fields r)

and pp_infix fmt _r =
  Format.fprintf fmt "@[<hv 2>infix(...)@]" (* TODO *)

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
    Format.fprintf fmt "@[<hv 2>abstract(...)@]"
  else if tag = Obj.custom_tag then
    Format.fprintf fmt "@[<hv 2>custom(...)@]"
  else
    pp_unknown fmt r

and pp_unknown fmt r =
  Format.fprintf fmt "@[<hv 2>unknown%i(...)@]" (Obj.tag r)

let pp fmt x = pp fmt (Obj.repr x)
