module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct

  open Tds
  open Ast
  open AstPlacement
  open Type

  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme

  (* analyse_placement_instruction : instruction -> int -> string -> int *)
  let rec analyse_placement_instruction i base reg =
    match i with
    | AstType.Declaration (_, info) ->
        begin
          match info_ast_to_info info with
          | InfoVar(_,t,_,_) -> let _ = modifier_adresse_info base reg info in getTaille t
          | _ -> failwith "Erreur"
        end
    | AstType.Conditionnelle (_,t,e) ->
        analyse_placement_bloc t base reg;
        analyse_placement_bloc e base reg;
        0
    | AstType.TantQue (_,b) ->
        analyse_placement_bloc b base reg;
        0
    | AstType.Switch (_, lc) ->
        let rec aux l =
          match l with
          | [] -> 0
          | c::q -> match c with
            | AstType.Case(_,li,_) -> analyse_placement_bloc li base reg; aux q
            | AstType.Default(li,_) -> analyse_placement_bloc li base reg; aux q
        in aux lc
    | _ -> 0
   
  (* analyse_placement_bloc : bloc -> int -> string -> int *)
  and analyse_placement_bloc li base reg = match li with 
    | t::q -> let taille = analyse_placement_instruction t (base) reg 
        in analyse_placement_bloc q (base+taille) reg
    | _ -> ()
  
  (* analyse_placement_fonction fonction -> fonction *)
  let analyse_placement_fonction(AstType.Fonction(n,lp,li,e)) = 
    let calcul param d =
      match info_ast_to_info param with
      | InfoVar(_,t,_,_) ->  let nd = d-getTaille t in modifier_adresse_info nd "LB" param; nd
      | _ -> failwith ""
    in
    let _ = List.fold_right calcul lp 0 in
    let _ = analyse_placement_bloc li 3 "LB" in
    Fonction(n, lp, li, e)
  
  (* analyse_placement_enumeration : enumeration -> int -> string -> enumeration * int *)
  let analyse_placement_enumeration (AstType.Enumeration(ia,lia)) indice =
    let aux d ia  =
      match info_ast_to_info ia with
      | InfoEnumVal _-> let nd = (d+1) in modifier_adresse_enum_info nd "SB" ia; nd
      | _ -> failwith ""
    in 
    let nd = List.fold_left aux indice lia in 
    AstType.Enumeration(ia,lia),nd
    
  (* analyser : programme -> programme*)
  let analyser (AstType.Programme (enumerations,fonctions,prog)) =
    let rec aux liste indice le=
      match liste with
      | [] -> le,indice
      | t::q -> let (enum,ind) = analyse_placement_enumeration t indice in aux q ind ([enum]@le)
    in
    let nenumerations,i = aux enumerations 0 [] in
    let nfonctions = List.map analyse_placement_fonction fonctions in
    let _ = analyse_placement_bloc prog i "SB" in
    Programme(nenumerations,nfonctions, prog)
end