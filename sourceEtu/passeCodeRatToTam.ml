module PasseCodeRatToTam : Passe.Passe  with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct

  open Ast
  open Tds
  open Type
  open Code
  open Exceptions

  type t1 = Ast.AstPlacement.programme
  type t2 = string

  (* analyse_valeur_affectable : AstPlacement.affectable -> string *)
  let rec analyse_valeur_affectable aff =
    match aff with
    | AstType.Valeur a ->
        let (t,s) = analyse_valeur_affectable a in
        begin
          match t with
          | Pointeur t_var -> (t_var, s^"LOADI (1)\n")
          | _ -> raise Erreur
        end
    | AstType.Ident ia ->
        begin
          match info_ast_to_info ia with
          | InfoVar (_,Pointeur t_var, d, r) -> (t_var, "LOAD (1)" ^ string_of_int(d) ^ "[" ^ r ^ "]" ^ "\n")
          | _ -> raise Erreur
        end

  (* analyse_valeur_affectable_d : AstPlacement.affectable -> string *)
  and analyse_code_affectable_d a =
    match a with
    | AstType.Valeur af ->
        let (t, s) = analyse_valeur_affectable af in s ^ "LOADI (" ^ string_of_int(getTaille t) ^")\n"
    | AstType.Ident ia ->
        begin
          match info_ast_to_info ia with
          | InfoVar (_,t, d, r) -> "LOAD (" ^ string_of_int(getTaille t) ^") "^ string_of_int(d) ^ "[" ^ r ^ "]" ^ "\n"
          | InfoConst(_,i) -> "LOADL " ^ string_of_int(i) ^ "\n"
          | _ -> raise Erreur
        end

  (* analyse_valeur_affectable_g : AstPlacement.affectable -> string *)
  and analyse_code_affectable_g a =
    match a with
    | AstType.Valeur af ->
        let (t, s) = analyse_valeur_affectable af in s^ "STOREI (" ^ string_of_int(getTaille t) ^ ")\n"
    | AstType.Ident ia ->
        begin
          match info_ast_to_info ia with
          | InfoVar (_,t, d, r) -> "STORE (" ^ string_of_int(getTaille t) ^ ") " ^ string_of_int(d) ^ "[" ^ r ^ "]" ^ "\n"
          | _ -> raise Erreur
        end

  (* analyse_code_expression : AstPlacement.expression -> string *)
  and analyse_code_expression e =
    match e with
    | AstType.AppelFonction(info, le) ->
        begin
          match info_ast_to_info info with
          | InfoFun(i,n,_,_) ->(List.fold_left (fun e1 e2 -> e1 ^ analyse_code_expression e2) "" le) ^ "CALL (ST) " ^ n^(string_of_int i)^ "\n"
          | _ -> failwith "Erreur"
        end
    | AstType.Rationnel(e1, e2) ->
        analyse_code_expression e1 ^ analyse_code_expression e2
    | AstType.Numerateur e -> analyse_code_expression e ^ "POP (0) 1\n"
    | AstType.Denominateur e -> analyse_code_expression e ^ "POP (1) 1\n"
    | AstType.True -> "LOADL 1\n"
    | AstType.False -> "LOADL 0\n"
    | AstType.Entier i -> "LOADL " ^ string_of_int(i) ^ "\n"
    | AstType.Binaire(b, e1, e2) ->
        let ne1 = (analyse_code_expression e1) and ne2 = (analyse_code_expression e2) in
        begin
          match b with
          | PlusInt ->  ne1 ^ ne2 ^ "SUBR IAdd\n"
          | PlusRat ->  ne1 ^ ne2 ^ "CALL (ST) RAdd\n"
          | MultInt ->  ne1 ^ ne2 ^ "SUBR IMul\n"
          | MultRat ->  ne1 ^ ne2 ^ "CALL (ST) RMul\n"
          | EquInt ->   ne1 ^ ne2 ^ "SUBR IEq\n"
          | EquBool ->  ne1 ^ ne2 ^ "SUBR BEq\n"
          | Inf ->      ne1 ^ ne2 ^ "SUBR ILss\n"
          | EquEnum ->  ne1 ^ ne2 ^ "CALL (ST) EnumEqu\n"
        end
    | AstType.Acces af -> analyse_code_affectable_d af
    | AstType.Vide -> "LOADL 0\n"
    | AstType.Adresse ia ->
        begin
          match info_ast_to_info ia with
          | InfoVar(_,_, d, r) -> "LOADA " ^ string_of_int(d) ^ "[" ^ r ^ "]" ^ "\n"
          | _ -> raise Erreur
        end
    | AstType.Allocation t -> "LOADL " ^ string_of_int(getTaille t) ^ "\nSUBR MAlloc\n"
    | AstType.ValeurEnum ia ->
        begin
          match info_ast_to_info ia with
          | InfoEnumVal (_,t,dep,reg) -> "LOAD (" ^ string_of_int(getTaille t)^") "^(string_of_int dep)^"["^reg^"]\n"
          | _ -> raise Erreur
        end

  (* analyse_code_instruction : AstPlacement.instruction -> string *)
  and analyse_code_instruction i =
    match i with
    | AstType.Declaration(e, info) ->
        begin
          match info_ast_to_info info with
          | InfoVar(_,t,dep,reg) -> "PUSH " ^ string_of_int(getTaille t) ^ "\n" ^ (analyse_code_expression e) ^ "\n" ^ 
                                    "STORE (" ^ string_of_int(getTaille t) ^") "^ string_of_int(dep) ^ "[" ^ reg ^ "]" ^ "\n" 
          | _ -> raise Erreur
        end
    | AstType.Affectation(e, aff) -> analyse_code_expression e ^ analyse_code_affectable_g aff
    | AstType.AffichageInt e -> (analyse_code_expression e) ^ "SUBR IOut" ^ "\n"
    | AstType.AffichageRat e -> (analyse_code_expression e) ^ "CALL (ST) ROut\n"
    | AstType.AffichageBool e -> (analyse_code_expression e) ^ "SUBR BOut\n"
    | AstType.Empty -> ""
    | AstType.TantQue(c, b) ->
        let nc = analyse_code_expression c in 
        let nb, pop_taille_tq = analyse_code_bloc b in
        let etiq = getEtiquette() in
        let etiq2 = getEtiquette() in 
        etiq^"\n"^nc^"JUMPIF (0) "^etiq2^"\n"^nb^"POP (0)"^(string_of_int pop_taille_tq)^"\n"^
        "JUMP "^etiq^"\n"^etiq2^"\n"   
    | AstType.Conditionnelle(cond, t, e) ->
        begin
          let lelse = getEtiquette() and lfinelse = getEtiquette() in
          let n1,_ = analyse_code_bloc t in
          let n2,_ = analyse_code_bloc e in
          (analyse_code_expression cond) ^
          "JUMPIF (0) " ^ lelse ^ "\n" ^
          n1 ^
          "JUMP " ^ lfinelse ^ "\n" ^
          lelse ^ "\n" ^ n2 ^
          lfinelse ^ "\n"
        end
    | AstType.Switch(e,lc) ->
        let lfinalswitch = getEtiquette() in
        let rec aux l =
          begin
            match l with
            | [] -> lfinalswitch ^ "\n"
            | c::q -> let nextcase = getEtiquette() in
                begin
                  match c with
                  | AstType.Case(t,li,b) ->
                      begin
                        match t with
                        | TCtid n -> 
                            let (nli,_) = analyse_code_bloc li in
                            (analyse_code_expression (AstType.Binaire(EquEnum,e,ValeurEnum n )))^"JUMPIF (0)" ^ 
                            nextcase ^ "\n" ^ nli ^ (if b then "JUMP " ^ lfinalswitch else "")
                            ^ "\n" ^ nextcase ^ "\n" ^ (aux q)
                        | TCentier n -> let (nli,_) = analyse_code_bloc li in
                            (analyse_code_expression (AstType.Binaire(EquInt,e,Entier n)))^"JUMPIF (0)" ^ 
                            nextcase ^ "\n" ^ nli ^ (if b then "JUMP " ^ lfinalswitch else "")
                            ^ "\n" ^ nextcase ^ "\n" ^ (aux q)
                        | TCtrue -> let (nli,_) = analyse_code_bloc li in
                            (analyse_code_expression (AstType.Binaire(EquInt,e,Entier 1)))^"JUMPIF (0)" ^ 
                            nextcase ^ "\n" ^ nli ^ (if b then "JUMP " ^ lfinalswitch else "")
                            ^ "\n" ^ nextcase ^ "\n" ^ (aux q)
                        | TCfalse -> let (nli,_) = analyse_code_bloc li in
                            (analyse_code_expression (AstType.Binaire(EquInt,e,Entier 0)))^"JUMPIF (0)" ^ 
                            nextcase ^ "\n" ^ nli ^ (if b then "JUMP " ^ lfinalswitch else "")
                            ^ "\n" ^ nextcase ^ "\n" ^ (aux q)
                      end
                  | AstType.Default(li,b) -> let (nli,_) = analyse_code_bloc li in 
                      nli ^ (if b then "JUMP " ^ lfinalswitch else "") ^ "\n" ^ (aux q)
                end
          end
        in aux lc 

  (* analyse_code_bloc : AstPlacement.instruction list -> string *)
  and analyse_code_bloc li = 
    let taille_variables_declarees i = 
      match i with
      |AstType.Declaration(_,info) -> 
          begin
            match info_ast_to_info info with
            | InfoVar(_,t,_,_) -> getTaille t
            | InfoFun(_,_,t,_) -> getTaille t
            | InfoConst(_,_) -> getTaille Int
            | InfoEnumVal _ -> 1
            | InfoEnum _ -> 1
          end
      | _ -> 0
    in 
    let taille = List.fold_right (fun i ti -> (taille_variables_declarees i) + ti) li 0 in
    let _ = "POP (0) "^(string_of_int taille)^"\n" in (analyse_code_li li), ( taille)

  (* analyse_code_li : AstPlacement.instruction list -> string *)
  and analyse_code_li li =
    String.concat "" (List.map analyse_code_instruction li)
  
  (* analyse_code_fonction : AstPlacement.fonction -> string *)
  and analyse_code_fonction (Ast.AstPlacement.Fonction(info, _, li, e)) = 
    match info_ast_to_info info with 
    | InfoFun(i,nom, t, list_types) ->  
        let anal_b,pop_bloc = analyse_code_bloc li in
        let anal_e = analyse_code_expression e in
        nom^(string_of_int i)^"\n"^anal_b^anal_e^
        "POP ("^(string_of_int (getTaille t))^")"^(string_of_int pop_bloc)^
        "\nRETURN ("^(string_of_int (getTaille t))^")"^(string_of_int(List.fold_left (fun nb t -> (getTaille t) + nb) 0 list_types))^"\n"            
    | _ -> failwith ""
  
  (* analyse_code_enumeration : AstPlacement.enumeration -> string *)
  and analyse_code_enumeration (Ast.AstType.Enumeration(_,lv)) =
    let rec analyse_valeurs l indice = 
      match l with
      | [] -> ""
      | t::q -> match info_ast_to_info t with 
        | InfoEnumVal(_,t,dep,reg) ->
            "PUSH "^(string_of_int (getTaille t))^"\n"^
            "LOADL "^(string_of_int indice)^"\n"^
            "STORE ("^(string_of_int (getTaille t))^") "^(string_of_int dep)^"["^reg^"]\n"^(analyse_valeurs q (indice+1))
        | _ -> raise Erreur
    in 
    (analyse_valeurs lv 1)^"PUSH 1\n"

  (* analyser : AstPlacement.programme -> string *)
  let analyser (Ast.AstPlacement.Programme(enumerations,fcts, prog)) =
    let enumerations = List.fold_right (fun enum ls ->  (analyse_code_enumeration enum) ^ ls ^ "\n") (List.rev enumerations) "" in
    let fcts_str = List.fold_left (fun ls fct -> ls ^ analyse_code_fonction fct ^ "\n") "" fcts in
    let nb,_ = analyse_code_bloc prog in
    let main = "main\n" ^nb^ "HALT\n" in
    let s =  enumerations ^ Code.getEntete() ^ fcts_str ^ main in 
    (*
    print_string s;
    print_newline();
    *)
    s;

end