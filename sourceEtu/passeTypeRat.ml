
(* Module de la passe de gestion des types *)
module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds
  open AstType
  open Type

  type t1 = Ast.AstTds.programme
  type t2 = Ast.AstType.programme

  (* analyse_type_enumeration : AstTds.enumeration -> AstType.enumeration *)
  let analyse_type_enumeration (e:AstTds.enumeration) =
    let Enumeration (i,lv) = e in 
    match info_ast_to_info i with 
    | InfoEnum n -> let t = Enumere (n) in 
        let _ = List.map (modifier_type_enum_info t ) lv in
        Enumeration (i,lv)
    | _ -> raise Erreur
  
  (* analyse_type_affectable : AstTds.affectable -> AstType.affectable * typ *)
  let rec analyse_type_affectable (aff:AstTds.affectable) : (affectable*typ) =
    match aff with
    | Ident info ->
        begin
          match info_ast_to_info info with
          | InfoVar(_,t,_,_) -> (Ident info, t)
          | InfoConst _ -> (Ident info, Int)
          | _ -> raise Erreur
        end
    | Valeur aff ->
        begin
          match analyse_type_affectable aff with
          | (naff, Pointeur t) -> Valeur (naff),t 
          | _ -> raise NotAPointer
        end
  (* analyse_type_expression : Asttds.expression -> Asttype.expression *)
  (* Paramètre e : l'expression à analyser *)
  (* Vérifie la bonne utilisation des types et tranforme l'expression
  en une expression de type Asttype.expression *)
  (* Erreur si mauvaise utilisation des types *)
  
  let rec analyse_type_expression e = 
    match e with 
    | AstTds.AppelFonction(infoList, le) ->
        begin
          let (nle, types_le) = List.split (List.map analyse_type_expression le) in
          let rec trouverFonction ialist list_parametres =
            match ialist with
            | [] -> raise (TypesParametresInattendus ([],list_parametres))
            | ia::q -> 
                begin
                  match (info_ast_to_info ia) with
                  | InfoFun (_,_,tRetour,typeParams) ->
                      if (est_compatible_list typeParams list_parametres) then (ia,tRetour)
                      else  (trouverFonction q list_parametres)
                  | _ -> raise Erreur
                end 
          in
          let (info, tRetour) = (trouverFonction infoList types_le) in
          (AppelFonction(info,nle), tRetour)
        end
    | AstTds.Rationnel(e1, e2) ->
        let (ne1,te1) = analyse_type_expression e1 and (ne2, te2) = analyse_type_expression e2 in
        if te1 == Int then
          if te2 == Int then (Rationnel(ne1,ne2), Rat)
          else raise (TypeInattendu(te2, Int))
        else raise (TypeInattendu(te1, Int))
    | AstTds.Numerateur(e) ->
        let (ne, te) = analyse_type_expression e in
        if te = Rat then (Numerateur(ne), Int)
        else raise (TypeInattendu(te, Rat))
    | AstTds.Denominateur(e) ->
        let (ne, te) = analyse_type_expression e in
        if te = Rat then (Denominateur(ne), Int)
        else raise (TypeInattendu(te, Rat))
    | AstTds.True -> (True, Bool)
    | AstTds.False -> (False, Bool)
    | AstTds.Entier(i) -> (Entier(i), Int)
    | AstTds.Binaire(b, e1, e2) ->
        let (ne1, te1) = analyse_type_expression e1 and (ne2,te2) = analyse_type_expression e2 in
        begin
          match b with
          | Plus ->
              if te1 = Int && te2 = Int then (Binaire(PlusInt, ne1, ne2), Int)
              else if te1 = Rat && te2 = Rat then (Binaire(PlusRat, ne1, ne2), Rat)
              else raise (TypeBinaireInattendu(b, te1, te2))
          | Mult ->
              if te1 = Int && te2 = Int then (Binaire(MultInt, ne1, ne2), Int)
              else if te1 = Rat && te2 = Rat then (Binaire(MultRat, ne1, ne2), Rat)
              else raise (TypeBinaireInattendu(Mult, te1, te2))
          | Equ ->
              if te1 = Int && te2 = Int then (Binaire(EquInt, ne1, ne2), Bool)
              else if te1 = Bool && te2 = Bool then (Binaire(EquBool, ne1, ne2), Bool)
              else 
                begin
                  match te1 with
                  | Enumere _ -> begin
                      match te2 with
                      | Enumere _ -> if est_compatible te1 te2 then (Binaire(EquEnum, ne1, ne2), Bool) 
                          else raise (TypeBinaireInattendu(Equ, te1, te2))
                      | _ -> raise  (TypeBinaireInattendu(Equ, te1, te2))
                    end
                  | _ -> raise  (TypeBinaireInattendu(Equ, te1, te2))
                end
          | Inf ->
              if te1 = Int && te2 = Int then (Binaire(Inf, ne1, ne2), Bool)
              else raise (TypeBinaireInattendu(Inf, te1, te2))
        end
    | AstTds.Acces a -> let (na,t) = analyse_type_affectable a in (Acces na, t)
    | AstTds.Vide -> Vide,Pointeur Undefined 
    | AstTds.Allocation t -> Allocation t, Pointeur t 
    | AstTds.Adresse info ->
        begin
          match info_ast_to_info info with
          | InfoVar(_,t,_,_) -> (Adresse info, Pointeur t)
          | _ -> raise Erreur
        end
    | AstTds.ValeurEnum info ->
        begin
          match info_ast_to_info info with
          | InfoEnumVal (_,t,_,_) -> (ValeurEnum info, t) 
          | _ -> raise Erreur 
        end

  (* analyse_type_instruction : AstTds.instruction -> AstType.instruction *)
  let rec analyse_type_instruction i =
    match i with
    | AstTds.Declaration(t, e, info) ->
        modifier_type_info t info;
        let (ne, te) = analyse_type_expression e in
        if est_compatible t te then
          Declaration(ne, info)
        else raise (TypeInattendu(te,t))
    | AstTds.Affectation (e, aff) ->
        let (naff, taff) = analyse_type_affectable aff in 
        let (ne, texp)   = analyse_type_expression e in 
        if est_compatible texp taff 
        then Affectation (ne, naff)
        else raise (TypeInattendu (texp, taff))
    | AstTds.Affichage(e) ->
        let (ne, te) = analyse_type_expression e in
        begin
          match te with
          | Bool -> AffichageBool(ne)
          | Int -> AffichageInt(ne)
          | Rat -> AffichageRat(ne)
          | _ -> raise Erreur
        end
    | AstTds.Conditionnelle(c,t,e) ->
        let (nc, tc) = analyse_type_expression c in
        if tc = Bool then
          Conditionnelle(nc, List.map (analyse_type_instruction) t, List.map (analyse_type_instruction) e)
        else raise (TypeInattendu(tc,Bool))
    | AstTds.TantQue(c,b) ->
        let (nc, tc) = analyse_type_expression c in
        if tc = Bool then TantQue(nc, List.map (analyse_type_instruction) b)
        else raise (TypeInattendu(tc, Bool))
    | Empty -> Empty
    | AstTds.Switch(e, lc) ->
        let (ne, te) = analyse_type_expression e in
        let rec analyse_type_cases l =
          match l with
          | [] -> []
          | c::q ->
              begin
                match c with
                | AstTds.Case(t,li,b) ->
                    begin
                      match t with
                      | AstTds.TCtid n ->
                          begin
                            match info_ast_to_info n with
                            | InfoEnumVal (_,tenum,_,_) -> if est_compatible tenum te then
                                  let nli = analyse_type_bloc li in (AstType.Case(t,nli,b))::(analyse_type_cases q)
                                else raise (TypeInattendu(te, tenum))
                            | _ -> raise Erreur
                          end
                      | AstTds.TCentier _ -> if est_compatible Int te then
                            let nli = analyse_type_bloc li in Case(t,nli,b)::(analyse_type_cases q)
                          else raise (TypeInattendu(te, Int))
                      | _ -> if est_compatible Bool te then
                            let nli = analyse_type_bloc li in Case(t,nli,b)::(analyse_type_cases q)
                          else raise (TypeInattendu(te, Bool))
                    end
                | AstTds.Default(li,b) -> Default(analyse_type_bloc li,b)::(analyse_type_cases q)
              end
        in
        let nlc = analyse_type_cases lc in
        Switch(ne, nlc)

  (* analyse_type_bloc : AstTds.instruction list -> AstType.instruction list *)
  and analyse_type_bloc b = List.map (analyse_type_instruction) b

  (* analyse_type_fonction : AstTds.fonction -> AstType.fonction *)
  let analyse_type_fonction (AstTds.Fonction(t, n, lp, li, e)) =
    let tlp, ialp = List.split lp in
    modifier_type_fonction_info t tlp n;
    let nli = List.map analyse_type_instruction li in
    let (ne, te) = analyse_type_expression e in
    if est_compatible te t then
      begin
        List.iter (fun (t,ia) -> modifier_type_info t ia) lp;
        AstType.Fonction(n, ialp, nli, ne);
      end
    else raise (TypeInattendu(te, t))
    
  (* analyser : AstTds.programme -> AstType.programme *)
  let analyser (AstTds.Programme (enumerations,fonctions,prog)) =
    let ne = List.map analyse_type_enumeration enumerations in
    let nf = List.map analyse_type_fonction fonctions in
    let nb = analyse_type_bloc prog in
    Programme (ne,nf,nb)
end