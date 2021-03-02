(* Module de la passe de gestion des identifiants *)
module PasseTdsRat : Passe.Passe with type t1 = Ast.AstSyntax.programme and type t2 = Ast.AstTds.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds
  open Type

  type t1 = Ast.AstSyntax.programme
  type t2 = Ast.AstTds.programme

(* analyse_tds_affectable : tds -> AstSyntax.ast -> bool -> Asttds.affectable )
( Paramètre gauche : précise si l'affectable est à gauche ou à droite d'une affectation )
( Paramètre tds : la table des symboles courante )
( Paramètre af : l'affectable à analyser )
( Vérifie la bonne utilisation des identifiants et transforme l'affectable
en un affectable de type Asttds.affectable )
( Erreur si mauvaise utilisation des identifiants *)
  let rec analyse_tds_affectable tds (a:AstSyntax.affectable) modif = 
    match a with
    | AstSyntax.Ident n ->
        begin
          match chercherGlobalement tds n with
          | None -> raise (IdentifiantNonDeclare n)
          | Some info ->
              begin
                match info_ast_to_info info with
                | InfoFun _ -> raise (MauvaiseUtilisationIdentifiant n)
                | InfoVar _ -> Ident info
                | InfoConst (_,_) -> if modif then raise (MauvaiseUtilisationIdentifiant n) else Ident info
                | _ -> raise Erreur
              end
        end
    | AstSyntax.Valeur a -> Valeur (analyse_tds_affectable tds a modif)

  let analyse_tds_enumeration tds (e:AstSyntax.enumeration) =
    let Enumeration(nom,lv) = e in
    match chercherGlobalement tds nom with
    | Some _ -> raise (DoubleDeclaration (nom))
    | None   ->  let rec aux tds liste la=
                   match liste with
                   | [] -> la
                   | t::q -> match chercherGlobalement tds t with
                     | Some _ -> raise (DoubleDeclaration t)
                     | None -> 
                         let ia = info_to_info_ast (InfoEnumVal(t,Undefined,0,"")) in
                         begin
                           ajouter tds t ia;
                           aux tds q [ia]@la;
                         end
        in let ls = aux tds lv [] in
        let ia_enum = info_to_info_ast (InfoEnum(nom)) in
        ajouter tds nom ia_enum;
        Enumeration(ia_enum,ls)

(* analyse_tds_expression : AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
  let rec analyse_tds_expression tds e = (* True ) ( failwith "todo"*)
    match e with 
    | AstSyntax.AppelFonction(id,le) ->
        begin
          let liste = chercher_fonction tds id in
          let analyse_info ia = 
            match (info_ast_to_info ia) with
            | InfoFun _ -> ia
            | _ -> raise (MauvaiseUtilisationIdentifiant id)
          in
          let nl = List.map analyse_info liste in
          let ne = List.map (analyse_tds_expression tds) le in
          AppelFonction(nl,ne)
        end
    | AstSyntax.Rationnel(e1,e2) -> Rationnel(analyse_tds_expression tds e1, analyse_tds_expression tds e2)
    | AstSyntax.Numerateur e1 -> Numerateur(analyse_tds_expression tds e1)
    | AstSyntax.Denominateur e1 -> Denominateur(analyse_tds_expression tds e1)
    | AstSyntax.True -> True
    | AstSyntax.False -> False
    | AstSyntax.Entier i -> Entier i 
    | AstSyntax.Binaire(b,e1,e2) -> Binaire(b, analyse_tds_expression tds e1, analyse_tds_expression tds e2)
    | AstSyntax.Acces a -> Acces (analyse_tds_affectable tds a false)
    | AstSyntax.Vide -> Vide
    | AstSyntax.Allocation t -> Allocation t
    | AstSyntax.Adresse n ->
        begin
          match chercherGlobalement tds n with
          | None -> raise (IdentifiantNonDeclare n)
          | Some info -> 
              begin
                match info_ast_to_info info with
                | InfoVar _ -> Adresse info
                |  _ -> raise (MauvaiseUtilisationIdentifiant n)
              end
        end
    | AstSyntax.ValeurEnum n ->
        begin
          match chercherGlobalement tds n with
          | None -> raise (IdentifiantNonDeclare n)
          | Some info ->
              begin
                match info_ast_to_info info with
                | InfoEnumVal _ -> ValeurEnum info
                | _ -> raise (MauvaiseUtilisationIdentifiant n)
              end
        end

(* analyse_tds_instruction : AstSyntax.instruction -> tds -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
  let rec analyse_tds_instruction tds i =
    match i with    
    | AstSyntax.Declaration (t, n, e) ->
        begin
          match chercherLocalement tds n with
          | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale, 
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *) 
              let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
              let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
              let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
              ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information 
            et l'expression remplacée par l'expression issue de l'analyse *)
              Declaration (t, ne, ia) 
          | Some _ ->
            (* L'identifiant est trouvé dans la tds locale, 
            il a donc déjà été déclaré dans le bloc courant *) 
              raise (DoubleDeclaration n)
        end
    | AstSyntax.Affectation (n,e) ->
    (* Dans affectation donc affectable en écriture *)
        Affectation (analyse_tds_expression tds e, analyse_tds_affectable tds n true)
    | AstSyntax.Constante (n,v) -> 
        begin
          match chercherLocalement tds n with
          | None -> 
        (* L'identifiant n'est pas trouvé dans la tds locale, 
        il n'a donc pas été déclaré dans le bloc courant *)
        (* Ajout dans la tds de la constante *)
              ajouter tds n (info_to_info_ast (InfoConst (n,v))); 
        (* Suppression du noeud de déclaration des constantes devenu inutile *)
              Empty
          | Some _ ->
          (* L'identifiant est trouvé dans la tds locale, 
          il a donc déjà été déclaré dans le bloc courant *) 
              raise (DoubleDeclaration n)
        end
    | AstSyntax.Affichage e -> 
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
        let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
        Affichage (ne)
    | AstSyntax.Conditionnelle (c,t,e) -> 
      (* Analyse de la condition *)
        let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
        let tast = analyse_tds_bloc tds t in
      (* Analyse du bloc else *)
        let east = analyse_tds_bloc tds e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
        Conditionnelle (nc, tast, east)
    | AstSyntax.TantQue (c,b) -> 
      (* Analyse de la condition *)
        let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
        let bast = analyse_tds_bloc tds b in
      (* Renvoie la nouvelle structure de la boucle *)
        TantQue (nc, bast)
    | AstSyntax.Switch (e, lc) ->
        let ne = analyse_tds_expression tds e in
        let rec analyse_cases l =
          match l with
          | [] -> []
          | c::q ->
              match c with
              | AstSyntax.Case(t,li,b) ->
                  begin
                    match t with
                    | AstSyntax.TCtid n ->
                        begin
                          match (chercherGlobalement tds n) with
                          | None -> raise (IdentifiantNonDeclare n)
                          | Some i -> (Case(TCtid i,(analyse_tds_bloc tds li),b))::(analyse_cases q)
                        end
                    | TCentier n -> (Case(TCentier n,analyse_tds_bloc tds li,b))::(analyse_cases q)
                    | TCtrue -> (Case(TCtrue,analyse_tds_bloc tds li,b))::(analyse_cases q)
                    | TCfalse -> (Case(TCfalse,analyse_tds_bloc tds li,b))::(analyse_cases q)
                  end
              | Default(li,b) -> Default(analyse_tds_bloc tds li,b)::(analyse_cases q)
        in Switch(ne, analyse_cases lc)
    



(* analyse_tds_bloc : AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc
en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
  and analyse_tds_bloc tds li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale 
  pointant sur la table du bloc parent *)
    let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc 
  Cette tds est modifiée par effet de bord *)
    let nli = List.map (analyse_tds_instruction tdsbloc) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
    nli

(* aux_fonction : AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Tranforme la fonction en une fonction de type AstTds.fonction
   sans vérifier la bonne gestion des identifiants*)
(* Erreur si mauvaise utilisation des identifiants *)
  let aux_fonction maintds (AstSyntax.Fonction(t,n,lp,li,e)) indice =
    let tds = creerTDSFille maintds in
    let analyse_parametre (t,nom) =
      match chercherLocalement tds nom with
      | Some _ -> raise (DoubleDeclaration nom)
      | None -> let infoparam = info_to_info_ast (InfoVar(nom,t, 0, "")) in
          ajouter tds nom infoparam;
          (t,infoparam)
    in
    let nlp = List.map analyse_parametre lp in
    let ia = info_to_info_ast (InfoFun(indice,n,Undefined, List.map (fun (a,_) -> a) lp)) in ajouter maintds n ia;
    let nli = List.map (analyse_tds_instruction tds) li in
    let ne = analyse_tds_expression tds e in
    Fonction(t, ia, nlp, nli, ne)

(* analyse_tds_fonction : AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et fait un appel
   à aux_fonction pour tranformer la fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
  let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li,e)) indice  =
    match (chercherLocalement maintds n) with
    | None -> aux_fonction maintds (AstSyntax.Fonction(t,n,lp,li,e)) indice
    | Some ia -> 
        begin 
          match (info_ast_to_info ia) with
          | InfoFun (_,_,_,list_params) -> 
              let tlp = List.map (fun (a,_) -> a) lp in
              if (est_compatible_list tlp list_params)
              then raise (DoubleDeclaration n)
              else aux_fonction maintds (AstSyntax.Fonction(t,n,lp,li,e)) indice
          | _ -> raise(MauvaiseUtilisationIdentifiant n)
        end
(* analyser : AstSyntax.ast -> AstTds.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.ast *)
(* Erreur si mauvaise utilisation des identifiants *)
  let analyser (AstSyntax.Programme (enumerations,fonctions,prog)) =
    let tds = creerTDSMere () in
    let rec aux lf liste i =
      match  lf with
      | [] -> liste
      | t::q -> 
          let el = analyse_tds_fonction tds t i in
          aux q (liste@[el]) (i+1)
    in
    let ne = List.map (analyse_tds_enumeration tds) enumerations in
    let nf = aux fonctions [] 0 in
    let nb = analyse_tds_bloc tds prog in
    Programme (ne,nf,nb)

end