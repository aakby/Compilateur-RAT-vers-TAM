(*
open Compilateur
open Exceptions

exception ErreurNonDetectee


(* Tests Type pour pointeur *)
let%test_unit "testPointeurType1"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testPointeurType1.rat" in ()

let%test_unit "testPointeurType2"= 
  try 
    let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testPointeurType2.rat"
    in  raise ErreurNonDetectee
  with
  | TypeInattendu(_,_) -> ()

let%test_unit "testPointeurType3"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testPointeurType3.rat" in ()

let%test_unit "testPointeurType4"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testPointeurType4.rat" in ()

let%test_unit "testPointeurType5"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testPointeurType5.rat" in ()



(* Tests Type pour surcharge des fonctions *)
let%test_unit "testSurchargeType1"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testSurchargeType1.rat" in ()

let%test_unit "testSurchargeType2"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testSurchargeType2.rat" in ()

let%test_unit "testSurchargeType3"= 
  try 
    let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testSurchargeType3.rat"
    in  raise ErreurNonDetectee
  with
  | TypesParametresInattendus ([],_) -> ()

let%test_unit "testSurchargeType4"= 
  try 
    let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testSurchargeType4.rat"
    in  raise ErreurNonDetectee
  with
  | TypeInattendu(Int,Rat) -> ()

let%test_unit "testSurchargeType5"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testPointeurType5.rat" in ()


(* Tests Type pour enumeration *)
let%test_unit "testEnumerationType1"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testEnumerationType1.rat" in ()

let%test_unit "testEnumerationType2"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testEnumerationType2.rat" in ()

let%test_unit "testEnumerationType3"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testEnumerationType3.rat" in ()

let%test_unit "testEnumerationType4"= 
  try 
    let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testEnumerationType4.rat"
    in  raise ErreurNonDetectee
  with
  | TypeInattendu(_, _) -> ()

let%test_unit "testEnumerationType5"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testEnumerationType5.rat" in ()


(* Tests Type pour switch *)
let%test_unit "testSwitchType1"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testSwitchType1.rat" in ()

let%test_unit "testSwitchType2"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testSwitchType2.rat" in ()

let%test_unit "testSwitchType3"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testSwitchType3.rat" in ()

let%test_unit "testSwitchType4"= 
  try 
    let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testSwitchType4.rat"
    in  raise ErreurNonDetectee
  with
  | TypeInattendu(_, _) -> ()

let%test_unit "testSwitchType5"= 
  let _ = compiler "../../fichiersRat/src-rat-type-complementaire-test/testSwitchType5.rat" in ()
*)