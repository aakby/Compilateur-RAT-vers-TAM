(*
open Compilateur
open Exceptions

exception ErreurNonDetectee;;

(* Test Pointeurs TDS*)
let%test_unit "testPointeursTds1" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testPointeursTds1.rat" in ()

let%test_unit "testPointeursTds2" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testPointeursTds2.rat" in ()

let%test_unit "testPointeursTds3" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testPointeursTds3.rat" in ()

let%test_unit "testPointeursTds4" = 
try
  let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testPointeursTds4.rat"
  in raise ErreurNonDetectee
with
  | MauvaiseUtilisationIdentifiant("x") -> ()


let%test_unit "testPointeursTds5" = 
try
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testPointeursTds5.rat"
in raise ErreurNonDetectee
with
  | DoubleDeclaration("x") -> ()

(* Test Surcharge TDS*)

let%test_unit "testSurchargeTds1" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testSurchargeTds1.rat" in ()

let%test_unit "testSurchargeTds2" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testSurchargeTds2.rat" in ()

let%test_unit "testSurchargeTds3" = 
try
  let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testSurchargeTds3.rat"
  in raise ErreurNonDetectee
with
  | DoubleDeclaration("fonc") -> ()

let%test_unit "testSurchargeTds4" = 
try
  let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testSurchargeTds4.rat"
  in raise ErreurNonDetectee
with
  | DoubleDeclaration("incr") -> ()

let%test_unit "testSurchargeTds5" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testSurchargeTds5.rat" in ()

(* Test Enumeration TDS*)

let%test_unit "testEnumerationTds1" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testEnumerationTds1.rat" in ()

let%test_unit "testEnumerationTds2" = 
try
  let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testEnumerationTds2.rat"
  in raise ErreurNonDetectee
with
  | DoubleDeclaration("Samedi") -> ()

let%test_unit "testEnumerationTds3" = 
try
  let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testEnumerationTds3.rat"
  in raise ErreurNonDetectee
with
  | DoubleDeclaration("Jour") -> ()

let%test_unit "testEnumerationTds4" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testEnumerationTds4.rat" in ()

let%test_unit "testEnumerationTds5" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testEnumerationTds5.rat" in ()

(* Test Switch TDS*)

let%test_unit "testSwitchTds1" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testSwitchTds1.rat" in ()

let%test_unit "testSwitchTds2" = 
try
  let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testSwitchTds2.rat"
  in raise ErreurNonDetectee
with
  | IdentifiantNonDeclare("b") -> ()

let%test_unit "testSwitchTds3" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testSwitchTds3.rat" in ()

let%test_unit "testSwitchTds4" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testSwitchTds4.rat" in ()

let%test_unit "testSwitchTds5" = 
let _ = compiler  "../../fichiersRat/src-rat-tds-complementaire-test/testSwitchTds5.rat" in ()
*)