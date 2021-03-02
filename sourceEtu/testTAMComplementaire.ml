
open Compilateur

(* Changer le chemin d'accès du jar. *)
let runtamcmde = "java -jar ../../runtam.jar"
(* let runtamcmde = "java -jar /mnt/n7fs/.../tools/runtam/runtam.jar" *)

(* Execute the TAM code obtained from the rat file and return the ouptut of this code *)
let runtamcode cmde ratfile =
  let tamcode = compiler ratfile in
  let (tamfile, chan) = Filename.open_temp_file "test" ".tam" in
  output_string chan tamcode;
  close_out chan;
  let ic = Unix.open_process_in (cmde ^ " " ^ tamfile) in
  let printed = input_line ic in
  close_in ic;
  Sys.remove tamfile;    (* à commenter si on veut étudier le code TAM. *)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile =
  print_string (runtamcode runtamcmde ratfile)

(* Test TAM Pointeurs *)
let%expect_test "testPointeursTam1" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testPointeursTam1.rat";
[%expect{| 10 |}]

let%expect_test "testPointeursTam2" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testPointeursTam2.rat";
[%expect{| 3 |}]

let%expect_test "testPointeursTam3" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testPointeursTam3.rat";
[%expect{| 2021 |}]

let%expect_test "testPointeursTam4" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testPointeursTam4.rat";
[%expect{| 20 |}]

let%expect_test "testPointeursTam5" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testPointeursTam5.rat";
[%expect{| 11 |}]

(* Test TAM Surcharge *)
let%expect_test "testSurchargeTam1" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testSurchargeTam1.rat";
[%expect{| 12 |}]

let%expect_test "testSurchargeTam2" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testSurchargeTam2.rat";
[%expect{| true[2020/2021]99 |}]

let%expect_test "testSurchargeTam3" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testSurchargeTam3.rat";
[%expect{| 105 |}]

let%expect_test "testSurchargeTam4" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testSurchargeTam4.rat";
[%expect{| truetruefalsefalse |}]

let%expect_test "testSurchargeTam5" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testSurchargeTam5.rat";
[%expect{| 25[2/1] |}]

(* Test TAM Enumeration *)

let%expect_test "testEnumerationTam1" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testEnumerationTam1.rat";
[%expect{| false |}]

let%expect_test "testEnumerationTam2" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testEnumerationTam2.rat";
[%expect{| 1 |}]

let%expect_test "testEnumerationTam3" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testEnumerationTam3.rat";
[%expect{| falsetrue |}]

let%expect_test "testEnumerationTam4" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testEnumerationTam4.rat";
[%expect{| true |}]

let%expect_test "testEnumerationTam5" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testEnumerationTam5.rat";
[%expect{| falsetrue |}]

(* Test TAM Switch *)

let%expect_test "testSwitchTam1" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testSwitchTam1.rat";
[%expect{| 0 |}]

let%expect_test "testSwitchTam2" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testSwitchTam2.rat";
[%expect{| 10 |}]

let%expect_test "testSwitchTam3" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testSwitchTam3.rat";
[%expect{| 4296 |}]

let%expect_test "testSwitchTam4" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testSwitchTam4.rat";
[%expect{| 12345 |}]

let%expect_test "testSwitchTam5" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/testSwitchTam5.rat";
[%expect{| 19 |}]

(* Autres tests *)

let%expect_test "AutreTestProgrammeLong" =
runtam "../../fichiersRat/src-rat-tam-complementaire-test/AutreTestProgrammeLong.rat";
[%expect{| [15/1][8/12][8/1][15/1][8/1][15/2] |}]
