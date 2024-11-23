let sum x y = x + y

(* tests *)
let test_sum_1 () = Alcotest.(check int) "" 3 (sum 1 2)
let test_sum_2 () = Alcotest.(check int) "" 100 (sum 1 99)
let test_sum_3 () = Alcotest.(check int) "" 42 (sum 40 2)

let () =
  let open Alcotest in
  run "Sum"
    [
      ( "sum-case",
        [
          test_case "Should return 3 because x is 1 and y is 2" `Quick
            test_sum_1;
          test_case "Should return 100 because x is 1 and y is 99" `Quick
            test_sum_2;
          test_case "Should return 42 because x is 40 and y is 2" `Quick
            test_sum_3;
        ] );
    ]
