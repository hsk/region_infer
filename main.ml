open Region

let () =
  let l = Lexing.from_channel stdin in
  let ast = Parser.parse Lexer.token l in
  let result = RegionInferenceAlgorithm.infer ast in
  match result with
  | None -> Printf.printf "error.\n"
  | Some(result) ->
      Printf.printf "%s" (RegionInferenceAlgorithm.RegionExp.fmt result)
