open Unix

let run () =
    let rec loop ()  =
      let () = Printf.printf "Sitting here" in
      (*let () = Unix.sleep 5 in *)
      loop ()
    in
    loop ()

let () = run ()
