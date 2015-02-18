open Lwt

module CoupleState =
  struct
    type t =
    {
      edam_s: int;
      ava_s:int;
      edam_do: (string*int) list;
      ava_do: (string*int) list;
      edam_now:string;
      ava_now:string;
      edam_out:int;
      ava_out:int;
    }
    let create_couple_state () =
    {
      edam_s = 10;
      ava_s = 10;
      edam_now = "init";
      ava_now = "init";
      edam_out = 0;
      ava_out = 0;
      edam_do = [("sleeping", 5);("dreaming",1);("working",3);("singing", 2);("naar theater", 3); ("leert italiaans", 2);("swimming", 2);("making love", 2)];
      ava_do = [("sleeping",5);("talking",3);("working",3);("P90Xing",3);("writing", 3);("op cafe", 5);("naar theater", 4);("making love", 2)];
    }

    let he_happy t = t.edam_s
    let she_happy t = t.ava_s
    let he_out t = float_of_int t.edam_out
    let she_out t = float_of_int t.ava_out
    let he_now_s t = t.edam_now
    let she_now_s t = t.ava_now
    let inc_he_happy t = {t with edam_s =  ((he_happy t) + 1);}
    let inc_she_happy t = {t with ava_s = ((she_happy t)+ 1);}
    let dec_he_happy t = {t with edam_s =  ((he_happy t) - 1);}
    let dec_she_happy t = {t with ava_s = ((she_happy t) - 1);}

    let set_he_now t edam_now = {t with edam_now;}
    let set_he_out t edam_out = {t with edam_out;}
    let set_she_now t ava_now = {t with ava_now;}
    let set_she_out t ava_out = {t with ava_out;}

    let give_nr t = Random.int (min (List.length t.edam_do) (List.length t.ava_do))
    let get_she_act t = let nr = (give_nr t) in List.nth t.ava_do nr
    let get_he_act t = let nr = (give_nr t) in List.nth t.edam_do nr
    let he_do t =
      let (act,out) = get_he_act t in
      let new_he = set_he_now t act in
      let new_he = inc_he_happy new_he in
      let new_he = set_he_out new_he out in
      new_he
    let she_do t =
      let (act,out) = get_she_act t in
      let new_she = set_she_now t act in
      let new_she = inc_she_happy new_she in
      let new_she = set_she_out new_she out in
      new_she
  end

module CoupleLog =
  struct
    let log_happy cs = let () = Printf.eprintf "*edam happy:%20d***ava happy:%20d*\n" (CoupleState.he_happy cs) (CoupleState.she_happy cs) in flush stderr
    let log_do cs = let () = Printf.eprintf "*edam doing:%20s***ava doing:%20s*\n" (CoupleState.he_now_s cs) (CoupleState.she_now_s cs) in flush stderr
  end

let rec edam cs =
 let cs = CoupleState.he_do cs in
 let () = CoupleLog.log_do cs in
 let () = CoupleLog.log_happy cs in
 Lwt_unix.sleep (CoupleState.he_out cs) >>= fun () ->
 edam cs

let rec ava cs =
 let cs = CoupleState.she_do cs in
 let () = CoupleLog.log_do cs in
 let () = CoupleLog.log_happy cs in
 Lwt_unix.sleep (CoupleState.she_out cs) >>= fun () ->
 ava cs

let main =
  let cs = CoupleState.create_couple_state () in
  Lwt.pick [edam cs; ava cs] in
  Lwt_main.run main
