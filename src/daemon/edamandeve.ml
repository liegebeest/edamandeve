open Lwt

module CoupleState =
  struct
    type t =
    {
      edam_s: int;
      ava_s:int;
      edam_do: (string*int*int) list;
      ava_do: (string*int*int) list;
      edam_now:string;
      ava_now:string;
      edam_out:int;
      ava_out:int;
    }
    let create_couple_state () =
            let () = Printf.eprintf "in couple state creation..." in
    {
      edam_s = 10;
      ava_s = 10;
      edam_now = "init";
      ava_now = "init";
      edam_out = 0;
      ava_out = 0;
      edam_do = [("sleeping", 5, 5);("dreaming",5, -3);("working",3, 1);("speelt met gust", 5, 4);("naar theater", 5, 5); ("leert italiaans", 5, 3);("swimming", 4, 2);("making love", 5, 5)];
      ava_do = [("sleeping",5, 5);("talking",5, -2 );("working",5, 1);("P90Xing",4, 2);("writing", 4, -2);("op cafe", 5, 1);("naar theater", 5, 5);("making love", 5, 5)];
    }

    let he_happy t = t.edam_s
    let she_happy t = t.ava_s
    let he_out t = float_of_int t.edam_out
    let she_out t = float_of_int t.ava_out
    let he_now_s t = t.edam_now
    let she_now_s t = t.ava_now
    let inc_he_happy t c = {t with edam_s = ((he_happy t) + c);}
    let inc_she_happy t c = {t with ava_s = ((she_happy t) + c);}
    let dec_he_happy t c = {t with edam_s = ((he_happy t) - c);}
    let dec_she_happy t c = {t with ava_s = ((she_happy t) - c);}

    let set_he_now t edam_now = {t with edam_now;}
    let set_he_out t edam_out = {t with edam_out;}
    let set_she_now t ava_now = {t with ava_now;}
    let set_she_out t ava_out = {t with ava_out;}

    let give_nr t = Random.int (min (List.length t.edam_do) (List.length t.ava_do))
    let get_she_act t = let nr = (give_nr t) in List.nth t.ava_do nr
    let get_he_act t = let nr = (give_nr t) in List.nth t.edam_do nr
    let he_do t =
      let (act,out_self, out_other) = get_he_act t in
      let cs = set_he_now t act in
      let cs = inc_he_happy cs out_self in
      let cs = inc_she_happy cs out_other in
      let cs = set_he_out cs out_self in
      cs
    let she_do t =
      let (act,out_self, out_other) = get_she_act t in
      let cs = set_she_now t act in
      let cs = inc_she_happy cs out_self in
      let cs = inc_he_happy cs out_other in
      let cs = set_she_out cs out_other in
      cs
  end

module CoupleLog =
  struct
    let log_happy cs = let () = Printf.eprintf "\n***edam happy:%20d\n***ava happy:%20d\n" (CoupleState.he_happy cs) (CoupleState.she_happy cs) in flush stderr
    let log_do cs = let () = Printf.eprintf "\n***edam doing:%20s\n***ava doing:%20s\n" (CoupleState.he_now_s cs) (CoupleState.she_now_s cs) in flush stderr
    let log_header () = let () = Printf.eprintf "\n---------------------------------------------------------------------\n" in flush stderr
  end

let rec live cs =
 let cs = CoupleState.he_do cs in
 let cs = CoupleState.she_do cs in
 let () = CoupleLog.log_header () in
 let () = CoupleLog.log_do cs in
 let () = CoupleLog.log_happy cs in
 let () = CoupleLog.log_header () in
 Lwt_unix.sleep 10.0 >>= fun () ->
 live cs

let main =
  let cs = CoupleState.create_couple_state () in
  Lwt.pick [live cs] in
  Lwt_main.run main
