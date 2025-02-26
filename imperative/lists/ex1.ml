type stateT = {
  mutable host_: ((Z.t) * (Z.t)) list;
  mutable participants_: ((Z.t) * (Z.t)) list;
  mutable active_: (Z.t) list;
  }

let mkMeeting (_: unit) : stateT =
  { host_ = [] ; participants_ = [] ; active_ = []  }

let schedule (m: Z.t) (h: Z.t) (state: stateT) : unit =
  state.host_ <- (m, h) :: state.host_

let rec getHost_ (m: Z.t) (l: ((Z.t) * (Z.t)) list) : Z.t =
  match l with
  | [] -> assert false (* absurd *)
  | (m2, u) :: t -> if Z.equal m2 m then u else getHost_ m t

let start (m: Z.t) (state: stateT) : unit =
  state.participants_ <- (m, getHost_ m state.host_) :: state.participants_;
  state.active_ <- m :: state.active_

let join (m: Z.t) (u: Z.t) (state: stateT) : unit =
  state.participants_ <- (m, u) :: state.participants_

let rec leaveMeet_ (m: Z.t) (u: Z.t) (l: ((Z.t) * (Z.t)) list) :
  ((Z.t) * (Z.t)) list =
  match l with
  | [] -> [] 
  | (m2, u2) :: t ->
    if Z.equal m2 m && Z.equal u2 u then t else (m2, u2) :: leaveMeet_ m u t

let leave (m: Z.t) (u: Z.t) (state: stateT) : unit =
  state.participants_ <- leaveMeet_ m u state.participants_

let rec remove_meeting_participants (m: Z.t) (l: ((Z.t) * (Z.t)) list) :
  ((Z.t) * (Z.t)) list =
  match l with
  | [] -> [] 
  | (m2, u2) :: t ->
    if Z.equal m2 m
    then remove_meeting_participants m t
    else (m2, u2) :: remove_meeting_participants m t

let rec deactivate_meeting_ (m: Z.t) (l: (Z.t) list) : (Z.t) list =
  match l with
  | [] -> [] 
  | m2 :: t -> if Z.equal m2 m then t else m2 :: deactivate_meeting_ m t

let end_ (m: Z.t) (state: stateT) : unit =
  state.participants_ <- remove_meeting_participants m state.participants_;
  state.active_ <- deactivate_meeting_ m state.active_

let rec printPairs (l: ((Z.t) * (Z.t)) list) : unit =
  match l with
  | [] -> print_string "\n"
  | (m, u) :: t ->
                  print_string "(";
                  print_string (Z.to_string m);
                  print_string ", ";
                  print_string (Z.to_string u);
                  print_string ") ";
                  printPairs t


let rec printSingles (l: (Z.t) list) : unit =
  match l with
  | [] -> print_string "\n"
  | m :: t ->
                  print_string (Z.to_string m);
                  print_string "  ";
                  printSingles t



let printState (state: stateT) : unit  =
  print_string "active meetings:  ";
  printSingles state.active_;
  print_string "\nhosts:  ";
  printPairs state.host_;
  print_string "\nparticipants:  ";
  printPairs state.participants_;
  print_string "\n\n"



let main (_: unit) : unit =
  let meet = mkMeeting () in
  schedule Z.one Z.one meet;
  schedule (Z.of_string "2") Z.one meet;
  start Z.one meet;
  start (Z.of_string "2") meet;
  schedule (Z.of_string "3") (Z.of_string "2") meet;
  start (Z.of_string "3") meet;
  join (Z.of_string "3") Z.one meet;
  join (Z.of_string "2") (Z.of_string "3") meet;
  join (Z.of_string "3") (Z.of_string "3") meet;
  join (Z.of_string "3") (Z.of_string "4") meet;
  end_ Z.one meet;
  leave (Z.of_string "3") Z.one meet;
  schedule (Z.of_string "4") (Z.of_string "5") meet;
  printState meet
  


let () = main()
