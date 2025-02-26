type stateT = {
  host_: ((Z.t) * (Z.t)) list;
  participants_: ((Z.t) * (Z.t)) list;
  active_: (Z.t) list;
  }

let mkMeeting (_: unit) : stateT =
  { host_ = [] ; participants_ = [] ; active_ = []  }

let schedule (m: Z.t) (h: Z.t) (state: stateT) : stateT =
  { host_ = (m, h) :: state.host_; participants_ = state.participants_;
    active_ = state.active_ }

let rec getHost_ (m: Z.t) (l: ((Z.t) * (Z.t)) list) : Z.t =
  match l with
  | [] -> assert false (* absurd *)
  | (m2, u) :: t -> if Z.equal m2 m then u else getHost_ m t

let start (m: Z.t) (state: stateT) : stateT =
  { host_ = state.host_; participants_ =
    (m, getHost_ m state.host_) :: state.participants_; active_ =
    m :: state.active_ }

let join (m: Z.t) (u: Z.t) (state: stateT) : stateT =
  { host_ = state.host_; participants_ = (m, u) :: state.participants_;
    active_ = state.active_ }

let rec leaveMeet_ (m: Z.t) (u: Z.t) (l: ((Z.t) * (Z.t)) list) :
  ((Z.t) * (Z.t)) list =
  match l with
  | [] -> [] 
  | (m2, u2) :: t ->
    if Z.equal m2 m && Z.equal u2 u then t else (m2, u2) :: leaveMeet_ m u t

let leave (m: Z.t) (u: Z.t) (state: stateT) : stateT =
  { host_ = state.host_; participants_ = leaveMeet_ m u state.participants_;
    active_ = state.active_ }

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

let end_ (m: Z.t) (state: stateT) : stateT =
  { host_ = state.host_; participants_ =
    remove_meeting_participants m state.participants_; active_ =
    deactivate_meeting_ m state.active_ }

let main (_: unit) : stateT =
  let meet = mkMeeting () in
  let meet1 = schedule Z.one Z.one meet in
  let meet2 = schedule (Z.of_string "2") Z.one meet1 in
  let meet3 = start Z.one meet2 in
  let meet4 = start (Z.of_string "2") meet3 in
  let meet5 = schedule (Z.of_string "3") (Z.of_string "2") meet4 in
  let meet6 = start (Z.of_string "3") meet5 in
  let meet7 = join (Z.of_string "3") Z.one meet6 in
  let meet8 = join (Z.of_string "2") (Z.of_string "3") meet7 in
  let meet9 = join (Z.of_string "3") (Z.of_string "3") meet8 in
  let meet10 = join (Z.of_string "3") (Z.of_string "4") meet9 in
  let meet11 = end_ Z.one meet10 in
  let meet12 = leave (Z.of_string "3") Z.one meet11 in
  schedule (Z.of_string "4") (Z.of_string "5") meet12

