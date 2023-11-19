open Printf

module Counter = Preface.Reader.Over (struct
    type t = int
  end)

let init = Counter.return "Waiting for data..."

type order =
  | Eq
  | Lt
  | Gt

let comp = function
  | 0 -> Eq
  | n when n > 0 -> Gt
  | _ -> Lt
;;

let smart =
  let open Counter.Syntax in
  let* n = Counter.ask in
  Counter.return
    (match comp n with
     | Eq -> "zero"
     | Gt -> "positive"
     | Lt -> "negative")
;;

let%test_unit _ =
  let ( => ) = [%test_eq: Base.string] in
  let test1 = Counter.run_identity init in
  let test2 = Counter.run_identity smart in
  ()
  ; test1 (-0) => "Waiting for data..."
  ; test2 (-0) => "zero"
  ; test2 9999 => "positive"
  ; test2 (-1) => "negative"
;;

type app_state =
  { bank_balance : int
  ; unread_mail_count : int
  }

module AppState = Preface.Reader.Over (struct
    type t = app_state
  end)

let account_status =
  let open AppState.Syntax in
  let* env = AppState.ask in
  AppState.return
    (match comp env.bank_balance with
     | Gt -> sprintf "Current balance: %d EUR" env.bank_balance
     | Eq -> sprintf "No money left :("
     | Lt -> sprintf "Your account is in overdraft!")
;;

let init = { bank_balance = 1000; unread_mail_count = 3 }

let%test_unit _ =
  let ( => ) = [%test_eq: Base.string] in
  let status_with ~bank_balance =
    AppState.run_identity account_status { init with bank_balance }
  in
  ()
  ; status_with ~bank_balance:9999 => "Current balance: 9999 EUR"
  ; status_with ~bank_balance:(-0) => "No money left :("
  ; status_with ~bank_balance:(-1) => "Your account is in overdraft!"
;;

let mailbox_status =
  let open AppState.Syntax in
  let* env = AppState.ask in
  AppState.return
    (match env.unread_mail_count > 0 with
     | true -> sprintf "You've got mail: %d" env.unread_mail_count
     | false -> sprintf "Your mailbox is empty!")
;;

let%test_unit _ =
  let ( => ) = [%test_eq: Base.string] in
  let status_with ~unread_mail_count =
    AppState.run_identity mailbox_status { init with unread_mail_count }
  in
  ()
  ; status_with ~unread_mail_count:3 => "You've got mail: 3"
  ; status_with ~unread_mail_count:0 => "Your mailbox is empty!"
;;

let app_status =
  let open AppState.Syntax in
  let* account = account_status in
  let* mailbox = mailbox_status in
  AppState.return @@ String.concat " | " [ account; mailbox ]
;;

let%test_module "app_status" =
  (module struct
    let eq = [%test_eq: Base.string]
    let status_with = AppState.run_identity app_status

    let%test_unit _ =
      eq "Current balance: 1000 EUR | You've got mail: 3" @@ status_with init
    ;;

    let%test_unit _ =
      eq "No money left :( | You've got mail: 3"
      @@ status_with { init with bank_balance = 0 }
    ;;

    let%test_unit _ =
      eq "Your account is in overdraft! | Your mailbox is empty!"
      @@ status_with { bank_balance = -1; unread_mail_count = 0 }
    ;;

    let%test_unit _ =
      eq "Your account is in overdraft! | You've got mail: 999"
      @@ status_with { bank_balance = -1; unread_mail_count = 999 }
    ;;
  end)
;;
