module Operation = struct
  type t = Withdrawal | Payment

  let pp ppf = function
    | Withdrawal -> Format.fprintf ppf "withdrawal"
    | Payment -> Format.fprintf ppf "payment"
end

module Tx = struct
  type t = {
    amount : int;
    fee : int;
    hash : string;
    date : string;
    src : string;
    dst : string;
    descr : string;
    op_type : Operation.t;
  }

  let pp_csv ppf { amount; fee; hash; date; src; dst; descr; op_type } =
    Format.fprintf ppf "%s,%d,MINA,,%s,,USD,%s,%a,%s,%s,%d" date amount hash
      descr Operation.pp op_type src dst fee

  let pp_headers ppf () =
    Format.fprintf ppf
      "Koinly Date,Amount,Currency,Label,TxHash,Net Worth Amount,Net Worth \
       Currency,Description,Type,SendingWallet,ReceivingWallet,Fee"
end

type txs = Tx.t list

let decode_base58_string str =
  let open Tezos_base58 in
  match decode ~prefix:"" (Base58 str) with None -> "" | Some s -> s

let transactions_to_csv addr txs =
  let txs =
    Ezjsonm.get_list
      (fun tx ->
        let o = Ezjsonm.get_dict tx in
        let get decode key = List.assoc key o |> decode in
        let amount = get Ezjsonm.get_int "amount" in
        let fee = get Ezjsonm.get_int "fee" in
        let hash = get Ezjsonm.get_string "hash" in
        let date = get Ezjsonm.get_string "dateTime" in
        let src = get Ezjsonm.get_string "from" in
        let dst = get Ezjsonm.get_string "to" in
        let descr = decode_base58_string (get Ezjsonm.get_string "memo") in
        let op_type =
          let open Operation in
          if String.equal src addr then Withdrawal else Payment
        in
        { Tx.amount; fee; hash; date; src; dst; descr; op_type })
      txs
  in
  txs

let total (txs : txs) =
  List.fold_left
    (fun sum { Tx.amount; op_type; _ } ->
      let op =
        match op_type with
        | Operation.Withdrawal -> Int.sub
        | Operation.Payment -> Int.add
      in
      op sum amount)
    0 txs

let () =
  Format.printf "Computing Mina csv@.";
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let addr = "B62qip7NtNvAnqkEY6oZZCtn9x2YHY8kQaP6ddBYuhhrzpFbeW3abc5" in
  let json = Ezjsonm.from_channel ic in
  close_in ic;
  let txs = transactions_to_csv addr json in
  Format.printf "Got %d txs@." (List.length txs);
  Format.open_vbox 0;
  Tx.pp_headers Format.std_formatter ();
  Format.print_cut ();
  List.iter
    (fun e ->
      Tx.pp_csv Format.std_formatter e;
      Format.print_cut ())
    txs;
  Format.printf "Total: %d@." (total txs);
  Format.close_box ()

let body =
  let open Lwt in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let headers = Header.init_with "Content-Type" "application/json" in
  let uri = Uri.of_string "https://graphql.minaexplorer.com" in
  Lwt_io.open_file ~mode:Lwt_io.Input "./transactions.gql" >>= fun ic ->
  let stream = Lwt_io.read_lines ic in
  let b = Buffer.create 1024 in
  Lwt_stream.iter_s
    (fun line ->
      Buffer.add_string b line;
      Buffer.add_char b ' ';
      Lwt.return_unit)
    stream
  >>= fun () ->
  let body_s = Printf.sprintf "{\"query\": %S}" (Buffer.contents b) in
  let body = Cohttp_lwt.Body.of_string body_s in
  Cohttp_lwt.Body.to_string body >>= fun body_s ->
  Format.printf "body: %s@." body_s;
  Client.post ~headers ~body uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let () =
  Format.printf "@.----@.@.";
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body)

(* let () =
 *   let body = Lwt_main.run usd_eur in
 *   let elts = Finance.Bce.parse_data body in
 *   let f = Finance.Bce.find_rate_for_date (CalendarLib.Date.make 2023 11 09) elts
 *           |> Option.get in 
 *   Format.printf "%.4f" f *)
