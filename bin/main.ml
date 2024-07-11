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
end

module Transactions = struct
  type t = Tx.t list

  let decode_base58_string str =
    let open Tezos_base58 in
    match decode ~prefix:"" (Base58 str) with None -> "" | Some s -> s

  let of_json addr txs =
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

  let total (txs : t) =
    List.fold_left
      (fun sum { Tx.amount; op_type; _ } ->
        let op =
          match op_type with
          | Operation.Withdrawal -> Int.sub
          | Operation.Payment -> Int.add
        in
        op sum amount)
      0 txs

  let of_file addr filename =
    let ic = open_in filename in
    let json = Ezjsonm.from_channel ic in
    close_in ic;
    of_json addr json

  module Csv = struct
    let pp_row ppf { Tx.amount; fee; hash; date; src; dst; descr; op_type } =
      Format.fprintf ppf "%s,%d,MINA,,%s,,USD,%s,%a,%s,%s,%d" date amount hash
        descr Operation.pp op_type src dst fee

    let pp_headers ppf () =
      Format.fprintf ppf
        "Koinly Date,Amount,Currency,Label,TxHash,Net Worth Amount,Net Worth \
         Currency,Description,Type,SendingWallet,ReceivingWallet,Fee"

    let pp ppf txs =
      Format.pp_open_vbox ppf 0;
      pp_headers ppf ();
      Format.pp_print_cut ppf ();
      List.iter
        (fun e ->
          pp_row ppf e;
          Format.pp_print_cut ppf ())
        txs;
      Format.pp_close_box ppf ()
  end
end

let addr = "B62qip7NtNvAnqkEY6oZZCtn9x2YHY8kQaP6ddBYuhhrzpFbeW3abc5"

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
  Client.post ~headers ~body uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  Format.printf "%s@." body;
  body

let extract_data json_resp =
  let open Ezjsonm in
  get_dict json_resp |> List.assoc "data" |> get_dict
  |> List.assoc "transactions"

let () =
  Format.printf "Computing Mina csv@.";
  let txs =
    if Array.length Sys.argv > 1 then (
      let filename = Sys.argv.(1) in
      assert (Sys.file_exists filename);
      Transactions.of_file addr filename)
    else
      let body = Lwt_main.run body in
      Ezjsonm.from_string body |> extract_data |> Transactions.of_json addr
  in
  Format.printf "Got %d txs@." (List.length txs);
  let oc, ppf =
    let oc = open_out_bin "koinly.csv" in
    (oc, Format.formatter_of_out_channel oc)
  in
  Format.fprintf ppf "%a@." Transactions.Csv.pp txs;
  close_out oc;
  Format.printf "Total: %d@." (Transactions.total txs)
