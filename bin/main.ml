open Lwt
open Cohttp
open Cohttp_lwt_unix

let _usd_eur =
  Client.get
    (Uri.of_string
       "https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/usd.xml")
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

module Tx = struct
  type t = {
    amount : int;
    fee : int;
    hash : string;
    date : string;
    src : string;
    dst : string;
    memo : string;
  }

  let pp_csv ppf { amount; fee; hash; date; src; dst; memo } =
    Format.fprintf ppf "%s,%d,%s,%s,%d,%s,%s" date amount src dst fee hash memo
end

let transactions_to_csv txs =
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
        let memo =
          Tezos_base58.Base58 (get Ezjsonm.get_string "memo")
          |> Tezos_base58.decode ~prefix:""
          |> Option.value ~default:""
        in
        { Tx.amount; fee; hash; date; src; dst; memo })
      txs
  in
  txs

let () =
  Format.printf "Computing Mina csv@.";
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let json = Ezjsonm.from_channel ic in
  close_in ic;
  let txs = transactions_to_csv json in
  Format.printf "Got %d txs@." (List.length txs);
  Format.open_vbox 0;
  List.iter
    (fun e ->
      Tx.pp_csv Format.std_formatter e;
      Format.print_cut ())
    txs;
  Format.close_box ()

(* let () =
 *   let body = Lwt_main.run usd_eur in
 *   let elts = Finance.Bce.parse_data body in
 *   let f = Finance.Bce.find_rate_for_date (CalendarLib.Date.make 2023 11 09) elts
 *           |> Option.get in 
 *   Format.printf "%.4f" f *)
