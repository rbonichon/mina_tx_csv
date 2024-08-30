module Operation = struct
  type t = Withdrawal | Deposit

  let pp ppf = function
    | Withdrawal -> Format.fprintf ppf "withdrawal"
    | Deposit -> Format.fprintf ppf "deposit"
end

module Decimal = struct
  type t = int * int

  let of_nano =
    let div = 1_000_000_000 in
    fun n : t -> (n / div, n mod div)

  let pp ppf (d, v) =
    let digits = 9 in
    let rec actual_digits_to_print d v =
      if v mod 10 = 0 then
        if d = 1 then (1, v) else actual_digits_to_print (d - 1) (v / 10)
      else (d, v)
    in
    let ndigits, v = actual_digits_to_print digits v in
    let fmt_str = Format.sprintf "%%d.%%0%dd" ndigits in
    let fmt = Scanf.format_from_string fmt_str "%d.%d" in
    Format.fprintf ppf fmt d v
end

module Tx = struct
  type nano = int

  let pp_nano ppf n = Decimal.pp ppf (Decimal.of_nano n)

  type t =
    | Concrete of {
        amount : nano;
        fee : nano;
        hash : string;
        date : string;
        src : string;
        dst : string;
        descr : string;
        op_type : Operation.t;
      }
    | Pseudo of { amount : nano; date : string; hash : string; descr : string }

  let[@warning "-8"] pseudo_of_concrete ~amount ?descr (Concrete { date; hash; descr = d; _ })
      =
    Pseudo
      {
        amount;
        date;
        hash;
        descr = (match descr with None -> d | Some text -> text);
      }
end

module Transactions = struct
  type t = Tx.t list

  let decode_base58_string str =
    let open Tezos_base58 in
    match decode ~prefix:"" (Base58 str) with None -> "" | Some s -> s

  let account_creation_fee = 1_000_000_000

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
          let descr =
            let s = decode_base58_string (get Ezjsonm.get_string "memo") in
            let s = String.sub s 3 (String.length s - 3) |> String.trim in
            let b = Buffer.create (String.length s) in
            String.iter (function '\x00' -> () | c -> Buffer.add_char b c) s;
            Buffer.contents b
          in
          let op_type =
            let open Operation in
            if String.equal src addr then Withdrawal else Deposit
          in
          Tx.Concrete { amount; fee; hash; date; src; dst; descr; op_type })
        txs
    in
    match List.rev txs with
    | [] -> []
    | tx1 :: txs ->
        List.rev
        @@ tx1
           :: Tx.pseudo_of_concrete ~amount:(-account_creation_fee)
                ~descr:"Ledger Fee" tx1
           :: txs

  let total (txs : t) =
    List.fold_left
      (fun sum tx ->
        match tx with
        | Tx.Concrete { amount; op_type; fee; _ } -> (
            match op_type with
            | Operation.Withdrawal -> Int.sub sum (amount + fee)
            | Operation.Deposit -> Int.add sum amount)
        | Tx.Pseudo { amount; _ } -> Int.add sum amount)
      0 txs

  let of_file addr filename =
    let ic = open_in filename in
    let json = Ezjsonm.from_channel ic in
    close_in ic;
    of_json addr json

  module Csv = struct
    let pp_row ppf = function
      | Tx.Concrete { amount; fee; hash; date; src; dst; descr; op_type } ->
          let amount =
            match op_type with
            | Operation.Deposit -> amount
            | Withdrawal -> amount + fee
          in
          let fee =
            match op_type with Operation.Deposit -> 0 | Withdrawal -> fee
          in
          Format.fprintf ppf "%s,%a,MINA,,%s,,USD,%s,%a,%s,%s,%d" date
            Tx.pp_nano amount hash descr Operation.pp op_type src dst fee
      | Tx.Pseudo { amount; date; hash; descr } ->
          Format.fprintf ppf "%s,%a,MINA,,%s,,USD,%s,deposit,,," date Tx.pp_nano
            amount hash descr

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

module Params = struct
  let addr = ref "B62qip7NtNvAnqkEY6oZZCtn9x2YHY8kQaP6ddBYuhhrzpFbeW3abc5"
  let limit = ref 10000

  let set_node_uri, get_node_uri =
    let value = ref (Uri.of_string "https://graphql.minaexplorer.com") in
    ((fun s -> value := Uri.of_string s), fun () -> !value)
end

let args =
  Arg.align
    [
      ( "-addr",
        Arg.Set_string Params.addr,
        " set account address for which to get transactions history" );
      ("-limit", Arg.Set_int Params.limit, " get at most <limit> transactions");
      ("-url", Arg.String Params.set_node_uri, " query endpoint at <url>");
    ]

let build_query limit addr =
  Printf.sprintf
    "query F { transactions(limit: %d, sortBy: DATETIME_DESC, query: { \
     canonical: true, OR: [{to: %S}, {from: %S}]}) { fee from to nonce amount \
     memo hash kind dateTime block { blockHeight stateHash } }}"
    limit addr addr

let extract_data json_resp =
  let open Ezjsonm in
  get_dict json_resp |> List.assoc "data" |> get_dict
  |> List.assoc "transactions"

let parse_body body =
  Ezjsonm.from_string body |> extract_data
  |> Transactions.of_json !Params.addr
  |> List.rev

let trace ~f ppf txt =
  Format.kfprintf
    (fun ppf ->
      Format.pp_print_string ppf " ... ";
      let res = f () in
      Format.fprintf ppf "done@.";
      res)
    ppf txt

let body limit addr =
  let open Lwt in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let headers = Header.init_with "Content-Type" "application/json" in
  let uri = Params.get_node_uri () in
  let body =
    let body_s = Printf.sprintf "{\"query\": %S}" (build_query limit addr) in
    Cohttp_lwt.Body.of_string body_s
  in
  Client.post ~headers ~body uri >>= fun (resp, body) ->
  let _code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= parse_body

let output_csv ~filename ~txs () =
  let oc, ppf =
    let oc = open_out_bin filename in
    (oc, Format.formatter_of_out_channel oc)
  in
  Format.fprintf ppf "%a@." Transactions.Csv.pp txs;
  close_out oc

let main () =
  let open Lwt.Syntax in
  Arg.parse args (fun _ -> ()) "";
  let addr = !Params.addr in
  let limit = !Params.limit in
  let txs =
    if Array.length Sys.argv > 1 then (
      let filename = Sys.argv.(1) in
      assert (Sys.file_exists filename);
      Transactions.of_file !Params.addr filename)
    else Lwt_main.run @@ body limit addr
  in
  let filename = Printf.sprintf "koinly_%s.csv" !Params.addr in
  trace Format.std_formatter "Writing file %s" filename
    ~f:(output_csv ~filename ~txs);
  let tx_total = Transactions.total txs in
  let total = Decimal.of_nano tx_total in
  Format.printf "@[<h>Total: %a MINA (%d txs)@]@." Decimal.pp total
    (List.length txs);
  let* rate = Coingecko.mina_eur_rate () in
  let rated_tx_total =
    rate *. float_of_int tx_total |> truncate |> Decimal.of_nano
  in
  Format.printf "@[<h>Total €: %a@]@." Decimal.pp rated_tx_total;
  Lwt.return_unit

let () = Lwt_main.run @@ main ()
