module Url = struct
  let base = "https://api.coingecko.com/api/v3"
  let simple_price = Uri.of_string @@ base ^ "/simple/price"
end

let eur = "eur" 
let mina = "mina-protocol"

let get_price json =
  let json = Ezjsonm.from_string json in
  let o = Ezjsonm.get_dict json in 
  List.assoc mina o |> Ezjsonm.get_dict |> List.assoc eur |> Ezjsonm.get_float 

let simple_price ids vs_currencies =
  let open Lwt in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let uri =
    Uri.add_query_params Url.simple_price
      [ ("ids", ids); ("vs_currencies", vs_currencies) ]
  in
  let headers = Header.init_with "Content-Type" "application/json" in
  Client.get ~headers uri >>= fun (resp, body) ->
  let _code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= get_price

let mina_eur_rate ()= 
  simple_price [ "mina-protocol" ] [ "eur" ]
