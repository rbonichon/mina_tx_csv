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


let () = Format.printf "Computing Mina csv@."
(* let () =
 *   let body = Lwt_main.run usd_eur in
 *   let elts = Finance.Bce.parse_data body in
 *   let f = Finance.Bce.find_rate_for_date (CalendarLib.Date.make 2023 11 09) elts
 *           |> Option.get in 
 *   Format.printf "%.4f" f *)
