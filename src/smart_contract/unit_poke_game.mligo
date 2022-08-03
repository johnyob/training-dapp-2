#include "./poke_game.mligo"

let _ = Test.reset_state 2n

let faucet = Test.nth_bootstrap_account 0 
let () = Test.set_baker faucet
let () = Test.set_source faucet

let sender_1 : address = Test.nth_bootstrap_account 1
let () = Test.log ("Sender 1 has balance: ", Test.get_balance sender_1)

let test_1 =
  let initial_storage = { poke_traces = (Map.empty : (address, poke_message) map); feedback = "liss" } in
  let taddr, _, _ = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract taddr in
  let contr_addr = Tezos.address contr in
  let () = Test.log ("Contract deployed with values: ", contr) in
  let () = Test.set_source sender_1 in
  let _status = Test.transfer_to_contract contr Poke 0tez in
  let store = Test.get_storage taddr in
  match Map.find_opt sender_1 store.poke_traces with
  | Some (poke_message) -> 
    (let _ = assert_with_error (poke_message.feedback = "") in
    let _ = assert_with_error (poke_message.receiver = contr_addr) in
    true) 
  | None -> false