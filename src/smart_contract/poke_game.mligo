type poke_message =
  { receiver : address
  ; feedback : string
  }

type storage = 
  { poke_traces : (address, poke_message) map
  ; feedback : string
  }

type returned_feedback = address * string
type oracle_param = returned_feedback contract

type parameter = 
  | Poke
  | Poke_and_get_feedback of address
  | Poke_and_get_feedback_callback of returned_feedback
  | Get_feedback of oracle_param

type return = operation list * storage

let poke (store : storage) : return = 
  let feedback_message = { receiver = Tezos.get_self_address (); feedback = "" } in
  ([] : operation list), { store with poke_traces = Map.add (Tezos.get_source ()) feedback_message store.poke_traces }

let get_feedback (contr_callback : oracle_param) (store : storage) : return = 
  let op : operation = 
    Tezos.transaction (Tezos.get_self_address (), store.feedback) 0mutez contr_callback
  in
  [ op ], store

let poke_and_get_feedback (oracle_addr : address) (store : storage) : return = 
  let call_to_oracle () : oracle_param contract = 
    match (Tezos.get_entrypoint_opt "%get_feedback" oracle_addr : oracle_param contract option) with
    | Some contract -> contract
    | None -> failwith "NO_ORACLE_FOUND"
  in
  let op : operation = 
    Tezos.transaction
      (Tezos.self "%poke_and_get_feedback_callback" : returned_feedback contract)
      0mutez
      (call_to_oracle ())
  in
  [ op ], store

let poke_and_get_feedback_callback ((receiver, feedback) : returned_feedback) (store : storage) : return = 
  let feedback_message = { receiver = receiver ; feedback = feedback } in
  ([] : operation list), { store with poke_traces = Map.add (Tezos.get_source ()) feedback_message store.poke_traces }

let main (action, store : parameter * storage) : return = 
  match action with
  | Poke -> poke store
  | Poke_and_get_feedback addr -> poke_and_get_feedback addr store
  | Poke_and_get_feedback_callback feedback -> poke_and_get_feedback_callback feedback store
  | Get_feedback contr_callback -> get_feedback contr_callback store

