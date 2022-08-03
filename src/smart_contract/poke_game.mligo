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

// @view
let feedback (_, store : unit * storage) : string = 
  store.feedback


let poke_and_get_feedback (oracle_addr : address) (store : storage) : return = 
  match (Tezos.call_view "feedback" unit oracle_addr : string option) with
  | Some feedback  ->
    let feedback_message = { receiver = oracle_addr; feedback = feedback } in
    [], { store with poke_traces = Map.add (Tezos.get_source ()) feedback_message store.poke_traces }
  | None ->
    failwith "Cannot find view feedback on given oracle address"

let poke_and_get_feedback_callback ((receiver, feedback) : returned_feedback) (store : storage) : return = 
  let feedback_message = { receiver = receiver ; feedback = feedback } in
  ([] : operation list), { store with poke_traces = Map.add (Tezos.get_source ()) feedback_message store.poke_traces }

let main (action, store : parameter * storage) : return = 
  match action with
  | Poke -> poke store
  | Poke_and_get_feedback addr -> poke_and_get_feedback addr store
  | Poke_and_get_feedback_callback feedback -> poke_and_get_feedback_callback feedback store
  | Get_feedback contr_callback -> get_feedback contr_callback store

