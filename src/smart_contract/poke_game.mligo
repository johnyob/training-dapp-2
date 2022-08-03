type poke_message =
  { receiver : address
  ; feedback : string
  }

type storage = 
  { poke_traces : (address, poke_message) map
  ; feedback : string
  }

type parameter = 
  | Poke

type return = operation list * storage

let poke (store : storage) : return = 
  let feedback_message = { receiver = Tezos.get_self_address (); feedback = "" } in
  ([] : operation list), { store with poke_traces = Map.add (Tezos.get_source ()) feedback_message store.poke_traces }

let main (action, store : parameter * storage) : return = 
  match action with
  | Poke -> poke store