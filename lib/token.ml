type token = {
  symbol : string;
  mutable supply : float; (* Change from int to float *)
}

let create_token symbol = { symbol; supply = 0. }

let mint_tokens token amount =
  if amount >= 0. then token.supply <- token.supply +. amount

let mint_token token amount =
  if amount >= 0. then token.supply <- token.supply +. amount;
  token

let to_string token = token.symbol
let get_supply token = token.supply

let transfer_tokens sender recipient amount =
  if amount >= 0. && sender.supply >= amount then (
    sender.supply <- sender.supply -. amount;
    recipient.supply <- recipient.supply +. amount)
