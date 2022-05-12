namespace TurnDamageSimulator

type Combatant =
    { Attack: uint32
      Defense: uint32
      Speed: uint32
      MaxHp: uint32
      MaxMp: uint32 }

type CombatantState =
    { Id: uint32
      Hp: uint32
      Mp: uint32 }

type Combatants = Map<uint32, (CombatantState * Combatant)>

type GameState =
    { Combatants: Combatants }

type Encounter = GameState list

module Combatants =
    let empty = Map.empty<uint32, (CombatantState * Combatant)>

module GameState =
    let empty = {Combatants = Combatants.empty}
    
    let addCombatant c id gs =
        let cs = {Id = id; Hp = c.MaxHp; Mp = c.MaxMp}
        {gs with Combatants = Map.add cs.Id (cs, c) gs.Combatants}
        
    let damageCombatant (cs, c) d gs =
        let cs' = {cs with Hp = cs.Hp - (min cs.Hp d)}
        let m = Map.change cs.Id (fun x ->
            match x with
            | Some _ -> Some(cs', c)
            | None -> None) gs.Combatants
        {gs with Combatants = m}
        
module Encounter =
    let empty = List<GameState>.Empty
    
    let addCombatant c id e =
        match e with
        | head :: _ -> (GameState.addCombatant c id head) :: e
        | _ -> List.singleton (GameState.empty |> GameState.addCombatant c id)
        
    let damageCombatant id d e =
        match e with
        | head :: _ ->
            let x = head.Combatants |> Map.find id
            (GameState.damageCombatant x id d) :: e
        | _ -> e