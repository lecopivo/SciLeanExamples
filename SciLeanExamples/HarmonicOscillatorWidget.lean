import Lean
import WidgetKit.Html
import WidgetKit.HtmlWidget
import WidgetKit.Svg

import SciLeanExamples.HarmonicOscillator

open Lean Widget Jsx
open SciLean

instance : ToJson ℝ where
  toJson x := toJson x.1

instance : FromJson ℝ where
  fromJson? json :=
    match fromJson? (α := Float) json with
    | .error msg => .error msg
    | .ok x => .ok ⟨x⟩

namespace SciLean.PowTypeCarrier

  variable {X n} {T : outParam Type} [PowType T (Fin n) X] -- [Inhabited X]

  def toArray (v : X^{n}) : Array X := Id.run do
    let mut array : Array X := Array.mkEmpty n
    for h : i in [0:n] do
      array := array.push v[⟨i,h.2⟩]
    return array

  instance [ToJson X] : ToJson (X^{n}) where
    toJson v := toJson (v.toArray)

  instance [FromJson X] : FromJson (X^{n}) where
    fromJson? json := 
      match fromJson? (α := Array X) json with
      | .error msg => .error msg
      | .ok array => 
        if h : n = array.size then
          .ok (introElem λ i => array[h ▸ i])
        else 
          .error "Failed to convert to json to PowType X^{n}, json size does not match `n`"

end SciLean.PowTypeCarrier


structure State where
  x : ℝ^{2}
  p : ℝ^{2}
  m : ℝ := 1
  k : ℝ := 10
  t : ℝ := 0
  deriving ToJson, FromJson


def State.toSvg (s : State) : Svg :=
  let elements : Array Svg.Element := 
    #[] 
      |>.push 
          { shape := .line ⟨0,0⟩ ⟨s.x.x.1,s.x.y.1⟩, 
            strokeWidth := some (.pixels 2), 
            strokeColor := some ⟨1,1,1⟩ }
      |>.push 
          { shape := .circle ⟨s.x.x.1,s.x.y.1⟩ (.absolute 0.1), 
            fillColor := let speed := 0.3*∥s.p∥/s.m |>.toFloat; some ⟨1-speed, speed, 0⟩, 
            clickData := some "circle"} 

  { elements := elements,
    frame := {min := ⟨-1,-1⟩, xSize := 2, width := 400, height := 400} }

def State.init : State := {
  x := ⟨0,0⟩
  p := ⟨0,0⟩
}

def State.update (s : State) (Δt : ℝ) : State := 
  let evolve := (solver s.m s.k 1).val
  let (x,p) := evolve Δt (s.x,s.p)
  {s with x := x, p := p, t := s.t + Δt}

inductive ActionKind where
  | timeout
  | click
  deriving ToJson, FromJson, DecidableEq

structure Action where
  -- can be 'timeout' or 'click'
  kind : ActionKind
  value : Json
  deriving ToJson, FromJson

structure UpdatePhysicsParams where
  elapsed : Float
  actions : Array Action
  state : State
  deriving ToJson, FromJson

structure UpdatePhysicsResult where
  html : Widget.Html
  state : State
  /-- Approximate number of milliseconds to wait before calling again. -/
  callbackTime : Option Float := some 100
  deriving ToJson, FromJson

open Server RequestM in

@[server_rpc_method]
def updatePhysics (params : UpdatePhysicsParams) : RequestM (RequestTask UpdatePhysicsResult) := do
  let Δt := ((params.elapsed / 1000).toReal - params.state.t)
  
  let mut state := params.state

  for action in params.actions do
    if action.kind = .click then
      let θ := 1000 * state.t
      let dir : ℝ^{2} := ⟨Math.cos θ, Math.sin θ⟩
      state := { state with p := state.p +  state.m * dir }

  state := state.update Δt

  return RequestTask.pure $ {
    html := <div>
      <div>
        {state.toSvg.toHtml}
      </div>

      {toString params.elapsed} {toString Δt} {toString <| toJson <| params.actions}</div>,
    state := state,
    callbackTime := some 33,
  }

@[widget]
def physics : UserWidgetDefinition where
  name := "Magic physics demo"
  javascript := include_str ".." / "lean_packages" / "widgetKit" / "widget" / "dist" / "physics.js"

def init : UpdatePhysicsResult := {
  html := <div>Init!!!</div>,
  state := State.init,
}

#widget physics (toJson init)
