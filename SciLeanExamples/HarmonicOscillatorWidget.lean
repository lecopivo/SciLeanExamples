import Lean
import WidgetKit.Html
import WidgetKit.HtmlWidget
import WidgetKit.Svg

import SciLeanExamples.HarmonicOscillator
import SciLeanExamples.WidgetUtils

open Lean Widget Jsx
open SciLean

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


open Server RequestM in

@[server_rpc_method]
def updatePhysics (params : UpdatePhysicsParams State) : RequestM (RequestTask (UpdatePhysicsResult State)) := do
  let new_t := params.elapsed / 1000 |>.toReal -- convert milliseconds to seconds
  let Δt := (new_t - params.state.t)
  
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

def init : UpdatePhysicsResult State := {
  html := <div>Init!!!</div>,
  state := State.init,
}

#widget physics (toJson init)
