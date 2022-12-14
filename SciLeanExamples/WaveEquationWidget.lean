import Lean
import WidgetKit.Html
import WidgetKit.HtmlWidget
import WidgetKit.Svg

import SciLeanExamples.WaveEquation
import SciLeanExamples.WidgetUtils

open Lean Widget Jsx
open SciLean

private def Float.toInt (x : Float) : Int :=
  if x >= 0 then
    x.toUInt64.toNat
  else
    -((-x).toUInt64.toNat)

private def Int.toFloat (i : Int) : Float :=
  if i >= 0 then
    i.toNat.toFloat
  else
    -((-i).toNat.toFloat)

structure State (n : Nat) where
  x : ℝ^{n}
  p : ℝ^{n}
  m : ℝ := 1
  k : ℝ := 500000
  c : ℝ := 10000
  t : ℝ := 0
  deriving ToJson, FromJson

def frame : Svg.Frame := {min := ⟨-1,-1⟩, xSize := 2, width := 400, height := 400}

def State.toSvg {n} (s : State n) : Svg :=
  let Δθ := 2 * Math.pi / n |>.toFloat
  let elements : Array Svg.Element := 
    #[]
      -- triangles to click on
      -- |>.append ((Array.mkArray n (0 : Nat)).mapIdx 
      --   fun idx _ => 
      --     let θ₁ := (idx.1 * 2 * Math.pi).toFloat / n.toFloat - Δθ / 2 
      --     let θ₂ := ((idx.1+1) * 2 * Math.pi).toFloat / n.toFloat 
      --     let s := Float.sqrt 2
      --     { shape := .polygon #[⟨0,0⟩, ⟨s * Float.cos θ₁, s * Float.sin θ₁⟩, ⟨s * Float.cos θ₂, s * Float.sin θ₂⟩]
      --       fillColor := .some ⟨1,1,1⟩
      --       id := .some (toString idx.1)})
      |>.push 
        (let points := ((Array.mkArray n (0 : Nat)).mapIdx 
          (fun idx _ => 
            let θ := (idx.1 * 2 * Math.pi).toFloat / n.toFloat
            let s := 0.5 + s.x[⟨idx,sorry⟩].toFloat
            ⟨s * Float.cos θ, s * Float.sin θ⟩))
        { shape := .polygon points,
          fillColor := some ⟨0,0,0⟩})

  { elements := elements,
    frame := frame }

def State.init (n) : State n := {
  x := 0
  p := 0
}

def State.update {n} (s : State n) (Δt : ℝ) : State n := 
  have _ : Nonempty (Fin n) := sorry_proof
  let evolve := (solver (n := n) s.m s.k s.c 1).val
  let (x,p) := evolve Δt (s.x,s.p)
  {s with x := x, p := p, t := s.t + Δt}

#eval Float.atan2 (-0) (-0) + Math.pi.toFloat
def N := 100

open Server RequestM in
@[server_rpc_method]
def updatePhysics (params : UpdatePhysicsParams (State N)) : RequestM (RequestTask (UpdatePhysicsResult (State N))) := do
  let new_t := params.elapsed / 1000 |>.toReal -- convert milliseconds to seconds
  let Δt := (new_t - params.state.t)
  
  let mut state := params.state

  for action in params.actions do
      if action.kind = .mousedown then
        if let some (mx,my) := params.mousePos then
          dbg_trace "click position: ({mx},{my})"
          let (mx,my) := frame.fromPixels mx.toInt my.toInt
          dbg_trace "click position: ({mx},{my})"
          let θ := Float.atan2 my mx
          let idx := (N.toFloat * θ / (2*Math.pi.toFloat)).toInt
          let idx := (idx + N) % N
        -- state := { state with p := state.p.modify ⟨idx,sorry⟩ fun pi => pi + 100 }
          state := { state with p := state.p.mapIdx λ i pi => 
                     let d := min (Float.abs (i.1.toFloat - idx.toFloat)) <|
                              min (Float.abs (i.1.toFloat - N.toFloat - idx.toFloat))
                                  (Float.abs (i.1.toFloat + N.toFloat - idx.toFloat))
                     let d := 60 * d / N.toFloat
                     let w := Float.exp (- d*d)
                     pi + 100 * w.toReal }

  state := state.update Δt

  return RequestTask.pure $ {
    html :=
      <div>
        <div>
          {state.toSvg.toHtml}
        </div>
        {toString params.elapsed} {toString <| toJson <| params.actions} {toString <| toJson <| params.mousePos}
      </div>
    state := state
    callbackTime := some 33
  }

@[widget]
def physics : UserWidgetDefinition where
  name := "Magic physics demo"
  javascript := include_str ".." / "lean_packages" / "widgetKit" / "widget" / "dist" / "physics.js"

def init : UpdatePhysicsResult (State N) := {
  html := <div>Init!!</div>,
  state := State.init N }

#widget physics (toJson init)
