import Lean
import WidgetKit.Html
import WidgetKit.HtmlWidget
import WidgetKit.Svg

import SciLean.Data.DataArray
import SciLean.Data.Bezier
import SciLean.Data.Curve
import SciLean.Data.FunRec
import SciLeanExamples.WidgetUtils

open Lean Widget Jsx
open SciLean

variable (n) (x : ℝ^{n,2}) (i)


structure State (n : Nat) where
  x : (ℝ^{2})^{n}
  t : ℝ := 0
  deriving ToJson, FromJson


def State.toSvg {n} (s : State n) : Svg :=
  let Δθ := 2 * Math.pi / n |>.toFloat
  let curvePoints := 
    Curve.sampleAdaptive (Bezier.eval s.x) (by simp; apply AutoImpl.finish) 0 1 100 1000 sorry

  let rec step : {n' : Nat} → (ℝ^{2})^{n'} × Array Svg.Element → Array Svg.Element
    | 0, (pts,elems) => elems
    | n+1, (pts,elems) =>
      let elems := elems
        |>.push
          { shape := .polyline (pts.toArray.map λ p => ⟨p.x.toFloat, p.y.toFloat⟩),
            strokeWidth := some (.pixels 2)
            strokeColor := some ⟨0.4,0.4,0.4⟩ }
        |>.append
          (pts.toArray.map (λ p => 
           { shape := .circle ⟨p.x.toFloat, p.y.toFloat⟩ (.pixels 3),
             strokeWidth := some (.pixels 2)
             strokeColor := some ⟨0.7,0.7,0.7⟩ }))
      let t' := 0.1*s.t
      step (pts.linearInterpolate (t' - Math.floor t'), elems)
  
  let casteljauElements : Array Svg.Element := step (s.x, #[])
  
  let elements : Array Svg.Element := 
    #[]
      |>.append casteljauElements
      -- triangles to click on
      |>.push 
        { shape := .polyline (curvePoints.map λ p => ⟨p.x.toFloat, p.y.toFloat⟩),
          strokeWidth := some (.pixels 2)
          strokeColor := some ⟨1,1,1⟩ }
      |>.append 
        (s.x.toArray.mapIdx (λ idx p => 
         { shape := .circle ⟨p.x.toFloat, p.y.toFloat⟩ (.pixels 5),
           fillColor := some ⟨1,1,1⟩}))

  { elements := elements,
    frame := {min := ⟨-1,-1⟩, xSize := 2, width := 400, height := 400} }

def State.init (n) : State n := {
  x := λ [i] => 
    let θ := (2*i.1*Math.pi)/(n.toReal)
    ⟨Math.cos θ, Math.sin θ⟩
}

def State.update {n} (s : State n) (Δt : ℝ) : State n :=
  let θ := 0.0*Δt
  let cos := Math.cos θ
  let sin := Math.sin θ
  {s with x := s.x.mapIdx (λ idx p => if idx.1 = 1 then ⟨cos*p.x - sin*p.y, sin*p.x + cos*p.y⟩ else p), t := s.t + Δt}

def N := 6

open Server RequestM in
@[server_rpc_method]
def updatePhysics (params : UpdatePhysicsParams (State N)) : RequestM (RequestTask (UpdatePhysicsResult (State N))) := do
  let new_t := params.elapsed / 1000 |>.toReal -- convert milliseconds to seconds
  let Δt := (new_t - params.state.t)
  
  let mut state := params.state

  for action in params.actions do
      match fromJson? (α := Nat) action.value with
      | .error _ => continue
      | .ok idx => continue

  state := state.update Δt

  return RequestTask.pure $ {
    html := <div>
      <div>
        {state.toSvg.toHtml}
      </div>

      {toString params.elapsed} {toString <| toJson <| params.actions}</div>,
    state := state,
    callbackTime := some 33,
  }

@[widget]
def physics : UserWidgetDefinition where
  name := "Magic physics demo"
  javascript := include_str ".." / "lean_packages" / "widgetKit" / "widget" / "dist" / "physics.js"

def init : UpdatePhysicsResult (State N) := {
  html := <div>Init!!</div>,
  state := State.init N }

#widget physics (toJson init)
