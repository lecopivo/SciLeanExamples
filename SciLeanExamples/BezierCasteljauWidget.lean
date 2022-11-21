import WidgetKit.InteractiveSvg

import SciLean.Data.DataArray
import SciLean.Data.Bezier
import SciLean.Data.Curve
import SciLean.Data.FunRec

open Lean Widget Jsx
open SciLean

structure State (n : Nat) where
  x : (ℝ^{2})^{n}
  deriving ToJson, FromJson

def casteljauElements {f} (pts : (ℝ^{2})^{n}) (t : ℝ) : Array (Svg.Element f) := step pts #[]
  where
    step {n' : Nat} (pts : (ℝ^{2})^{n'}) (elems : Array (Svg.Element f)) : Array (Svg.Element f) :=
    match n' with
    | 0 => elems
    | _+1 =>
      let elems := elems
        |>.push
          { shape := .polyline (pts.toArray.map λ p => (p.x.toFloat, p.y.toFloat)),
            strokeWidth := some (.px 1)
            strokeColor := some ⟨0.4,0.4,0.4⟩ }
        |>.append
          (pts.toArray.map (λ p => 
           { shape := .circle (p.x.toFloat, p.y.toFloat) (.px 2),
             strokeWidth := some (.px 2)
             strokeColor := some ⟨0.7,0.7,0.7⟩ }))
      let t' := t
      step (pts.linearInterpolate t') elems

def N := 4

def isvg : Svg.InteractiveSvg (State N) where
  init := { x := λ [i] => let θ := 2*Math.pi * i.1.toReal/N.toReal; ⟨Math.cos θ, Math.sin θ⟩}

  frame :=
    { xmin := -2
      ymin := -2
      xSize := 4
      width := 400
      height := 400 }

  update time Δt action mouseStart mouseEnd selected getData state :=
    match getData Nat, mouseEnd with
    | some id, some p => {state with x := state.x.set ⟨id,sorry⟩ ⟨p.toAbsolute.1.toReal, p.toAbsolute.2.toReal⟩}
    | _, _ => state

  render time mouseStart mouseEnd state :=
    {
      elements := 
        let w := (time/1000).toReal |> λ x => 0.1*x |>  λ x => (x - Math.floor x)
        let casteljauelements := casteljauElements state.x w
        let curvePoints := Curve.sampleAdaptive (Bezier.eval state.x) (by simp; apply AutoImpl.finish) 0 1 20 100 sorry
        let controlpoints : Array (Svg.Element _) := (state.x.toArray.mapIdx fun idx p =>
              Svg.circle (p.x.toFloat, p.y.toFloat) (.px 5) |>.setFill (0.7,0.7,0.7) |>.setId s!"circle{idx}" |>.setData idx.1
            )
        casteljauelements 
          |>.push ((Svg.polyline (curvePoints.map (λ p => (p.x.toFloat, p.y.toFloat)))).setStroke (1.,1.,1.) (.px 1))
          |>.append controlpoints
    }

open Svg

open Server RequestM in
@[server_rpc_method]
def updateSvg (params : UpdateParams (State N)) : RequestM (RequestTask (UpdateResult (State N))) := isvg.serverRpcMethod params

@[widget]
def svgWidget : UserWidgetDefinition where
  name := "Interactive SVG"
  javascript := include_str ".." / "lean_packages" / "widgetKit" / "widget" / "dist" / "interactiveSvg.js"


def init : UpdateResult (State N) := {
  html := <div>Init!!!</div>,
  state := { state := isvg.init
             time := 0
             selected := none
             mousePos := none
             idToData := isvg.render 0 none none isvg.init |>.idToDataList}
}

#widget svgWidget (toJson init)

