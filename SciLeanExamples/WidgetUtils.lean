import Lean
import WidgetKit.Html
import WidgetKit.HtmlWidget
import WidgetKit.Svg

open Lean Widget Jsx

inductive ButtonState where
  | pressed
  | released
  deriving ToJson, FromJson, DecidableEq

inductive ActionKind where
  | timeout
  | mousedown
  | mouseup
  | mousemove -- [note] mouse moves only happen when mouse button is down.
  deriving ToJson, FromJson, DecidableEq

structure Action where
  kind : ActionKind
  id : Option String
  deriving ToJson, FromJson

structure UpdatePhysicsParams (State : Type) where
  elapsed : Float
  actions : Array Action
  state : State
  mousePos : Option (Float × Float)
  deriving ToJson, FromJson

structure UpdatePhysicsResult (State : Type) where
  html : Widget.Html
  state : State
  /-- Approximate number of milliseconds to wait before calling again. -/
  callbackTime : Option Float := some 100
  deriving ToJson, FromJson
