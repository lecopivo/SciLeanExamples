import Lean
import WidgetKit.Html
import WidgetKit.HtmlWidget
import WidgetKit.Svg

open Lean Widget Jsx


inductive ActionKind where
  | timeout
  | click
  deriving ToJson, FromJson, DecidableEq

structure Action where
  -- can be 'timeout' or 'click'
  kind : ActionKind
  value : Json
  deriving ToJson, FromJson



structure UpdatePhysicsParams (State : Type) where
  elapsed : Float
  actions : Array Action
  state : State
  deriving ToJson, FromJson

structure UpdatePhysicsResult (State : Type) where
  html : Widget.Html
  state : State
  /-- Approximate number of milliseconds to wait before calling again. -/
  callbackTime : Option Float := some 100
  deriving ToJson, FromJson

