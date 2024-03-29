module Shared.Void exposing (Void , absurd)

type Void
  = Void Void

absurd : Void -> a
absurd (Void v) =
  absurd v
