(** A grade system bundles together a resource grade and an effect grade plus a
    grade morphism connecting the two. The rest of the application is
    parameterised by a grade system rather than by an individual grade. *)

module type S = sig
  module ResourceGrade : Grade.S
  module EffectGrade : Grade.S

  val name : string
  val resourceOfEffect : EffectGrade.t -> ResourceGrade.t
end

module TimeLowerBoundSystem : S = struct
  module ResourceGrade = Grade.TimeLowerBoundGrade
  module EffectGrade = Grade.TimeLowerBoundGrade

  let name = "time-lower-bound"
  let resourceOfEffect = fun e -> e
end

module TimeUpperBoundSystem : S = struct
  module ResourceGrade = Grade.TimeUpperBoundGrade
  module EffectGrade = Grade.TimeUpperBoundGrade

  let name = "time-upper-bound"
  let resourceOfEffect = fun e -> e
end

module TimeIntervalSystem : S = struct
  module ResourceGrade = Grade.TimeIntervalGrade
  module EffectGrade = Grade.TimeIntervalGrade

  let name = "time-interval"
  let resourceOfEffect = fun e -> e
end

(** List of all available grade systems, in order of definition. The accepted
    names (used by the [grades] source declaration and by the CLI/Web grade
    selectors) are taken from the [S.name] field of each system. *)
let grade_systems : (string * (module S)) list =
  [
    (TimeLowerBoundSystem.name, (module TimeLowerBoundSystem : S));
    (TimeUpperBoundSystem.name, (module TimeUpperBoundSystem : S));
    (TimeIntervalSystem.name, (module TimeIntervalSystem : S));
  ]
