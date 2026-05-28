(** A grade system bundles together a resource grade and (in the future) an
    effect grade plus information connecting the two. The rest of the
    application is parameterised by a grade system rather than by an individual
    resource grade. *)

module type S = sig
  module ResourceGrade : Grade.S
  (* Future extension: a module EffectGrade and a sub-module describing the
     interaction between effect grades and resource grades will live here. *)
end

module TimeLowerBoundSystem : S = struct
  module ResourceGrade = Grade.TimeLowerBoundGrade
end

module TimeUpperBoundSystem : S = struct
  module ResourceGrade = Grade.TimeUpperBoundGrade
end

module IntervalSystem : S = struct
  module ResourceGrade = Grade.IntervalResourceGrade
end

(** All available grade systems, in order of definition. The accepted names
    (used by the [resources] source declaration and by the CLI/Web grade
    selectors) are taken from the [ResourceGrade.name] field of each system. *)
let grade_systems : (string * (module S)) list =
  [
    (TimeLowerBoundSystem.ResourceGrade.name, (module TimeLowerBoundSystem : S));
    (TimeUpperBoundSystem.ResourceGrade.name, (module TimeUpperBoundSystem : S));
    (IntervalSystem.ResourceGrade.name, (module IntervalSystem : S));
  ]
