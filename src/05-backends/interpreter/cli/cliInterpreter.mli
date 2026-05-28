module Make (GS : Language.GradeSystem.S) :
  CliBackend.S with module GradeSystem = GS
