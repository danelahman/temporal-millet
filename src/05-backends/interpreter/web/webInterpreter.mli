module SyntaxHighlight = SyntaxHighlight

module Make (GS : Language.GradeSystem.S) :
  WebBackend.S with module GradeSystem = GS
