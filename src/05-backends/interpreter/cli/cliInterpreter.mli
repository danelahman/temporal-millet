module Make (ResourceGrade : Language.ResourceGrade.S) :
  CliBackend.S with module ResourceGrade = ResourceGrade
