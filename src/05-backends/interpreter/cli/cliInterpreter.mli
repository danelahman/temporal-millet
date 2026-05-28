module Make (ResourceGrade : Language.ResourceGrade.Grade) :
  CliBackend.S with module ResourceGrade = ResourceGrade
