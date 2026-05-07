module Make (ResourceGrade : Language.ResourceGrade.S) :
  WebBackend.S with module ResourceGrade = ResourceGrade
