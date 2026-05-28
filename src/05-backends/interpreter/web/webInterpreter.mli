module SyntaxHighlight = SyntaxHighlight

module Make (ResourceGrade : Language.ResourceGrade.Grade) :
  WebBackend.S with module ResourceGrade = ResourceGrade
