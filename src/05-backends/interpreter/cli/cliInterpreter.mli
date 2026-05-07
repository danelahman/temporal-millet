module Make (Resource : Language.Resource.S) :
  CliBackend.S with module Resource = Resource
