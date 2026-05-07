module Make (Resource : Language.Resource.S) :
  WebBackend.S with module Resource = Resource
