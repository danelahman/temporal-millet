module type S = sig
  include Backend.S

  val view_step_label : step_label -> 'a Vdom.vdom
  val view_run_state : run_state -> step_label option -> 'a Vdom.vdom
  val is_return_label : step_label -> bool
  val is_done : run_state -> bool
end
