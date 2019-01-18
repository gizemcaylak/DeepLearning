val sigmoid : float -> float
val derivative_sigmoid : float -> float
val relu : float -> float
val derivative_relu : float -> float
val square : float -> float
type neuron = {
  mutable output : float;
  mutable weights : float array;
  weight_size : int;
  mutable prev_delta : float;
  mutable delta : float;
  mutable neuron_id : int;
}
type layer = {
  neurons : (int, neuron) Hashtbl.t;
  mutable next_neuron_id : int;
  mutable layer_id : int;
}
type network = {
  layers : (int, layer) Hashtbl.t;
  mutable next_layer_id : int;
}
val sum : float array -> float
val matrix : int -> int -> 'a -> 'a array array
val replace : 'a list -> int -> 'a -> 'a list
val knuth_shuffle : 'a array -> 'b array -> unit
val read_file : string -> string list
val parse_line : string -> Str.regexp -> float list
val process_file : string list -> Str.regexp -> float array array * int array
val test_train_split :
  'a array ->
  'b array -> float -> ('a array * 'b array) * ('a array * 'b array)
val get_statistics : float array array -> float array * float array
val normalize :
  float array array -> float array -> float array -> float array array
val create_neuron : int -> int -> neuron
val create_layer : int -> int -> layer
val create_network : int -> network
val get_neuron : int -> layer -> neuron
val get_layer : int -> network -> layer
val print_network : network -> unit
val randomly_initialize_weights : int -> layer -> int -> unit
val add_neuron : layer -> int -> int
val add_multiple_neuron : int -> layer -> int -> unit
val add_layer : layer -> network -> int
val initialize_network : int -> int list -> int -> int -> network
val activate : float array -> int -> float list ref -> float
val forward_propagate : network -> float array -> int -> float list ref
val backpropagate : network -> float list -> int -> unit
val update_weights : network -> float array -> float -> float -> unit
val train_network :
  network ->
  float array array ->
  int array -> int -> float ref -> int -> int -> float -> int -> unit
val predict : network -> float array -> int -> int
val create_confusion_matrix :
  network ->
  float array array -> int array -> int -> int -> float array array
