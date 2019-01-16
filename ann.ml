(* File ann.ml *)

(* Activation functions and derivatives *)
let sigmoid z = 1.0/.(1.0+.exp(-.z));;
let derivative_sigmoid s = s*.(1.0 -. s);;
let relu z = max 0. z;;
let derivative_relu s = if s <= 0 then 0 else 1;;

(* take square of x *)
let square x = x *. x;;

(* Define Neural Network structures: neuron, layer, network *)
type neuron = {
		mutable output : float; 
		mutable weights : float list; 
		mutable prev_delta: float;
		mutable delta: float;
		mutable neuron_id: int
	};;
type layer = { 
		neurons: (int, neuron) Hashtbl.t; 
		mutable next_neuron_id : int;
		mutable layer_id : int;
	};;
type network = { 
		layers : (int, layer) Hashtbl.t;
		mutable next_layer_id : int
	};;


(* append a float element to the list *)
let append_el el f_list = f_list@[el];;

let read_file filename = 
	let lines = ref [] in
	let chan = open_in filename in
	try
	  while true; do
	    lines := input_line chan :: !lines
	  done; !lines
	with End_of_file ->
	  close_in chan;
	  List.rev !lines ;;

(* Parse the rows of the data based on the delimeter *)
let process_file data delimeter = 
	let p_data = ref [] in
	let labels = ref [] in
	let rec convert_to_nums data = 
		if data = [] then ()
		else
			let row = (List.nth data 0) in
			let splitted = (String.split_on_char delimeter row) in
			let nums = (List.map float_of_string splitted) in
			labels := append_el (int_of_float (List.nth nums ((List.length nums)-1))) !labels;
			p_data := append_el (List.rev (List.tl (List.rev nums))) !p_data;
			convert_to_nums (List.tl data) in
	convert_to_nums data;
	(!p_data, !labels);;

let getStatistics data =
	let mean = ref [] in
	let sd = ref [] in

	List.iter ( fun r ->
		mean := append_el r !mean;
	) (List.hd data);
	let curr_mean = ref 0. in
	let curr_sd = ref 0. in
	let counter := 
		List.iter ( fun r->
			
		) (List.tl data) 
	)mean;
	let rec calc cdr =
		List.iter ()
		 mean := append_el (List.hd row) !mean;
		calc List.tl row

let normalizeData data mean sd =
(* create a neuron with specified id *)
let create_neuron neuron_id = {
	output = 0.; 
	weights = []; 
	prev_delta = 0.; 
	delta = 0.; 
	neuron_id = neuron_id
};;

(* create a layer with specified id with mutable look up table that has initial size neuron_no*)
let create_layer neuron_no layer_id = {
	neurons = Hashtbl.create neuron_no; 
	next_neuron_id = 0; 
	layer_id = layer_id
};;

(* create a network with specified id with mutable look up table that has initial size layer_no*)
let create_network layer_no = {
	layers = Hashtbl.create layer_no; 
	next_layer_id = 0
};;

(*  returns the neuron with specified id with in specified layer *)
let get_neuron neuron_id layer = Hashtbl.find layer.neurons neuron_id;;

(*  returns the layer with specified id with in specified network *)
let get_layer layer_id ann = Hashtbl.find ann.layers layer_id;;

(* Print the network layer by layer*)
let print_network ann = 
	let layers = ann.layers in
	Hashtbl.iter (fun id layers -> Printf.printf "Layer_%d:\n Weights:\n" id; let neurons = layers.neurons in 
		Hashtbl.iter (fun id neurons -> Printf.printf "Neuron_%d:\n Output: %f\n Delta: %f\n" id neurons.output neurons.delta; 
				List.iter (Printf.printf "  %f\n") neurons.weights) neurons;) layers;;
	

(* Initializes neuron weights randomly *)
let randomly_initialize_weights neuron_id layer prev_layer_size =
	Random.self_init();
	let neuron = (get_neuron neuron_id layer) in
	let counter = ref 0 in
	let rec random_append counter =
		let random_n = (Random.float 2.)-. 1. in
		neuron.weights <- append_el random_n neuron.weights;
		counter := !counter + 1;
		if !counter < prev_layer_size then random_append counter in
	random_append counter;;

(* Add newly created neuron to the specified layer *)
let add_neuron layer prev_layer_size = 
	let id = layer.next_neuron_id in
	Hashtbl.add layer.neurons id (create_neuron id);
	randomly_initialize_weights id layer prev_layer_size;
	layer.next_neuron_id <- layer.next_neuron_id + 1;
	id;;

(* Add multiple neurons to the specified layer *)
let add_multiple_neuron neuron_no layer prev_layer_size =
	let counter = ref 0 in
	let rec add_neurons counter =
		ignore(add_neuron layer prev_layer_size);
		counter := !counter + 1;
		if !counter < neuron_no then add_neurons counter in
	add_neurons counter;;	
	

(* Add newly created layer to the network *)
let add_layer layer ann = 
	let id = ann.next_layer_id in
	Hashtbl.add ann.layers id layer;
	ann.next_layer_id <- ann.next_layer_id + 1;
	id;;

(* 
 * Creates the network with specified properties such as number of layers and neurons. 
 * weight_initialization_choice : 0 = random_init, 1 = pretraining?
 * layer_no: the number of hidden layers
 * input_dim: the dimension of the input
 * output_dim: the number of the labels, the neurons in the output layer
 * neuron_no_per_layer: a list that contains the number of neurons in each hidden layer
 *)
let initialize_network hidden_layer_no neuron_no_per_layer input_dim output_dim =
	
	(* allocate hash storage for hidden layers (hidden layers + 1 output layer) *)
	let ann = create_network (hidden_layer_no + 1) in

	(* create the first hidden layer *)
	let first_layer = (create_layer (List.nth neuron_no_per_layer 0) 0) in
	(* weight size: input_no + 1 bias term*)
	add_multiple_neuron (List.nth neuron_no_per_layer 0) first_layer (input_dim+1);
	ignore(add_layer first_layer ann);

	(* create other hidden layers *)
	for i = 1 to hidden_layer_no-1 do
		let h_layer = (create_layer (List.nth neuron_no_per_layer i) i) in
		add_multiple_neuron (List.nth neuron_no_per_layer i) h_layer ((List.nth neuron_no_per_layer (i-1))+1);
		ignore(add_layer h_layer ann);
	done;

	(* create last layer:output layer *)
	let output_layer = (create_layer output_dim hidden_layer_no) in
	add_multiple_neuron output_dim output_layer ((List.nth neuron_no_per_layer (hidden_layer_no-1))+1);
	ignore(add_layer output_layer ann);
	ann;;

(* calculate "weighted sum" of inputs added a bias 
 *
 *
 *)
let activate weights input =
	let activation = ref (List.hd (List.rev weights)) in (* activation = bias *)
	let rev_weights = (List.rev (List.tl (List.rev weights))) in
	List.iter2 (fun w i ->
		activation := !activation +. (w *. i);
	) rev_weights !input;	
	!activation;;

(* 
 * ann: network
 * input: 
 * activation_func
 *)
let forward_propagate ann input activation_func =
	let next_inputs = ref [] in
	List.iter (fun i ->
		next_inputs := append_el i !next_inputs;
	) input;

	let outputs = ref [] in
	for i = 0 to ((Hashtbl.length ann.layers)-1) do
		let curr_l = (get_layer i ann) in
		for j = 0 to ((Hashtbl.length curr_l.neurons)-1) do
			let curr_n = (get_neuron j curr_l) in
			let activation =  ref (activate curr_n.weights next_inputs) in
			if activation_func = 0 then 
				activation := (sigmoid !activation)
			else
				activation := (relu !activation);
			curr_n.output <- !activation;
			outputs := append_el (!activation) !outputs;
		done;
		next_inputs := [];
		List.iter ( fun o ->
			next_inputs := append_el o !next_inputs;
		) !outputs;	
		outputs := [];
	done;
	next_inputs;;

(*  
 * Calculate a gradient that is needed in the calculation of the weights to be used in the network
 * ann: network
 * expected: 
 *)
let backpropagate ann expected =
	(* backpropagate output layer *)
	let output_l = (get_layer ((Hashtbl.length ann.layers)-1) ann) in
	
	let neurons = output_l.neurons in

	Hashtbl.iter ( fun id n ->
		let diff = ((n.output)-. (List.nth expected id)) in
		n.prev_delta <- n.delta; 
		n.delta <- (diff *. (derivative_sigmoid n.output));
	) neurons;

	let err = ref 0. in
	(* backpropagate other layers *)
	for i = ((Hashtbl.length ann.layers)-2) downto 0 do
		let curr_l = (get_layer i ann) in
		let next_l = (get_layer (i+1) ann) in
		for j = 0 to ((Hashtbl.length curr_l.neurons)-1) do
			err := 0.;
			Hashtbl.iter (fun id nln ->
				err := !err +. ((nln.delta) *.(List.nth (nln.weights) j));
			)(next_l.neurons);
			let curr_n = (get_neuron j curr_l) in
			curr_n.prev_delta <- curr_n.delta; (* add moment: keep one previous delta*)
			curr_n.delta <- !err *. (derivative_sigmoid curr_n.output);
		done;
	done;;


let update_weights ann input learning_rate alpha =
	let tmp = ref [] in
	tmp := List.append input !tmp;
	
	(* update weights for first layer *)
	let first_l = (get_layer 0 ann) in
	let neurons = first_l.neurons in
	Hashtbl.iter ( fun id n ->
		let weights = ref [] in
		for i = 0 to (List.length !tmp)-1 do
			weights := append_el ((List.nth n.weights i) -. (learning_rate *. (List.nth !tmp i) *. ((1.-.alpha) *. n.delta+.alpha*.n.prev_delta))) !weights;
		done;
		weights := append_el ((List.nth n.weights (List.length !tmp)) -. (learning_rate *. ((1.-.alpha) *. n.delta+.alpha *. n.prev_delta))) !weights;
		n.weights <- !weights;
	) neurons;
	
	for i = 1 to ((Hashtbl.length ann.layers)-1) do
		tmp := [];
		let curr_l = (get_layer i ann) in
		let prev_l = (get_layer (i-1) ann) in
		for j = 0 to ((Hashtbl.length prev_l.neurons)-1) do
				let curr_n = (get_neuron j prev_l) in
				tmp := append_el (curr_n.output) !tmp;
		done;
		Hashtbl.iter (fun id curr_n ->
			let weights = ref [] in
			List.iter2 (fun t w->			
				weights := append_el (w -. (learning_rate *.t *. ((1.-.alpha) *. curr_n.delta +. alpha*.curr_n.prev_delta))) !weights;
			) !tmp (List.rev (List.tl (List.rev (curr_n.weights))));		
			weights := append_el ((List.nth curr_n.weights (List.length !tmp)) -. (learning_rate *. ((1.-.alpha) *. curr_n.delta +. alpha *. curr_n.prev_delta))) !weights;
			curr_n.weights <- !weights;
		) (curr_l.neurons);
	done;;

(* 
 *
 * labels_no : number of classes
 *)
let trainNetwork ann train_data labels epochs learning_rate labels_no lr_method alpha =
	let total_err = ref 0. in
	let outputs = ref [] in
	for i = 1 to epochs do
		total_err := 0.;
		let expected = ref [] in
		let counter = ref 0 in
		List.iter2 (fun sample label->
			outputs := !(forward_propagate ann sample 0);
			expected := [];
			for k = 0 to labels_no-1 do
				if k = label then
					expected := append_el 1. !expected
				else
					expected := append_el 0. !expected;

				total_err := !total_err +. (square ((List.nth !outputs k)-.(List.nth !expected k)));
			done;
			counter := !counter+1;
			backpropagate ann !expected;
			update_weights ann sample !learning_rate alpha;
		) train_data labels;

		if lr_method = 1 then begin
			if i mod 300 = 0 then learning_rate := !learning_rate /. 1.5;
		end;
		Printf.printf "Epoch = %d\t Lrate = %f\t Error = %f" i !learning_rate !total_err;
		print_newline ();
	done;;


let predict ann input =
	let outputs = !(forward_propagate ann input 0) in
	let max = ref (List.nth outputs 0) in
	let ind_max = ref 0 in
	let counter = ref 0 in
	List.iter (fun o ->
		if o >= !max then begin
			max := o;
			ind_max := !counter;
		end;
		counter := !counter + 1;
	) outputs;
	!ind_max;;

let create_test_network2 () =
	let ann = (initialize_network 1 [1] 2 2) in
	 (* create the first hidden layer *)
	let first_layer = (get_layer 0 ann) in
	let weights_0 = [0.4; 0.3; 0.2] in
	(get_neuron 0 first_layer).weights <- weights_0;
	let weights_1 = [0.3; 0.1] in
	let weights_2 = [0.4; 0.3] in

	let sec_layer = (get_layer 1 ann) in
	(get_neuron 0 sec_layer).weights <- weights_1;
	(get_neuron 1 sec_layer).weights <- weights_2;
	ann;;

let create_test_network3 () =
	let ann = (initialize_network 1 [1] 2 2) in
	 (* create the first hidden layer *)
	let first_layer = (get_layer 0 ann) in
	let weights_0 = [0.13436424411240122; 0.8474337369372327; 0.763774618976614] in
	(get_neuron 0 first_layer).weights <- weights_0;
	(get_neuron 0 first_layer).output <-  0.7105668883115941;
	let weights_1 = [0.2550690257394217; 0.49543508709194095] in
	let weights_2 = [0.4494910647887381; 0.651592972722763] in

	let sec_layer = (get_layer 1 ann) in
	(get_neuron 0 sec_layer).output <-  0.6213859615555266;
	(get_neuron 1 sec_layer).output <-  0.6573693455986976;
	(get_neuron 0 sec_layer).weights <- weights_1;
	(get_neuron 1 sec_layer).weights <- weights_2;

	ann;;

let create_test_network () =
	let ann = (initialize_network 1 [2] 2 2) in
	 (* create the first hidden layer *)
	let first_layer = (get_layer 0 ann) in
	let weights_0 = [1.482313569067226;1.8308790073202204;1.078381922048799] in
	(get_neuron 0 first_layer).weights <- weights_0;
	let weights_1 = [0.23244990332399884;0.3621998343835864;0.40289821191094327] in
	(get_neuron 1 first_layer).weights <- weights_1;

	let sec_layer = (get_layer 1 ann) in
	(get_neuron 0 sec_layer).output <- 0.6213859615555266;
	(get_neuron 1 sec_layer).output <- 0.6573693455986976;

	let weights_2 = [2.5001872433501404;0.7887233511355132;-1.1026649757805829] in
	(get_neuron 0 sec_layer).weights <- weights_2;

	let weights_3 = [2.5001872433501404;0.7887233511355132;-1.1026649757805829] in
	(get_neuron 1 sec_layer).weights <- weights_3;

	ann;;
(*

let getStatistics (data, row, col, mean, sd) =

let normalizeData (data, row, col, mean , sd) =

let loadData (filename) = *)

let main () =
	(* {output=5.2; } *)
	(* for i = 0 to Array.length Sys.argv - 1 do
      Printf.printf "[%i] %s\n" i Sys.argv.(i)
    done;; *)

	(* let ann = create_test_network3 () in *)
	(* print_network ann; *)
	let train_filepath = "./DATASET/Iris/train.txt" in
	let m_train_data = (read_file train_filepath) in
	let train_data = (process_file m_train_data ',') in
	let test_filepath = "./DATASET/Iris/test.txt" in
	print_string "Data load is started" ;
	print_newline ();
	let m_test_data = (read_file test_filepath) in
	print_string "Data load is complete" ;
	print_newline ();
	print_string "Data process is started" ;
	print_newline ();
	let test_data = (process_file m_test_data ',') in
	print_string "Data process is completed" ;
	print_newline ();
	let label_no = (List.length (List.sort_uniq compare (snd train_data))) in
	let input_dim = (List.length (List.nth (fst train_data) 0)) in
	let learning_rate = ref 0.5 in
	let hidden_layer_no = 1 in
	let neuron_no_per_layer = [6] in
	let epochs = 500 in
	let ann = (initialize_network hidden_layer_no neuron_no_per_layer input_dim label_no) in
	Printf.printf "Input dimension: %d " input_dim;
	print_newline ();
	Printf.printf "The number of unique labels: %d " label_no;
	print_newline ();
	Printf.printf "Learning rate: %f " !learning_rate;
	print_newline ();

	trainNetwork ann (fst train_data) (snd train_data) epochs learning_rate label_no 1 0.7;
	(* Prediction *)
	(* let n = label_no in *)
	(* let confMat = Array.make_matrix n n 0 in *)
	let accuracy = ref 0. in
	List.iter2 (fun sample label ->
		let prediction = (predict ann sample) in
		if label = prediction then accuracy := !accuracy +. 1.;
	) (fst train_data) (snd train_data);
	accuracy := !accuracy /. (float_of_int (List.length (snd train_data)));
	Printf.printf "Train accuracy: %f \n" !accuracy;

	accuracy := 0.;
	List.iter2 (fun sample label->
		let prediction = (predict ann sample) in
		if label = prediction then accuracy := !accuracy +. 1.;
	) (fst test_data) (snd test_data);
	accuracy := !accuracy /. (float_of_int(List.length (snd test_data)));
	Printf.printf "Test accuracy: %f \n" !accuracy;
	(* Printf.printf "hello2";
	let x = [[2.7810836;2.550537003];[1.465489372;2.362125076];[3.396561688;4.400293529];[1.38807019;1.850220317];[3.06407232;3.005305973];[7.627531214;2.759262235];[5.332441248;2.088626775];[6.922596716;1.77106367];[8.675418651;-0.242068655];[7.673756466;3.508563011]] in
	let labels = [0;0;0;0;0;1;1;1;1;1] in
	let expected = [0.;1.] in *)
	(* print_network ann; *)
	(* print_network ann; *)
	(*List.iter (Printf.printf "  %f\n") !next_inputs; *)

	exit 0;;

main ();;