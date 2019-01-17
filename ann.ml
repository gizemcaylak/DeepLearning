(* File ann.ml *)

(* Activation functions and derivatives *)
let sigmoid z = 1.0/.(1.0+.exp(-.z));;
let derivative_sigmoid s = (s*.(1.0 -. s));;
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


(* append an element to the list 
 * el 		: element to be appended
 * f_list 	: list
 * Return appended list
 *)
let append_el el f_list = f_list@[el];;

(* replace an element in a list 
 * l 	: list
 * pos 	: position to be replaced
 * a 	: new element which would replace old element
 * Return new list
 *)
let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;

(* 
 * reads a file line by line
 * filename	: path of the file to read
 * Return list of lines
 *)
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

(* Parse the rows of the data based on the delimeter 
 * data 		: list of strings where each string consists of equal number of floating numbers 
 				   and the last number of each string is label
 * delimeter	: field delimeter in the sequence of floating numbers for each string in data
 * Return features and label data
 *)
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

(* calculate the mean and standard deviation per column of data 
 * data = list of list of float
 * Return tuple: (list of float:mean for each column, list of float:standard deviation for each column)
 *)
let get_statistics data =
	let length = float_of_int (List.length data) in
	let mean = ref [] in
	let sd = ref [] in

	for col = 0 to ((List.length(List.hd data))-1) do
		(* calculate the mean per column *)
		let curr_mean = ref 0. in
		List.iter ( fun row ->
			curr_mean := !curr_mean +. (List.nth row col);
		)data;
		curr_mean := !curr_mean /.length;

		(* calculate the std per column *)
		let curr_std = ref 0. in
		List.iter ( fun row ->
			curr_std := !curr_std +. ((!curr_mean -. (List.nth row col))*.(!curr_mean -. (List.nth row col)));
		) data;
		curr_std := !curr_std /.length;
		curr_std := sqrt !curr_std;

		mean := append_el !curr_mean !mean;
		sd := append_el !curr_std !sd;
	done;
	(!mean, !sd);;

(* normalize data based on the standard score: ((X-mean)/deviance) 
 * data 	: list of list of float
 * mean 	: list of float (means for each column of data)
 * sd 		: list of float (sdev for each column of data)
 * Return list of list of float (normalized data)
 *)
let normalize data mean sd =
	let normalized_data = ref [] in
	List.iter (fun row ->
		let sample = ref [] in
		List.iteri (fun id r->
			(* Printf.printf "%f %f %f %f\n" r (List.nth mean id) (List.nth sd id) ((r-.(List.nth mean id))/.(List.nth sd id)); *)
			if (List.nth sd id) = 0. then begin
				sample := append_el (r-.((List.nth mean id))) !sample;
			end
			else begin
				sample := append_el (((r-.(List.nth mean id))/.(List.nth sd id))) !sample;
			end
		)row;
		normalized_data := append_el !sample !normalized_data;
	)data;
	!normalized_data;;

(* create a neuron with specified id 
 * neuron_id : integer id (unique id of neuron)
 *)
let create_neuron neuron_id = {
	output = 0.; 
	weights = []; 
	prev_delta = 0.; 
	delta = 0.; 
	neuron_id = neuron_id
};;

(* create a layer with specified id with mutable look up table that has initial size neuron_no
 * neuron_no 	: integer (initial number of neurons to be created)
 * layer_id 	: integer id (unique id of layer_id)
 *)
let create_layer neuron_no layer_id = {
	neurons = Hashtbl.create neuron_no; 
	next_neuron_id = 0; 
	layer_id = layer_id
};;

(* create a network with specified id with mutable look up table that has initial size layer_no
 * layer_no : integer (initial number of layers to be created)
 *)
let create_network layer_no = {
	layers = Hashtbl.create layer_no; 
	next_layer_id = 0
};;

(*  returns the neuron with specified neuron_id with in specified layer *)
let get_neuron neuron_id layer = Hashtbl.find layer.neurons neuron_id;;

(*  returns the layer with specified layer_id with in specified network ann *)
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
		let random_n = (Random.float 2.)-. 1. in (* Random number between -1 and 1 *)
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
 * hidden_layer_no 				: the number of hidden layers
 * neuron_no_per_layer 			: a list that contains the number of neurons in each hidden layer
 * input_dim 					: the dimension of the input
 * output_dim 					: the number of the labels, the neurons in the output layer
 * weight_initialization_choice : 0 = random_init, 1 = pretraining?
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
 * weights 	: a list of float numbers
 * input 	: a list of float numbers
 * return activation result, float number
 *)
let activate weights input =
	let activation = ref (List.hd (List.rev weights)) in (* activation = bias *)
	let rev_weights = (List.rev (List.tl (List.rev weights))) in
	List.iter2 (fun w i ->
		activation := !activation +. (w *. i);
	) rev_weights !input;	
	!activation;;

(* 
 * Propagates input to the hidden units at each layer and finally produce the output
 * ann 				: network
 * input 			: a list of float numbers to be propagated
 * activation_func	: 	0 = sigmoid function
 						1 = relu function
 * Return the final output probabilities
 *)
let forward_propagate ann input activation_func =
	let next_inputs = ref [] in
	List.iter (fun i ->
		next_inputs := append_el i !next_inputs;
	) input;

	let outputs = ref [] in
	(* Propagate the input layer by layer *)
	for i = 0 to ((Hashtbl.length ann.layers)-1) do
		let curr_l = (get_layer i ann) in
		(* Calculate the output for each layer *)
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
 * calculate a gradient that is needed in the calculation of the weights to be used in the network
 * ann 		: network
 * expected	: a list of floats, indicates the probability of having each class label.
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
			curr_n.delta <- (!err *. (derivative_sigmoid curr_n.output));
		done;
	done;;

(*  
 * Update the weights for each sample based on the gradient loss function
 * The momentum is used to increase the stability. 
 * ann 			: network
 * input 		: the list of float (sample)
 * learning_rate: a float number that determines how big the step for each update of parameters.
 * alpha		: a float number that determines what fraction of the previous weight updates are included into the learning rule
 *)
let update_weights ann input learning_rate alpha =
	let tmp = ref [] in
	tmp := List.append input !tmp;
	
	(* update weights for first layer *)
	let first_l = (get_layer 0 ann) in
	let neurons = first_l.neurons in
	Hashtbl.iter ( fun id n ->
		let weights = ref [] in
		List.iter2 (fun t w->	
			weights := append_el (w -. (learning_rate *. t *. (((1.-.alpha) *. n.delta)+.(alpha*.n.prev_delta)))) !weights;
		) !tmp (List.rev (List.tl (List.rev (n.weights))));		
		weights := append_el ((List.nth n.weights (List.length !tmp)) -. (learning_rate *. (((1.-.alpha) *. n.delta)+.(alpha *. n.prev_delta)))) !weights;
		n.weights <- !weights;
	) neurons;
	
	(* Update weigths for other hidden layers *)
	for i = 1 to ((Hashtbl.length ann.layers)-1) do
		tmp := [];
		let curr_l = (get_layer i ann) in
		let prev_l = (get_layer (i-1) ann) in
		for j = 0 to ((Hashtbl.length prev_l.neurons)-1) do
				let curr_n = (get_neuron j prev_l) in
				tmp := append_el (curr_n.output) !tmp;
		done;
		(* calculate new weigths based on update formula *)
		Hashtbl.iter (fun id curr_n ->
			let weights = ref [] in
			List.iter2 (fun t w->			
				weights := append_el (w -. (learning_rate *.t *. (((1.-.alpha) *. (curr_n.delta))+. (alpha*.(curr_n.prev_delta))))) !weights;
			) !tmp (List.rev (List.tl (List.rev (curr_n.weights))));		
			weights := append_el ((List.nth curr_n.weights (List.length !tmp)) -. (learning_rate *. (((1.-.alpha) *. (curr_n.delta)) +. (alpha *. (curr_n.prev_delta))))) !weights;
			curr_n.weights <- !weights;
		) (curr_l.neurons);
	done;;

(* 
 * Train the network
 * ann 			: network
 * train_data 	: a list of list of float
 * labels 		: a list of float indicates class labels for each sample
 * epochs 		: the number of iterations
 * learning_rate: a float number that determines how big the step for each update of parameters.
 * labels_no  	: the number of unique labels
 * lr_method	: 0 = learning rate is fixed for all epochs
 				  1 = learning rate is decreasing for every 150 epochs
 * alpha 		: a float number that determines what fraction of the previous weight updates are included into the learning rule
 *)
let trainNetwork ann train_data labels epochs learning_rate labels_no lr_method alpha =
	let total_err = ref 0. in
	let outputs = ref [] in
	for i = 1 to epochs do
		total_err := 0.;
		let expected = ref [] in
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
			backpropagate ann !expected;
			update_weights ann sample !learning_rate alpha;
		) train_data labels;

		if lr_method = 1 then begin
			if i mod 300 = 0 then learning_rate := !learning_rate /. 1.5;
		end;
		Printf.printf "Epoch = %d\t Lrate = %f\t Error = %f" i !learning_rate !total_err;
		print_newline ();
	done;;

(* Predict the label of a sample after training
 * ann 	 : network
 * input : list of float numbers -> sample
 * Returns the label
 *)
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
	let weights_0 = [0.13436424411240122; 0.8474337369372327; 0.763774618976614] in
	(get_neuron 0 first_layer).weights <- weights_0;
	let weights_1 = [0.2550690257394217; 0.49543508709194095] in
	let weights_2 = [0.4494910647887381; 0.651592972722763] in

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
