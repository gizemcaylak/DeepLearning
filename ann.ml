(* File ann.ml *)
open Str
(* Activation functions and derivatives *)
let sigmoid z = 1.0/.(1.0+.exp(-.z));;
let derivative_sigmoid s = (s*.(1.0 -. s));;
let relu z = max 0. z;;
let derivative_relu s = if s <= 0. then 0. else 1.;;

(* take square of x *)
let square x = x *. x;;

(* Define Neural Network structures: neuron, layer, network *)
type neuron = {
		mutable output : float; 
		mutable weights : float array;
		weight_size : int; 
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

(* Sum the float element of an array *)
let sum l = Array.fold_left (+.) 0. l;;

(* Create n by m matrix that have elements equal to init *)
let matrix n m init =
	let result = Array.make n (Array.make m init) in
	for i = 1 to n - 1 do
	  result.(i) <- Array.make m init
	done;
	result;;

(* replace an element in a list 
 * l 	: list
 * pos 	: position to be replaced
 * a 	: new element which would replace old element
 * Return new list
 *)
let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;

(* shuffle lists a and b  *)
let knuth_shuffle a b =
	let n = Array.length a in
	for i = n - 1 downto 1 do
		let k = Random.int (i+1) in
		let x = a.(k) in
		a.(k) <- a.(i);
		a.(i) <- x;
		let y = b.(k) in
		b.(k) <- b.(i);
		b.(i) <- y;
	done;;

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

let parse_line line delimeter = List.map float_of_string (Str.split_delim delimeter line);;

(* Parse the rows of the data based on the delimeter
 * data 		: list of strings where each string consists of equal number of floating numbers 
 				   and the last number of each string is label
 * delimeter	: field delimeter in the sequence of floating numbers for each string in data
 * Return features and label data
 *)
let process_file data delimeter = 
	let tmp = parse_line (List.hd data) delimeter in
	let data_mat = Array.make (List.length data) (Array.make (List.length tmp) 0.) in
	let labels = Array.make (List.length data) 0 in
	List.iteri ( fun id row ->
		let nums = parse_line row delimeter in
		labels.(id) <- (int_of_float (List.nth nums ((List.length nums)-1)));
		data_mat.(id) <- Array.of_list (List.rev (List.tl (List.rev nums)));
	) data; 
	(data_mat, labels);;

(* 
 * Split data into random train and test subsets
 * features		: array of float array
 * labels 		: float array
 * test_ratio 	: should be between 0.0 and 1.0 and 
 				represent the proportion of the dataset to include in the test split. 
 *)
let test_train_split features labels test_ratio =
	knuth_shuffle labels features;
	let sample_size = (Array.length labels) in
	let test_size = int_of_float ((float_of_int sample_size) *. test_ratio) in
	let train_size = (sample_size - test_size) in
	let train_data = (Array.sub features 0 train_size, Array.sub labels 0 train_size) in
	let test_data = (Array.sub features (train_size-1) test_size, Array.sub labels (train_size-1) test_size) in
	(train_data, test_data);;


(* calculate the mean and standard deviation per column of data 
 * data = list of list of float
 * Return tuple: (list of float:mean for each column, list of float:standard deviation for each column)
 *)
let get_statistics data =
	let length = float_of_int (Array.length data) in
	let mean = Array.make (Array.length (data.(0))) 0. in
	let sd = Array.make (Array.length (data.(0))) 0. in

	for col = 0 to ((Array.length(data.(0)))-1) do
		(* calculate the mean per column *)
		let curr_mean = ref 0. in
		Array.iter ( fun row ->
			curr_mean := !curr_mean +. (row.(col));
		)data;
		curr_mean := !curr_mean /.length;

		(* calculate the std per column *)
		let curr_std = ref 0. in
		Array.iter ( fun row ->
			curr_std := !curr_std +. ((!curr_mean -. (row.(col)))*.(!curr_mean -. (row.(col))));
		) data;
		curr_std := !curr_std /.length;
		curr_std := sqrt !curr_std;

		mean.(col) <- !curr_mean;
		sd.(col) <- !curr_std;
	done;

	(mean, sd);;

(* normalize data based on the standard score: ((X-mean)/deviance) 
 * data 	: list of list of float
 * mean 	: list of float (means for each column of data)
 * sd 		: list of float (sdev for each column of data)
 * Return list of list of float (normalized data)
 *)
let normalize data mean sd =
	let normalized_data = Array.copy data in
	Array.iteri (fun id row ->
		let sample =  (Array.make (Array.length data.(0)) 0.) in
		Array.iteri (fun idx r->
			if (sd.(idx)) = 0. then begin
				sample.(idx) <- (r-.( mean.(idx)));
			end
			else begin
				sample.(idx) <- (((r-.(mean.(idx)))/.(sd.(idx))));
			end
		)row;
		normalized_data.(id) <- sample;
	)data;
	normalized_data;;

(* create a neuron with specified id 
 * neuron_id : integer id (unique id of neuron)
 *)
let create_neuron neuron_id weights_size = {
	output = 0.; 
	weights = Array.make weights_size 0.; 
	weight_size = weights_size;
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
				Array.iter (Printf.printf "  %f\n") neurons.weights) neurons;) layers;;
	
(* Initializes neuron weights randomly *)
let randomly_initialize_weights neuron_id layer prev_layer_size =
	Random.self_init();
	let neuron = (get_neuron neuron_id layer) in
	let counter = ref 0 in
	let rec random_append counter =
		let random_n = (Random.float 2.)-. 1. in (* Random number between -1 and 1 *)
		neuron.weights.(!counter) <- random_n;
		counter := !counter + 1;
		if !counter < prev_layer_size then random_append counter in
	random_append counter;;

(* Add newly created neuron to the specified layer *)
let add_neuron layer prev_layer_size = 
	let id = layer.next_neuron_id in
	Hashtbl.add layer.neurons id (create_neuron id prev_layer_size);
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
	let first_layer = (create_layer (List.hd neuron_no_per_layer) 0) in
	(* weight size: input_no + 1 bias term*)
	add_multiple_neuron (List.hd neuron_no_per_layer) first_layer (input_dim+1);
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
 * weights 	: an array of float numbers
 * input 	: a list of float numbers
 * return activation result, float number
 *)
let activate weights weight_size input =
	let activation = ref weights.(weight_size-1) in (* activation = bias *)
	List.iteri (fun id i ->
		activation := !activation +. ((weights.(id)) *. i);
	) !input;	
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
	Array.iter (fun i ->
		next_inputs := i :: !next_inputs;
	) input;
	next_inputs := (List.rev !next_inputs);
	let outputs = ref [] in
	(* Propagate the input layer by layer *)
	for i = 0 to ((Hashtbl.length ann.layers)-1) do
		let curr_l = (get_layer i ann) in
		(* Calculate the output for each layer *)
		for j = 0 to ((Hashtbl.length curr_l.neurons)-1) do
			let curr_n = (get_neuron j curr_l) in
			let activation =  ref (activate curr_n.weights (curr_n.weight_size) next_inputs) in
			if activation_func = 0 then 
				activation := (sigmoid !activation)
			else
				activation := (relu !activation);
			curr_n.output <- !activation;
			outputs := (!activation) :: !outputs;
		done;
		next_inputs := [];
		List.iter ( fun o ->
			next_inputs := o :: !next_inputs;
		) !outputs;	
		outputs := [];
	done;
	next_inputs;;

(*  
 * calculate a gradient that is needed in the calculation of the weights to be used in the network
 * ann 		: network
 * expected	: a list of floats, indicates the probability of having each class label.
 *)
let backpropagate ann expected activation_func =
	(* backpropagate output layer *)
	let output_l = (get_layer ((Hashtbl.length ann.layers)-1) ann) in
	
	let derivative = ref derivative_relu in
	if activation_func = 0 then derivative := derivative_sigmoid;
	 
	List.iteri ( fun id e ->
		let n = get_neuron id output_l in
		let diff = ((n.output)-. e) in
		n.prev_delta <- n.delta; 
		n.delta <- (diff *. (!derivative n.output));
	) expected;

	let err = ref 0. in
	(* backpropagate other layers *)
	for i = ((Hashtbl.length ann.layers)-2) downto 0 do
		let curr_l = (get_layer i ann) in
		let next_l = (get_layer (i+1) ann) in
		for j = 0 to ((Hashtbl.length curr_l.neurons)-1) do
			err := 0.;
			Hashtbl.iter (fun id nln ->
				err := !err +. ((nln.delta) *.(nln.weights.(j)));
			)(next_l.neurons);
			let curr_n = (get_neuron j curr_l) in
			curr_n.prev_delta <- curr_n.delta; (* add moment: keep one previous delta*)
			curr_n.delta <- (!err *. (!derivative curr_n.output));
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
	Array.iter (fun i ->
		tmp := i :: !tmp;
	)input;
	tmp := (List.rev !tmp);

	(* update weights for first layer *)
	let first_l = (get_layer 0 ann) in
	let neurons = first_l.neurons in
	Hashtbl.iter ( fun id n ->
		List.iteri (fun id t->	
			n.weights.(id) <- (n.weights.(id)) -. (learning_rate *. t *. (((1.-.alpha) *. n.delta)+.(alpha*.n.prev_delta))) ;
		) !tmp;
		n.weights.((n.weight_size)-1) <- (n.weights.((n.weight_size)-1)) -. (learning_rate *. (((1.-.alpha) *. n.delta)+.(alpha *. n.prev_delta)));
	) neurons;
	
	(* Update weigths for other hidden layers *)
	for i = 1 to ((Hashtbl.length ann.layers)-1) do
		tmp := [];
		let curr_l = (get_layer i ann) in
		let prev_l = (get_layer (i-1) ann) in
		for j = 0 to ((Hashtbl.length prev_l.neurons)-1) do
				let curr_n = (get_neuron j prev_l) in
				tmp := (curr_n.output) :: !tmp;
		done;
		tmp := (List.rev !tmp);
		(* calculate new weigths based on update formula *)
		Hashtbl.iter (fun id curr_n ->
			List.iteri (fun id t->			
				curr_n.weights.(id) <- (curr_n.weights.(id)) -. (learning_rate *. t *. (((1.-.alpha) *. curr_n.delta)+.(alpha*.curr_n.prev_delta))) ;
			) !tmp ;		
			curr_n.weights.((curr_n.weight_size)-1) <- (curr_n.weights.((curr_n.weight_size)-1)) -. (learning_rate *. (((1.-.alpha) *. curr_n.delta)+.(alpha *. curr_n.prev_delta)));
		) (curr_l.neurons);
	done;;

(* 
 * Train the network
 * ann 				: network
 * train_data 		: a list of list of float
 * labels 			: an array of float indicates class labels for each sample
 * epochs 			: the number of iterations
 * learning_rate	: a float number that determines how big the step for each update of parameters.
 * labels_no  		: the number of unique labels
 * lr_method		: 0 = learning rate is fixed for all epochs
 				  	  1 = learning rate is decreasing for every sqrt(sample size) epochs
 * alpha 			: a float number that determines what fraction of the previous weight updates are included into the learning rule
 * activation_func 	: 0 = sigmoid function
 					  1 = relu function
 *)
let train_network ann train_data labels epochs learning_rate labels_no lr_method alpha activation_func =
	let total_err = ref 0. in
	let outputs = ref [] in
	let sample_size = float_of_int(Array.length train_data) in
	for i = 1 to epochs do
		total_err := 0.;
		let expected = ref [] in
		Array.iteri (fun id label->
			let sample = train_data.(id) in
			outputs := !(forward_propagate ann sample activation_func);
			expected := [];
			for k = 0 to labels_no-1 do
				if k = label then
					expected := 1. :: !expected
				else
					expected := 0. :: !expected;
			done;

			expected := (List.rev !expected);
			List.iter2 (fun o e ->
				total_err := !total_err +. (square (o-.e));
			)!outputs !expected;

			backpropagate ann !expected activation_func;
			update_weights ann sample !learning_rate alpha;
		) labels;

		if lr_method = 1 then begin
			if i mod int_of_float((sqrt sample_size)) = 0 then learning_rate := !learning_rate /. 1.5;
		end;
		Printf.printf "Epoch = %d\t Lrate = %f\t Error = %f" i !learning_rate !total_err;
		print_newline ();
	done;;

(* Predict the label of a sample after training
 * ann 	 : network
 * input : list of float numbers -> sample
 * Returns the label
 *)
let predict ann input activation_func=
	let outputs = !(forward_propagate ann input activation_func) in
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

(* 
 * Creates confusion matrix where columns are true values, rows are predicted values
 * Returns labels_no by labels_no array matrix
 *)
let create_confusion_matrix ann data labels labels_no activation_func =
	let confusion_matrix = matrix labels_no labels_no 0. in

	Array.iteri (fun id label ->
		let sample = data.(id) in
		let prediction = (predict ann sample activation_func) in
		confusion_matrix.(label).(prediction) <- (confusion_matrix.(label).(prediction))+.1.;
	) labels;
	confusion_matrix;;