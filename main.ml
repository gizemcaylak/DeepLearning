open Ann
open Str
(* 
 * data_filepath		: Sys.argv.(1) 	: string
 * delimeter 			: Sys.argv.(2) 	: char
 * test_ratio			: Sys.argv.(3)	: float between 0 and 1
 * learning_rate 		: Sys.argv.(4) 	: float
 * learning_rate_method	: Sys.argv.(5)	: 0 or 1 
 * alpha 				: Sys.argv.(6)	: float between 0 and 1
 * epochs 				: Sys.argv.(7)	: positive integer
 * activation_func 		: Sys.argv.(8) 	: 0 or 1 
 * hidden_layer_no 		: Sys.argv.(9) 	: positive integer
 * neuron_no_per_layer 	: Sys.argv.(10) to  Sys.argv.(9+hidden_layer_no)
 *)

let main () =
	for i = 0 to Array.length Sys.argv - 1 do
      Printf.printf "[%i] %s\n" i Sys.argv.(i);
    done;;
	(* Read train files *)
	let data_filepath = Sys.argv.(1) in
	let delimeter = (Str.regexp Sys.argv.(2)) in
	let test_ratio = (float_of_string((Sys.argv.(3)))) in

	(* Read test files *)
  	Printf.printf "Data file path %s\n" data_filepath;
	print_string "Data load is started" ;
	print_newline ();

	let m_data = (Ann.read_file data_filepath) in
	print_string "Data load is complete" ;
	print_newline ();

	print_string "Data process is started" ;
	print_newline ();
	let p_data = (Ann.process_file m_data delimeter) in
	print_string "Data process is completed" ;
	print_newline ();


	print_string "Splitting data as train and test" ;
	print_newline ();
	let result = Ann.test_train_split (fst p_data) (snd p_data) test_ratio in
	print_string "Split completed" ;
	print_newline ();

	let train_features = (fst (fst result)) in
	let train_labels = (snd (fst result)) in
	let test_features = (fst (snd result)) in
	let test_labels = (snd (snd result)) in

	(* Get the statistics, mean and standard deviation, for data *)
	print_string "Calculating mean and standard deviation for training data.." ;
	print_newline ();
	let stats = (Ann.get_statistics train_features) in
	print_string "Mean and standard deviation are calculated" ;
	print_newline ();
	let mean = (fst stats) in
	let std = (snd stats) in
	print_string "Normalization is started" ;
	print_newline ();
	let normalized_train = Ann.normalize train_features mean std in
	let normalized_test = Ann.normalize test_features mean std in
	print_string "Normalization is completed" ;
	print_newline ();

	let label_no = (List.length (List.sort_uniq compare (Array.to_list train_labels))) in
	let input_dim = (Array.length (normalized_train.(0))) in
	let learning_rate = ref (float_of_string((Sys.argv.(4)))) in
	let learning_rate_method = int_of_string(Sys.argv.(5)) in
	let alpha = float_of_string(Sys.argv.(6)) in
	let epochs = int_of_string(Sys.argv.(7)) in
	let activation_func = int_of_string(Sys.argv.(8)) in
	let hidden_layer_no = int_of_string(Sys.argv.(9)) in
	let sample_no = List.length m_data in
	let train_sample_no = Array.length train_labels in
	let test_sample_no = Array.length test_labels in
	let neuron_no_per_layer = ref [] in
	for i = 10 to (9+hidden_layer_no) do
		neuron_no_per_layer := (int_of_string(Sys.argv.(i))) :: !neuron_no_per_layer;
	done;
	neuron_no_per_layer := (List.rev !neuron_no_per_layer);
	(* Creates the network *)
	print_string "Network creation is started" ;
	print_newline ();
	let ann = (Ann.initialize_network hidden_layer_no !neuron_no_per_layer input_dim label_no) in
	print_string "Network is created" ;
	print_newline ();
	Printf.printf "The number of total samples: %d " sample_no;
	print_newline ();
	Printf.printf "The number of train samples: %d " train_sample_no;
	print_newline ();
	Printf.printf "The number of test samples: %d " test_sample_no;
	print_newline ();
	Printf.printf "Input dimension: %d " input_dim;
	print_newline ();
	Printf.printf "The number of unique labels: %d " label_no;
	print_newline ();
	Printf.printf "Initial learning rate: %f " !learning_rate;
	print_newline ();
	Printf.printf "Alpha: %f " alpha;
	print_newline ();
	Printf.printf "The number of epochs: %d " epochs;
	print_newline ();

	print_string "Training is started" ;
	print_newline ();
	Ann.train_network ann normalized_train train_labels epochs learning_rate label_no learning_rate_method alpha activation_func;
	print_newline ();
	print_string "Training is completed" ;
	print_newline ();
	print_newline ();

	print_string "Prediction is started" ;
	print_newline ();
	print_newline ();
	let accuracy = ref 0. in
	Array.iteri (fun id label ->
		let sample = normalized_train.(id) in
		let prediction = (Ann.predict ann sample activation_func) in
		if label = prediction then accuracy := !accuracy +. 1.;
	) train_labels;
	accuracy := !accuracy /. (float_of_int (Array.length train_labels));
	Printf.printf "Overall train accuracy: %f \n" !accuracy;

	accuracy := 0.;
	Array.iteri (fun id label ->
		let sample = normalized_test.(id) in
		let prediction = (Ann.predict ann sample activation_func) in
		if label = prediction then accuracy := !accuracy +. 1.;
	) test_labels;
	accuracy := !accuracy /. (float_of_int(Array.length test_labels));
	Printf.printf "Overall test accuracy: %f \n" !accuracy;
	
	let conf_mat = create_confusion_matrix ann normalized_test test_labels label_no activation_func in
		
	print_string "Test class accuracies \n";
	Array.iteri ( fun id c ->
		 Printf.printf "Class-%d accuracy: %f \n" id ((c.(id))/. (Ann.sum c));
	) conf_mat;

	print_newline ();
	print_string "Prediction is completed" ;
	print_newline ();

	exit 0;;

main ();;