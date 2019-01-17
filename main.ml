open Ann

(* 
 * train_filepath		: Sys.argv.(1) 	: string
 * test_filepath		: Sys.argv.(2) 	: string
 * delimeter 			: Sys.argv.(3) 	: char
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
	let train_filepath = Sys.argv.(1) in
	let test_filepath = Sys.argv.(2) in
	let delimeter = ref (String.get Sys.argv.(3) 0) in

	(* Read test files *)
  	Printf.printf "Train file path %s\n" train_filepath;
  	Printf.printf "Test file path %s\n" test_filepath;
	print_string "Data load is started" ;
	print_newline ();

	let m_train_data = (Ann.read_file train_filepath) in
	let m_test_data = (Ann.read_file test_filepath) in
	print_string "Data load is complete" ;
	print_newline ();

	print_string "Data process is started" ;
	print_newline ();
	let train_data = (Ann.process_file m_train_data !delimeter) in
	let test_data = (Ann.process_file m_test_data !delimeter) in
	print_string "Data process is completed" ;
	print_newline ();

	(* Get the statistics, mean and standard deviation, for data *)
	print_string "Calculating mean and standard deviation for training data.." ;
	print_newline ();
	let stats = (Ann.get_statistics (fst train_data)) in
	print_string "Mean and standard deviation are calculated" ;
	print_newline ();
	let mean = (fst stats) in
	let std = (snd stats) in
	print_string "Normalization is started" ;
	print_newline ();
	let normalized_train = Ann.normalize (fst train_data) mean std in
	let normalized_test = Ann.normalize (fst test_data) mean std in
	print_string "Normalization is completed" ;
	print_newline ();

	let label_no = (List.length (List.sort_uniq compare (snd train_data))) in
	let input_dim = (List.length (List.nth normalized_train 0)) in
	let learning_rate = ref (float_of_string((Sys.argv.(4)))) in
	let learning_rate_method = int_of_string(Sys.argv.(5)) in
	let alpha = float_of_string(Sys.argv.(6)) in
	let epochs = int_of_string(Sys.argv.(7)) in
	let activation_func = int_of_string(Sys.argv.(8)) in
	let hidden_layer_no = int_of_string(Sys.argv.(9)) in
	let neuron_no_per_layer = ref [] in
	for i = 10 to (9+hidden_layer_no) do
		neuron_no_per_layer := Ann.append_el (int_of_string(Sys.argv.(i))) !neuron_no_per_layer;
	done;

	(* Creates the network *)
	print_string "Network creation is started" ;
	print_newline ();
	let ann = (Ann.initialize_network hidden_layer_no !neuron_no_per_layer input_dim label_no) in
	print_string "Network is created" ;
	print_newline ();
	(* Ann.print_network ann; *)
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
	Ann.train_network ann normalized_train (snd train_data) epochs learning_rate label_no learning_rate_method alpha activation_func;
	print_string "Training is completed" ;
	print_newline ();

	print_string "Prediction is started" ;
	print_newline ();
	let accuracy = ref 0. in
	List.iter2 (fun sample label ->
		let prediction = (Ann.predict ann sample activation_func) in
		if label = prediction then accuracy := !accuracy +. 1.;
	) normalized_train (snd train_data);
	accuracy := !accuracy /. (float_of_int (List.length (snd train_data)));
	Printf.printf "Overall train accuracy: %f \n" !accuracy;

	accuracy := 0.;
	List.iter2 (fun sample label->
		let prediction = (Ann.predict ann sample activation_func) in
		if label = prediction then accuracy := !accuracy +. 1.;
	) normalized_test (snd test_data);
	accuracy := !accuracy /. (float_of_int(List.length (snd test_data)));
	Printf.printf "Overall test accuracy: %f \n" !accuracy;
	
	let conf_mat = create_confusion_matrix ann normalized_test (snd test_data) label_no activation_func in
		
	print_string "Test class accuracies \n";
	Array.iteri ( fun id c ->
		 Printf.printf "Class-%d accuracy: %f \n" id ((c.(id))/. (Ann.sum c));
	) conf_mat;
	exit 0;;

main ();;