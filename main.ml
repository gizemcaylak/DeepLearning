open Ann

(* 
 * train_filepath: Sys.argv.(1) 
 * test_filepath: Sys.argv.(2) 
 * learning_rate: Sys.argv.(3) 
 * learning_rate_method: Sys.argv.(4)
 * alpha : Sys.argv.(5)
 * epochs : Sys.argv.(6)
 * hidden_layer_no: Sys.argv.(7) 
 * neuron_no_per_layer : Sys.argv.(8) to  Sys.argv.(7+hidden_layer_no)
 *)
let main () =
	for i = 0 to Array.length Sys.argv - 1 do
      Printf.printf "[%i] %s\n" i Sys.argv.(i);
    done;;
	(* Read train files *)
	let train_filepath = Sys.argv.(1) in
	let m_train_data = (Ann.read_file train_filepath) in
	let train_data = (Ann.process_file m_train_data ',') in

	(* Read test files *)
	let test_filepath = Sys.argv.(2) in
  	Printf.printf "Train file path %s\n" train_filepath;
  	Printf.printf "Test file path %s\n" test_filepath;
	print_string "Data load is started" ;
	print_newline ();

	let m_test_data = (Ann.read_file test_filepath) in
	print_string "Data load is complete" ;
	print_newline ();

	print_string "Data process is started" ;
	print_newline ();
	let test_data = (Ann.process_file m_test_data ',') in
	print_string "Data process is completed" ;
	print_newline ();

	(* Get the statistics, mean and standard deviation, for data *)
	print_string "Calculating mean and standard deviation for train data.." ;
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
	let learning_rate = ref (float_of_string((Sys.argv.(3)))) in
	let learning_rate_method = int_of_string(Sys.argv.(4)) in
	let alpha = float_of_string(Sys.argv.(5)) in
	let epochs = int_of_string(Sys.argv.(6)) in
	let hidden_layer_no = int_of_string(Sys.argv.(7)) in
	let neuron_no_per_layer = ref [] in
	for i = 8 to (7+hidden_layer_no) do
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
	Ann.trainNetwork ann normalized_train (snd train_data) epochs learning_rate label_no learning_rate_method alpha;
	print_string "Training is completed" ;
	print_newline ();

	(* Ann.print_network ann; *)
	(* Prediction *)
	(* let n = label_no in *)
	(* let confMat = Array.make_matrix n n 0 in *)
	print_string "Prediction is started" ;
	print_newline ();
	let accuracy = ref 0. in
	List.iter2 (fun sample label ->
		let prediction = (Ann.predict ann sample) in
		if label = prediction then accuracy := !accuracy +. 1.;
	) normalized_train (snd train_data);
	accuracy := !accuracy /. (float_of_int (List.length (snd train_data)));
	Printf.printf "Train accuracy: %f \n" !accuracy;

	accuracy := 0.;
	List.iter2 (fun sample label->
		let prediction = (Ann.predict ann sample) in
		if label = prediction then accuracy := !accuracy +. 1.;
	) normalized_test (snd test_data);
	accuracy := !accuracy /. (float_of_int(List.length (snd test_data)));
	Printf.printf "Test accuracy: %f \n" !accuracy;
	
	exit 0;;

main ();;