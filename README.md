# DeepLearning
OCaml implementation of fully connected deep neural network

# How to compile the code?
Just type make on the terminal!
```
$ make
```

# How to run the code?

### Parameters
```
@train_filepath				: file path to train file
@test_filepath				: file path to corresponding test file
@delimeter 					: a char that is field delimeter in the sequence of floating numbers for each 
								string in data e.g. ','
@Learning_rate 				: initial learning rate, a floating point number generally small
@learning_rate_method		:	0 = learning rate does not change througout program
								1 = learning rate decreases after every sqrt(sample size) iteration
@alpha						: a floating point number between [0,1] to determine the effect of previous 								weight changes
@epochs						: the number of epochs	
@activation_func			: 0 = sigmoid function
							  1	= relu function					
@hidden_layer_no 			: the number of hidden layers in the network
@neuron_no_per_layer		: the number of neurons in each hidden layer
```

```
./ann train_filepath test_file_path delimeter learning_rate learning_rate_method alpha epochs activation_func hidden_layer_no neuron_number_1 neuron_number2 neuron_numbern  
```

# Examples
## MNIST
	The number of hidden layers 1.
		The number of neurons in hidden layers 
		0 : 8
	The number of epochs	: 100
	Delimeter 				: '\ ' -> space
	Learning rate method 	: Learning rate decreases
	Learning rate 			: starts with 0.5
	Activation Function 	: sigmoid
	Alpha 					: 0.01
	Run on MNIST data

### Terminal code
```
$ ./ann DATASET/MNIST/train_data.txt DATASET/MNIST/test_data.txt \  0.5 1 0.01 100 0 1 8
```

## Iris
	The number of hidden layers 2.
		The number of neurons in hidden layers 
		0 : 8
		1 : 8
	The number of epochs	: 500
	Delimeter 				: ',' -> comma
	Learning rate method 	: Learning rate decreases
	Learning rate 			: starts with 0.8 (1) and starts with 0.1 (2) 
	Activation Function 	: sigmoid(1) and relu(2)
	Alpha 					: 0.05
	Run on Iris data

### Terminal code
(1) <!-- with Sigmoid Function -->
```
$ ./ann DATASET/Iris/train.txt DATASET/Iris/test.txt , 0.8 1 0.05 500 0 2 8 8	
```
(2) <!-- with Relu Function    -->
```
$ ./ann DATASET/Iris/train.txt DATASET/Iris/test.txt , 0.1 1 0.05 500 1 2 8 8	
```

## Wine
	The number of hidden layers 1.
		The number of neurons in hidden layers 
		0 : 6
	The number of epochs	: 100
	Delimeter 				: ',' -> comma
	Learning rate method 	: Learning rate decreases
	Learning rate 			: starts with 3 
	Activation Function 	: sigmoid
	Alpha 					: 0.05
	Run on Wine data

### Terminal code
```
$ ./ann DATASET/Wine/train.txt DATASET/Wine/test.txt , 3 0 0.05 100 0 1 6
```

## Breast Cancer
	The number of hidden layers 1.
		The number of neurons in hidden layers 
		0 : 10
	The number of epochs	: 100
	Delimeter 				: ',' -> comma
	Learning rate method 	: Learning rate decreases
	Learning rate 			: starts with 0.5 
	Activation Function 	: sigmoid
	Alpha 					: 0.01
	Run on Wine data

### Terminal code
```
$ ./ann DATASET/Breast_cancer/train.txt DATASET/Breast_cancer/test.txt , 0.5 1 0.01 100 0 1 10
```

## Digits
	The number of hidden layers 2.
		The number of neurons in hidden layers 
		0 : 10
		1 : 10
	The number of epochs	: 100
	Delimeter 				: ',' -> comma
	Learning rate method 	: Learning rate decreases
	Learning rate 			: starts with 0.5 
	Activation Function 	: sigmoid
	Alpha 					: 0.05
	Run on Wine data

### Terminal code
```
$ ./ann DATASET/Digits/train.txt DATASET/Digits/test.txt , 0.5 1 0.01 100 0 2 10 10
```

