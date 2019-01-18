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
@data_filepath			: file path to train file
@delimeter 			: a char that is field delimeter in the sequence of floating numbers for each 
								string in data e.g. ','
@test_ratio			: represent the proportion of the dataset to include in the test split, should be 								between 0.0 and 1.0  
@Learning_rate 			: initial learning rate, a floating point number generally small
@learning_rate_method		:0 = learning rate does not change througout program
				 1 = learning rate decreases after every sqrt(sample size) iteration
@alpha				: a floating point number between [0,1] to determine the effect of previous weight changes
@epochs				: the number of epochs	
@activation_func		: 0 = sigmoid function
				  1 = relu function					
@hidden_layer_no 		: the number of hidden layers in the network
@neuron_no_per_layer		: the number of neurons in each hidden layer
```

```
./ann data_filepath test_ratio delimeter learning_rate learning_rate_method alpha epochs activation_func hidden_layer_no neuron_number_1 neuron_number2 neuron_numbern  
```

# Examples
## MNIST
	The number of hidden layers 1.
		The number of neurons in hidden layers 
		0 : 8
	The number of epochs	: 100
	Delimeter 				: ',' -> comma
	Test ratio 				: 0.2
	Learning rate method 	: Learning rate decreases
	Learning rate 			: starts with 0.5
	Activation Function 	: sigmoid
	Alpha 					: 0.01
	Run on MNIST data

### Terminal code
```
$ ./ann DATASET/MNIST/train_data.txt DATASET/MNIST/test_data.txt ,  0.2 0.5 1 0.01 100 0 1 8 > Results/MNIST.txt	
```

## Iris
	The number of hidden layers 2.
		The number of neurons in hidden layers 
		0 : 8
		1 : 8
	The number of epochs	: 500
	Delimeter 				: ',' -> comma
	Test ratio 				: 0.2
	Learning rate method 	: Learning rate decreases
	Learning rate 			: starts with 0.8 (1) and starts with 0.1 (2) 
	Activation Function 	: sigmoid(1) and relu(2)
	Alpha 					: 0.05
	Run on Iris data

### Terminal code
(1) with Sigmoid Function
```
$ ./ann DATASET/Iris/data.txt , 0.2 3 1 0.05 100 0 2 3 5  > Results/iris_sigmoid.txt	
```
(2) with Relu Function 
```
$ ./ann DATASET/Iris/data.txt , 0.2 0.01 1 0.05 100 1 2 8 8 > Results/iris_relu.txt
```

## Wine
	The number of hidden layers 1.
		The number of neurons in hidden layers 
		0 : 6
	The number of epochs	: 100
	Delimeter 				: ',' -> comma
	Test ratio 				: 0.2
	Learning rate method 	: Learning rate decreases
	Learning rate 			: starts with 3 
	Activation Function 	: sigmoid
	Alpha 					: 0.05
	Run on Wine data

### Terminal code
```
$ ./ann DATASET/Wine/data.txt , 0.2 3 0 0.05 100 0 1 6 > Results/wine.txt
```

## Breast Cancer
	The number of hidden layers 1.
		The number of neurons in hidden layers 
		0 : 10
	The number of epochs	: 100
	Delimeter 				: ',' -> comma
	Test ratio 				: 0.2
	Learning rate method 	: Learning rate decreases
	Learning rate 			: starts with 0.5 
	Activation Function 	: sigmoid
	Alpha 					: 0.01
	Run on Wine data

### Terminal code
```
$ ./ann DATASET/Breast_cancer/data.txt , 0.2 0.5 1 0.01 100 0 1 10 > Results/breast_cancer.txt
```

## Digits
	The number of hidden layers 2.
		The number of neurons in hidden layers 
		0 : 10
		1 : 10
	The number of epochs	: 100
	Delimeter 				: ',' -> comma
	Test ratio 				: 0.2
	Learning rate method 	: Learning rate decreases
	Learning rate 			: starts with 0.5 
	Activation Function 	: sigmoid
	Alpha 					: 0.05
	Run on Wine data

### Terminal code
```
$ ./ann DATASET/Digits/data.txt , 0.2 0.5 1 0.01 100 0 2 10 10 > Results/digits.txt
```

