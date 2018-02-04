//takes an array and returns a new array with the elements reversed
function reverseArray(arr){
    var newArr = [];
    var x = 0;
    for(var i = arr.length-1; i >= 0 ; i--){
	newArr[x] = arr[i];
	x++;
    }
    return newArr;
}

//takes an array and reverses it in place
function reverseArrayInPlace(arr){
    var end = arr.length - 1;
    var start  = 0;
    var temp = 0;
    for(var i = 0; i < Math.floor(arr.length / 2); i++){
	temp = arr[start];
	arr[start] = arr[end];
	arr[end] = temp;
	start++;
	end--;
    }
}
