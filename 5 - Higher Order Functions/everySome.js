//returns true if predicate true for every array element
/*function every(arr, pred){
    for(var i = 0; i < arr.length; i++){
	if(!pred(arr[i]))
	   return false;
    }
	return true;
	}*/

//defined using forEach
function every(arr, pred){
    var ret = true;
    arr.forEach(function(a){
	ret = ret && pred(a);
    });
    return ret;
}

//returns true if some array alement is true
function some(arr, pred){
    for(var i = 0; i < arr.length; i++){
	if(pred(arr[i]))
	   return true;
    }
	return false;
}


console.log(every([NaN, NaN, NaN], isNaN));
console.log(every([NaN, NaN, 4], isNaN));
console.log(some([NaN, 3, 4], isNaN));
console.log(some([2, 3, 4], isNaN));
