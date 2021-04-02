//example of a list
var list = {
    value: 1,
    rest: {
	value: 2,
        rest: {
	    value: 3,
	    rest: null
	}
    }
}

//converts a list to an array
function listToArray(list){
    var arr = [];
    while(list.rest !== null){
	arr.push(list.value);
	list = list.rest;
    }
    //push the last value
    arr.push(list.value);
    return arr;
}

//adds a new element to the front of a list
function prepend(elem, list){
    return {value: elem, rest: list};
    
}

//returns an element at the given position in the list
function nth(n, list){
    if(list === null)
	return undefined;
    else if (n === 0)
	return list.value;
    else
	return nth(n-1, list.rest);	    
}

//converts an array to a list
function arrayToList(arr){
    var list = {value: arr[arr.length-1], rest: null};
    for(var i = (arr.length - 2); i >= 0; i--){
	list = prepend(arr[i],list);
    }
    return list;
}    
