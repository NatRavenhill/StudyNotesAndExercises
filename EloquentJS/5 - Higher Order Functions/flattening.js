var arrays = [[1, 2, 3], [4, 5], [6]];

//use reduce method with concat to flatten an array of arrays into a single array
function flatten(arr){
    return arr.reduce(function(a,b){ return a.concat(b);},[]);
}
