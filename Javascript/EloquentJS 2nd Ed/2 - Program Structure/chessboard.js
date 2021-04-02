/* A  program that creates a string that represents an 8¡Á8 grid, using newline characters to separate lines. At each position of the grid there is either a space or a ¡°#¡± character. The characters should form a chess board */

var size = 20;
var board = "";

for(var i = 0;i < size; i++){
    for(var j = 0;j < size; j++){
	if(((i + j) % 2) === 0){
	    board += "#";
	}
	else{
	    board += " ";
	}
    }
    board += "\n";
}

console.log(board);
