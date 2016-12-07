//represents a vector in two-dimensional space. It takes x and y parameters (numbers), which it should save to properties of the same name.
function Vector(x,y){
    this.x = x;
    this.y = y;
};

//Give the Vector prototype two methods, plus and minus  that take another vector as a parameter and return a new vector that has the sum or difference of the two vectors�f (the one in this and the parameter) x and y values.
Vector.prototype.plus = function(vector){
    var newX = this.x + vector.x;
    var newY = this.y + vector.y;
    
    return new Vector(newX,newY);
};

Vector.prototype.minus = function(vector){
    var newX = this.x - vector.x;
    var newY = this.y - vector.y;
    
    return new Vector(newX,newY);
};

//Add a getter property length to the prototype that computes the length of the vector�\that is, the distance of the point (x, y) from the origin (0, 0).
Object.defineProperty(Vector.prototype, "length", {
    get: function() {
	return Math.sqrt(this.x * this.x + this.y * this.y);
    
}});

console.log(new Vector(1,2).plus(new Vector(2,3)));
console.log(new Vector(1,2).minus(new Vector(2,3)));
console.log(new Vector(3,4).length);
