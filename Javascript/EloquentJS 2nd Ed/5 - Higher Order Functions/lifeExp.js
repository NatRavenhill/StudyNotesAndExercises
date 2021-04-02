var ancestry = JSON.parse(ANCESTRY_FILE);

//calculate average of an array
function average(array) {
  function plus(a, b) { return a + b; }
  return array.reduce(plus) / array.length;
}

//byName contains an array of all people, indexed by name
var byName = {};
ancestry.forEach(function(person) {
    byName[person.name] = person;
});

//get the century in which someone was born
function getCentury(person){
    return Math.ceil(person.died / 100);
}

function getAge(person){
    return person.died - person.born;
}

//take an array and function that computes the group for an element of the array and returns an object mapping group names to arrays of group numbers
function groupBy(arr, f) {
    var groups = {};
    arr.forEach(function(person) {
	var groupOfPerson = f(person);
	if(groupOfPerson in groups){
	    groups[groupOfPerson].push(person);
	}
	else {
	    groups[groupOfPerson] = [person];
	}
    });
		return groups;
    }


//get the people by century
var byCentury = groupBy(ancestry, getCentury);

//return the average ages for each century
for(var century in byCentury){
    console.log(century + ": " + Math.ceil(average(byCentury[century].map(getAge))));
}


