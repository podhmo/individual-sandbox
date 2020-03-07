class Spacecraft {
  String name;
  DateTime launchDate;

  // constructor
  Spacecraft(this.name, this.launchDate){}

  Spacecraft.unlaunched(String name): this(name, null);

  // property
  int get launchYear => launchDate?.year;

  // method
  void describe() {
    print("Spacecraft: $name");
    if (launchDate != null) {
      int years = DateTime.now().difference(launchDate).inDays ~/ 365;
      print("Launched: $launchYear ($years years ago)");
    } else {
      print("Unlaunched");
    }
  }
}

main(){
  var voyager = Spacecraft("Voyager 1", DateTime(1977, 9, 5));
  voyager.describe();

  var voyager3 = Spacecraft.unlaunched("Voyager 3");
  voyager3.describe();
}
