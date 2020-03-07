main(){
  var year = 2001;
  if (year >= 2001) {
    print('21st centry');
  } else if (year >= 1901) {
    print('20st centry');
  }

  print("----------------------------------------");

  var flybyObjects = ["foo", "bar", "boo"];
  for (var object in flybyObjects){
    print(object);
  }

  print("----------------------------------------");

  for (int month = 1; month <= 12; month++){
    print(month);
  }

  while (year < 2016){
    year += 1;
  }
  print(year);
}
