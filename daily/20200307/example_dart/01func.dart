// https://dart.dev/guides/language/language-tour

printInteger(int aNumber) {
  print('The number is $aNumber.');
  print('The number is ${aNumber + 1}.');
}

main(){
  var number = 42;
  printInteger(number);
}
