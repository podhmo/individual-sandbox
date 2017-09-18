class Range{
  constructor(range, version){
    this.range = range
    this.version = version
  }
}

console.log(new Range(">=1.2.3", true))
console.log(Range(">=1.2.3", true))
