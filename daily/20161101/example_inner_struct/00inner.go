package main

// compile error
func main(){
    type S struct {}
    func (s *S) say() {
        return fmt.Println("hello")
    }
    return (&S{}).say()
}
