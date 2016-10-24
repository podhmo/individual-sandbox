package main

import "fmt"

type X int
type Y int
type Z Y
type PX *int

func main() {
	{
		// int -> X; int -> Y
		// X => Y
		x := X(1)
		// [(coerce X int), (coerce int Y)] = [(cast Y)]
		fmt.Printf("%T -> %T\n", x, Y(int(x)))
		fmt.Printf("%T -> %T\n", x, Y(x))
	}
	{
		// int -> X; int -> Y, Y -> Z
		// X => Z
		x := X(1)
		// [(coerce X int), (coerce int Y), (coerce Y Z)] = [(cast Y)]
		fmt.Printf("%T -> %T\n", x, Z(Y(int(x))))
		fmt.Printf("%T -> %T\n", x, Z(x))
	}
	fmt.Println("----------------------------------------")
	{
		// X => *Y
		x := X(1)
		// [(coerce X int), (coerce int Y), (ref)] = [(cast Y), (ref)] = [(ref), (cast pointer Y)]
		{
			z := Y(x)
			fmt.Printf("%T -> %T\n", x, &z)
		}
		fmt.Printf("%T -> %T\n", x, (*Y)(&x))

		// X => *Z
        // [(coerce X int), (coerce int Y), (coerce Y Z), (ref)]
        // = [(cast int), (cast Y), (cast Z), (ref)]
        {
            tmp := Z(Y(int(x)))
            fmt.Printf("%T -> %T\n", x, &tmp)
        }
        // => [(cast Z), (ref)]
        {
            tmp := Z(x)
            fmt.Printf("%T -> %T\n", x, &tmp)
        }
	}
	fmt.Println("----------------------------------------")
	{
		// *X => Y
		_x := X(1)
		x := &_x
		{
			// [(deref), (coerce X int), (coerce int Y)] = [(deref), (cast Y)]
			fmt.Printf("%T -> %T\n", x, Y(*x))
			fmt.Printf("%T -> %T\n", x, *((*Y)(x)))
		}
		// *X => Z
        {
            // [(deref), (coerce X int), (coerce int Y), (coerce Y Z)]
            // = [(deref), (cast Z)]
			fmt.Printf("%T -> %T\n", x, Z(*x))
        }
	}
	fmt.Println("----------------------------------------")
	{
		// *X => *Y
		_x := X(1)
		x := &_x
		// [(deref), (coerce X int), (coerce int Y), (ref)] = [(deref), (cast Y), (ref)]
		{
			z := Y(*x)
			fmt.Printf("%T -> %T\n", x, &z)
		}
		fmt.Printf("%T -> %T\n", x, (*Y)(x))

        // *X => *Z
		// [(deref), (coerce X int), (coerce int Y), (coerce Y Z), (ref)] = [(deref), (cast Z), (ref)]
        {
            tmp := Z(Y(int(*x)))
            fmt.Printf("%T -> %T\n", x, &tmp)
        }
	}
	fmt.Println("----------------------------------------")
	{
		// int* => int*
		_x := 1
		x := &_x
		// [(deref), (ref))] = []
		{
			fmt.Printf("%T -> %T\n", x, x)
		}
	}
	fmt.Println("----------------------------------------")
	{
		{
            // int => PX
            // int, pointer int -> PX
            // src : int [0]
            // dst : PX = pointer int [+1]
            // ref int => pointer int => PX
            x := 1
            // [ref, (coerce (pointer int) PX)]
            tmp := &x
			fmt.Printf("%T -> %T\n", x, PX(tmp))
		}
		{
            // *int => PX
            _x := 1
            x := &_x
            // [deref, ref, (coerce (pointer int) PX)] = [(coerce (pointer int) PX)]
			fmt.Printf("%T -> %T\n", x, PX(x))
		}
	}
	fmt.Println("----------------------------------------")
	{
		{
            // PX => int
            x := 1
            px := PX(&x)
            // src : pointer int -> PX [+1]
            // dst : int [0]
            // pointer int => (deref, int)
            // [(coerce PX (pointer int)), (deref)]
			fmt.Printf("%T -> %T\n", px, *((*int)(px)))
		}
	}
}
