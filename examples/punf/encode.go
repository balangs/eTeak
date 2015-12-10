package main
import "fmt"

const (
	foo =1; bar =2
)

type Foo uint
//(5)
//
/*
type Word uint32

func (w *Word) _bitslice(offset int, size int) {
»·······return (w >> offset) & ((2^size)-1)
}

type Inst Word
func (w *Inst) low() { w._bitslice(0,30) }
func (w *Inst) op() { w._bitslice(30,2) }
*/

/*procedure onehot (
	input i : 2 bits;
	output o : 4 bits
) is
begin
	loop	
		i -> then
			case i of
			| 0 then o <- 1
			| 1 then o <- 2
			| 2 then o <- 4
			| 3 then o <- 8
			end
		end
	end
end
*/

func onehot (i <-chan byte, o chan<- byte) {
	for {
		v := <-i
		switch v {
		case 0: o <- 1
		case 1: o <- 2
		case 2: o <- 4
		case 4: o <- 8
		}
	}
}

		/*
procedure top
is
	channel i : 2 bits
	channel o : 4 bits
begin
	loop
		o -> then print o end
	end ||
	loop
		for ; j in 0..3 then i <- j end
	end
	||
	onehot (i, o)
end
*/

func top (i chan byte, o chan byte) {
	go func (o chan byte) {
		for {
			v:= <-o
			fmt.Printf("%d",v)
		}
	} (o)
	go func(i chan byte) {
		for {
			for j := byte(0); j < 4; j++ {
				i <- j
			}
		}
	} (i)
	onehot(i,o)
}

func main() {
/*	var i Inst
	i = 0xF000000F
	
	fmt.Printf("%d",i.op())
	fmt.Printf("%d",i.low())
*/
	var i = make (chan byte)
	var o = make (chan byte)
	top (i,o)
}
