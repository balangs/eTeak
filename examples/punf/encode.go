package main
import "fmt"

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
			v := <-o
			// qualified identifiers are not supported at this time
			//fmt.Printf("%d",v)
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

/*
func main() {
	var i Inst
	i = 0xF000000F
	
	fmt.Printf("%d",i.op())
	fmt.Printf("%d",i.low())

    // make is not a supported construct at this time
	var i (chan byte) = make (chan byte)
	var o (chan byte) = make (chan byte)
	top (i,o)
}
*/
