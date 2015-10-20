
MEMORY
{
    MYMEM (rx) : ORIGIN = 0x00000000, LENGTH = 96K
    MYDATA (rw) : ORIGIN = 0x10000000, LENGTH = 96K
}

SECTIONS
{
	.text :
	{
		. = ALIGN (4);
		*(.text)
	} > MYMEM

	.data :
	{
		. = ALIGN (4);
		*(.data)
	} > MYDATA
}

