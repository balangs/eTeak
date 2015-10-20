#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <iso646.h>
#include <string.h>

bool readWord (FILE *file, bool bigEnd, uint32_t *ret)
{
	*ret = 0;

	int b0 = getc (file);
	int b1 = getc (file);
	int b2 = getc (file);
	int b3 = getc (file);

	if (bigEnd)
	{
		if (b0 != EOF) *ret |= ((uint32_t) ((uint8_t) b0)) << 24;
		if (b1 != EOF) *ret |= ((uint32_t) ((uint8_t) b1)) << 16;
		if (b2 != EOF) *ret |= ((uint32_t) ((uint8_t) b2)) << 8;
		if (b3 != EOF) *ret |= ((uint32_t) ((uint8_t) b3)) << 0;
	} else {
		if (b0 != EOF) *ret |= ((uint32_t) ((uint8_t) b0)) << 0;
		if (b1 != EOF) *ret |= ((uint32_t) ((uint8_t) b1)) << 8;
		if (b2 != EOF) *ret |= ((uint32_t) ((uint8_t) b2)) << 16;
		if (b3 != EOF) *ret |= ((uint32_t) ((uint8_t) b3)) << 24;
	}

	return b0 == EOF or b1 == EOF or b2 == EOF or b3 == EOF;
}

main (int argc, char **argv)
{
	uint32_t a = 0;
	bool last;
	bool withAddress = false;
	bool bigEnd = false;

	argv ++;
	while (argc > 1)
	{
		if (strcmp (argv[0], "-a") == 0)
		{
			withAddress = true;
		} else if (strcmp (argv[0], "-b") == 0)
		{
			bigEnd = true;
		}
		argv ++;
		argc --;
	}

	do {
		uint32_t word;
		last = readWord (stdin, bigEnd, &word);

		if (withAddress)
			printf ("%08X %08X\n", a, word);
		else printf ("%08X\n", word);

		a += 4;
	} while (not last);
}
