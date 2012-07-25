#include <stdio.h>
#include <stdlib.h>

#include <time.h>

#define PRON_NUM_CHARS 47
#define STREET_NUM_CHARS 56

#define BUFFER_SIZE 200

unsigned char street_mapping[STREET_NUM_CHARS] = {
'\0',
'\xD4',	// Ô
'\x38',	// 8
'\x37',	// 7
'\x4D',	// M
'\x52',	// R
'\x53',	// S
'\x45',	// E
'\xCB',	// Ë
'\xC6',	// Æ
'\x4F',	// O
'\x20',	//  
'\x49',	// I
'\x47',	// G
'\x4C',	// L
'\x48',	// H
'\x4E',	// N
'\x54',	// T
'\n',	// End of Word marker
'\x41',	// A
'\xCA',	// Ê
'\xD8',	// Ø
'\xC7',	// Ç
'\xC2',	// Â
'\x2F',	// /
'\x2C',	// ,
'\x29',	// )
'\x28',	// (
'\x39',	// 9
'\x30',	// 0
'\x35',	// 5
'\x36',	// 6
'\x34',	// 4
'\x33',	// 3
'\x27',	// '
'\x32',	// 2
'\x31',	// 1
'\x51',	// Q
'\x58',	// X
'\x2E',	// .
'\x59',	// Y
'\x4A',	// J
'\x56',	// V
'\xC4',	// Ä
'\xD6',	// Ö
'\xDC',	// Ü
'\x5A',	// Z
'\x50',	// P
'\x46',	// F
'\x2D',	// -
'\x4B',	// K
'\x43',	// C
'\x55',	// U
'\x57',	// W
'\x42',	// B
'\x44'	// D
};

unsigned char pron_mapping[PRON_NUM_CHARS] = {
'\0',
'\x76',	// v
'\x6D',	// m
'\x65',	// e
'\x32',	// 2
'\x27',	// '
'\x52',	// R
'\x2E',	// .
'\x26',	// &
'\x53',	// S
'\x6C',	// l
'\x6B',	// k
'\x73',	// s
'\x6E',	// n
'\x5F',	// _
'\x74',	// t
'\n',	// End of Word Marker
'\x61',	// a
'\x3A',	// :
'\x24',	// $
'\x41',	// A
'\x5A',	// Z
'\x7E',	// ~
'\x25',	// %
'\x6A',	// j
'\x59',	// Y
'\x78',	// x
'\x3F',	// ?
'\x2B',	// +
'\x43',	// C
'\x79',	// y
'\x4B',	// K
'\x7A',	// z
'\x55',	// U
'\x75',	// u
'\x70',	// p
'\x6F',	// o
'\x67',	// g
'\x4F',	// O
'\x64',	// d
'\x66',	// f
'\x68',	// h
'\x49',	// I
'\x69',	// i
'\x45',	// E
'\x62'	// b
};

int main(int argc, char * argv[])
{
	FILE * infile = NULL;
	FILE * outfile = NULL;

	infile = fopen(argv[1], "r");
	outfile = fopen(argv[2], "wb");

	if (infile == NULL || outfile == NULL)
	{
		fclose(infile);
		fclose(outfile);
		fprintf(stderr, "Error opening file.\n");
		return EXIT_FAILURE;
	}

	unsigned char street_name[BUFFER_SIZE];
	unsigned char pron[BUFFER_SIZE];
	unsigned char index = 0;

	unsigned char c = 0;
	char bits_left = -1; // -ve number to trigger first read
	unsigned char accum_char = 0;
	unsigned char accum_len = 0;

	// Start reading dictionary header information until [Data] is reached
	char found = 0;
	while ( !feof(infile) && !found )
	{
		fgets(street_name, BUFFER_SIZE, infile);
		fwrite(street_name, sizeof(char), strlen(street_name), outfile);
		if (strstr(street_name, "[Data]") != NULL)
		{
			found = 1;
			break;
		}
	}

	while (1)
	{
		index = 0;
		// Uncompress street name until End of Word Marker is reached.
		while (1)
		{
			if (bits_left < 0) // only read in a new byte when all bits have been processed, codewords can be less than 8 bits
			{
				fread(&c, sizeof(char), 1, infile);
				if (feof(infile)) goto FINISH;
				bits_left = accum_len;
				accum_char |= c >> accum_len;
			}

			// use number range to determine the codeword length
			if ((accum_char >= 56 && accum_char <= 111) || (accum_char >= 4 && accum_char <= 7))
			{
				// len = 7
				street_name[index++] = street_mapping[accum_char >> 1];
				accum_char = (accum_char << 7) | ((c << (8 - accum_len)) >> 1);
				bits_left = bits_left - 7;
				accum_len = (1 + accum_len) % 8;
			}
			else if (accum_char >= 160)
			{
				// len = 3
				street_name[index++] = street_mapping[accum_char >> 5];
				accum_char = (accum_char << 3) | ((c << (8 - accum_len)) >> 5);
				bits_left = bits_left - 3;
				accum_len = (5 + accum_len) % 8;
			}
			else if (accum_char >= 112)
			{
				// len = 5
				if (accum_char >> 3 == 18) // End of Word Marker reached
				{
					accum_char = (accum_char << 5) | ((c << (8 - accum_len)) >> 3);
					bits_left = bits_left - 5;
					accum_len = (3 + accum_len) % 8;
					break;
				}
				street_name[index++] = street_mapping[accum_char >> 3];
				accum_char = (accum_char << 5) | ((c << (8 - accum_len)) >> 3);
				bits_left = bits_left - 5;
				accum_len = (3 + accum_len) % 8;
			}
			else if (accum_char >= 40 || (accum_char >= 16 && accum_char <= 19))
			{
				// len = 6
				street_name[index++] = street_mapping[accum_char >> 2];
				accum_char = (accum_char << 6) | ((c << (8 - accum_len)) >> 2);
				bits_left = bits_left - 6;
				accum_len = (2 + accum_len) % 8;
			}
			else
			{
				// len = 8
				street_name[index++] = street_mapping[accum_char];
				accum_char = (c << (8 - accum_len));
				bits_left = bits_left - 8;
			}
		}
		street_name[index] = '\0';

		char has_space = 0;
		if (strstr(street_name, " ") != NULL)
		{
			has_space = 1;
		}
		// write street name to file
		// TODO: write more efficient if-statement
		if (has_space)
		{
			fwrite("\"", sizeof(char), 1, outfile);
		}
		fwrite(street_name, sizeof(char), strlen(street_name), outfile);
		if (has_space)
		{
			fwrite("\"", sizeof(char), 1, outfile);
		}
		fwrite("\t//", sizeof(char), 3, outfile);

		// uncompress and write pronounciation
		while (1)
		{
			if (bits_left < 0) // read new byte only when all bits have been processed
			{
				fread(&c, sizeof(char), 1, infile);
				if (feof(infile)) goto FINISH;
				bits_left = accum_len;
				accum_char |= c >> accum_len;
			}
	
			if (accum_char >= 56 && accum_char <= 91)
			{
				// len = 7
				fwrite(&pron_mapping[accum_char >> 1], sizeof(char), 1, outfile);
				accum_char = (accum_char << 7) | ((c << (8 - accum_len)) >> 1);
				bits_left = bits_left - 7;
				accum_len = (1 + accum_len) % 8;
			}
			else if (accum_char >= 160)
			{
				// len = 3
				fwrite(&pron_mapping[accum_char >> 5], sizeof(char), 1, outfile);
				accum_char = (accum_char << 3) | ((c << (8 - accum_len)) >> 5);
				bits_left = bits_left - 3;
				accum_len = (5 + accum_len) % 8;
			}
			else if (accum_char >= 112)
			{
				// len = 5
				fwrite(&pron_mapping[accum_char >> 3], sizeof(char), 1, outfile);
				
				if (accum_char >> 3 == 16) // End of Word Marker
				{
					accum_char = (accum_char << 5) | ((c << (8 - accum_len)) >> 3);
					bits_left = bits_left - 5;
					accum_len = (3 + accum_len) % 8;
					break;
				}
				else
				{
					accum_char = (accum_char << 5) | ((c << (8 - accum_len)) >> 3);
					bits_left = bits_left - 5;
					accum_len = (3 + accum_len) % 8;
				}
			}
			else if (accum_char >= 20 && accum_char <= 27)
			{
				// len = 8
				fwrite(&pron_mapping[accum_char], sizeof(char), 1, outfile);
				accum_char = (c << (8 - accum_len));
				bits_left = bits_left - 8;
			}
			else
			{
				// len = 6
				fwrite(&pron_mapping[accum_char >> 2], sizeof(char), 1, outfile);
				accum_char = (accum_char << 6) | ((c << (8 - accum_len)) >> 2);
				bits_left = bits_left - 6;
				accum_len = (2 + accum_len) % 8;
			}
		}
	}

FINISH:

	fclose(infile);
	fclose(outfile);

	return EXIT_SUCCESS;
}
