#include <stdio.h>
#include <stdlib.h>

#define BUFFER_SIZE 200
#define PRON_NUM_CHARS 46
#define STREET_NUM_CHARS 55

typedef struct STRING_MAPPING
{
	unsigned char original; // original character before compression
	unsigned char code; // codeword
	int codelen; // length of codeword
} STRING_MAPPING;

// street name mapping
STRING_MAPPING street_mapping[STREET_NUM_CHARS] = {
{'\x45', '\xE0', 3},	// E
{'\x53', '\xC0', 3},	// S
{'\x52', '\xA0', 3},	// R
{'\x41', '\x98', 5},	// A
{'\n', '\x90', 5},	// End of Word marker
{'\x54', '\x88', 5},	// T
{'\x4E', '\x80', 5},	// N
{'\x48', '\x78', 5},	// H
{'\x4C', '\x70', 5},	// L
{'\x47', '\x34', 6},	// G
{'\x49', '\x30', 6},	// I
{'\x20', '\x2C', 6},	//  
{'\x4F', '\x28', 6},	// O
{'\x4D', '\x10', 6},	// M
{'\x44', '\x6E', 7},	// D
{'\x42', '\x6C', 7},	// B
{'\x57', '\x6A', 7},	// W
{'\x55', '\x68', 7},	// U
{'\x43', '\x66', 7},	// C
{'\x4B', '\x64', 7},	// K
{'\x2D', '\x62', 7},	// -
{'\x46', '\x60', 7},	// F
{'\x50', '\x5E', 7},	// P
{'\x5A', '\x5C', 7},	// Z
{'\xDC', '\x5A', 7},	// Ü
{'\xD6', '\x58', 7},	// Ö
{'\xC4', '\x56', 7},	// Ä
{'\x56', '\x54', 7},	// V
{'\x4A', '\x52', 7},	// J
{'\x59', '\x50', 7},	// Y
{'\x2E', '\x4E', 7},	// .
{'\x58', '\x4C', 7},	// X
{'\x51', '\x4A', 7},	// Q
{'\x31', '\x48', 7},	// 1
{'\x32', '\x46', 7},	// 2
{'\x27', '\x44', 7},	// '
{'\x33', '\x42', 7},	// 3
{'\x34', '\x40', 7},	// 4
{'\x36', '\x3E', 7},	// 6
{'\x35', '\x3C', 7},	// 5
{'\x30', '\x3A', 7},	// 0
{'\x39', '\x38', 7},	// 9
{'\x37', '\x6', 7},	// 7
{'\x38', '\x4', 7},	// 8
{'\x28', '\x1B', 8},	// (
{'\x29', '\x1A', 8},	// )
{'\x2C', '\x19', 8},	// ,
{'\x2F', '\x18', 8},	// /
{'\xC2', '\x17', 8},	// Â
{'\xC7', '\x16', 8},	// Ç
{'\xD8', '\x15', 8},	// Ø
{'\xCA', '\x14', 8},	// Ê
{'\xCB', '\x8', 8},	// Ë
{'\xC6', '\x9', 8},	// Æ
{'\xD4', '\x1', 8}	// Ô
};

// pronunciation mapping
STRING_MAPPING pron_mapping[PRON_NUM_CHARS] = {
{'\x2E', '\xE0', 3},	// .
{'\x52', '\xC0', 3},	// R
{'\x27', '\xA0', 3},	// '
{'\x24', '\x98', 5},	// $
{'\x3A', '\x90', 5},	// :
{'\x61', '\x88', 5},	// a
{'\n', '\x80', 5},	// End of Word Marker
{'\x74', '\x78', 5},	// t
{'\x5F', '\x70', 5},	// _
{'\x6E', '\x34', 6},	// n
{'\x73', '\x30', 6},	// s
{'\x6B', '\x2C', 6},	// k
{'\x6C', '\x28', 6},	// l
{'\x53', '\x24', 6},	// S
{'\x26', '\x20', 6},	// &
{'\x32', '\x10', 6},	// 2
{'\x65', '\xC', 6},	// e
{'\x6D', '\x8', 6},	// m
{'\x76', '\x4', 6},	// v
{'\x62', '\x5A', 7},	// b
{'\x45', '\x58', 7},	// E
{'\x69', '\x56', 7},	// i
{'\x49', '\x54', 7},	// I
{'\x68', '\x52', 7},	// h
{'\x66', '\x50', 7},	// f
{'\x64', '\x4E', 7},	// d
{'\x4F', '\x4C', 7},	// O
{'\x67', '\x4A', 7},	// g
{'\x6F', '\x48', 7},	// o
{'\x70', '\x46', 7},	// p
{'\x75', '\x44', 7},	// u
{'\x55', '\x42', 7},	// U
{'\x7A', '\x40', 7},	// z
{'\x4B', '\x3E', 7},	// K
{'\x79', '\x3C', 7},	// y
{'\x43', '\x3A', 7},	// C
{'\x2B', '\x38', 7},	// +
{'\x3F', '\x1B', 8},	// ?
{'\x78', '\x1A', 8},	// x
{'\x59', '\x19', 8},	// Y
{'\x6A', '\x18', 8},	// j
{'\x25', '\x17', 8},	// %
{'\x7E', '\x16', 8},	// ~
{'\x5A', '\x15', 8},	// Z
{'\x41', '\x14', 8},	// A
};

// global variables used to accumulate codewords until 8 bits is reached, at which point
// the byte is written to output file
char accum_len = 0;
char accum_char = 0;

/**
 * This function appends given character's codeword to accum_char.
 * Once accum_char reaches 8 bits, the value is written to output file.
 */
void compress_char(unsigned char c, unsigned char max_chars, STRING_MAPPING mapping[], FILE * output)
{
	// go through the array to find equivalent code for current character
	unsigned char i;
	for (i = 0; i < max_chars; i++)
	{
		if (c == mapping[i].original)
		{
			// write equivalent code
			accum_char = accum_char | (mapping[i].code >> accum_len); // append codeword
			accum_len += mapping[i].codelen;
			if (accum_len >= 8)
			{
				fputc(accum_char, output);
				accum_len %= 8;
				accum_char = 0 | (mapping[i].code << (mapping[i].codelen - accum_len));
			}
			break;
		}
	}
}

int main(int argc, char * argv[])
{
	FILE * infile = NULL;
	FILE * outfile = NULL;
	char buffer[BUFFER_SIZE];

	infile = fopen(argv[1], "r");
	outfile = fopen(argv[2], "w");

	if (infile == NULL || outfile == NULL)
	{
		fclose(infile);
		fclose(outfile);
		fprintf(stderr, "Failed to open file.\n");
		return EXIT_FAILURE;
	}

	// Start reading dictionary header information until [Data] is reached
	char found = 0;
	while ( !feof(infile) && !found )
	{
		fgets(buffer, BUFFER_SIZE, infile);
		fwrite(buffer, sizeof(char), strlen(buffer), outfile);
		if (strstr(buffer, "[Data]") != NULL)
		{
			found = 1;
			break;
		}
	}

	if (!found)
	{
		// Invalid input file...
		fprintf(stderr, "Invalid input file. [Data] not found.\n");
		return EXIT_FAILURE;
	}

	char street[BUFFER_SIZE];
	char pron[BUFFER_SIZE];

	int i;
	// Start parsing street name and pronounciation, then compress them.
	while ( fgets(buffer, BUFFER_SIZE, infile) != NULL )
	{
		if (buffer[0] == '\"') // if street name has quotation mark in it
			sscanf(buffer, "\"%[^\"]\"\t//%s", &street, &pron);
		else
			sscanf(buffer, "%s\t//%s", &street, &pron);

		// Compress each character of street name and save to output
		for (i = 0; i < strlen(street); i++)
		{
			compress_char(street[i], STREET_NUM_CHARS, street_mapping, outfile);
		}
		compress_char('\n', STREET_NUM_CHARS, street_mapping, outfile); // write End of Word marker

		// Compress each character of pronounciation and save to output
		for (i = 0; i < strlen(pron); i++)
		{
			compress_char(pron[i], PRON_NUM_CHARS, pron_mapping, outfile);
		}
		compress_char('\n', PRON_NUM_CHARS, pron_mapping, outfile); // write End of Word marker
	}

	// if accum_char still has something left then write that to file too
	if (accum_char != 0)
	{
		fputc(accum_char, outfile);
	}

	fclose(infile);
	fclose(outfile);
	return EXIT_SUCCESS;
}
