/*
***** BEGIN LICENSE BLOCK *****

This file is part of the erlXPCOM (Erlang XPCOM binding) project.

Copyright (C) 2005 Hector Rivas Gandara <keymon@gmail.com>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published replyby the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

***** END LICENSE BLOCK *****
*/
#include <stdlib.h>
#include "command_line_tokenizer.h"

/** Tokenize the command line */
char **comand_line_tokenizer(const char *line) {
	const int MAX_ARGS = 256;
	const int ARGSIZE = 256;
	
	char **args = (char **)malloc(MAX_ARGS*sizeof(char*));
	int i;
	char last = 0; 
	char instring = 0; 
	char **args_p = args;
	const char *p = line;

	while (1) {
		i = 0;
		last = 0;
		*args_p = (char *) malloc(255*sizeof(char));
		while (*p && (!(*p == ' ' || *p=='\n') || instring)) {

			if ((*p == '\"' || *p == '\'') && last!='\\') {
				instring = 
					instring? (instring == *p? 0: instring): *p;
				p++;
				continue;
			}

			last = (*args_p)[i++] = *p++;

			if (i%ARGSIZE == 0) {
				*args_p = (char *) realloc(*args_p, (i+255)*sizeof(char));
			}
		}
		(*args_p++)[i] = 0;
		if (*p) p++;
		else break;
	}
	*args_p = 0;

	return args;
}

void argv_free(char**argv) {
	char **p = argv;
	while (*p) free(*p++);
	free(argv);
}

