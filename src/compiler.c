#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

void compile(const char* source) {
    initScanner(source);
    int line = -1;

    for (;;) {
        Token token = scanToken();
        if (token.line != line) {
            printf("%4d ", token.line);
            line = token.line;
        } else {
            printf("   | ");
        }

        // the first column in the output is the line number,
        // second is numeric value of the tokentype, third is lexeme
        printf("%2d '%.*s'\n", token.tokenType, token.length, token.start);

        if (token.tokenType == TOKEN_EOF) {
            break;
        }
    }
}
