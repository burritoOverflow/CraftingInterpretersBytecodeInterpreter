#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

typedef struct {
    const char* start;    // beginning of the current lexeme
    const char* current;  // current character in the lexeme
    int line;             // the line the lexeme is on
} Scanner;

Scanner scanner;

// start the scanner at the first character of the first line
void initScanner(const char* source) {
    scanner.start = source;
    scanner.current = source;
    scanner.line = 1;
}

static bool isAtEnd(void) {
    return *scanner.current == '\0';
}

// Consume the current character and return it
static char advance(void) {
    scanner.current++;
    return scanner.current[-1];
}

// get, but do not consume, the scanner's current character
static char peek(void) {
    return *scanner.current;
}

// get, but do not consume, the character one index greater than current
static char peekNext(void) {
    if (isAtEnd()) {
        return '\0';
    }
    return scanner.current[1];
}

// conditionally consume the seconds token if it matches
static bool match(char expected) {
    if (isAtEnd()) {
        return false;
    }

    if (*scanner.current != expected) {
        return false;
    }

    scanner.current++;
    return true;
}

// create a token from the provided tokentype
static Token makeToken(TokenType tokenType) {
    Token token;
    token.tokenType = tokenType;
    token.start = scanner.start;
    token.length = (int)(scanner.current - scanner.start);
    token.line = scanner.line;
    return token;
}

// create a token corresponding to an error
static Token errorToken(const char* message) {
    Token token;
    token.tokenType = TOKEN_ERROR;
    token.start = message;
    token.length = strlen(message);
    token.line = scanner.line;
    return token;
}

// advance the scanner past any leading whitespace
static void skipWhitespace(void) {
    for (;;) {
        const char c = peek();
        switch (c) {
            // consume whitespace characters
            case ' ':
            case '\r':
            case '\t':
                advance();
                break;

            // consume a newline
            case '\n':
                scanner.line++;
                advance();
                return;

            case '/':
                // comment found
                if (peekNext() == '/') {
                    // consume the comment (end of the current line)
                    while (peek() != '\n' && !isAtEnd()) {
                        advance();
                    }
                } else {
                    return;
                }
                break;

            default:
                return;
        }
    }
}

Token scanToken(void) {
    skipWhitespace();
    scanner.start = scanner.current;
    if (isAtEnd()) {
        return makeToken(TOKEN_EOF);
    }

    const char c = advance();

    switch (c) {
        case '(':
            return makeToken(TOKEN_LEFT_PAREN);

        case ')':
            return makeToken(TOKEN_RIGHT_PAREN);

        case '{':
            return makeToken(TOKEN_LEFT_BRACE);

        case '}':
            return makeToken(TOKEN_RIGHT_BRACE);

        case ';':
            return makeToken(TOKEN_SEMICOLON);

        case ',':
            return makeToken(TOKEN_COMMA);

        case '.':
            return makeToken(TOKEN_DOT);

        case '-':
            return makeToken(TOKEN_MINUS);

        case '+':
            return makeToken(TOKEN_PLUS);

        case '/':
            return makeToken(TOKEN_SLASH);

        case '*':
            return makeToken(TOKEN_STAR);

        case '!':
            return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);

        case '=':
            return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);

        case '<':
            return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);

        case '>':
            return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
    }

    return errorToken("Unexpected character");
}
