#include <stdbool.h>
#include <string.h>

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

static bool isAlpha(const char c) {
    // allow identifiers with a leading underscore
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

static bool isDigit(const char c) {
    return c >= '0' && c <= '9';
}

// determine if we are at the end of the string
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

                // consume additional newlines
                while (peek() == '\n') {
                    advance();
                }
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

static TokenType checkKeyword(int start, int length, const char* rest, TokenType tokenType) {
    if (scanner.current - scanner.start == start + length &&
        memcmp(scanner.start + start, rest, length) == 0) {
        return tokenType;
    }
    return TOKEN_IDENTIFIER;
}

static TokenType identifierType(void) {
    switch (scanner.start[0]) {
        // initial letters that correspond to keywords
        case 'a':
            return checkKeyword(1, 2, "nd", TOKEN_AND);

        case 'c':
            return checkKeyword(1, 4, "lass", TOKEN_CLASS);

        case 'e':
            return checkKeyword(1, 3, "lse", TOKEN_ELSE);

        case 'f':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'a':
                        return checkKeyword(2, 3, "lse", TOKEN_FALSE);

                    case 'o':
                        return checkKeyword(2, 1, "r", TOKEN_FOR);

                    case 'u':
                        return checkKeyword(2, 1, "n", TOKEN_FUN);
                }
            }
            // fallthrough to identifier
            break;

        case 'i':
            return checkKeyword(1, 1, "f", TOKEN_IF);

        case 'n':
            return checkKeyword(1, 2, "il", TOKEN_NIL);

        case 'o':
            return checkKeyword(1, 1, "r", TOKEN_OR);

        case 'p':
            return checkKeyword(1, 4, "rint", TOKEN_PRINT);

        case 'r':
            return checkKeyword(1, 5, "eturn", TOKEN_RETURN);

        case 's':
            return checkKeyword(1, 4, "uper", TOKEN_SUPER);

        case 't':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'h':
                        return checkKeyword(2, 2, "is", TOKEN_THIS);

                    case 'r':
                        return checkKeyword(2, 2, "ue", TOKEN_TRUE);
                }
            }

        case 'v':
            return checkKeyword(1, 2, "ar", TOKEN_VAR);

        case 'w':
            return checkKeyword(1, 4, "hile", TOKEN_WHILE);
    }

    // not a keyword, therefore identifier
    return TOKEN_IDENTIFIER;
}

static Token number(void) {
    while (isDigit(peek())) {
        advance();
    }

    // look for a fractional part
    if (peek() == '.' && isDigit(peekNext())) {
        // consume the '.'
        advance();

        // consume the remaining digits
        while (isDigit(peek())) {
            advance();
        }
    }

    return makeToken(TOKEN_NUMBER);
}

static Token string(void) {
    // consume each character in the string
    while (peek() != '"' && !isAtEnd()) {
        if (peek() == '\n') {
            // support multi-line strings
            scanner.line++;
        }
        advance();
    }

    // string ended before encountering closing '"'
    if (isAtEnd()) {
        return errorToken("Unterminated string");
    }

    // consume the closing quote
    advance();
    return makeToken(TOKEN_STRING);
}

static Token identifier(void) {
    // consume the characters in the identifier
    while (isAlpha(peek()) || isDigit(peek())) {
        advance();
    }

    return makeToken(identifierType());
}

Token scanToken(void) {
    skipWhitespace();
    scanner.start = scanner.current;
    if (isAtEnd()) {
        return makeToken(TOKEN_EOF);
    }

    const char c = advance();

    if (isDigit(c)) {
        return number();
    }

    if (isAlpha(c)) {
        return identifier();
    }

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

        case '"':
            return string();

        default:
            return errorToken("Unexpected character.");
    }
}
