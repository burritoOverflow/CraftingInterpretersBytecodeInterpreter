#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;  // once flag is set future errors are suppressed
} Parser;

// precedence levels from lowest to highest
typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(void);

// covered at the start of section 17.6
typedef struct {
    ParseFn
        prefix;  // the function to compile a prefix expression starting with a token of that type
    ParseFn infix;  // the function to compile an infix expression whose left operand is followed by
                    // a token of that type
    Precedence
        precedence;  // the precedence of an infix expression that uses that token as an operator
} ParseRule;

Parser parser;

Chunk* compilingChunk;

// forward declarations
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static Chunk* currentChunk(void) {
    return compilingChunk;
}

static void errorAt(Token* token, const char* errorMsg) {
    // additional errors are supressed once panicMode is started
    if (parser.panicMode)
        return;

    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->tokenType == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->tokenType == TOKEN_ERROR) {
        // nothing..
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", errorMsg);
    parser.hadError = true;
}

// Report error at the location of the token we just consumed
static void error(const char* errorMsg) {
    errorAt(&parser.previous, errorMsg);
}

static void errorAtCurrent(const char* errorMsg) {
    errorAt(&parser.current, errorMsg);
}

static void advance(void) {
    parser.previous = parser.current;
    // iterate until reaching a non-error token or reach the end
    for (;;) {
        parser.current = scanToken();
        if (parser.current.tokenType != TOKEN_ERROR)
            break;

        errorAtCurrent(parser.current.start);
    }
}

// conditionally advance iff current parser type is the expected type
// otherwise error detected
static void consume(TokenType tokenType, const char* errorMsg) {
    if (parser.current.tokenType == tokenType) {
        advance();
        return;
    }
    errorAtCurrent(errorMsg);
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitReturn(void) {
    emitByte(OP_RETURN);
}

// add the value to the constant table and emit `OP_CONSTANT` that pushes it into
// the stack at runtime
static uint8_t makeConstant(Value value) {
    // get the index back from the constant table
    const int constant = addConstant(currentChunk(), value);

    // if too large, we'r at the limit for constants
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk");
        return 0;
    }

    return (uint8_t)constant;
}

static void endCompiler(void) {
    emitReturn();
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

// responsible for compiling the right operand, and emits the
// bytecode that performs the binary operation
static void binary(void) {
    TokenType operatorType = parser.previous.tokenType;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence(rule->precedence + 1);

    switch (operatorType) {
        case TOKEN_BANG_EQUAL:
            emitBytes(OP_EQUAL, OP_NOT);
            break;
        case TOKEN_EQUAL_EQUAL:
            emitByte(OP_EQUAL);
            break;
        case TOKEN_GREATER:
            emitByte(OP_GREATER);
            break;
        case TOKEN_GREATER_EQUAL:
            emitBytes(OP_LESS, OP_NOT);
            break;
        case TOKEN_LESS:
            emitByte(OP_LESS);
            break;
        case TOKEN_LESS_EQUAL:
            emitBytes(OP_GREATER, OP_NOT);
            break;
        case TOKEN_PLUS:
            emitByte(OP_ADD);
            break;
        case TOKEN_MINUS:
            emitByte(OP_SUBTRACT);
            break;
        case TOKEN_STAR:
            emitByte(OP_MULTIPLY);
            break;
        case TOKEN_SLASH:
            emitByte(OP_DIVIDE);
            break;
        default:
            return;  // unreachable
    }
}

// emit the byte for the corresponding literal tokentype
static void literal(void) {
    // parsePrecedence has already consumed the keyword token
    //  so we'll just output the proper instruction
    switch (parser.previous.tokenType) {
        case TOKEN_FALSE:
            emitByte(OP_FALSE);
            break;

        case TOKEN_NIL:
            emitByte(OP_NIL);
            break;

        case TOKEN_TRUE:
            emitByte(OP_TRUE);
            break;

        default:
            return;  // unreachable
    }
}

static void expression(void) {
    // parse the lower precedence level (subsumes all the higher precedences too)
    parsePrecedence(PREC_ASSIGNMENT);
}

static void grouping(void) {
    // assumes the initial '(' was already consumed
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

// emit a constant for the `value` provided
static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

// parse the value from the parser's previous location and emit a constant with
// the parsed value
static void number(void) {
    const double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string() {
    // avoid the leading and trailing `"`
    ObjString* objStr = copyString(parser.previous.start + 1, parser.previous.length - 2);
    Value value = OBJ_VAL(objStr);
    emitConstant(value);
}

// emit a byte for the corresponding unary prefix expression
static void unary(void) {
    // leading '-' has been previously consumed
    TokenType operatorType = parser.previous.tokenType;

    // compile the operand
    parsePrecedence(PREC_UNARY);

    // emit the operator instruction
    switch (operatorType) {
        // `!`
        case TOKEN_BANG:
            emitByte(OP_NOT);
            break;

        // `-`
        case TOKEN_MINUS:
            emitByte(OP_NEGATE);
            break;

        // unreachable
        default:
            return;
    }
}

// prefixFunc, infixFunc, precedence
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, NULL, PREC_NONE},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, NULL, PREC_NONE},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {NULL, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, NULL, PREC_NONE},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, NULL, PREC_NONE},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS] = {NULL, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
    // starts at the current token and parses any expression
    // at the given precedence level or higher
    advance();

    // get the prefix parser for the current token
    ParseFn prefixFn = getRule(parser.previous.tokenType)->prefix;
    if (prefixFn == NULL) {
        error("Expect expression.");
        return;
    }

    prefixFn();

    while (precedence <= getRule(parser.current.tokenType)->precedence) {
        advance();
        ParseFn infixFn = getRule(parser.previous.tokenType)->infix;
        infixFn();
    }
}

static ParseRule* getRule(TokenType tokenType) {
    return &rules[tokenType];
}

bool compile(const char* source, Chunk* chunk) {
    initScanner(source);
    // module variable chunk initialized before writing bytecode
    compilingChunk = chunk;

    // initialize fields when compilation starts
    parser.hadError = false;
    parser.panicMode = false;

    advance();
    expression();
    consume(TOKEN_EOF, "Expect end of expression");
    endCompiler();

    return !parser.hadError;
}
