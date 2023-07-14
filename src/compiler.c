#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

/*
 statement   → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | returnStmt
               | whileStmt
               | block ;

block -> "{" declaration "}" ;

declaration    → classDecl
              | funDecl
              | varDecl
              | statement ;
 */
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

typedef void (*ParseFn)(bool canAssign);

// covered at the start of section 17.6
typedef struct {
    ParseFn prefix;         // the function to compile a prefix expression starting with a
                            // token of that type
    ParseFn infix;          // the function to compile an infix expression whose left
                            // operand is followed by a token of that type
    Precedence precedence;  // the precedence of an infix expression that uses that
                            // token as an operator
} ParseRule;

typedef struct {
    Token name;       // identifier's lexeme
    int depth;        // the scope depth of the block, where the local was declared
    bool isCaptured;  // is a given local captured by a closure
} Local;

typedef struct {
    uint8_t index;  // the slot the upvalue is capturing
    bool isLocal;
} Upvalue;

// support both top-level code (implicit function) and declared functions
typedef enum { TYPE_FUNCTION, TYPE_SCRIPT, TYPE_METHOD, TYPE_INITIALIZER } FunctionType;

typedef struct Compiler {
    struct Compiler*
        enclosing;  // each Compiler points to the Compiler for the function that encloses it
    ObjFunction* function;
    FunctionType functionType;
    Local locals[UINT8_COUNT];  // store all locals that are in scope during each
                                // point in compilation, ordered by declarations in
                                // code
    int localCount;             // number of locals in use
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;  // 0 is global scope, etc
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    bool hasSuperclass;
} ClassCompiler;

Parser parser;

Compiler* currentCompiler = NULL;

ClassCompiler* currentClass = NULL;

// forward declarations
static ParseRule* getRule(TokenType type);

static void statement(void);

static void declaration(void);

static void expression(void);

static void variable(bool canAssign);

static void declareVariable(void);

static void defineVariable(uint8_t global);

static void namedVariable(Token name, bool canAssign);

static Token syntheticToken(const char* text);

static uint8_t argumentList(void);

static uint8_t identifierConstant(Token* name);

static bool identifiersEqual(const Token* a, const Token* b);

static uint8_t parseVariable(const char* errMsg);

static void parsePrecedence(Precedence precedence);

static int resolveLocal(Compiler* compiler, Token* name);

static void addLocal(Token name);

static void patchJump(int offset);

static void and_(bool canAssign);

static void markInitialized(void);

static void initCompiler(Compiler* compiler, FunctionType FunctionType);

static int resolveUpvalue(Compiler* compiler, Token* name);

// end forward declarations

static const int MAX_ARITY = 255;

static Chunk* currentChunk(void) {
    /* create and return a function that contains the compiled top-level code
        current chunk is always the chunk owned by the function
        we're in the middle of compiling
    */
    return &currentCompiler->function->chunk;
}

static void errorAt(Token* token, const char* errorMsg) {
    // subsequent errors are suppressed once panicMode is started
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

    // iterate until reaching a non-error token or until we've reached the end
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

static bool check(TokenType tokenType) {
    return parser.current.tokenType == tokenType;
}

static bool match(TokenType type) {
    if (!check(type))
        return false;

    advance();
    return true;
}

// Write the given byte (OPCODE or OPERAND to and instruction
// provide the line information so errors are associated with the line
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitLoop(const int loopStart) {
    emitByte(OP_LOOP);

    // jump backward by a given offset
    const int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) {
        error("Loop body too large.");
    }

    emitByte((uint8_t)(offset >> 8) & 0xff);
    emitByte((uint8_t)offset & 0xff);
}

// emit a bytecode operand and write placeholder for the jump offset
static int emitJump(uint8_t instruction) {
    emitByte(instruction);

    // two bytes for the jump offset operand (placeholder operand)
    emitByte(0xff);
    emitByte(0xff);

    return currentChunk()->count - 2;
}

static void emitReturn(void) {
    // implictly returns nil if no implicit return value specified
    if (currentCompiler->functionType == TYPE_INITIALIZER) {
        // load slot 0 which contains the instance
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }

    emitByte(OP_RETURN);
}

// add the value to the constant table and emit `OP_CONSTANT` that pushes it
// into the stack at runtime
static uint8_t makeConstant(Value value) {
    // get the index back from the constant table
    const int constant = addConstant(currentChunk(), value);

    // if too large, we're at the limit for constants
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk");
        return 0;
    }

    return (uint8_t)constant;
}

static ObjFunction* endCompiler(void) {
    emitReturn();
    ObjFunction* function = currentCompiler->function;

#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        // implicit top-level function does not have a name (unlike-user defined functions)
        disassembleChunk(currentChunk(), function->functionName != NULL
                                             ? function->functionName->chars
                                             : "<script>");
    }
#endif

    // once this compiler is finished restore the previous compiler
    currentCompiler = (Compiler*)currentCompiler->enclosing;
    return function;
}

// all that's done to create a scope is to increment the current depth
static void beginScope(void) {
    currentCompiler->scopeDepth++;
}

// and the opposite for ending a scope; dispose of locals when the scope is
// exited
static void endScope(void) {
    currentCompiler->scopeDepth--;

    // iterate backward through the local array looking for
    // variables in the scope we've just exited
    while (currentCompiler->localCount > 0 &&
           currentCompiler->locals[currentCompiler->localCount - 1].depth >
               currentCompiler->scopeDepth) {
        // slot is no longer needed when local goes out of scope (remove from
        // runtime stack)
        if (currentCompiler->locals[currentCompiler->localCount - 1].isCaptured) {
            // when the scope ends and emits code to free the stack slots for the locals
            // emit instruction to hoist those that need to be closed over to the heap
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }
        // discard the locals by simply decrementing the count
        currentCompiler->localCount--;
    }
}

// responsible for compiling the right operand, and emits the
// bytecode that performs the binary operation
static void binary(__attribute__((unused)) bool canAssign) {
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
            __builtin_unreachable();
    }
}

static void call(__attribute__((unused)) bool canAssign) {
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    const uint8_t name = identifierConstant(&parser.previous);

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_PROPERTY, name);
    } else if (match(TOKEN_LEFT_PAREN)) {
        // compiler has parsed the identifier (prop name)
        // with a left paren found, omit `OP_INVOKE`
        const uint8_t argCount = argumentList();
        emitBytes(OP_INVOKE, name);
        emitByte(argCount);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
    }
}

// emit the byte for the corresponding literal token type
static void literal(__attribute__((unused)) bool canAssign) {
    // parsePrecedence has already consumed the keyword token,
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
            __builtin_unreachable();
    }
}

static void expression(void) {
    // parse the lower precedence level (subsumes all the higher precedences too)
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block(void) {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType functionType) {
    // a compiler for each function (see section 24.4.1)
    Compiler compiler;
    initCompiler(&compiler, functionType);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    // handle function's parameters
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            // for each parameter
            currentCompiler->function->arity++;
            if (currentCompiler->function->arity > MAX_ARITY) {
                errorAtCurrent("Can't have more than 255 function parameters.");
            }

            const uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    // yields the newly compiled function object and stores as a constant in the surrounding
    // function's constant table
    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++) {
        // each closure has two single byte operands
        // if first byte is 1 - captures a local variable, is 0 - captures one of the function's
        // upvalues; next byte is the local slot or upvalue idx to capture
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

static void method(void) {
    // method requires three things: name, closure, class to bind method to.

    // first handle the name for the method
    consume(TOKEN_IDENTIFIER, "Expect method name.");
    const uint8_t constant = identifierConstant(&parser.previous);

    FunctionType functionType = TYPE_METHOD;

    // determine if the method is the initializer
    if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
        functionType = TYPE_INITIALIZER;
    }  // otherwise, it's just a normal method

    function(functionType);

    emitBytes(OP_METHOD, constant);
}

static void classDeclaration(void) {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    const Token className = parser.previous;
    const uint8_t nameConstant = identifierConstant(&parser.previous);
    declareVariable();

    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant);

    ClassCompiler classCompiler;
    classCompiler.hasSuperclass = false;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    if (match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expect superclass name.");
        variable(false);

        if (identifiersEqual(&className, &parser.previous)) {
            error("A class cannot inherit from itself.");
        }

        beginScope();
        addLocal(syntheticToken("super"));
        defineVariable(0);

        namedVariable(className, false);
        emitByte(OP_INHERIT);
        classCompiler.hasSuperclass = true;
    }

    // generate code to load the variable with the given name on the stack
    namedVariable(className, false);

    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        // lox does not support field declarations, so anything
        // prior to the closing brace must be a method
        method();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");

    // all methods consumed; we no longer need the class, so pop it.
    emitByte(OP_POP);

    // pop the scope and disvard "super" after compilation of the class body
    if (classCompiler.hasSuperclass) {
        endScope();
    }

    currentClass = currentClass->enclosing;
}

// create and store a function in a newly created variable
static void funDeclaration(void) {
    const uint8_t global = parseVariable("Expect function name.");
    // mark initialized now so that the name can be referenced in the body w/o error
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global);
}

static void varDeclaration(void) {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);  // de-sugars into var a = nil;
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global);
}

// an expression followed by a semicolon (expression where statement is
// expected)
static void expressionStatement(void) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression");
    emitByte(OP_POP);
}

static void forStatement(void) {
    // if a for statement declares a variable, it should be scoped to loop body
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

    // begin initializer clause
    if (match(TOKEN_SEMICOLON)) {
        // no initializer
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;

    // the condition expression that can exit the loop
    int exitJump = -1;

    // begin condition
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // exit the loop if the condition is false
        exitJump = emitJump(OP_JUMP_IF_FALSE);

        // pop the condition
        emitByte(OP_POP);
    }  // otherwise no expression present

    // begin increment clause
    if (!match(TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }  // no increment clause present

    statement();
    emitLoop(loopStart);

    // patch the jump when there is a condition clause
    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP);  // pop the condition
    }                      // otherwise, no condition was present

    // end the scope for the declared variable
    endScope();
}

static void ifStatement(void) {
    // 'if' already consumed; consume the expression in the parentheses
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    const int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();

    const int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);
    emitByte(OP_POP);

    if (match(TOKEN_ELSE))
        statement();

    patchJump(elseJump);
}

static void printStatement(void) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void returnStatement(void) {
    if (currentCompiler->functionType == TYPE_SCRIPT) {
        error("Can't return from top-level.");
    }

    if (match(TOKEN_SEMICOLON)) {
        // implicit return (returns nil)
        emitReturn();
    } else {
        if (currentCompiler->functionType == TYPE_INITIALIZER) {
            error("Can't return a value from an initializer.");
        }

        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

static void whileStatement(void) {
    // after executing the loop body, we jump back to before the condition
    // evaluate the loop condition after each iteration
    const int loopStart = currentChunk()->count;

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    const int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();

    // restart the iteration
    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OP_POP);
}

// synchronize to avoid cascading compiler errors
static void synchronize(void) {
    parser.panicMode = false;

    // skip tokens until we reach a statement boundary
    while (parser.current.tokenType != TOKEN_EOF) {
        if (parser.previous.tokenType == TOKEN_SEMICOLON)
            return;

        switch (parser.current.tokenType) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;

            default:;
        }
    }
}

static void declaration(void) {
    if (match(TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) {
        synchronize();
    }
}

static void statement(void) {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        // we've encountered a block
        // create a scope and evaluate statements w/in the block
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

static void grouping(__attribute__((unused)) bool canAssign) {
    // assumes the initial '(' was already consumed
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

// emit a constant for the `value` provided
static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

// replace the operand at the given location with the calculated jump offset (23.1)
static void patchJump(const int offset) {
    // -2 to adjust for bytecode from the jump offset itself
    const int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset] = (uint8_t)(jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = (uint8_t)jump & 0xff;
}

// set the initial state of `compiler` to a zero-state.
static void initCompiler(Compiler* compiler, FunctionType functionType) {
    // store the compiler that shortly will no longer be current
    compiler->enclosing = (struct Compiler*)currentCompiler;
    compiler->function = NULL;
    compiler->functionType = functionType;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();

    // and set this compiler as the current
    currentCompiler = compiler;

    // if a non top-level function, set the function's name
    if (functionType != TYPE_SCRIPT) {
        currentCompiler->function->functionName =
            copyString(parser.previous.start, parser.previous.length);
    }

    Local* local = &currentCompiler->locals[currentCompiler->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if (functionType != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

// parse the value from the parser's previous location and emit a constant with
// the parsed value
static void number(__attribute__((unused)) bool canAssign) {
    const double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void or_(__attribute__((unused)) bool canAssign) {
    const int elseJump = emitJump(OP_JUMP_IF_FALSE);
    const int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

static void string(__attribute__((unused)) bool canAssign) {
    // avoid the leading and trailing `"`
    ObjString* objStr = copyString(parser.previous.start + 1, parser.previous.length - 2);
    Value value = OBJ_VAL(objStr);
    emitConstant(value);
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(currentCompiler, &name);

    // local found with `name`, so emit instructions for dealing with a local var
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(currentCompiler, &name)) != -1) {
        // resolution step for outer local scopes
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        // otherwise it's a global
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

static void super_(__attribute__((unused)) bool canAssign) {
    if (currentClass == NULL) {
        error("Cannot use 'super' outside of a class.");
    } else if (!currentClass->hasSuperclass) {
        error("Cannot use 'super' in a class with no superclass.");
    }

    consume(TOKEN_DOT, "Expect '.' after 'super'.");
    consume(TOKEN_IDENTIFIER, "Expect superclass after class name.");
    const uint8_t name = identifierConstant(&parser.previous);

    namedVariable(syntheticToken("this"), false);

    // see 29.3.2
    // same optimatization approach as method calls
    if (match(TOKEN_LEFT_PAREN)) {
        uint8_t argcount = argumentList();
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_SUPER_INVOKE, name);
        emitByte(argcount);
    } else {
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_GET_SUPER, name);
    }
}

static void this_(__attribute__((unused)) bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'this' outside of a class.");
        return;
    }

    // details in section 28.3
    variable(false);
}

// emit a byte for the corresponding unary prefix expression
static void unary(__attribute__((unused)) bool canAssign) {
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

        default:
            __builtin_unreachable();
    }
}

// prefixFunc, infixFunc, precedence
/*
    1. function to compile a prefix expression starting with a token of that type
    2. function to compile an infix expression whose left operand is followed by
        a token of that type
    3. the precedence of an infix expression that uses that token as an operator
*/
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, dot, PREC_CALL},
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
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, and_, PREC_AND},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, or_, PREC_OR},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {super_, NULL, PREC_NONE},
    [TOKEN_THIS] = {this_, NULL, PREC_NONE},
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
        // syntax error; missing prefix parser
        error("Expect expression.");
        return;
    }

    // compile the rest of the prefix expression (see 17.61)
    const bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixFn(canAssign);

    // infix expressions
    while (precedence <= getRule(parser.current.tokenType)->precedence) {
        // iterate while valid infix operators (see 17.6.1)
        advance();
        ParseFn infixFn = getRule(parser.previous.tokenType)->infix;
        infixFn(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL))
        error("Invalid assignment target.");
}

// takes the given token, adds the lexeme to the constant table and returns the
// corresponding index
static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(const Token* a, const Token* b) {
    if (a->length != b->length)
        return false;

    return memcmp(a->start, b->start, a->length) == 0;
}

// walk the list of identifiers in the current scope; return the index if found; -1 otherwise
static int resolveLocal(Compiler* compiler, Token* name) {
    // walk backwards, so we find the last declared variable with the identifier
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];

        // the runtime stack slot index is the same as the locals index
        // so return the found index in compiler's locals
        if (identifiersEqual(name, &local->name)) {
            // edge case
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    const int upvalueCount = compiler->function->upvalueCount;

    // before adding a new upvalue check if there exists an upvalue that closes over that variable
    // return upvalue idx if true
    for (int i = 0; i < upvalueCount; i++) {
        const Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    // add new upvalue if not present
    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

// Resolve the upvalue identifier by looking for a local variable declared in
// any of the caller's enclosing functions; return the "upvalue index" if found, "-1" otherwise.
static int resolveUpvalue(Compiler* compiler, Token* name) {
    // reached the outermost function w/o finding a local variable (must be global)
    if (compiler->enclosing == NULL)
        return -1;

    // base case: a local variable is found in the enclosing function
    const int local = resolveLocal((Compiler*)compiler->enclosing, name);
    if (local != -1) {
        // upvalue for local is marked as captured
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    // otherwise, we look for a local variable in the enclosing function
    // eventually, we either find a local variable or run out of enclosing compilers
    // detailed in 25.2.2
    const int upvalue = resolveUpvalue((Compiler*)compiler->enclosing, name);
    if (upvalue != -1) {
        // upvalue from an enclosing function (so isLocal == false)
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

// add to the compiler's list of variables in the current scope
static void addLocal(Token name) {
    if (currentCompiler->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    // initialize the next available Local in the compiler's array of variables
    Local* local = &currentCompiler->locals[currentCompiler->localCount++];
    local->name = name;

    // special sentinel value indicating that the variable is in an "unitialized"
    // state
    local->depth = -1;
    local->isCaptured = false;
}

static void declareVariable(void) {
    if (currentCompiler->scopeDepth == 0)
        return;

    Token* name = &parser.previous;

    // current scope is always at the end of the array
    for (int i = currentCompiler->localCount - 1; i >= 0; i--) {
        Local* local = &currentCompiler->locals[i];

        if (local->depth != -1 && local->depth < currentCompiler->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("A variable with this identifier already exists in this scope");
        }
    }

    addLocal(*name);
}

static uint8_t parseVariable(const char* errMsg) {
    consume(TOKEN_IDENTIFIER, errMsg);

    declareVariable();

    if (currentCompiler->scopeDepth > 0)
        return 0;

    return identifierConstant(&parser.previous);
}

// define a variable as "available" for use
static void markInitialized(void) {
    if (currentCompiler->scopeDepth == 0)
        return;

    currentCompiler->locals[currentCompiler->localCount - 1].depth = currentCompiler->scopeDepth;
}

// output instruction for defining the new variable and store the initial value
static void defineVariable(uint8_t global) {
    if (currentCompiler->scopeDepth > 0) {
        markInitialized();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

// compile the arguments to the function
static uint8_t argumentList(void) {
    uint8_t argCount = 0;

    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            // evaluate each argument to the function
            expression();
            if (argCount == MAX_ARITY) {
                error("Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));  // continue as long as commas are present
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

static void and_(__attribute__((unused)) bool canAssign) {
    // at this point the left expression has already been compiled
    // if left value is falsey left value is the result of the expression
    const int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

static ParseRule* getRule(TokenType tokenType) {
    return &rules[tokenType];
}

ObjFunction* compile(const char* source) {
    initScanner(source);

    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    // initialize fields when compilation starts
    parser.hadError = false;
    parser.panicMode = false;

    advance();
    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();

    return parser.hadError ? NULL : function;
}

void markCompilerRoots(void) {
    Compiler* compiler = currentCompiler;

    while (compiler != NULL) {
        // mark the compiler's list of ObjFuncs it's compiling
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}
