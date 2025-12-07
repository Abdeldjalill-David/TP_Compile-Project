import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/*
 J'AI UTULISER LA METHODE DE PREDICTION PARSIING (RECURSIVE DESCENT PARSING)
*/

public class Main {

    
    // Token types
    
    enum TokenType {
        EOF,
        IDENT, NUMBER, STRING,
        IF, ELSE, WHILE, FOR, RETURN,
        INT, FLOAT, DOUBLE, CHAR, BOOL, VOID,
        TRUE, FALSE, NULL,
        PLUS, MINUS, STAR, SLASH, PERCENT,
        PLUSPLUS, MINUSMINUS,
        ASSIGN, PLUSEQ, MINUSEQ, STAREQ, SLASHEQ,
        EQEQ, NOTEQ, LT, GT, LTE, GTE,
        BANG, ANDAND, OROR,
        SEMI, COMMA, LPAREN, RPAREN, LBRACE, RBRACE, LBRACK, RBRACK
    }

    
    // Token
    
    static class Token {
        TokenType type;
        String lexeme;
        int line;
        int col;
        Token(TokenType t, String l, int line, int col) {
            this.type = t; this.lexeme = l; this.line = line; this.col = col;
        }
        public String toString() {
            return type + "('" + lexeme + "')@" + line + ":" + col;
        }
    }

    
    // Lexer
   
    static class Lexer {
        String src;
        int i;
        int line;
        int col;
        List<Token> tokens = new ArrayList<>();
        List<String> errors = new ArrayList<>();

        Lexer(String s) {
            src = s;
            i = 0; line = 1; col = 1;
        }

        boolean isAtEnd() { return i >= src.length(); }
        char peek() { return isAtEnd() ? '\0' : src.charAt(i); }
        char peekNext() { return (i+1 >= src.length()) ? '\0' : src.charAt(i+1); }
        char advance() {
            char c = peek();
            i++;
            if (c == '\n') { line++; col = 1; } else col++;
            return c;
        }

        void addToken(TokenType t, String lex) {
            tokens.add(new Token(t, lex, line, Math.max(1, col - lex.length())));
        }

        void addError(String msg) { errors.add("Lex error at " + line + ":" + col + " - " + msg); }

        boolean isLetter(char c) {
            return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_';
        }
        boolean isDigit(char c) { return (c >= '0' && c <= '9'); }

        void skipWhitespaceAndComments() {
            while (!isAtEnd()) {
                char c = peek();
                if (c == ' ' || c == '\t' || c == '\r' || c == '\n') { advance(); continue; }
                if (c == '/' && peekNext() == '/') {
                    // line comment
                    advance(); advance();
                    while (!isAtEnd() && peek() != '\n') advance();
                    continue;
                }
                if (c == '/' && peekNext() == '*') {
                    // block comment
                    advance(); advance();
                    while (!isAtEnd()) {
                        if (peek() == '*' && peekNext() == '/') { advance(); advance(); break; }
                        advance();
                    }
                    continue;
                }
                break;
            }
        }

        List<Token> tokenize() {
            while (!isAtEnd()) {
                skipWhitespaceAndComments();
                if (isAtEnd()) break;
                char c = peek();

                // identifiers / keywords
                if (isLetter(c)) {
                    int startLine = line, startCol = col;
                    StringBuilder sb = new StringBuilder();
                    while (!isAtEnd() && (isLetter(peek()) || isDigit(peek()))) sb.append(advance());
                    String w = sb.toString();
                    // keywords via if-else (no HashMap)
                    if (w.equals("if")) addToken(TokenType.IF, w);
                    else if (w.equals("else")) addToken(TokenType.ELSE, w);
                    else if (w.equals("while")) addToken(TokenType.WHILE, w);
                    else if (w.equals("for")) addToken(TokenType.FOR, w);
                    else if (w.equals("return")) addToken(TokenType.RETURN, w);
                    else if (w.equals("int")) addToken(TokenType.INT, w);
                    else if (w.equals("float")) addToken(TokenType.FLOAT, w);
                    else if (w.equals("double")) addToken(TokenType.DOUBLE, w);
                    else if (w.equals("char")) addToken(TokenType.CHAR, w);
                    else if (w.equals("bool")) addToken(TokenType.BOOL, w);
                    else if (w.equals("void")) addToken(TokenType.VOID, w);
                    else if (w.equals("true")) addToken(TokenType.TRUE, w);
                    else if (w.equals("false")) addToken(TokenType.FALSE, w);
                    else if (w.equals("NULL")) addToken(TokenType.NULL, w);
                    else addToken(TokenType.IDENT, w);
                    continue;
                }

                // numbers
                if (isDigit(c)) {
                    StringBuilder sb = new StringBuilder();
                    while (!isAtEnd() && isDigit(peek())) sb.append(advance());
                    if (!isAtEnd() && peek() == '.') {
                        sb.append(advance());
                        if (!isAtEnd() && isDigit(peek())) {
                            while (!isAtEnd() && isDigit(peek())) sb.append(advance());
                        } else addError("malformed number");
                    }
                    addToken(TokenType.NUMBER, sb.toString());
                    continue;
                }

                // strings
                if (c == '"') {
                    advance(); // consume "
                    StringBuilder sb = new StringBuilder();
                    while (!isAtEnd() && peek() != '"') {
                        char ch = advance();
                        if (ch == '\\' && !isAtEnd()) {
                            char esc = advance();
                            sb.append('\\'); sb.append(esc);
                        } else sb.append(ch);
                    }
                    if (isAtEnd()) addError("Unterminated string");
                    else advance(); // closing "
                    addToken(TokenType.STRING, sb.toString());
                    continue;
                }

                // multi-char operators and single-char
                if (c == '+') {
                    advance();
                    if (peek() == '+') { advance(); addToken(TokenType.PLUSPLUS, "++"); }
                    else if (peek() == '=') { advance(); addToken(TokenType.PLUSEQ, "+="); }
                    else addToken(TokenType.PLUS, "+");
                    continue;
                }
                if (c == '-') {
                    advance();
                    if (peek() == '-') { advance(); addToken(TokenType.MINUSMINUS, "--"); }
                    else if (peek() == '=') { advance(); addToken(TokenType.MINUSEQ, "-="); }
                    else addToken(TokenType.MINUS, "-");
                    continue;
                }
                if (c == '*') {
                    advance();
                    if (peek() == '=') { advance(); addToken(TokenType.STAREQ, "*="); }
                    else addToken(TokenType.STAR, "*");
                    continue;
                }
                if (c == '/') {
                    advance();
                    if (peek() == '=') { advance(); addToken(TokenType.SLASHEQ, "/="); }
                    else addToken(TokenType.SLASH, "/");
                    continue;
                }
                if (c == '%') { advance(); addToken(TokenType.PERCENT, "%"); continue; }

                if (c == '=') {
                    advance();
                    if (peek() == '=') { advance(); addToken(TokenType.EQEQ, "=="); }
                    else addToken(TokenType.ASSIGN, "=");
                    continue;
                }
                if (c == '!') {
                    advance();
                    if (peek() == '=') { advance(); addToken(TokenType.NOTEQ, "!="); }
                    else addToken(TokenType.BANG, "!");
                    continue;
                }
                if (c == '<') {
                    advance();
                    if (peek() == '=') { advance(); addToken(TokenType.LTE, "<="); }
                    else addToken(TokenType.LT, "<");
                    continue;
                }
                if (c == '>') {
                    advance();
                    if (peek() == '=') { advance(); addToken(TokenType.GTE, ">="); }
                    else addToken(TokenType.GT, ">");
                    continue;
                }
                if (c == '&') {
                    advance();
                    if (peek() == '&') { advance(); addToken(TokenType.ANDAND, "&&"); }
                    else addError("Unexpected '&'");
                    continue;
                }
                if (c == '|') {
                    advance();
                    if (peek() == '|') { advance(); addToken(TokenType.OROR, "||"); }
                    else addError("Unexpected '|'");
                    continue;
                }

                // separators
                if (c == ';') { advance(); addToken(TokenType.SEMI, ";"); continue; }
                if (c == ',') { advance(); addToken(TokenType.COMMA, ","); continue; }
                if (c == '(') { advance(); addToken(TokenType.LPAREN, "("); continue; }
                if (c == ')') { advance(); addToken(TokenType.RPAREN, ")"); continue; }
                if (c == '{') { advance(); addToken(TokenType.LBRACE, "{"); continue; }
                if (c == '}') { advance(); addToken(TokenType.RBRACE, "}"); continue; }
                if (c == '[') { advance(); addToken(TokenType.LBRACK, "["); continue; }
                if (c == ']') { advance(); addToken(TokenType.RBRACK, "]"); continue; }

                // unknown char
                addError("Unknown character: '" + c + "'");
                advance();
            }

            // EOF token
            tokens.add(new Token(TokenType.EOF, "<EOF>", line, col));
            return tokens;
        }
    }

   
    // Parser (recursive-descent)
    
    static class Parser {
        List<Token> tokens;
        int pos;
        Token look;
        List<String> errors = new ArrayList<>();

        Parser(List<Token> toks) {
            tokens = toks;
            pos = 0;
            look = tokens.get(pos);
        }

        void advance() {
            if (pos < tokens.size() - 1) {
                pos++;
                look = tokens.get(pos);
            }
        }

        boolean match(TokenType t) {
            if (look.type == t) { advance(); return true; }
            return false;
        }

        Token consume(TokenType t, String msg) {
            if (look.type == t) {
                Token cur = look; advance(); return cur;
            } else {
                error("Expected " + t + " but found " + look.type + " : " + msg);
                return look; // try to continue
            }
        }

        void error(String msg) {
            errors.add("Syntax error at " + look.line + ":" + look.col + " - " + msg + " near '" + look.lexeme + "'");
            // panic: skip to next ; or } or EOF
            while (look.type != TokenType.SEMI && look.type != TokenType.RBRACE && look.type != TokenType.EOF) {
                advance();
            }
            if (look.type == TokenType.SEMI) advance();
        }

        Token peek(int n) {
            int idx = pos + n;
            if (idx >= tokens.size()) return tokens.get(tokens.size() - 1);
            return tokens.get(idx);
        }

        boolean isType(TokenType t) {
            return t == TokenType.INT || t == TokenType.FLOAT || t == TokenType.DOUBLE
                    || t == TokenType.CHAR || t == TokenType.BOOL || t == TokenType.VOID;
        }

        
        // Program           -> TranslationUnit*
        
        void parseProgram() {
            while (look.type != TokenType.EOF) {
                parseTranslationUnit();
            }
        }

        
        // TranslationUnit   -> ExternalDecl
        
        void parseTranslationUnit() {
            parseExternalDecl();
        }

        
        // ExternalDecl      -> VarDecl | FunctionDecl | Statement
        
        void parseExternalDecl() {
            if (isType(look.type)) {
                // lookahead: if type IDENT '(' -> function, else var
                Token t2 = peek(1);
                Token t3 = peek(2);
                if (t2.type == TokenType.IDENT && t3.type == TokenType.LPAREN) {
                    parseFunctionDecl();
                } else {
                    parseVarDecl();
                }
            } else {
                parseStatement();
            }
        }

        
        // VarDecl -> Type DeclaratorList ';'
        
        void parseVarDecl() {
            parseType();
            parseDeclaratorList();
            consume(TokenType.SEMI, "';' expected after declaration");
        }

        // DeclaratorList -> Declarator (',' Declarator)*
        void parseDeclaratorList() {
            parseDeclarator();
            while (look.type == TokenType.COMMA) {
                advance();
                parseDeclarator();
            }
        }

        // Declarator -> IDENT ( '=' Expression )?
        void parseDeclarator() {
            consume(TokenType.IDENT, "identifier expected in declarator");
            if (look.type == TokenType.ASSIGN || look.type == TokenType.PLUSEQ || look.type == TokenType.MINUSEQ
                    || look.type == TokenType.STAREQ || look.type == TokenType.SLASHEQ) {
                advance();
                parseExpression();
            }
        }

        // Type -> ...
        void parseType() {
            if (isType(look.type)) advance();
            else error("type expected");
        }

        // FunctionDecl -> Type IDENT '(' ParamList? ')' CompoundStmt
        void parseFunctionDecl() {
            parseType();
            consume(TokenType.IDENT, "function name expected");
            consume(TokenType.LPAREN, "'(' expected");
            if (look.type != TokenType.RPAREN) parseParamList();
            consume(TokenType.RPAREN, "')' expected");
            parseCompoundStmt();
        }

        // ParamList -> Param (',' Param)*
        void parseParamList() {
            parseParam();
            while (look.type == TokenType.COMMA) {
                advance();
                parseParam();
            }
        }

        // Param -> Type IDENT
        void parseParam() {
            parseType();
            consume(TokenType.IDENT, "param name expected");
        }

        // Statement -> CompoundStmt | IfStmt | WhileStmt | ForStmt | ExprStmt | ReturnStmt | ';'
        void parseStatement() {
            if (look.type == TokenType.LBRACE) { parseCompoundStmt(); return; }
            if (look.type == TokenType.IF) { parseIfStmt(); return; }
            if (look.type == TokenType.WHILE) { parseWhileStmt(); return; }
            if (look.type == TokenType.FOR) { parseForStmt(); return; }
            if (look.type == TokenType.RETURN) { parseReturnStmt(); return; }
            if (look.type == TokenType.SEMI) { advance(); return; }
            parseExprStmt();
        }

        // CompoundStmt -> '{' Statement* '}'
        void parseCompoundStmt() {
            consume(TokenType.LBRACE, "'{' expected to start block");
            while (look.type != TokenType.RBRACE && look.type != TokenType.EOF) parseStatement();
            consume(TokenType.RBRACE, "'}' expected to close block");
        }

        // IfStmt -> 'if' '(' Expression ')' Statement ElsePart
        // ElsePart -> 'else' Statement | ε
        void parseIfStmt() {
            consume(TokenType.IF, "'if' expected");
            consume(TokenType.LPAREN, "'(' expected after if");
            parseExpression();
            consume(TokenType.RPAREN, "')' expected after condition");
            parseStatement();
            if (look.type == TokenType.ELSE) { advance(); parseStatement(); }
        }

        // WhileStmt -> 'while' '(' Expression ')' Statement
        void parseWhileStmt() {
            consume(TokenType.WHILE, "'while' expected");
            consume(TokenType.LPAREN, "'(' expected after while");
            parseExpression();
            consume(TokenType.RPAREN, "')' expected after condition");
            parseStatement();
        }

        // ForStmt -> 'for' '(' ForInit? ';' ForCond? ';' ForPost? ')' Statement
        void parseForStmt() {
            consume(TokenType.FOR, "'for' expected");
            consume(TokenType.LPAREN, "'(' expected after for");
            // ForInit -> VarDecl | ExprStmt | empty
            if (isType(look.type)) parseVarDecl();
            else parseExprStmt();
            // ForCond
            if (look.type != TokenType.SEMI) parseExpression();
            consume(TokenType.SEMI, "';' expected in for");
            // ForPost
            if (look.type != TokenType.RPAREN) parseExpressionList();
            consume(TokenType.RPAREN, "')' expected after for'");
            parseStatement();
        }

        // ExpressionList -> Expression (',' Expression)*
        void parseExpressionList() {
            parseExpression();
            while (look.type == TokenType.COMMA) { advance(); parseExpression(); }
        }

        // ExprStmt -> Expression? ';'
        void parseExprStmt() {
            if (look.type == TokenType.SEMI) { advance(); return; }
            parseExpression();
            consume(TokenType.SEMI, "';' expected after expression");
        }

        // ReturnStmt -> 'return' Expression? ';'
        void parseReturnStmt() {
            consume(TokenType.RETURN, "'return' expected");
            if (look.type != TokenType.SEMI) parseExpression();
            consume(TokenType.SEMI, "';' expected after return");
        }

        // Expression -> Assignment
        void parseExpression() { parseAssignment(); }

        // Assignment -> LogicalOr ( AssignmentOp Assignment )?
        // AssignmentOp -> '=' | '+=' | '-=' | '*=' | '/='
        void parseAssignment() {
            parseLogicalOr();
            if (look.type == TokenType.ASSIGN || look.type == TokenType.PLUSEQ || look.type == TokenType.MINUSEQ
                    || look.type == TokenType.STAREQ || look.type == TokenType.SLASHEQ) {
                advance();
                parseAssignment();
            }
        }

        // LogicalOr -> LogicalAnd ( '||' LogicalAnd )*
        void parseLogicalOr() {
            parseLogicalAnd();
            while (look.type == TokenType.OROR) { advance(); parseLogicalAnd(); }
        }

        // LogicalAnd -> Equality ( '&&' Equality )*
        void parseLogicalAnd() {
            parseEquality();
            while (look.type == TokenType.ANDAND) { advance(); parseEquality(); }
        }

        // Equality -> Relational ( ( '==' | '!=' ) Relational )*
        void parseEquality() {
            parseRelational();
            while (look.type == TokenType.EQEQ || look.type == TokenType.NOTEQ) { advance(); parseRelational(); }
        }

        // Relational -> Additive ( ( '<' | '>' | '<=' | '>=' ) Additive )*
        void parseRelational() {
            parseAdditive();
            while (look.type == TokenType.LT || look.type == TokenType.GT || look.type == TokenType.LTE || look.type == TokenType.GTE) {
                advance(); parseAdditive();
            }
        }

        // Additive -> Multiplicative ( ( '+' | '-' ) Multiplicative )*
        void parseAdditive() {
            parseMultiplicative();
            while (look.type == TokenType.PLUS || look.type == TokenType.MINUS) { advance(); parseMultiplicative(); }
        }

        // TERM → Factor ( ('*' | '/') Factor )*
        // Multiplicative -> Unary ( ( '*' | '/' | '%' ) Unary )*
        void parseMultiplicative() {
            parseUnary();
            while (look.type == TokenType.STAR || look.type == TokenType.SLASH || look.type == TokenType.PERCENT) {
                advance(); parseUnary();
            }
        }

        // Unary -> ( '!' | '-' | '++' | '--' ) Unary | Postfix
        void parseUnary() {
            if (look.type == TokenType.BANG || look.type == TokenType.MINUS || look.type == TokenType.PLUSPLUS || look.type == TokenType.MINUSMINUS) {
                advance(); parseUnary();
            } else parsePostfix();
        }

        // Postfix -> Primary ( '++' | '--' )*
        void parsePostfix() {
            parsePrimary();
            while (look.type == TokenType.PLUSPLUS || look.type == TokenType.MINUSMINUS) advance();
        }

        // Primary -> IDENT | NUMBER | STRING | '(' Expression ')' | 'true' | 'false' | 'NULL'
        void parsePrimary() {
            if (look.type == TokenType.IDENT || look.type == TokenType.NUMBER || look.type == TokenType.STRING
                    || look.type == TokenType.TRUE || look.type == TokenType.FALSE || look.type == TokenType.NULL) {
                advance(); return;
            }
            if (look.type == TokenType.LPAREN) {
                advance(); parseExpression(); consume(TokenType.RPAREN, "')' expected"); return;
            }
            error("Primary expression expected");
        }

        void reportErrors() {
            if (errors.isEmpty()) System.out.println("Parsing: no syntax errors.");
            else {
                System.err.println("Parsing: found " + errors.size() + " error(s):");
                for (String e : errors) System.err.println(e);
            }
        }
    }

   
    // MAIN: interactive input -> lex -> parse
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.println("Hello this is me jalil , now we start the parsing process.");
        System.out.println("Paste source lines. When finished enter a single line: #");

        StringBuilder sb = new StringBuilder();
        while (true) {
            String line = sc.nextLine();
            if (line.equals("#")) break;
            sb.append(line).append("\n");
        }
        String source = sb.toString();

        
        Lexer lexer = new Lexer(source);
        List<Token> tokens = lexer.tokenize();

        if (!lexer.errors.isEmpty()) {
            System.err.println("Lexical errors:");
            for (String e : lexer.errors) System.err.println(e);
        }

        

        
        Parser parser = new Parser(tokens);
        parser.parseProgram();
        parser.reportErrors();

        System.out.println("ALL Correct ✅");
        sc.close();
    }
}
