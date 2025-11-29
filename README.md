

Nom : Khaldi Abdeldjalil.
Group : B1.
Choise ; 1.( c /if__else)



**The chosen Grammar**

Program           -> TranslationUnit*

TranslationUnit   -> ExternalDecl

ExternalDecl      -> VarDecl
                   | FunctionDecl
                   | Statement


VarDecl           -> Type DeclaratorList ';'
DeclaratorList    -> Declarator (',' Declarator)*
Declarator        -> IDENT ( '=' Expression )?

Type              -> 'int' | 'float' | 'double' | 'char' | 'bool' | 'void'


FunctionDecl      -> Type IDENT '(' ParamList? ')' CompoundStmt
ParamList         -> Param (',' Param)*
Param             -> Type IDENT


Statement         -> CompoundStmt
                   | IfStmt
                   | WhileStmt
                   | ForStmt
                   | ExprStmt
                   | ReturnStmt
                   | ';'

CompoundStmt      -> '{' Statement* '}'

IfStmt            -> 'if' '(' Expression ')' Statement ElsePart
ElsePart          -> 'else' Statement | ε

WhileStmt         -> 'while' '(' Expression ')' Statement
ForStmt           -> 'for' '(' ForInit? ';' ForCond? ';' ForPost? ')' Statement
ForInit           -> VarDecl | ExprStmt
ForCond           -> Expression
ForPost           -> ExpressionList
ExpressionList    -> Expression (',' Expression)*

ExprStmt          -> Expression? ';'
ReturnStmt        -> 'return' Expression? ';'


Expression        -> Assignment

Assignment        -> LogicalOr ( AssignmentOp Assignment )?
AssignmentOp      -> '=' | '+=' | '-=' | '*=' | '/='

LogicalOr         -> LogicalAnd ( '||' LogicalAnd )*
LogicalAnd        -> Equality ( '&&' Equality )*
Equality          -> Relational ( ( '==' | '!=' ) Relational )*
Relational        -> Additive ( ( '<' | '>' | '<=' | '>=' ) Additive )*
Additive          -> Multiplicative ( ( '+' | '-' ) Multiplicative )*
Multiplicative    -> Unary ( ( '*' | '/' | '%' ) Unary )*
Unary             -> ( '!' | '-' | '++' | '--' ) Unary
                   | Postfix

Postfix           -> Primary ( '++' | '--' )*

Primary           -> IDENT
                   | NUMBER
                   | STRING
                   | '(' Expression ')'
                   | 'true' | 'false' | 'NULL'




**ARchitecture**
Main.java   → Full recursive-descent compiler
README.md           → Documentation

