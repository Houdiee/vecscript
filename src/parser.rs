/*
Program ::= { Statement } ;

Statement ::= SolveForInDeclaration TERMINATE
            | LetInDeclaration TERMINATE
            | LetDeclaration TERMINATE
            | SetDeclaration TERMINATE
            | Expression TERMINATE ;

SolveForInDeclaration ::= SOLVE FOR IDENTIFIER [ TypeAnnotation ] IN Expression EQUALS Expression ;
LetInDeclaration      ::= LET Binding IN Expression ;
LetDeclaration        ::= LET Binding [ WhereClause ] ;
WhereClause ::= WHERE Binding { COMMA Binding } ;

SetDeclaration ::= LET IDENTIFIER [ TypeAnnotation ] EQUALS LBRACE [ Expression { COMMA Expression } ] RBRACE ;

Binding ::= IDENTIFIER [ TypeAnnotation ] EQUALS Expression ;
TypeAnnotation ::= COLON TYPE ;

Expression ::= ComparisonExpression ;
ComparisonExpression ::= ArithmeticExpression { OPERATOR ArithmeticExpression } ;
ArithmeticExpression ::= MultiplicativeExpression {( PLUS | MINUS ) MultiplicativeExpression } ;
MultiplicativeExpression ::= UnaryExpression {( MULTIPLY | DIVIDE ) UnaryExpression } ;
UnaryExpression ::= [ MINUS ] PowerExpression ;
PowerExpression ::= Atom [ POWER PowerExpression ];
Atom ::= NUMBER
         | BOOL
         | STRING
         | IDENTIFIER
         | LPAREN Expression RPAREN ;

TERMINATE ::= NEWLINE ;
*/
