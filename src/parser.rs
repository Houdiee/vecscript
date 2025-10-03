/*
SolveForInDeclaration ::= SOLVE FOR IDENTIFIER IN Expression EQUALS Expression TERMINATE ;
LetDeclaration ::= LET Assignment [ WhereClause ] TERMINATE ;
WhereClause ::= WHERE Assignment { COMMA Assignment } ;
Assignment ::= IDENTIFIER [ TypeAnnotation ] EQUALS EXPRESSION ;
TypeAnnotation ::= COLON TYPE ;

Expression ::= MultiplicativeExpression {( PLUS | MINUS ) MultiplicativeExpression } ;
MultiplicativeExpression ::= ImplicitMultiplicationExpression {( MULTIPLY | DIVIDE ) ImplicitMultiplicationExpression } ;
ImplicitMultiplicationExpression ::= UnaryExpression { UnaryExpression } ;
UnaryExpression ::= [ MINUS ] PowerExpression ;
PowerExpression ::= Atom [ POWER PowerExpression ];
Atom ::= NUMBER
         | IDENTIFIER
         | LPAREN Expression RPAREN ;

TERMINATE ::= NEWLINE ;
*/
