package crux;
import java.util.HashSet;
import java.util.Set;

public enum NonTerminal {
    
    // TODO: mention that we are not modeling the empty string
    // TODO: mention that we are not doing a first set for every line in the grammar
    //       some lines have already been handled by the CruxScanner
	LITERAL(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	     	add(Token.Kind.INTEGER);
	     	add(Token.Kind.FLOAT);
	     	add(Token.Kind.TRUE);
	     	add(Token.Kind.FALSE);
	     }
	}),
	
	 OP0(new HashSet<Token.Kind>() {
	        private static final long serialVersionUID = 1L;
	        {
	            add(Token.Kind.GREATER_EQUAL);
	            add(Token.Kind.LESSER_EQUAL);
	            add(Token.Kind.NOT_EQUAL);
	            add(Token.Kind.EQUAL);
	            add(Token.Kind.GREATER_THAN);
	            add(Token.Kind.LESS_THAN);
	       }
	 }),
	 
	EXPRESSION0(new HashSet<Token.Kind>() {
        private static final long serialVersionUID = 1L;
        {
            addAll(OP0.firstSet);
            add(Token.Kind.INTEGER);
            add(Token.Kind.IDENTIFIER);
        }
    }),

	DESIGNATOR(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	         add(Token.Kind.IDENTIFIER);
	         add(Token.Kind.OPEN_BRACKET);
	         addAll(EXPRESSION0.firstSet);
	         add(Token.Kind.CLOSE_BRACKET);
	     }
	 }),
	 
	 TYPE(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	        add(Token.Kind.IDENTIFIER);
	     }
     }),
 
     OP1(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	     	add(Token.Kind.ADD);
	     	add(Token.Kind.SUB);
	     	add(Token.Kind.OR);
	     }
	 }),
	 
	 OP2(new HashSet<Token.Kind>() {
		 private static final long serialVersionUID = 1L;
		 {	
     	   add(Token.Kind.MUL);
            add(Token.Kind.DIV);
            add(Token.Kind.AND);
		 }
	 }),
 
	 EXPRESSION_LIST(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	     	addAll(EXPRESSION0.firstSet);
	     	add(Token.Kind.COMMA);	
	     }
	 }),
	     
	 CALL_EXPRESSION(new HashSet<Token.Kind>() {
         private static final long serialVersionUID = 1L;
         {
            add(Token.Kind.CALL);
            add(Token.Kind.IDENTIFIER);
            add(Token.Kind.OPEN_PAREN);
            addAll(EXPRESSION_LIST.firstSet);
            add(Token.Kind.CLOSE_PAREN);
         }
     }),
        
	 EXPRESSION3(new HashSet<Token.Kind>() {
         private static final long serialVersionUID = 1L;
         {
             add(Token.Kind.NOT);
             add(Token.Kind.OPEN_PAREN);
             addAll(EXPRESSION0.firstSet);
             add(Token.Kind.CLOSE_PAREN);
             addAll(DESIGNATOR.firstSet);
             addAll(CALL_EXPRESSION.firstSet);
             addAll(LITERAL.firstSet);
        }
	 }),
        
	 EXPRESSION2(new HashSet<Token.Kind>() {
         private static final long serialVersionUID = 1L;
         {
            addAll(OP2.firstSet);
            addAll(EXPRESSION3.firstSet);
         }
	 }),
         
	 EXPRESSION1(new HashSet<Token.Kind>() {
		 private static final long serialVersionUID = 1L;
		 {
			 addAll(OP1.firstSet);
			 addAll(EXPRESSION2.firstSet);
		 }
	 }),
	 
	 VARIABLE_DECLARATION(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	        add(Token.Kind.VAR);
	        add(Token.Kind.IDENTIFIER);
	        add(Token.Kind.COLON);
	        addAll(TYPE.firstSet);
	        add(Token.Kind.SEMICOLON);
	     }
	 }),

	 ARRAY_DECLARATION(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	         add(Token.Kind.ARRAY);
	         add(Token.Kind.IDENTIFIER);
	         add(Token.Kind.COLON);
	         addAll(TYPE.firstSet);
	         add(Token.Kind.OPEN_BRACKET);
	         add(Token.Kind.INTEGER);
	         add(Token.Kind.CLOSE_BRACKET);
	         add(Token.Kind.SEMICOLON);
	     }
	 }), 
	 
	 PARAMETER(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	     	add(Token.Kind.IDENTIFIER);
	     	add(Token.Kind.COLON);
	     	addAll(TYPE.firstSet);
	     }
	 }),
	     
	 PARAMETER_LIST(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	     	addAll(PARAMETER.firstSet);
	     	add(Token.Kind.COMMA);
	     }
	 }),
	 
	 CALL_STATEMENT(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	         addAll(CALL_EXPRESSION.firstSet);
	         add(Token.Kind.SEMICOLON);
	     }
	 }),
	 
	 ASSIGNMENT_STATEMENT(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	        add(Token.Kind.LET);
	        addAll(DESIGNATOR.firstSet);
	        add(Token.Kind.ASSIGN);
	        addAll(EXPRESSION0.firstSet);
	        add(Token.Kind.SEMICOLON);
	     }
	 }),
	 

     
     RETURN_STATEMENT(new HashSet<Token.Kind>() {
         private static final long serialVersionUID = 1L;
         {
            add(Token.Kind.RETURN);
            addAll(EXPRESSION0.firstSet);
            add(Token.Kind.SEMICOLON);
         }
     }),
	     
     STATEMENT(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	         addAll(VARIABLE_DECLARATION.firstSet);
	         addAll(CALL_STATEMENT.firstSet);
	         addAll(ASSIGNMENT_STATEMENT.firstSet);
	         add(Token.Kind.IF);
	         add(Token.Kind.WHILE);
             addAll(EXPRESSION0.firstSet);
	         addAll(RETURN_STATEMENT.firstSet);
	     }
	 }),
	 
	 STATEMENT_LIST(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	        addAll(STATEMENT.firstSet);
	     }
	 }), 
	     
     STATEMENT_BLOCK(new HashSet<Token.Kind>() {
         private static final long serialVersionUID = 1L;
         {
         	   add(Token.Kind.OPEN_BRACKET);
         	   addAll(STATEMENT_LIST.firstSet);
         	   add(Token.Kind.CLOSE_BRACKET);
         }
     }),
     
	 WHILE_STATEMENT(new HashSet<Token.Kind>() {
         private static final long serialVersionUID = 1L;
         {
             add(Token.Kind.WHILE);
             addAll(EXPRESSION0.firstSet);
             addAll(STATEMENT_BLOCK.firstSet);
         }
     }),
     
	 
	 
	
	 FUNCTION_DEFINITION(new HashSet<Token.Kind>() {
         private static final long serialVersionUID = 1L;
         {
         	add(Token.Kind.FUNC);
         	add(Token.Kind.IDENTIFIER);
         	add(Token.Kind.OPEN_PAREN);
         	addAll(PARAMETER_LIST.firstSet);
            add(Token.Kind.COLON);
            addAll(TYPE.firstSet);
            addAll(STATEMENT_BLOCK.firstSet);
         }
     }),

        
     IF_STATEMENT(new HashSet<Token.Kind>() {
    	 private static final long serialVersionUID = 1L;
         {
        	add(Token.Kind.IF);
            addAll(EXPRESSION0.firstSet);
            addAll(STATEMENT_BLOCK.firstSet);
            add(Token.Kind.ELSE);
         }
      }),
         
     DECLARATION(new HashSet<Token.Kind>() {
         private static final long serialVersionUID = 1L;
         {
         	addAll(VARIABLE_DECLARATION.firstSet);
         	addAll(ARRAY_DECLARATION.firstSet);
         	addAll(FUNCTION_DEFINITION.firstSet);
         }
     }),
                 
     DECLARATION_LIST(new HashSet<Token.Kind>() {
         private static final long serialVersionUID = 1L;
         {
         	addAll(DECLARATION.firstSet);
         }
     }),
                     
     PROGRAM(new HashSet<Token.Kind>() {
	     private static final long serialVersionUID = 1L;
	     {
	         addAll(DECLARATION_LIST.firstSet);
	         add(Token.Kind.EOF);
	     }
     });
        
	 public final HashSet<Token.Kind> firstSet = new HashSet<Token.Kind>();
	
	 NonTerminal(HashSet<Token.Kind> t)
	 {
	     firstSet.addAll(t);
	 }
	 
	 public final Set<Token.Kind> firstSet()
	 {
	     return firstSet;
	 }
 
}