package crux;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import ast.*;


public class Parser {
    public static String studentName = "kasean herrera";
    public static String studentID = "33531582";
    public static String uciNetID = "kaseanh";
	private String stringMatch = "\\d+";
	private String numberMatch = "(?:\\d+)\\.?\\d*";
	private String identifierMatch = "[a-zA-Z]+";
    private SymbolTable symbolTable;
    
// Grammar Rule Reporting ==========================================
    private int parseTreeRecursionDepth = 0;
    private StringBuffer parseTreeBuffer = new StringBuffer();

    public void enterRule(NonTerminal nonTerminal) {
        String lineData = new String();
        for(int i = 0; i < parseTreeRecursionDepth; i++)
        {
            lineData += "  ";
        }
        lineData += nonTerminal.name();
      
        parseTreeBuffer.append(lineData + "\n");
        parseTreeRecursionDepth++;
    }
    
    private void exitRule(NonTerminal nonTerminal)
    {
        parseTreeRecursionDepth--;
    }
    
    public String parseTreeReport()
    {
        return parseTreeBuffer.toString();
    }

// Error Reporting ==========================================
    private StringBuffer errorBuffer = new StringBuffer();
    
    private String reportSyntaxError(NonTerminal nt)
    {
    	System.out.println(currentToken.lexeme());
        String message = "SyntaxError(" + lineNumber() + "," + charPosition() + ")[Expected a token from " + nt.name() + " but got " + currentToken.kind() + ".]";
        errorBuffer.append(message + "\n");
        return message;
    }
     
    private String reportSyntaxError(Token.Kind kind)
    {
    	System.out.println(currentToken.lexeme());
        String message = "SyntaxError(" + lineNumber() + "," + charPosition() + ")[Expected " + kind + " but got " + currentToken.kind() + ".]";
        errorBuffer.append(message + "\n");
        return message;
    }
    
    public String errorReport()
    {
        return errorBuffer.toString();
    }
    
    public boolean hasError()
    {
        return errorBuffer.length() != 0;
    }
    
    private class QuitParseException extends RuntimeException
    {
        private static final long serialVersionUID = 1L;
        public QuitParseException(String errorMessage) {
            super(errorMessage);
        }
    }
    
    private int lineNumber()
    {
        return currentToken.lineNumber();
    }
    
    private int charPosition()
    {
        return currentToken.charPosition();
    }
          
// Parser ==========================================
    private Scanner scanner;
    private Token currentToken;
    
    public Parser(Scanner scanner)
    {
        this.scanner = scanner;
        this.currentToken = scanner.next();
    }
 
    
    public ast.Command parse()
    {
        initSymbolTable();
        try {
            return program();
        } catch (QuitParseException q) {
            return new ast.Error(lineNumber(), charPosition(), "Could not complete parsing.");
        }
    }
    

// Helper Methods ==========================================
    private boolean have(Token.Kind kind)
    {
        return currentToken.is(kind);
    }
    
    private boolean have(NonTerminal nt)
    {
        return nt.firstSet().contains(currentToken.kind());
    }

    private boolean accept(Token.Kind kind)
    {
        if (have(kind)) {
            currentToken = scanner.next();
            return true;
        }
        return false;
    }    
    
    private boolean accept(NonTerminal nt)
    {
        if (have(nt)) {
            currentToken = scanner.next();
            return true;
        }
        return false;
    }
   
    private boolean expect(Token.Kind kind)
    {
        if (accept(kind))
            return true;
        String errorMessage = reportSyntaxError(kind);
        throw new QuitParseException(errorMessage);
        //return false;
    }
        
    private boolean expect(NonTerminal nt)
    {
        if (accept(nt))
            return true;
        String errorMessage = reportSyntaxError(nt);
        throw new QuitParseException(errorMessage);
        //return false;
    }
    
    private Token expectRetrieve(Token.Kind kind)
    {
        Token tok = currentToken;
        if (accept(kind))
            return tok;
        String errorMessage = reportSyntaxError(kind);
        throw new QuitParseException(errorMessage);
        //return ErrorToken(errorMessage);
    }
        
    private Token expectRetrieve(NonTerminal nt)
    {
        Token tok = currentToken;
        if (accept(nt))
            return tok;
        String errorMessage = reportSyntaxError(nt);
        throw new QuitParseException(errorMessage);
        //return ErrorToken(errorMessage);
    }

// Grammar Rules =====================================================
    
    
    // literal := INTEGER | FLOAT | TRUE | FALSE .
    public ast.Expression literal()
    {
    	Expression exp;

        enterRule(NonTerminal.LITERAL);
        
        Token tok = expectRetrieve(NonTerminal.LITERAL);
        exp = Command.newLiteral(tok);
        
        exitRule(NonTerminal.LITERAL);
        return exp;
    }
    
    // designator := IDENTIFIER { "[" expression0 "]" } .
    public ast.Expression designator()
    {
    	Token t = expectRetrieve(Token.Kind.IDENTIFIER);
    	Symbol sym = tryResolveSymbol(t);
    	ast.Expression exp = new ast.AddressOf(t.lineNumber(), t.charPosition(), sym);
        while (accept(Token.Kind.OPEN_BRACKET)) {
            ast.Expression expression = expression0();
            exp = new ast.Index(t.lineNumber(), t.charPosition(), exp, expression);
            expect(Token.Kind.CLOSE_BRACKET);
        }
        return exp;
        
    } 

    //program := declaration-list EOF .
    public ast.DeclarationList program()
    {
    	//get the current line number and position 
    	return declerationList();
    } 
    
    //declaration-list := { declaration } .
    private ast.DeclarationList declerationList() {
    	ast.DeclarationList dl = new DeclarationList(this.currentToken.lineNumber(), this.charPosition() - this.currentToken.lexeme().length()+1);
    	
		while(this.currentToken.kind == Token.Kind.VAR || this.currentToken.kind == Token.Kind.ARRAY || this.currentToken.kind == Token.Kind.FUNC)
			dl.add(decleration());
		return dl;
	}

    //declaration := variable-declaration | array-declaration | function-definition .
	private Declaration decleration() {
		if(this.currentToken.kind == Token.Kind.VAR){
			return variableDeclaration();
		}
		else if(this.currentToken.kind == Token.Kind.ARRAY){
			return arrayDeclaration();
		}
		else if(this.currentToken.kind == Token.Kind.FUNC){
			return functionDefinition();
		}else{
			return functionDefinition();
		}
		
	
		
	}

	//function-definition := "func" IDENTIFIER "(" parameter-list ")" ":" type statement-block .
	private Declaration functionDefinition() {
		
		expect(Token.Kind.FUNC);
		Token token = expectRetrieve(Token.Kind.IDENTIFIER);
		tryDeclareSymbol(token);
		Symbol s = tryResolveSymbol(token);
		this.symbolTable.createScope();
		expect(Token.Kind.OPEN_PAREN);
		//get the arguments from the paramater 
		List<Symbol> args = paramerterList();
		expect(Token.Kind.CLOSE_PAREN);
		expect(Token.Kind.COLON);
		type();
		ast.StatementList body = statementBlock();
		ast.FunctionDefinition fd = new ast.FunctionDefinition(token.lineNumber(), token.charPosition(), s , args, body);
		this.symbolTable.exitScope();
		return fd;
		
	}

	//statement-block := "{" statement-list "}"
	private ast.StatementList statementBlock() {
		expect(Token.Kind.OPEN_BRACE);
		ast.StatementList statementList = statementList();
		expect(Token.Kind.CLOSE_BRACE);
		return statementList;
	}

	//statement-list := { statement } .
	private ast.StatementList statementList() {
		ast.StatementList slist = new ast.StatementList(this.currentToken.lineNumber(), this.currentToken.charPosition());
		
		
		while(have(NonTerminal.STATEMENT)){
			slist.add(statement());
		}

		return slist;
		
	}

	/*statement := variable-declaration
           | call-statement
           | assignment-statement
           | if-statement
           | while-statement
           | return-statement . */
	private ast.Statement statement() {

	   if(this.currentToken.kind == Token.Kind.VAR)
			return this.variableDeclaration();
	   if(this.currentToken.kind == Token.Kind.CALL)
			return this.callStatement();
	   else if(this.currentToken.kind == Token.Kind.LET)
			return this.assignmentStatement();
		else if(this.currentToken.kind == Token.Kind.IF)
			return this.ifStatement();
		else if(this.currentToken.kind == Token.Kind.WHILE)
			return this.whileStatement();
		else if(this.currentToken.kind == Token.Kind.RETURN)
			return this.returnStatement();
		else{
			String errorMessage = reportSyntaxError(Token.Kind.CLOSE_BRACE);
	        throw new QuitParseException(errorMessage);
		}
		
	
		
	}

	private ast.Call callStatement() {
		//call-statement := call-expression ";"
		ast.Call call =  this.callExpression();
		expect(Token.Kind.SEMICOLON);
		return call;
	}

	//return-statement := "return" expression0 ";" .
	private ast.Return returnStatement() {
		enterRule(NonTerminal.RETURN_STATEMENT);
		Token t = expectRetrieve(Token.Kind.RETURN);
		Expression arg = this.expression0();
		this.expect(Token.Kind.SEMICOLON);
		exitRule(NonTerminal.RETURN_STATEMENT);
		return new ast.Return(t.lineNumber(), t.charPosition(), arg);
	}
//while-statement := "while" expression0 statement-block .
	private ast.WhileLoop whileStatement() {
		enterRule(NonTerminal.WHILE_STATEMENT);
		Token tok  = this.expectRetrieve(Token.Kind.WHILE);
		Expression cond = this.expression0();
		ast.StatementList body = statementBlock();
		exitRule(NonTerminal.WHILE_STATEMENT);
		return new ast.WhileLoop(tok.lineNumber(), tok.charPosition(), cond, body);
	}

	//if-statement := "if" expression0 statement-block [ "else" statement-block ] .
	private ast.IfElseBranch ifStatement() {
		enterRule(NonTerminal.IF_STATEMENT);
		expect(Token.Kind.IF);
		int lineNum = this.currentToken.lineNumber();
		int charPos = this.currentToken.charPosition();
		
		ast.Expression cond = expression0();
		this.symbolTable.createScope();
		ast.StatementList thenBlock = statementBlock();
		ast.StatementList elseBlock;
		this.symbolTable.exitScope();
		if(accept(Token.Kind.ELSE)){
			this.symbolTable.createScope();
			elseBlock = statementBlock();
			this.symbolTable.exitScope();
		}else{
			elseBlock = new ast.StatementList(currentToken.lineNumber(), currentToken.charPosition());
		}
		exitRule(NonTerminal.IF_STATEMENT);
		
		return new ast.IfElseBranch(lineNum, charPos, cond, thenBlock, elseBlock);
	}

	//assignment-statement := "let" designator "=" expression0 ";"
	private ast.Assignment assignmentStatement() {
		enterRule(NonTerminal.ASSIGNMENT_STATEMENT);
		Token nt = this.expectRetrieve(Token.Kind.LET);
		Expression dest = designator();
		expect(Token.Kind.ASSIGN);
		Expression source = expression0();
		expect(Token.Kind.SEMICOLON);
		exitRule(NonTerminal.ASSIGNMENT_STATEMENT);
		return new ast.Assignment(nt.lineNumber(), nt.charPosition(), dest, source);
		
	}

	//type := IDENTIFIER .
	private void type() {
		enterRule(NonTerminal.TYPE);
		expect(Token.Kind.IDENTIFIER);
		exitRule(NonTerminal.TYPE);
		
	}
	//parameter-list := [ parameter { "," parameter } ] .
	private List<Symbol> paramerterList() {
		List<Symbol> args = new ArrayList<Symbol>();
		
		if(currentToken.kind != Token.Kind.CLOSE_PAREN){
			args.add(parameter());
			while(accept(Token.Kind.COMMA))
				args.add(parameter());
		}
		
		return args;
	}

	//parameter := IDENTIFIER ":" type .
	private  Symbol parameter() {
		//get the symbol and return it
		Symbol s = tryDeclareSymbol(expectRetrieve(Token.Kind.IDENTIFIER));
		expect(Token.Kind.COLON);
		type();
		return s;
	}

	//array-declaration := "array" IDENTIFIER ":" type "[" INTEGER "]" { "[" INTEGER "]" } ";"
	private ast.ArrayDeclaration arrayDeclaration() {
		
		enterRule(NonTerminal.ARRAY_DECLARATION);
		Token arr = expectRetrieve(Token.Kind.ARRAY);
		Token iden = expectRetrieve(Token.Kind.IDENTIFIER);
		tryDeclareSymbol(iden);
		Symbol sym = new Symbol(iden.lexeme());

		ast.ArrayDeclaration array = new ast.ArrayDeclaration(arr.lineNumber(), arr.charPosition(), sym);
		
		expect(Token.Kind.COLON);
		this.type();
		expect(Token.Kind.OPEN_BRACKET);
		
		expect(Token.Kind.INTEGER);
		expect(Token.Kind.CLOSE_BRACKET);
		while(this.accept(Token.Kind.OPEN_BRACKET)){
			expect(Token.Kind.INTEGER);
			expect(Token.Kind.CLOSE_BRACKET);
		}
		expect(Token.Kind.SEMICOLON);
		exitRule(NonTerminal.ARRAY_DECLARATION);
		
		return array;
		
	}

	//variable-declaration := "var" IDENTIFIER ":" type ";"
	private ast.VariableDeclaration variableDeclaration() {
		expect(Token.Kind.VAR);
		Token t = expectRetrieve(Token.Kind.IDENTIFIER);
		tryDeclareSymbol(t);
		Symbol sym = tryResolveSymbol(t);
		expect(Token.Kind.COLON);
		type();
		expect(Token.Kind.SEMICOLON);
		ast.VariableDeclaration vd = new ast.VariableDeclaration(this.currentToken.lineNumber(), this.currentToken.charPosition(), sym);
		return vd;
	}



	private ast.Expression expression0() {
		ast.Expression exp =  expression1();
		if(this.currentToken.kind != Token.Kind.SEMICOLON){
			if(have(NonTerminal.OP0)){
				ast.Expression left = exp;
				Token op  = op0();
				ast.Expression right = expression1();
				exp = ast.Command.newExpression(left, op, right);
			}
		}
		return exp;
	}
	
	//op0 := ">=" | "<=" | "!=" | "==" | ">" | "<" .
	private Token  op0() {
		enterRule(NonTerminal.OP0);
		Token t = currentToken;
		expect(currentToken.kind);
		exitRule(NonTerminal.OP0);
		return t;
		
	}

	//expression1 := expression2 { op1  expression2 } .
	private ast.Expression expression1() {
		ast.Expression exp = expression2();
		while(have(NonTerminal.OP1)){
			ast.Expression left = exp;
			Token op = op1();
			ast.Expression right = expression2();
			exp = ast.Command.newExpression(left, op, right);
			
		}
		
		return exp;
	} 
	
	

	//op1 := "+" | "-" | "or" .
	private Token op1() {
		ast.Expression e;
		enterRule(NonTerminal.OP1);
		Token t = this.currentToken;
		expect(this.currentToken.kind());
		exitRule(NonTerminal.OP1);
		return t;
		
	}

	//expression2 := expression3 { op2 expression3 } .
	private ast.Expression expression2() {
		ast.Expression exp = expression3();
		
		
		while(have(NonTerminal.OP2)){
			ast.Expression left = exp;
			Token op = op2();
			Expression right = expression3();
			exp = ast.Command.newExpression(left, op, right);
		}	
		return exp;
	}
	
	

	
	//op2 := "*" | "/" | "and" 
	private Token op2() {
		enterRule(NonTerminal.OP2);
		Token k = this.currentToken;
		expect(this.currentToken.kind());
		exitRule(NonTerminal.OP2);
		return k;
		
	}

	/*expression3 := "not" expression3
		       | "(" expression0 ")"
		       | designator
		       | call-expression
		       | literal . */
	private ast.Expression expression3() {	

		
		ast.Expression expression;
	
		if(this.currentToken.lexeme().equals("not")){
			Token tok = expectRetrieve(Token.Kind.NOT);
			Expression right = expression3();
			expression = Command.newExpression(right, tok, null);
		}
		else if(this.currentToken.kind == Token.Kind.OPEN_PAREN){
			expect(Token.Kind.OPEN_PAREN);
			expression = expression0();
			expect(Token.Kind.CLOSE_PAREN);
			
		}else if(this.currentToken.kind == Token.Kind.IDENTIFIER){
			Expression d =  designator();
			expression = new ast.Dereference(this.currentToken.lineNumber(), this.currentToken.charPosition(), d);
		}else if(this.currentToken.kind == Token.Kind.CALL){
			expression = callExpression();
		}else if (isLiteral()){
			expression = literal();
		}else {
			throw new QuitParseException(reportSyntaxError(NonTerminal.EXPRESSION3));
		}
		
		return expression;
		
	}		

	private boolean isLiteral() {
		String token = currentToken.lexeme();
		
		return (token.equals("true") || token.equals("false") || token.matches(stringMatch) || token.matches(numberMatch) && token.contains("."));

	
	}

	//call-expression := "::" IDENTIFIER "(" expression-list ")" .
	private ast.Call callExpression() {
		
		expect(Token.Kind.CALL);
		Symbol sym = tryResolveSymbol(expectRetrieve(Token.Kind.IDENTIFIER));
		expect(Token.Kind.OPEN_PAREN);
		ast.ExpressionList args = expressionList();
		expect(Token.Kind.CLOSE_PAREN);
	
		return  new ast.Call(this.currentToken.lineNumber(), this.currentToken.charPosition(), sym, args);
		
	}

	private ast.ExpressionList expressionList() {
		//expression-list := [ expression0 { "," expression0 } ] .
		ast.ExpressionList expressionList = new ast.ExpressionList(this.currentToken.lineNumber(), this.currentToken.charPosition());
		if(NonTerminal.EXPRESSION0.firstSet.contains(this.currentToken.kind) || this.currentToken.lexeme().equals("not")){
			expressionList.add(expression0());
			while(accept(Token.Kind.COMMA)){
				expressionList.add(expression0());
			}
		}
		return expressionList;
	}
    

	// SymbolTable Management ==========================

    
    private void initSymbolTable()
    {	
    	symbolTable = new SymbolTable();
    }
    
    private void enterScope()
    {
        symbolTable.createScope();
    }
    
    private void exitScope()
    {
        symbolTable.exitScope();
    }

    private Symbol tryResolveSymbol(Token ident)
    {
        assert(ident.is(Token.Kind.IDENTIFIER));
        String name = ident.lexeme();
        try {
            return symbolTable.lookup(name);
        } catch (SymbolNotFoundError e) {
            String message = reportResolveSymbolError(name, ident.lineNumber(), ident.charPosition());
            return new ErrorSymbol(message);
        }
    }

    private String reportResolveSymbolError(String name, int lineNum, int charPos)
    {
        String message = "ResolveSymbolError(" + lineNum + "," + charPos + ")[Could not find " + name + ".]";
        errorBuffer.append(message + "\n");
        errorBuffer.append(symbolTable.toString() + "\n");
        return message;
    }

    private Symbol tryDeclareSymbol(Token ident)
    {
        assert(ident.is(Token.Kind.IDENTIFIER));
        String name = ident.lexeme();
        try {
            return symbolTable.insert(name);
        } catch (RedeclarationError re) {
            String message = reportDeclareSymbolError(name, ident.lineNumber(), ident.charPosition());
            return new ErrorSymbol(message);
        }
    }

    private String reportDeclareSymbolError(String name, int lineNum, int charPos)
    {
        String message = "DeclareSymbolError(" + lineNum + "," + charPos + ")[" + name + " already exists.]";
        errorBuffer.append(message + "\n");
        errorBuffer.append(symbolTable.toString() + "\n");
        return message;
    }    
}
