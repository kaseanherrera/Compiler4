package crux;

import crux.Token.Kind;

public class Token {
	//strings for regular expression matching 
	private String stringMatch = "\\d+";
	private String numberMatch = "(?:\\d+)\\.?\\d*";
	private String identifierMatch = "[{}a-zA-Z\\_]+";
	
	//global variables 
	private int lineNum;
	private int charPos;
	Kind kind;
	private String lexeme;

	//kind enum 
	public static enum Kind {
		AND("and"),
		OR("or"),
		NOT("not"),
		LET("let"),
		VAR("var"),
		ARRAY("array"),
		FUNC("func"),
		IF("if"),
		ELSE("else"),
		WHILE("while"),
		TRUE("true"),
		FALSE("false"),
		RETURN("return"),
		OPEN_PAREN("("),
		CLOSE_PAREN(")"),
		OPEN_BRACE("{"),
		CLOSE_BRACE("}"),
		OPEN_BRACKET("["),
		CLOSE_BRACKET("]"),
		ADD("+"),
		SUB("-"),
		MUL("*"),
		DIV("/"),
		GREATER_THAN(">"),
		LESS_THAN("<"),
		ASSIGN("="),
		COMMA(","),
		SEMICOLON(";"),
		COLON(":"),
		CALL("::"),
		EQUAL("=="),
		IDENTIFIER(),
		INTEGER(),
		FLOAT(),
		ERROR(),
		EOF(), 
		GREATER_EQUAL(">="), 
		NOT_EQUAL("!="),
		LESSER_EQUAL("<=");
		
		private String default_lexeme;
		
		//constructor 
		Kind()
		{
			this.default_lexeme = "";
		}
		//constructor 
		Kind(String lexeme)
		{
			this.default_lexeme = lexeme;
		}
		
	
		public boolean hasStaticLexeme()
		{
			return default_lexeme != null;
		}
	}
	
	//init for the error case
	public Token(int lineNum, int charPos)
	{
		this.lineNum = lineNum;
		this.charPos = charPos;
		this.kind = Kind.ERROR;
		this.lexeme = "EOF";
	}
	
	//init - sets all variables 
	public Token(String lexeme, int lineNum, int charPos)
	{
		this.lineNum = lineNum;
		this.charPos = charPos;
		this.lexeme = lexeme;
		this.kind = getKind(lexeme);
	}
	
	//returns the kind from the lexeme passed 
	private Kind getKind(String lexeme2) {
		//iterate through the values of the kind object, 
		for(Kind k : Kind.values()){
			if(k.default_lexeme.equals(lexeme2)){
				return k;
			}
		}
		
		if(isInteger(lexeme))
			return Kind.INTEGER;
		else if(isFloat(lexeme))
			return Kind.FLOAT;
		else if (isIdentifier(lexeme))
			return Kind.IDENTIFIER;
		else
			return Kind.ERROR;
	}
	
	//get the line number 
	public int lineNumber()
	{
		return lineNum;
	}
	
	//get the char postion 
	public int charPosition()
	{
		return charPos;
	}
	
	// Return the lexeme representing or held by this token
	public String lexeme()
	{
		return lexeme;
	}
	
	//returns true is the input is an Integer
	public boolean isInteger(String input){
		return input.matches(stringMatch);
	}
	
	//return true if string represents a float 
	public boolean isFloat(String input){
		return (input.matches(numberMatch) && input.contains("."));
	}
	
	//check if the input is an Identifier
	public boolean isIdentifier(String input){
		return input.matches(identifierMatch);
	}
	
	//string presentation of a token 
	public String toString()
	{
		StringBuilder str = new StringBuilder();
		str.append(this.kind.name()); //kind
		str.append("(lineNum :" + this.lineNumber() + ", " + "charPos:" + this.charPos + ')'); //line number and postion 
		return str.toString();
	}

	public Kind kind() {
		// TODO Auto-generated method stub
		return this.kind;
	}

	public boolean is(Kind kind2) {
		return kind2.name().equals(this.kind.name());
	}
}
