package crux;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;

public class SymbolTable {
    
	
	LinkedList<LinkedHashMap<String, Symbol>> table;
	
    public SymbolTable()
    {
        table = new LinkedList<LinkedHashMap<String, Symbol>>();
        this.addDefault();
    }
    
    public void addDefault(){
    	this.createScope();
    	//add the defaults
    	this.table.peekFirst().put("readInt", new Symbol("readInt"));
    	this.table.peekFirst().put("readFloat", new Symbol("readFloat"));
    	this.table.peekFirst().put("printBool", new Symbol("printBool"));
    	this.table.peekFirst().put("printInt", new Symbol("printInt"));
    	this.table.peekFirst().put("printFloat", new Symbol("printFloat"));
    	this.table.peekFirst().put("println", new Symbol("println"));
    	
    	
    }
    
    public Symbol lookup(String name) throws SymbolNotFoundError
    {
        for(int index = 0; index < table.size() ; index++){
        	if(table.get(index).containsKey(name)){
        		return table.get(index).get(name);
        	}
        }
        
        throw new SymbolNotFoundError(name);
    }
    
    
    public Symbol insert(String name) throws RedeclarationError
    {
    	if(table.peekFirst().containsKey(name))
    		throw new RedeclarationError(table.peekFirst().get(name));
    
        return table.peekFirst().put(name, new Symbol(name));
    }
    
    public void createScope(){
    	LinkedHashMap<String, Symbol> map = new LinkedHashMap<String, Symbol>();
    	table.push(map);
    }
    
    public void exitScope(){ 
    	table.pop();
    }
    
    
    public String toString()
    {
    	int count = 0;
       StringBuffer sb = new StringBuffer();
       for(int i = table.size()-1; i >= 0; i--){
    	   //indent is equal to current space 
    	   String indent = new String();
    	   for(int x = 0; x < count; x++){
    		   indent +=  "  ";
    	   }
    	   
    	   for(Symbol s : table.get(i).values())
    	   {
    		   sb.append(indent + s.toString() + "\n");
    	   } 
    	   
    	   count++;
       }
       
       return sb.toString();
    }  

}

class SymbolNotFoundError extends Error
{
    private static final long serialVersionUID = 1L;
    private String name;
    
    SymbolNotFoundError(String name)
    {
        this.name = name;
    }
    
    public String name()
    {
        return name;
    }
}

class RedeclarationError extends Error
{
    private static final long serialVersionUID = 1L;

    public RedeclarationError(Symbol sym)
    {
        super("Symbol " + sym + " being redeclared.");
    }
}
