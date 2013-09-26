package ua.ho.gloryofrobots.childtalk.compilation;

import ua.ho.gloryofrobots.childtalk.Universe;
import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.node.EvalNode;
import ua.ho.gloryofrobots.childtalk.node.ProgramNode;
import ua.ho.gloryofrobots.childtalk.stobject.STClass;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STImage;
import ua.ho.gloryofrobots.childtalk.stobject.STScope;
import ua.ho.gloryofrobots.childtalk.stobject.STString;

public class CompileSuite {
    public static ProgramNode parseWithLexer(Lexer lexer) throws FileEvalException {
        Parser parser = new Parser(lexer);
        return parser.parse();
    }
    
    public static void compileProgramStream(ProgramTextStreamInterface programStream, STImage image){
        Lexer lexer;
        try {
            lexer = Lexer.create(programStream);
            ProgramNode program = parseWithLexer(lexer);            
            compile(program, image);
        } catch (FileEvalException e) {
            SignalSuite.error("Parse error %s", e.getMessage());
        }
    }
    
    public static void compile(ProgramNode program, STScope scope) throws FileEvalException{
        Compiler compiler = new Compiler();
        compiler.compileProgram(program, scope);
    }
    
    public static STExecutableObject compileEval(ProgramTextStreamInterface programStream){
        Lexer lexer;
        STExecutableObject executable = null;
        try {
            lexer = Lexer.create(programStream);
            ProgramNode program = parseWithLexer(lexer);
            if(program.size() < 1) {
                SignalSuite.error("Evaluate error : there are no statements to Eval");
            }
            
            EvalNode node = null;
            try {
                node = (EvalNode) program.getNode(0);
                
            } catch(ClassCastException e) {
                SignalSuite.error("Evaluate error : statement is not valid eval block");
            }
            
            Compiler compiler = new Compiler();
            executable = compiler.compileEval(node);            
        } catch (FileEvalException e) {
            SignalSuite.error("Parse error %s", e.getMessage());
        }
        
        return executable;
    }
}
