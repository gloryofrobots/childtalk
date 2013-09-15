package ua.ho.gloryofrobots.yellowtalk.compilation;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.yellowtalk.node.EvalNode;
import ua.ho.gloryofrobots.yellowtalk.node.ProgramNode;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STImage;
import ua.ho.gloryofrobots.yellowtalk.stobject.STScope;

public class CompileSuite {
    public static ProgramNode parseWithLexer(LexerInterface lexer) throws FileEvalException {
        Parser parser = new Parser(lexer);
        return parser.parse();
    }
    
    public static void compileProgramStream(ProgramTextStreamInterface programStream, STImage image){
        LexerInterface lexer;
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
        LexerInterface lexer;
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
