package ua.ho.gloryofrobots.yellowtalk.compilation;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeArray;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeType;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeWriter;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeType.Constant;
import ua.ho.gloryofrobots.yellowtalk.command.CommandStack;
import ua.ho.gloryofrobots.yellowtalk.compilation.ProgramTextStream.ProgramReadException;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.yellowtalk.node.*;
import ua.ho.gloryofrobots.yellowtalk.node.ClassNode.VariableNames;
import ua.ho.gloryofrobots.yellowtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.*;

/*TODO!!!
 * if(assign_name.equals("true")) {

 } else if(assign_name.equals("false")) {

 } else if(assign_name.equals("nil")) {

 } 
 * */

public class Compiler {
    public class UnsupportedNodeException extends Exception {
        private static final long serialVersionUID = 1L;

        public UnsupportedNodeException(Node node) {
            super(String.format("Compiler unknown node %s ", node.getClass()
                    .getName()));
        }
    }

    public class MethodCompiler implements Visitor {
        private STExecutableObject mExecutable;
        private BytecodeWriter mBytecode;

        MethodCompiler(STExecutableObject executable) {
            mExecutable = executable;
            mBytecode = executable.getBytecodeWriter();
        }

        @Override
        public void visit(BodyNode node) {
            List<StatementNode> statements = node.getStatements();
            for (StatementNode statement : statements) {
                statement.accept(this);
            }
        }

        @Override
        public void visit(StatementNode statement) {
            List<Node> nodes = statement.getNodes();
            for (Node node : nodes) {
                node.accept(this);
            }
        }

        @Override
        public void visit(BlockNode node) {
            STBlock block = compileBlock(node);
            int index = mExecutable.placeLiteral(block);
            mBytecode.append(BytecodeType.PUSH_BLOCK, index, node);
        }

        @Override
        public void visit(AssignNode node) {
            StatementNode statement = node.getValue();
            statement.accept(this);

            String assignString = node.getAssignName();
            STSymbol assignName = STSymbol.unique(assignString);
            
            int index = mExecutable.placeLiteral(assignName);

            if (index < 0) {
                compileError(node, "Unknown variable %s", assignString);
            }

            mBytecode.append(BytecodeType.ASSIGN, index, node);
        }

        @Override
        public void visit(ArrayNode array) {
            List<Node> elements = array.getElements();
            for (Node element : elements) {
                element.accept(this);
            }
            int size = array.getSize();

            mBytecode.append(BytecodeType.PUSH_ARRAY, size, array);
        }

        @Override
        public void visit(ReturnNode node) {
            mBytecode.append(BytecodeType.STACK_RETURN, 0, node);
        }

        @Override
        public void visit(Node node) {
            compileError(node, "Unknown node to compile");
        }

        @Override
        public void visit(NameTermNode node) {
            String name = node.getName();
            if (name.equals("false")) {
                mBytecode.pushConstant(BytecodeType.Constant.FALSE, node);
            } else if (name.equals("true")) {
                mBytecode.pushConstant(BytecodeType.Constant.TRUE, node);
            } else if (name.equals("nil")) {
                mBytecode.pushConstant(BytecodeType.Constant.NIL, node);
            }  else {
                STSymbol symbol = STSymbol.unique(name);
                int index = mExecutable.placeLiteral(symbol);

                mBytecode.append(BytecodeType.PUSH_OBJECT, index, node);
            }
        }

        @Override
        public void visit(MessageSelectorNode node) {
            int countArguments = node.getCountArguments();
            if (countArguments == -1) {
                compileError(node, "Illegal message selector %s",
                        node.getSelector());
            }

            STSymbol selector = STSymbol.unique(node.getSelector());
            int index = mExecutable.placeLiteral(selector);
            mBytecode.append(BytecodeType.PUSH_LITERAL, index, node);
            mBytecode.append(BytecodeType.SEND_MESSAGE, countArguments, node);
        }

        @Override
        public void visit(LiteralArrayNode node) {
            STObject array = null;
            try {
                array = node.createObject();
            } catch (NodeFactoryException e) {
                e.printStackTrace();
            }
            int index = mExecutable.placeLiteral(array);
            mBytecode.append(BytecodeType.PUSH_LITERAL, index, node);
        }

        @Override
        public void visit(FloatNode node) {
            STObject floating = node.createObject();
            int index = mExecutable.placeLiteral(floating);
            mBytecode.append(BytecodeType.PUSH_LITERAL, index, node);
        }

        @Override
        public void visit(CharacterNode node) {
            STObject character = node.createObject();
            int index = mExecutable.placeLiteral(character);
            mBytecode.append(BytecodeType.PUSH_LITERAL, index, node);
        }

        @Override
        public void visit(SymbolNode node) {
            STObject symbol = node.createObject();
            int index = mExecutable.placeLiteral(symbol);
            mBytecode.append(BytecodeType.PUSH_LITERAL, index, node);
        }

        @Override
        public void visit(StringNode node) {
            STObject string = node.createObject();
            int index = mExecutable.placeLiteral(string);
            mBytecode.append(BytecodeType.PUSH_LITERAL, index, node);
        }

        @Override
        public void visit(LargeIntegerNode node) {
            STObject integer = node.createObject();
            int index = mExecutable.placeLiteral(integer);
            mBytecode.append(BytecodeType.PUSH_LITERAL, index, node);
        }

        @Override
        public void visit(IntegerNode node) {
            STObject integer = node.createObject();
            int index = mExecutable.placeLiteral(integer);
            mBytecode.append(BytecodeType.PUSH_LITERAL, index, node);
        }

    }
    
    //////////////////////////////////////////////////////////////////
    
    public void compileProgram(ProgramNode program, STScope scope){
        List<Node> nodes = program.getNodes();
        for (Node node : nodes) {
            if(node instanceof EvalNode) {
                compileAndExecuteEval((EvalNode) node);
            } else if( node instanceof ClassNode) {
                compileClass((ClassNode) node, scope);
            } 
            else if( node instanceof ExtendNode) {
                compileExtend((ExtendNode) node, scope);
            } 
        }
    }
    
    private void compileExtend(ExtendNode node, STScope scope) {
        STSymbol className = STSymbol.unique(node.getClassName());
        STClass stclass = scope.getAndCast(className);

        if (stclass == null) {
            compileError(node, "class extend error: class %s not exist",
                    className.toString());
        }

        List<MethodNode> methods = node.getMethods();
        for (MethodNode method : methods) {
            compileMethod(method, stclass);
        }
        
        scope.put(className, stclass);
    }

    private void compileAndExecuteEval(EvalNode node) {
        STExecutableObject executable = compileEval(node);
        //call Object immediately
        SchedulingSuite.callExecutableInNewProcess(executable);
    }
    
    public STExecutableObject compileEval(EvalNode node) {
      
        STExecutableObject executable = null;
        try {
            executable = (STExecutableObject) node.createObject();
        } catch (NodeFactoryException e) {
            compileError(node, e.getMessage());
        }
        executable.getBytecodeWriter().pushConstant(BytecodeType.Constant.NIL, null);
        compileExecutableObjectBody(node, executable);
        executable.getBytecodeWriter().append(BytecodeType.STACK_RETURN, 0, null);
        return executable;
    }
    
    private void compileError(Node node, String message, Object... args) {
        SignalSuite.error("Compile Error in " + node.getClass().getSimpleName() + " " + message, args);
    }

    private void compileClass(ClassNode classNode, STScope scope) {
        STSymbol className = STSymbol.unique(classNode.getClassName());
        STClass existed = scope.getAndCast(className);

        if (existed != null) {
            compileError(classNode, "class %s already exist",
                    className.toString());
        }

        STSymbol superClassName = STSymbol
                .unique(classNode.getSuperclassName());
        STClass superclass = scope.getAndCast(superClassName);
        
        if (superclass == null) {
            compileError(classNode, "superclass %s not found",
                    superClassName.toString());
        }
        
        STMetaclass metaclass = null;
        if(classNode.getMetaClassName() != null) {
            STSymbol metaClassName = STSymbol
                    .unique(classNode.getMetaClassName());
            metaclass = scope.getAndCast(metaClassName);
        }
        else {
            metaclass = STMetaclass.create();
        }
        
        STClass stclass = metaclass.createSubclassOf(className, superclass);
        
        stclass.setComment(STSymbol.unique(classNode.getComment()));
        stclass.setCategory(STSymbol.unique(classNode.getCategory()));
        stclass.setName(className);

        VariableNames instanceVariables = classNode.getInstanceVariableNames();

        for (String varName : instanceVariables) {
            try {
                stclass.addInstanceVariable(STSymbol.unique(varName));
            } catch (DuplicateVariableException e) {
                compileError(classNode, "Duplicate instance variable %s",
                        varName);
            }
        }

        VariableNames classVariableNames = classNode.getClassVariableNames();

        for (String varName : classVariableNames) {
            try {
                stclass.addClassVariable(STSymbol.unique(varName));
            } catch (DuplicateVariableException e) {
                compileError(classNode, "Duplicate class variable %s", varName);
            }
        }

        List<MethodNode> methods = classNode.getMethods();
        for (MethodNode method : methods) {
            compileMethod(method, stclass);
        }
        
        scope.put(className, stclass);
    }

    private void compileMethod(MethodNode methodNode, STClass stclass) {
        STSymbol selector = STSymbol.unique(methodNode.getSelector());
        STMethod method = null;
       
        try {
            method = (STMethod) methodNode.createObject();
        } catch (NodeFactoryException e) {
           compileError(methodNode, e.getMessage());
        }
        
        method.getBytecodeWriter().pushConstant(BytecodeType.Constant.SELF, null);
        compileExecutableObjectBody(methodNode, method);
        //FAKE RETURN
        method.getBytecodeWriter().append(BytecodeType.STACK_RETURN, 0, null);
        
        if(methodNode.isStatic()) {
            stclass.addStaticMethod(selector, method);
        }
        else {
            stclass.addMethod(selector, method);
        }
    }
    
    private void compileExecutableObjectBody(ExecutableNode node, STExecutableObject executable) {
        MethodCompiler compiler = new MethodCompiler(executable);
        node.accept(compiler);
    }
    
    private STBlock compileBlock(BlockNode blockNode) {
        STBlock block = null;
        try {
            block = (STBlock) blockNode.createObject();
        } catch (NodeFactoryException e) {
            compileError(blockNode, e.getMessage());
        }
        BodyNode body = blockNode.getBody();
        
        block.getBytecodeWriter().pushConstant(BytecodeType.Constant.NIL, null);
        compileExecutableObjectBody(body, block);
        //FAKE RETURN TOP ELEMENT
        block.getBytecodeWriter().append(BytecodeType.STACK_RETURN, 0, null);
        
        return block;
    }

    
}
