package ua.ho.gloryofrobots.childtalk.compilation;

import java.util.List;

import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeType;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeWriter;
import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.node.*;
import ua.ho.gloryofrobots.childtalk.node.ClassNode.VariableNames;
import ua.ho.gloryofrobots.childtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.childtalk.stobject.*;


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
            STSymbol assignName = STSymbol.create(assignString);
            
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
            if(mExecutable instanceof STBlock) {
                mBytecode.append(BytecodeType.BLOCK_RETURN, 0, node);
            } else {
                mBytecode.append(BytecodeType.STACK_RETURN, 0, node);
            }
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
            } else if (name.equals("super")) {
                mBytecode.append(BytecodeType.PUSH_SUPER, 0, node);
            } else {
                STSymbol symbol = STSymbol.create(name);
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
            
            STSymbol selector = STSymbol.create(node.getSelector());
            int index = mExecutable.placeLiteral(selector);
            mBytecode.append(BytecodeType.PUSH_LITERAL, index, node);
            
            BytecodeType receiverCode = mBytecode.getCodeFromEnd(countArguments + 2);
            if(receiverCode == BytecodeType.PUSH_SUPER) {
                mBytecode.append(BytecodeType.SEND_MESSAGE_TO_SUPER, countArguments, node);
            } else {
                mBytecode.append(BytecodeType.SEND_MESSAGE, countArguments, node);
            }
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
        STSymbol className = STSymbol.create(node.getClassName());
        STClass stclass = scope.getAndCast(className);

        if (stclass == null) {
            compileError(node, "class extend error: class %s not exist",
                    className.toString());
        }

        List<MethodNode> methods = node.getMethods();
        for (MethodNode method : methods) {
            compileMethod(method, stclass);
        }        
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
        BodyNode body = node.getBody();
        compileExecutableObjectBody(body, executable);
        
        executable.getBytecodeWriter().append(BytecodeType.STACK_RETURN, 0, null);
        return executable;
    }
    
    private void compileError(Node node, String message, Object... args) {
        SignalSuite.error("Compile Error in " + node.getClass().getSimpleName() + " " + message, args);
    }

    private void compileClass(ClassNode classNode, STScope scope) {
        STSymbol className = STSymbol.create(classNode.getClassName());
        STClass existed = scope.getAndCast(className);

        if (existed != null) {
            compileError(classNode, "class %s already exist",
                    className.toString());
        }

        STSymbol superClassName = STSymbol
                .create(classNode.getSuperclassName());
        STClass superclass = scope.getAndCast(superClassName);
        
        if (superclass == null) {
            compileError(classNode, "superclass %s not found",
                    superClassName.toString());
        }
        
        STMetaclass metaclass = null;
        if(classNode.getMetaClassName() != null) {
            STSymbol metaClassName = STSymbol
                    .create(classNode.getMetaClassName());
            metaclass = scope.getAndCast(metaClassName);
        }
        else {
            metaclass = STMetaclass.create();
        }
        
        STClass stclass = metaclass.createSubclassOf(className, superclass);
        
        stclass.setComment(STSymbol.create(classNode.getComment()));
        stclass.setCategory(STSymbol.create(classNode.getCategory()));
        stclass.setName(className);

        VariableNames instanceVariables = classNode.getInstanceVariableNames();

        for (String varName : instanceVariables) {
            try {
                stclass.addInstanceVariable(STSymbol.create(varName));
            } catch (DuplicateVariableException e) {
                compileError(classNode, "Duplicate instance variable %s",
                        varName);
            }
        }

        VariableNames classVariableNames = classNode.getClassVariableNames();

        for (String varName : classVariableNames) {
            try {
                stclass.addClassVariable(STSymbol.create(varName));
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
        STSymbol selector = STSymbol.create(methodNode.getSelector());
        STMethod method = null;
       
        try {
            method = (STMethod) methodNode.createObject();
        } catch (NodeFactoryException e) {
           compileError(methodNode, e.getMessage());
        }
        
        //method.getBytecodeWriter().pushConstant(BytecodeType.Constant.SELF, null);
        BodyNode body = methodNode.getBody();
        compileExecutableObjectBody(body, method);
        //FAKE RETURN
        method.getBytecodeWriter().append(BytecodeType.SELF_RETURN, 0, null);
        
        if(methodNode.isStatic()) {
            stclass.addStaticMethod(selector, method);
        }
        else {
            stclass.addMethod(selector, method);
        }
    }
    
    private void compileExecutableObjectBody(Node node, STExecutableObject executable) {
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
        
        block.getBytecodeWriter().pushConstant(BytecodeType.Constant.NIL, null);
        
        BodyNode body = blockNode.getBody();
        compileExecutableObjectBody(body, block);
        
        //FAKE RETURN TOP ELEMENT
        block.getBytecodeWriter().append(BytecodeType.STACK_RETURN, 0, null);
        
        return block;
    }

    
}
