package ua.ho.gloryofrobots.childtalk.compilation;

import java.util.List;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
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
        private BytecodeWriter mWriter;

        MethodCompiler(STExecutableObject executable, BytecodeWriter writer) {
            mExecutable = executable;
            mWriter = writer;
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
            mWriter.append(BytecodeType.PUSH_BLOCK, index, node);
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

            mWriter.append(BytecodeType.ASSIGN, index, node);
        }

        @Override
        public void visit(ArrayNode array) {
            List<Node> elements = array.getElements();
            for (Node element : elements) {
                element.accept(this);
            }
            int size = array.getSize();

            mWriter.append(BytecodeType.PUSH_ARRAY, size, array);
        }

        @Override
        public void visit(ReturnNode node) {
            if (mExecutable instanceof STBlock) {
                mWriter.append(BytecodeType.BLOCK_RETURN, 0, node);
            } else {
                mWriter.append(BytecodeType.STACK_RETURN, 0, node);
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
                mWriter.pushConstant(BytecodeType.Constant.FALSE, node);
            } else if (name.equals("true")) {
                mWriter.pushConstant(BytecodeType.Constant.TRUE, node);
            } else if (name.equals("nil")) {
                mWriter.pushConstant(BytecodeType.Constant.NIL, node);
            } else if (name.equals("super")) {
                mWriter.append(BytecodeType.PUSH_SUPER, 0, node);
            } else {
                STSymbol symbol = STSymbol.create(name);
                int index = mExecutable.placeLiteral(symbol);

                mWriter.append(BytecodeType.PUSH_OBJECT, index, node);
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
            mWriter.append(BytecodeType.PUSH_LITERAL, index, node);

            BytecodeType receiverCode = mWriter
                    .getCodeFromEnd(countArguments + 2);
            if (receiverCode == BytecodeType.PUSH_SUPER) {
                mWriter.append(BytecodeType.SEND_MESSAGE_TO_SUPER,
                        countArguments, node);
            } else {
                mWriter.append(BytecodeType.SEND_MESSAGE, countArguments,
                        node);
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
            mWriter.append(BytecodeType.PUSH_LITERAL, index, node);
        }

        @Override
        public void visit(FloatNode node) {
            STObject floating = node.createObject();
            int index = mExecutable.placeLiteral(floating);
            mWriter.append(BytecodeType.PUSH_LITERAL, index, node);
        }

        @Override
        public void visit(CharacterNode node) {
            STObject character = node.createObject();
            int index = mExecutable.placeLiteral(character);
            mWriter.append(BytecodeType.PUSH_LITERAL, index, node);
        }

        @Override
        public void visit(SymbolNode node) {
            STObject symbol = node.createObject();
            int index = mExecutable.placeLiteral(symbol);
            mWriter.append(BytecodeType.PUSH_LITERAL, index, node);
        }

        @Override
        public void visit(StringNode node) {
            STObject string = node.createObject();
            int index = mExecutable.placeLiteral(string);
            mWriter.append(BytecodeType.PUSH_LITERAL, index, node);
        }

        @Override
        public void visit(LargeIntegerNode node) {
            STObject integer = node.createObject();
            int index = mExecutable.placeLiteral(integer);
            mWriter.append(BytecodeType.PUSH_LITERAL, index, node);
        }

        @Override
        public void visit(IntegerNode node) {
            STObject integer = node.createObject();
            int index = mExecutable.placeLiteral(integer);
            mWriter.append(BytecodeType.PUSH_LITERAL, index, node);
        }

    }

    // ////////////////////////////////////////////////////////////////

    public void compileProgram(ProgramNode program, STScope scope) {
        List<Node> nodes = program.getNodes();
        for (Node node : nodes) {
            if (node instanceof EvalNode) {
                compileAndExecuteEval((EvalNode) node);
            } else if (node instanceof ClassNode) {
                compileClass((ClassNode) node, scope);
            } else if (node instanceof ExtendNode) {
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

    public STProcess compileAndExecuteEval(EvalNode node) {
        STExecutableObject executable = compileEval(node);
        // call Object immediately
        return SchedulingSuite.callExecutableInNewProcess(executable,
                ImageSuite.image().objects().NIL);
    }

    public STExecutableObject compileEval(EvalNode node) {

        STExecutableObject executable = null;
        try {
            executable = (STExecutableObject) node.createObject();
        } catch (NodeFactoryException e) {
            compileError(node, e.getMessage());
        }
        
        BytecodeWriter writer = new BytecodeWriter(executable);
        writer.pushConstant(BytecodeType.Constant.NIL, null);
        BodyNode body = node.getBody();
        compileExecutableObjectBody(body, executable, writer);

        writer.append(BytecodeType.STACK_RETURN, 0, null);
        return executable;
    }

    private void compileError(Node node, String message, Object... args) {
        SignalSuite.error("Compile Error in " + node.getClass().getSimpleName()
                + " " + message, args);
    }

    private void compileClass(ClassNode classNode, STScope scope) {
        STSymbol className = STSymbol.create(classNode.getClassName());
        STClass existed = scope.getAndCast(className);

        if (existed != null) {
            return;
            /*
             * compileError(classNode, "class %s already exist",
             * className.toString());
             */
        }

        STSymbol superClassName = STSymbol
                .create(classNode.getSuperclassName());
        STClass superclass = scope.getAndCast(superClassName);

        if (superclass == null) {
            compileError(classNode, "superclass %s not found",
                    superClassName.toString());
        }

        STMetaclass metaclass = null;
        if (classNode.getMetaClassName() != null) {
            STSymbol metaClassName = STSymbol.create(classNode
                    .getMetaClassName());
            metaclass = scope.getAndCast(metaClassName);
        } else {
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

        // method.getBytecodeWriter().pushConstant(BytecodeType.Constant.SELF,
        // null);
        BytecodeWriter writer = new BytecodeWriter(method);
        BodyNode body = methodNode.getBody();
        compileExecutableObjectBody(body, method, writer);
        // FAKE RETURN
        writer.append(BytecodeType.SELF_RETURN, 0, null);

        if (methodNode.isStatic()) {
            stclass.addStaticMethod(selector, method);
        } else {
            stclass.addMethod(selector, method);
        }
    }

    private void compileExecutableObjectBody(Node node,
            STExecutableObject executable, BytecodeWriter writer) {
        MethodCompiler compiler = new MethodCompiler(executable, writer);
        node.accept(compiler);
    }

    private STBlock compileBlock(BlockNode blockNode) {
        STBlock block = null;
        try {
            block = (STBlock) blockNode.createObject();
        } catch (NodeFactoryException e) {
            compileError(blockNode, e.getMessage());
        }
        
        BytecodeWriter writer = new BytecodeWriter(block);
        writer.pushConstant(BytecodeType.Constant.NIL, null);

        BodyNode body = blockNode.getBody();
        compileExecutableObjectBody(body, block, writer);

        // FAKE RETURN TOP ELEMENT
        writer.append(BytecodeType.STACK_RETURN, 0, null);

        return block;
    }

}
