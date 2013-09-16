package ua.ho.gloryofrobots.yellowtalk.compilation;

import java.io.FileInputStream;
import java.io.IOException;

import ua.ho.gloryofrobots.yellowtalk.compilation.ProgramTextStream.ProgramReadException;
import ua.ho.gloryofrobots.yellowtalk.compilation.Token.Type;
import ua.ho.gloryofrobots.yellowtalk.node.*;
import ua.ho.gloryofrobots.yellowtalk.node.NodeWithMetaData.UnknownMetaDataException;

public class Parser{
    final String SPECIAL_TOKEN_SUBCLASS = "subclass:";
    final String SPECIAL_TOKEN_EXTEND = "extend";
    final String SPECIAL_TOKEN_EVAL = "Eval";
    
    Lexer mLexer;
    
    public Parser(Lexer lexer) {
        mLexer = lexer;
    }

    private void parsingError(String txt, Token token) throws FileEvalException {
        System.out.println(token.stringValue());
        ((Lexer) mLexer).lexerError(txt, token);
    }
    
    public ProgramNode parse() throws FileEvalException {
        
        ProgramNode node = new ProgramNode();
        
        while (mLexer.current().type != Token.Type.END) {
            _parse(node);
        }

        return node;
    }

    protected void _parse(ProgramNode node) throws FileEvalException {
        
        if (parseClass(node) == true) {
            return;
        }

        if (parseExtend(node) == true) {
            return;
        }

        if (parseEval(node) == true) {
            return;
        }

        parsingError("Unknown parsing object", mLexer.current());
    }

    private boolean parseEval(ProgramNode node) throws FileEvalException {
        mLexer.startTransaction();
        if (mLexer.lookup(2) == false) {
            return false;
        }

        Token token = mLexer.current();

        if (token.type != Token.Type.NAME_CONST
                || token.equalValue("Eval") == false) {
            mLexer.rollBackTransaction();
            return false;
        }

        token = mLexer.next();

        if ((token.type == Token.Type.BINARY && token.equalValue('[')) == false) {
            parsingError("Missed [ in Eval declaration", token);
            return false;
        }
        
        mLexer.next();
        
        EvalNode eval = new EvalNode();
        
        int startPosition = mLexer.getCurrentTokenPosition();
        
        parseBody(eval);
        node.addNode(eval);
        
        int endPosition = mLexer.getPreviousTokenPosition();
        ProgramTextStreamInterface stream = mLexer.getStream();
        eval.initCompileInfo(startPosition, endPosition, stream);
        
        token = mLexer.current();
        if ((token.type == Token.Type.CLOSING && token.equalValue(']')) == false) {
            parsingError("Missed closing ]", token);
            return false;
        }
        
        mLexer.next();
        mLexer.endTransaction();
        return true;
    }

    protected boolean parseExtend(ProgramNode node) throws FileEvalException {
        mLexer.startTransaction();
        
        if (mLexer.lookup(2) == false) {
            return false;
        }
        
        Token tokenClassName = mLexer.current();

        if (tokenClassName.type != Token.Type.NAME_CONST) {
            return false;
        }

        String className = tokenClassName.stringValue();
        Token token = mLexer.next();
        
        if (token.equalValue(SPECIAL_TOKEN_EXTEND) == false) {
            mLexer.rollBackTransaction();
            return false;
        }

        token = mLexer.next();

        if ((token.type != Token.Type.BINARY || token.equalValue('[')) == false) {
            parsingError("Missed [ in class declaration", token);
            return false;
        }

        mLexer.next();

        ExtendNode extendNode = new ExtendNode();
        extendNode.setClassName(className);

        while (true) {
            token = mLexer.current();
            if (token.type == Token.Type.CLOSING && token.equalValue(']')) {
                break;
            } 
            if (parseClassMethod(extendNode) == false) {
                parsingError("extend block can contain only methods", token);
                return false;
            }
        }

        token = mLexer.current();
        if ((token.type != Token.Type.BINARY || token.equalValue(']')) == false) {
            parsingError("extend declaration must ended with ]", token);
            return false;
        }

        node.addNode(extendNode);
        mLexer.next();
        
        mLexer.endTransaction();
        return true;
    }

    protected boolean parseClass(ProgramNode node) throws FileEvalException {
        mLexer.startTransaction();
        
        if (mLexer.lookup(2) == false) {
            return false;
        }

        Token tokenSubclass = mLexer.current();

        if (tokenSubclass.type != Token.Type.NAME_CONST) {
            return false;
        }
        String superClassName = tokenSubclass.stringValue();
        Token token = null;

        token = mLexer.next();
        if (token.equalValue(SPECIAL_TOKEN_SUBCLASS) == false) {
            mLexer.rollBackTransaction();
            return false;
        }

        token = mLexer.next();
        if (token.type != Token.Type.NAME_CONST) {
            parsingError("Invalid class name", token);
            return false;
        }

        String className = token.stringValue();
        token = mLexer.next();

        if ((token.type == Token.Type.BINARY && token.equalValue('[')) == false) {
            mLexer.rollBackTransaction();
            parsingError("Missed [ in class declaration", token);
            return false;
        }

        mLexer.next();

        ClassNode classNode = new ClassNode();
        classNode.setSuperclassName(superClassName);
        classNode.seClassName(className);

        while (true) {
            token = mLexer.current();
            if (token.type == Token.Type.CLOSING && token.equalValue(']')) {
                break;
            } else if (parseMetaData(classNode)) {
                continue;
            } else if (parseInstanceVariables(classNode)) {
                continue;
            } else if (parseClassVariables(classNode)) {
                continue;
            } else if (parseClassMethod(classNode)) {
                continue;
            } else {
                parsingError("Class declaration must ended with ]", token);
                return false;
            }
        }

        node.addNode(classNode);
        mLexer.next();
        mLexer.endTransaction();
        return true;
    }

    private boolean parseInstanceVariables(ClassNode classNode)
            throws FileEvalException {
        Token token = null;
        token = mLexer.current();
        if ((token.type == Token.Type.BINARY && token.equalValue('|')) == false) {
            return false;
        }

        token = mLexer.next();
        while (token.type == Token.Type.NAME_CONST) {
            classNode.addInstanceVariable(token.stringValue());
            token = mLexer.next();
        }

        if ((token.type == Token.Type.BINARY && token.equalValue('|')) == false) {
            parsingError("Missed closing |", token);
            return false;
        }

        mLexer.next();
        return true;
    }

    private boolean parseClassVariables(ClassNode classNode)
            throws FileEvalException {
        Token token = null;
        token = mLexer.current();
        if ((token.type == Token.Type.BINARY && token.equalValue('/')) == false) {
            return false;
        }

        token = mLexer.next();
        while (token.type == Token.Type.NAME_CONST) {
            classNode.addClassVariable(token.stringValue());
            token = mLexer.next();
        }

        if ((token.type == Token.Type.BINARY && token.equalValue('/')) == false) {
            parsingError("Missed closing /", token);
            return false;
        }

        mLexer.next();
        return true;
    }

    protected boolean parseMetaData(NodeWithMetaData node)
            throws FileEvalException {
        Token tokenCheck = null;
        tokenCheck = mLexer.current();
        if ((tokenCheck.type == Token.Type.BINARY && tokenCheck.equalValue('<')) == false) {
            return false;
        }
        Token tokenLabel = mLexer.next();
        if (tokenLabel.type != Token.Type.NAME_COLON) {
            parsingError("Invalid class metadata label", tokenCheck);
            return false;
        }

        Token tokenData = mLexer.next();
        if (tokenData.type != Token.Type.STRING_CONST) {
            parsingError("Metadata name must be string", tokenCheck);
            return false;
        }

        tokenCheck = mLexer.next();
        if ((tokenCheck.type == Token.Type.BINARY && tokenCheck.equalValue('>')) == false) {
            parsingError("Missed closing >", tokenCheck);
            return false;
        }

        try {
            node.setMetaData(tokenLabel.stringValue(), tokenData.stringValue());
        } catch (UnknownMetaDataException e) {
            parsingError(e.getMessage(), tokenLabel);
        }

        mLexer.next();
        return true;
    }

    protected boolean parseClassMethod(MethodsContainerNode methodsContainerNode)
            throws FileEvalException {
        MethodNode method = new MethodNode();
        parseMethodModificator(method);
        parseMethodMessagePattern(method);
        
        //DEBUG ONLY
        if(methodsContainerNode instanceof ClassNode) {
            method.setClassName( ((ClassNode)methodsContainerNode).getClassName());
        }
        
        Token token = mLexer.current();
        if ((token.type == Token.Type.BINARY && token.equalValue('[')) == false) {
            parsingError("Missed [ in method declaration", token);
            return false;
        }
        
        mLexer.next();
        BodyNode body = method.getBody();
        //catch method body first position
        int startPosition = mLexer.getCurrentTokenPosition();
        
        while (true) {
            if (parseMetaData(method) == false) {
                break;
            }
        }

        parseMethodTemporaries(method);
        parseBody(body);
        
        //catch method body last position
        int endPosition = mLexer.getPreviousTokenPosition();
        ProgramTextStreamInterface stream = mLexer.getStream();
        method.initCompileInfo(startPosition, endPosition, stream);
        
        token = mLexer.current();
        if ((token.type == Token.Type.CLOSING && token.equalValue(']')) == false) {
            parsingError("Missed closing ] in method declaration", token);
            return false;
        }
        
        mLexer.next();
        methodsContainerNode.addMethod(method);
        return true;
    }

    protected void parseMethodTemporaries(MethodNode method)
            throws FileEvalException {
        Token token = mLexer.current();
        if ((token.type == Token.Type.BINARY && token.equalValue('|')) == false) {
            return;
        }

        token = mLexer.next();
        while (token.type == Token.Type.NAME_CONST) {
            method.addTemporary(token.stringValue());
            token = mLexer.next();
        }

        if (!(token.type == Token.Type.BINARY && token.equalValue('|'))) {
            parsingError("Temporary list not terminated by bar", token);

        }

        mLexer.next();
    }
    
    protected void parseMethodModificator(MethodNode method) throws FileEvalException {
        Token token = mLexer.current();
        
        if(token.type != Token.Type.BINARY || token.equalValue('(') == false) {
            return;
        }
            
        token = mLexer.next();
        if(token.type != Token.Type.BINARY){
            parsingError("class/instance method modificator must be + or -", token);
        }
        
        if(token.equalValue('+')) {
            method.setStatic(true);
        } else if(token.equalValue('-')) {
            method.setStatic(false);
        } else {
            parsingError("class/instance method modificator must be + or -", token);
        }
        
        token = mLexer.next();
        
        if(token.type != Token.Type.CLOSING || token.equalValue(')') == false){
            parsingError("missed closing ) in class/instance method modificator", token);
        }
        
        mLexer.next();        
    }
    
    protected void parseMethodMessagePattern(MethodNode method)
            throws FileEvalException {
        
        Token token = mLexer.current();
        switch (token.type) {
        case NAME_CONST:
            method.setSelector(token.stringValue());
            mLexer.next();
            break;
        case BINARY:
            /* Binary message pattern */
            method.setSelector(token.stringValue());

            token = mLexer.next();
            if (token.type != Token.Type.NAME_CONST) {
                parsingError("Expected name constant for argument name\n",
                        token);
            }

            method.addArgument(token.stringValue());
            mLexer.next();
            break;
        case NAME_COLON:
            /* Keyword message pattern */
            String selector = new String();
            while (token.type == Token.Type.NAME_COLON) {

                selector += token.stringValue();
                token = mLexer.next();

                if (token.type != Token.Type.NAME_CONST) {
                    parsingError("Expected name constant for argument name\n",
                            token);

                }

                method.addArgument(token.stringValue());

                token = mLexer.next();
            }
            method.setSelector(selector);

            break;
        default:
            parsingError("Invalid message pattern\n", token);
            break;

        }

    }

    public void DEBUG_LOG(String message, Token token) {
        System.out.printf("LOG : %s Token : %s\n", message, token);
    }

    public void DEBUG_LOG(String function, String message) {
        System.out.printf("LOG : %s  %s\n", function, message);
    }

    protected void parseBody(BodyNode body) throws FileEvalException {
        Token token = mLexer.current();

        while (true) {
            DEBUG_LOG("parseBody", token);

            StatementNode statement = parseStatement();
            DEBUG_LOG("getStatement", statement.toString());
            body.addStatement(statement);

            token = mLexer.current();
            if (token.type == Token.Type.CLOSING && token.equalValue(']')) {
                return;
            }

            token = mLexer.current();
        }
        //
        // if (m_in_block)
        // {
        // ERROR_SIGNAL(GSE_ERROR_INTERP,
        // NEW_OBJECT<String>("Expected ] after block body"));
        // }

    }

    protected void parseBlockBody(BodyNode body) throws FileEvalException {
        //TODO DELETE DUPPLICATE
        Token token = mLexer.current();

        while (true) {
            DEBUG_LOG("parseBlockBody", token);

            StatementNode statement = parseStatement();
            DEBUG_LOG("getStatement parseBlockBody", statement.toString());
            body.addStatement(statement);
            token = mLexer.current();

            if (token.type == Token.Type.CLOSING && token.equalValue(']')) {
                break;
            }

            token = mLexer.current();
        }

        //
        // if (m_in_block)
        // {
        // ERROR_SIGNAL(GSE_ERROR_INTERP,
        // NEW_OBJECT<String>("Expected ] after block body"));
        // }

    }

    private StatementNode parseStatement() throws FileEvalException {
        Token token = mLexer.current();
        DEBUG_LOG("parseStatement", token);
        StatementNode statement = new StatementNode();
        statement.itIsYourToken(token);
        if (token.type == Token.Type.BINARY && token.equalValue("^")) {
            ReturnNode returnNode = new ReturnNode();
            returnNode.itIsYourToken(token);
            mLexer.next();
            parseExpression(statement);
            statement.addNode(returnNode);
        } else {
            parseExpression(statement);
        }
        
        token = mLexer.current();
        while (token.type == Token.Type.CLOSING && token.equalValue('.')) {
            token = mLexer.next();
        }
        return statement;
    }

    private void parseExpression(StatementNode statement)
            throws FileEvalException {
        Token token = mLexer.current();
        DEBUG_LOG("parseExpression", token);

        if (token.type == Token.Type.NAME_CONST) {
            String assign_name = token.stringValue();

            token = mLexer.next();
            if (token.type == Token.Type.BINARY
                    && (token.equalValue(":=") || token.equalValue("<-"))) {

                mLexer.next();
                DEBUG_LOG("parseAssignment", mLexer.current());
                AssignNode assign = parseAssignment(assign_name);
                statement.addNode(assign);
            } else {

                NameTermNode term = parseNameTerm(assign_name);
                statement.addNode(term);
            }
        } else {
            Node term = parseTerm();
            statement.addNode(term);
        }

        /* After we got the initial object, we can do message continuation on it */
        parseContinuation(statement);

    }

    private void parseContinuation(StatementNode statement)
            throws FileEvalException {
        DEBUG_LOG("parseContinuation", mLexer.current());
        parseKeyContinuation(statement);

        Token token = mLexer.current();

        while (token.type == Token.Type.CLOSING && token.charValue() == ';') {
            mLexer.next();
            statement.onCascade();
            
            parseKeyContinuation(statement);
            token = mLexer.current();
        }
    }

    private void parseKeyContinuation(StatementNode statement)
            throws FileEvalException {
        DEBUG_LOG("parseKeyContinuation", mLexer.current());

        parseBinaryContinuation(statement);

        Token token = mLexer.current();
        if (token.type != Token.Type.NAME_COLON) {
            return;
        }
        String selector = new String();
        StatementNode messageStatement = new StatementNode();
        messageStatement.itIsYourToken(token);
        
        Token messageBegin = token;

        MessageSelectorNode message = new MessageSelectorNode();
        message.itIsYourToken(messageBegin);
        
        
        int count = 0;
        while (token.type == Token.Type.NAME_COLON) {
            selector += token.stringValue();
            count++;
            mLexer.next();

            Node term = parseTerm();
            messageStatement.addNode(term);

            parseBinaryContinuation(messageStatement);

            token = mLexer.current();
        }

        statement.addNode(messageStatement);
        
        message.setCountArguments(count);
        message.setSelector(selector);
        statement.addNode(message);
    }

    private void parseBinaryContinuation(StatementNode statement)
            throws FileEvalException {
        DEBUG_LOG("parseBinaryContinuation", mLexer.current());
        parseUnaryContinuation(statement);
        Token token = mLexer.current();

        while (token.type == Token.Type.BINARY) {

            String selector = token.stringValue();

            mLexer.next();
            
            StatementNode messageStatement = new StatementNode();
            messageStatement.itIsYourToken(mLexer.current());
            
            Node term = parseTerm();
            messageStatement.addNode(term);

            parseUnaryContinuation(messageStatement);
            statement.addNode(messageStatement);
            MessageSelectorNode message = new MessageSelectorNode();
            message.setSelector(selector);
            message.setCountArguments(messageStatement.getSize());
            statement.addNode(message);
            // token = m_lexer.next();
            token = mLexer.current();
        }
    }

    private void parseUnaryContinuation(StatementNode messageStatement)
            throws FileEvalException {
        DEBUG_LOG("parseUnaryContinuation", mLexer.current());
        Token token = mLexer.current();

        while (token.type == Token.Type.NAME_CONST) {
            MessageSelectorNode message = new MessageSelectorNode();
            message.setSelector(token.stringValue());
            message.itIsYourToken(token);
            message.setCountArguments(0);
            messageStatement.addNode(message);
            token = mLexer.next();
        }
    }

    private Node parseTerm() throws FileEvalException {
        Token token = mLexer.current();
        Token tokenStart = token;
        DEBUG_LOG("parseTerm", mLexer.current());
        Node result = null;
        switch (token.type) {
        case NAME_CONST:
            result = parseNameTerm(token.stringValue());
            break;
        case STRING_CONST:
            result = new StringNode(token.stringValue());
            break;
        case INTEGER_CONST:
            result = new IntegerNode(token.integerValue());
            break;
        case LARGE_INTEGER_CONST:
            result = new LargeIntegerNode(token.largeIntegerValue());
            break;

        case FLOAT_CONST:
            result = new FloatNode(token.floatValue());
            break;
        case SYMBOL_CONST:
            result = new SymbolNode(token.stringValue());

            break;
        case CHAR_CONST:
            result = new CharacterNode(token.charValue());
            break;
        case ARRAY_BEGIN:
            result = parseLiteralArray();
            break;
        case BINARY:
            if (token.equalValue("(")) {
                token = mLexer.next();
                result = new StatementNode();
                parseExpression((StatementNode) result);

                token = mLexer.current();
                if ((token.type == Token.Type.CLOSING && token.charValue() == ')') == false) {
                    parsingError("Expected ) after sub expression", token);
                }

                break;
            } else if (token.equalValue("[")) {
                result = parseBlock();
                break;
            } else if (token.equalValue("{")) {
                result = parseArray();
                break;
            }
        default:
            switch (token.type) {
            case END:
                parsingError("Unexpected end of input", token);
                break;
            case STRING_ENTRY:
                parsingError(
                        String.format("Invalid expression start: %s",
                                token.stringValue()), token);
                break;
            case CLOSING:
                parsingError(
                        String.format("Unexpected closing: %s",
                                token.stringValue()), token);
                break;
            default:
                parsingError("Expected expression", token);
                break;
            }
            break;
        }
        mLexer.next();
        result.itIsYourToken(tokenStart);
        return result;
    }

    private Node parseBlock() throws FileEvalException {
        BlockNode block = new BlockNode();
        DEBUG_LOG("parseBlock", mLexer.current());
        Token token = mLexer.current();
        if ((token.type == Token.Type.BINARY && token.equalValue('[')) == false) {
            parsingError("Missed [ in block declaration", token);
        }
        
        mLexer.next();
        
        BodyNode body = block.getBody();
        //catch first position of block body
        int startPosition = mLexer.getCurrentTokenPosition();
        parseBlockMessagePattern(block);
        parseBlockBody(body);
        //catch last position of block body
        int endPosition = mLexer.getPreviousTokenPosition();
        ProgramTextStreamInterface stream = mLexer.getStream();
        block.initCompileInfo(startPosition, endPosition, stream);
        
        token = mLexer.current();
        if ((token.type == Token.Type.CLOSING && token.equalValue(']')) == false) {
            parsingError("Missed closing ] in block declaration", token);
        }

        return block;
    }

    private void parseBlockMessagePattern(BlockNode block)
            throws FileEvalException {

        Token token = mLexer.current();
        DEBUG_LOG("parseBlockMessagePattern", mLexer.current());

        if (!(token.type == Token.Type.BINARY && token.equalValue(":"))) {
            return;
        }

        while (token.type == Token.Type.BINARY && token.equalValue(":")) {

            token = mLexer.next();

            if (token.type != Token.Type.NAME_CONST) {
                parsingError("Expected name  for argument\n", token);
            }

            block.addArgument(token.stringValue());
            token = mLexer.next();
        }

        if ((token.type == Token.Type.BINARY && token.equalValue("|")) == false) {
            parsingError("Expected | after block message pattern\n", token);
        }

        mLexer.next();
    }

    private ArrayNode parseArray() throws FileEvalException {
        Token token = mLexer.next();
        ArrayNode array = new ArrayNode();

        token = mLexer.next();
        while ((token.type == Token.Type.END || (token.type == Token.Type.CLOSING && token
                .charValue() == '}')) == false) {
            StatementNode element = new StatementNode();
            parseExpression(element);
            array.addElement(element);

            token = mLexer.next();
            if (token.type == Token.Type.CLOSING && token.charValue() == '.')
                token = mLexer.next();
        }

        return array;
    }

    private Node parseLiteralArray() throws FileEvalException {
        LiteralArrayNode array = new LiteralArrayNode();

        Token token = mLexer.next();
        while (!(token.type == Token.Type.END || (token.type == Token.Type.CLOSING && token
                .charValue() == ')'))) {
            switch (token.type) {
            case ARRAY_BEGIN:
                array.addElement(parseLiteralArray());

                break;
            case INTEGER_CONST:
                array.addElement(new IntegerNode(token.integerValue()));
                break;
            case BINARY:
                if (token.equalValue("(")) {
                    array.addElement(parseLiteralArray());

                    break;
                }
            case NAME_CONST:
            case NAME_COLON:
            case SYMBOL_CONST:
                array.addElement(new SymbolNode(token.stringValue()));

                break;
            case STRING_CONST:
                array.addElement(new StringNode(token.stringValue()));

                break;
            case CHAR_CONST:
                array.addElement(new CharacterNode(token.charValue()));
                break;
            default:
                parsingError("Illegal text in literal array\n", token);
                break;
            }

            token = mLexer.next();
        }

        return array;
    }

    private NameTermNode parseNameTerm(String assign_name) {
        NameTermNode term = new NameTermNode();
        term.setName(assign_name);
        return term;
    }

    private AssignNode parseAssignment(String assignName)
            throws FileEvalException {
        AssignNode assign = new AssignNode();
        assign.setAssignName(assignName);
        StatementNode value = new StatementNode();
        parseExpression(value);
        assign.setValue(value);
        return assign;
    }

   

    public static void main(String[] args) {
        FileInputStream fileStream = null;
        try {
            fileStream = new FileInputStream(
                    "/home/gloryofrobots/develop/smalltalk/yellowtalk/st/tests/parser_test.st");
            /*fileStream = new FileInputStream(
                    "/home/gloryofrobots/develop/smalltalk/yellowtalk/st/core/Behaviour.st");*/
            ProgramTextStreamInterface programStream = new ProgramTextStream(
                    fileStream);
            Lexer lexer = Lexer.create(programStream);
            Node node = CompileSuite.parseWithLexer(lexer);
            
            System.out.println("Result:");
            System.out.println(node.toString());

        } catch (IOException x) {
            System.err.println(x);
        } catch (ProgramReadException e) {
            System.err.println(e);
        } catch (FileEvalException e) {
            e.printStackTrace();
        } finally {
            if (fileStream != null) {
                try {
                    fileStream.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
    }
}