package ua.ho.gloryofrobots.yellowtalk;

import java.io.FileInputStream;
import java.io.IOException;

import ua.ho.gloryofrobots.yellowtalk.ProgramTextStream.ProgramReadException;
import ua.ho.gloryofrobots.yellowtalk.node.*;
import ua.ho.gloryofrobots.yellowtalk.node.NodeWithMetaData.UnknownMetaDataException;

public class Parser implements ParserInterface {
    final String SPECIAL_TOKEN_SUBCLASS = "subclass:";
    final String SPECIAL_TOKEN_METHODS = "methods:";
    
    public Parser(LexerInterface lexer) {
        m_lexer = lexer;
    }
   
    private void parsingError(String txt, Token token) throws FileEvalException {
        System.out.println(token.stringValue());
        ((Lexer)m_lexer).throwError(txt, token);
    }

    @Override
    public void setLexer(LexerInterface lexer) {
        m_lexer = lexer;
    }

    @Override
    public Node parse() throws FileEvalException {

        ProgramNode node = new ProgramNode();

        while (m_lexer.current().type != Token.Type.END) {
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
        
        parseSimple(node);
    }

    protected boolean parseExtend(ProgramNode node) throws FileEvalException {
        if (m_lexer.lookup(2) == false) {
            return false;
        }
        return false;
    }

    protected boolean parseSimple(ProgramNode node) throws FileEvalException {
        BodyNode body = new BodyNode();
        parseBody(body);
        node.addNode(body);
        return true;
    }

    protected boolean parseClass(ProgramNode node) throws FileEvalException {

        if (m_lexer.lookup(2) == false) {
            return false;
        }

        Token tokenSubclass = m_lexer.next();

        if (tokenSubclass.type != Token.Type.NAME_CONST) {
            return false;
        }
        String superClassName = tokenSubclass.stringValue();
        Token token = null;

        token = m_lexer.next();
        if ((token.type == Token.Type.NAME_COLON && token
                .equalValue(SPECIAL_TOKEN_SUBCLASS)) == false) {
            m_lexer.previous();
            return false;
        }
        
        token = m_lexer.next();
        if(token.type != Token.Type.NAME_CONST){
            parsingError("Invalid class name", token);
            return false;
        }
        
        String className = token.stringValue();
        token = m_lexer.next();
        
        if ((token.type == Token.Type.BINARY && token.equalValue('[')) == false) {
            m_lexer.previous();
            parsingError("Missed [ in class declaration", token);
            return false;
        }
        
        m_lexer.next();
        
        ClassNode classNode = new ClassNode();
        classNode.setSuperclassName(superClassName);
        classNode.seClassName(className);
        
        while (true) {
            token = m_lexer.current();
            if (token.type == Token.Type.CLOSING && token.equalValue(']')) {
                break;
            }
            else if (parseMetaData(classNode)) {
                continue;
            } else if (parseClassMethod(classNode)) {
                continue;
            }  else {
                parsingError("Class declaration must ended with ]", token);
                return false;
            }
        }
        
        node.addNode(classNode);
        m_lexer.next();
        // parsingError()
        return true;
    }

    protected boolean parseMetaData(NodeWithMetaData node)
            throws FileEvalException {
        Token tokenCheck = null;
        tokenCheck = m_lexer.current();
        if ((tokenCheck.type == Token.Type.BINARY && tokenCheck.equalValue('<')) == false) {
            return false;
        }
        Token tokenLabel = m_lexer.next();
        if (tokenLabel.type != Token.Type.NAME_COLON) {
            parsingError("Invalid class metadata label", tokenCheck);
            return false;
        }

        Token tokenData = m_lexer.next();
        if (tokenData.type != Token.Type.STRING_CONST) {
            parsingError("Metadata name must be string", tokenCheck);
            return false;
        }

        tokenCheck = m_lexer.next();
        if ((tokenCheck.type == Token.Type.BINARY && tokenCheck.equalValue('>')) == false) {
            parsingError("Missed closing >", tokenCheck);
            return false;
        }
        
        try {
            node.setMetaData(tokenLabel.stringValue(), tokenData.stringValue());
        } catch (UnknownMetaDataException e) {
            parsingError(e.getMessage(), tokenLabel);
        }
        
        m_lexer.next();
        return true;
    }

    protected boolean parseClassMethod(ClassNode classNode)
            throws FileEvalException {
        MethodNode method = new MethodNode();
        parseMethodMessagePattern(method);
        Token token = m_lexer.current();
        if ((token.type == Token.Type.BINARY && token.equalValue('[')) == false) {
            parsingError("Missed [ in method declaration", token);
            return false;
        }

        m_lexer.next();

        while (true) {
            if (parseMetaData(method) == false) {
                break;
            }
        }

        parseMethodTemporaries(method);
        BodyNode body = method.getBody();
        parseBody(body);
        
        
        token = m_lexer.current();
        if ((token.type == Token.Type.CLOSING && token.equalValue(']')) == false) {
            parsingError("Missed closing ] in method declaration", token);
            return false;
        }
        
        m_lexer.next();
        classNode.addMethod(method);
        return true;
    }

    protected void parseMethodTemporaries(MethodNode method)
            throws FileEvalException {
        Token token = m_lexer.current();
        if ((token.type == Token.Type.BINARY && token.equalValue('|'))== false) {
            return;
        }

        token = m_lexer.next();
        while (token.type == Token.Type.NAME_CONST) {
            method.addTemporary(token.stringValue());
            token = m_lexer.next();
        }

        if (!(token.type == Token.Type.BINARY && token.equalValue('|'))) {
            parsingError("Temporary list not terminated by bar", token);

        }

        m_lexer.next();
    }

    protected void parseMethodMessagePattern(MethodNode method)
            throws FileEvalException {
        Token token = m_lexer.current();
        switch (token.type) {
        case NAME_CONST:
            method.setSelector(token.stringValue());
            m_lexer.next();
            break;
        case BINARY:
            /* Binary message pattern */
            method.setSelector(token.stringValue());

            token = m_lexer.next();
            if (token.type != Token.Type.NAME_CONST) {
                parsingError("Expected name constant for argument name\n",
                        token);
            }

            method.addArgument(token.stringValue());
            m_lexer.next();
            break;
        case NAME_COLON:
            /* Keyword message pattern */
            String selector = new String();
            while (token.type == Token.Type.NAME_COLON) {

                selector += token.stringValue();
                token = m_lexer.next();

                if (token.type != Token.Type.NAME_CONST) {
                    parsingError("Expected name constant for argument name\n",
                            token);

                }

                method.addArgument(token.stringValue());

                token = m_lexer.next();
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
        System.out.printf("LOG %s  %s\n", function, message);
    }
    
    protected void parseBody(BodyNode body) throws FileEvalException {
        Token token = m_lexer.current();

        while (true) {
            while (token.type == Token.Type.CLOSING) {
                // if (m_in_block && token.value.character == ']')
                // return;

                if (token.charValue() == '.') {
                    token = m_lexer.next();
                } else {
                    token = m_lexer.next();
                }
            }
            
            DEBUG_LOG("parseBody" , token);
            
            StatementNode statement = parseStatement();
            DEBUG_LOG("getStatement" , statement.toString());
            body.addStatement(statement);
            
            token = m_lexer.current();
            if(token.type == Token.Type.CLOSING && token.equalValue(']')) {
                return;
            }
            
            token = m_lexer.next();
        }
        //
        // if (m_in_block)
        // {
        // ERROR_SIGNAL(GSE_ERROR_INTERP,
        // NEW_OBJECT<String>("Expected ] after block body"));
        // }

    }
    
    protected void parseBlockBody(BodyNode body) throws FileEvalException {
        Token token = m_lexer.current();

        while (true) {
            while (token.type == Token.Type.CLOSING) {
                // if (m_in_block && token.value.character == ']')
                // return;
                if (token.charValue() == '.') {
                    token = m_lexer.next();
                } else {
                    token = m_lexer.next();
                }
            }
            
            DEBUG_LOG("parseBlockBody" , token);
            
            StatementNode statement = parseStatement();
            DEBUG_LOG("getStatement parseBlockBody" , statement.toString());
            body.addStatement(statement);
            token = m_lexer.current();
            
            if(token.type == Token.Type.CLOSING && token.equalValue(']')) {
                return;
            }
            
            token = m_lexer.next();
        }
        //
        // if (m_in_block)
        // {
        // ERROR_SIGNAL(GSE_ERROR_INTERP,
        // NEW_OBJECT<String>("Expected ] after block body"));
        // }

    }
    
    private StatementNode parseStatement() throws FileEvalException {
        Token token = m_lexer.current();
        DEBUG_LOG("parseStatement" , token);
        StatementNode statement = new StatementNode();

        if (token.type == Token.Type.BINARY && token.equalValue("^")) {
            m_lexer.next();
            parseExpression(statement);
            statement.addNode(new ReturnNode());
        } else {
            parseExpression(statement);
        }

        return statement;
    }

    private void parseExpression(StatementNode statement)
            throws FileEvalException {
        Token token = m_lexer.current();
        DEBUG_LOG("parseExpression" , token);
        
        if (token.type == Token.Type.NAME_CONST) {
            String assign_name = token.stringValue();

            token = m_lexer.next();
            if (token.type == Token.Type.BINARY
                    && (token.equalValue(":=") || token.equalValue("<-"))) {

                m_lexer.next();
                DEBUG_LOG("parseAssignment" , m_lexer.current());
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
        DEBUG_LOG("parseContinuation" , m_lexer.current());
        parseKeyContinuation(statement);

        Token token = m_lexer.current();

        while (token.type == Token.Type.CLOSING && token.charValue() == ';') {
            m_lexer.next();
            parseKeyContinuation(statement);
            token = m_lexer.next();
        }
    }

    private void parseKeyContinuation(StatementNode statement)
            throws FileEvalException {
        DEBUG_LOG("parseKeyContinuation" , m_lexer.current());
        
        parseBinaryContinuation(statement);

        Token token = m_lexer.current();
        if (token.type != Token.Type.NAME_COLON) {
            return;
        }
        String selector = new String();
        StatementNode messageStatement = new StatementNode();
        while (token.type == Token.Type.NAME_COLON) {
            selector += token.stringValue();

            m_lexer.next();

            Node term = parseTerm();
            messageStatement.addNode(term);

            parseBinaryContinuation(messageStatement);

            token = m_lexer.current();
        }
        
        statement.addNode(messageStatement);
        MessageSelectorNode messageSelector();
        MessageNode message = new MessageNode();
        message.setSelector(selector);
        message.setStatement(messageStatement);
        statement.addNode(message);
    }

    private void parseBinaryContinuation(StatementNode statement)
            throws FileEvalException {
        DEBUG_LOG("parseBinaryContinuation" , m_lexer.current());
        parseUnaryContinuation(statement);
        Token token = m_lexer.current();
        
        while (token.type == Token.Type.BINARY) {
            
            String selector = token.stringValue();

            m_lexer.next();
            StatementNode messageStatement = new StatementNode();
            Node term = parseTerm();
            messageStatement.addNode(term);

            parseUnaryContinuation(messageStatement);

            MessageNode message = new MessageNode();
            message.setSelector(selector);
            message.setStatement(messageStatement);

            statement.addNode(message);
            //token = m_lexer.next();
            token = m_lexer.current();
        }
    }

    private void parseUnaryContinuation(StatementNode messageStatement) throws FileEvalException {
        DEBUG_LOG("parseUnaryContinuation" , m_lexer.current());
        Token token = m_lexer.current();

        while (token.type == Token.Type.NAME_CONST) {
            MessageNode message = new MessageNode();
            message.setSelector(token.stringValue());
            message.setStatement(null);
            messageStatement.addNode(message);
            token = m_lexer.next();
        }
    }

    private Node parseTerm() throws FileEvalException {
        Token token = m_lexer.current();
        DEBUG_LOG("parseTerm" , m_lexer.current());
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
                token = m_lexer.next();
                result = new StatementNode();
                parseExpression((StatementNode) result);

                token = m_lexer.current();
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
        m_lexer.next();

        return result;
    }

    private Node parseBlock() throws FileEvalException {
        BlockNode block = new BlockNode();
        DEBUG_LOG("parseBlock" , m_lexer.current());
        Token token = m_lexer.current();
        if ((token.type == Token.Type.BINARY && token.equalValue('[')) == false) {
            parsingError("Missed [ in block declaration", token);
        }

        m_lexer.next();
        parseBlockMessagePattern(block);

        BodyNode body = block.getBody();
        parseBlockBody(body);

        token = m_lexer.current();
        if ((token.type == Token.Type.CLOSING && token.equalValue(']')) == false) {
            parsingError("Missed closing ] in block declaration", token);
        }

        return block;
    }

    private void parseBlockMessagePattern(BlockNode block)
            throws FileEvalException {
        
        Token token = m_lexer.current();
        DEBUG_LOG("parseBlockMessagePattern" , m_lexer.current());
        
        if (!(token.type == Token.Type.BINARY && token.equalValue(":"))) {
            return;
        }

        while (token.type == Token.Type.BINARY && token.equalValue(":")) {

            token = m_lexer.next();

            if (token.type != Token.Type.NAME_CONST) {
                parsingError("Expected name  for argument\n", token);
            }

            block.addArgument(token.stringValue());
            token = m_lexer.next();
        }

        if ((token.type == Token.Type.BINARY && token.equalValue("|")) == false) {
            parsingError("Expected | after block message pattern\n", token);
        }

        m_lexer.next();
    }

    private ArrayNode parseArray() throws FileEvalException {
        Token token = m_lexer.next();
        ArrayNode array = new ArrayNode();

        token = m_lexer.next();
        while ((token.type == Token.Type.END || (token.type == Token.Type.CLOSING && token
                .charValue() == '}')) == false) {
            StatementNode element = new StatementNode();
            parseExpression(element);
            array.addElement(element);

            token = m_lexer.next();
            if (token.type == Token.Type.CLOSING && token.charValue() == '.')
                token = m_lexer.next();
        }

        return array;
    }

    private Node parseLiteralArray() throws FileEvalException {
        LiteralArrayNode array = new LiteralArrayNode();

        Token token = m_lexer.next();
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

            token = m_lexer.next();
        }

        return array;
    }

    private NameTermNode parseNameTerm(String assign_name) {
        NameTermNode term = new NameTermNode();
        term.setName(assign_name);
        return term;
    }

    private AssignNode parseAssignment(String assignName) throws FileEvalException {
        AssignNode assign = new AssignNode();
        assign.setAssignName(assignName);
        StatementNode value = new StatementNode();
        parseExpression(value);
        assign.setValue(value);
        return assign;
    }
    
    LexerInterface m_lexer;
    
    public static void main(String[] args) {
        FileInputStream fileStream = null;
        try {
            fileStream = new FileInputStream(
                    "/home/gloryofrobots/develop/smalltalk/yellowtalk/st/parser_test.st");
            ProgramTextStreamInterface programStream = new ProgramTextStream(
                    fileStream);
            LexerInterface lexer = new Lexer(programStream);
            Parser parser = new Parser(lexer);
            Node node = parser.parse();
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