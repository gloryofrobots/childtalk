package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.DuplicateVariableException;
import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeArray;

public class STExecutableObject extends STObjectWithScope {

    private static final long serialVersionUID = 1L;
    private List<STSymbol> mArguments;
    private List<STObject> mArgumentsValues;
    private List<STObject> mLiterals;

    private BytecodeArray mBytecode;

    public STExecutableObject() {
        mArguments = new ArrayList<STSymbol>();
        mArgumentsValues = new ArrayList<STObject>();
        mLiterals = new ArrayList<STObject>();
    }

    public int placeLiteral(STObject obj) {
        int index = mLiterals.indexOf(obj);
        if (index < 0) {
            mLiterals.add(obj);
            return mLiterals.size() - 1;
        }

        return index;
    }

    public STObject getLiteral(int index) {
        return mLiterals.get(index);
    }
    
    public void addArgument(STSymbol name) throws DuplicateVariableException {
        if (mArguments.contains(name)) {
            throw new DuplicateVariableException(name.toString());
        }

        mArguments.add(name);
    }
    
    public void addArgumentValue(int index, STObject value)  {
        mArgumentsValues.add(index, value);
    }
    
    public int getCountArguments() {
        return mArguments.size();
    }
    
    public STSymbol getArgument(int index) {
        return mArguments.get(index);
   }
    
//    public int getArgumentIndex(STSymbol name) {
//         return mArguments.indexOf(name);
//    }
    
    public BytecodeArray getBytecode() {
        return mBytecode;
    }
    
    
    public STScope createScope() {
        STScope scope = mScope.copySelf();
        fillScope(scope);
        return scope;
    }
    
    public void fillScope(STScope scope) {
        int count = getCountArguments();
        for(int i = 0; i < count; i++){
            STSymbol varName = mArguments.get(i);
            STObject value = mArgumentsValues.get(i);
            scope.put(varName, value);
        }
    }

    

}