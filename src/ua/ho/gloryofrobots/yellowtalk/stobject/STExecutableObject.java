package ua.ho.gloryofrobots.yellowtalk.stobject;


import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeArray;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeWriter;
import ua.ho.gloryofrobots.yellowtalk.compilation.CompileInfo;
import ua.ho.gloryofrobots.yellowtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;

public abstract class STExecutableObject extends STObject {
    private static final long serialVersionUID = 1L;
    private STArray mArguments;
    private STArray mArgumentsValues;
    private STArray mLiterals;
    
    private BytecodeArray mBytecode;
    private BytecodeWriter mBytecodeWriter;
    
    protected STExecutableObject() {
        transformToScopedObject();
        mArguments = STArray.create();
        mArgumentsValues = STArray.create();
        mLiterals = STArray.create();
        mBytecode = new BytecodeArray();
        //TODO REFACTOR
        mBytecodeWriter = new BytecodeWriter(mBytecode);
    }
    
    public BytecodeWriter getBytecodeWriter() {
        return mBytecodeWriter;
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
    
    public void addArgument(STSymbol name) throws DuplicateVariableException{
        if (mArguments.has(name)) {
            throw new DuplicateVariableException(name.toString());
        }

        mArguments.add(name);
    }
    
    public void setArguments(String[] names){
        mArguments.clear();
        for(String name : names) {
            mArguments.add(STSymbol.unique(name));
        }
    }
    
    public void addArgumentValue(int index, STObject value)  {
        mArgumentsValues.set(index, value);
    }
    
    public int getCountArguments() {
        return mArguments.size();
    }
    
    public STSymbol getArgument(int index) {
        return mArguments.getAndCast(index);
   }
    
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
            STSymbol varName = mArguments.getAndCast(i);
            STObject value = mArgumentsValues.get(i);
            scope.put(varName, value);
        }
    }

    public abstract Routine createRoutine();
}