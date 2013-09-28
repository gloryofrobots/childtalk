package ua.ho.gloryofrobots.childtalk.stobject;


import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeArray;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeWriter;
import ua.ho.gloryofrobots.childtalk.compilation.CompileInfo;
import ua.ho.gloryofrobots.childtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;

public abstract class STExecutableObject extends STObject {
    private static final long serialVersionUID = 1L;
    private STArray mArguments;
    private STArray mArgumentsValues;
    private STArray mLiterals;
    private CompileInfo mCompileInfo;
    private BytecodeArray mBytecode;
    private BytecodeWriter mBytecodeWriter;
    private STArray mTemporaries;
    
    protected STExecutableObject() {
        transformToScopedObject();
        mArguments = STArray.create();
        mArgumentsValues = STArray.create();
        mLiterals = STArray.create();
        mBytecode = new BytecodeArray();
        mTemporaries = STArray.create();
        mCompileInfo = null;
        mBytecodeWriter = new BytecodeWriter(mBytecode);
    }
    
    public CompileInfo getCompileInfo() {
        return mCompileInfo;
    }
    
    public void setCompileInfo(CompileInfo info) {
         mCompileInfo = info;
         mBytecodeWriter.setCompileInfo(info);
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
        return mLiterals.at(index);
    }
    
    public void addArgument(STSymbol name) throws DuplicateVariableException{
        if (mArguments.has(name)) {
            throw new DuplicateVariableException(name.toString());
        }
        mArgumentsValues.add(ImageSuite.image().objects().NIL);
        mArguments.add(name);
    }
    
    public void setArguments(String[] names){
        mArguments.clear();
        for(String name : names) {
            try {
                addArgument(STSymbol.create(name));
            } catch (DuplicateVariableException e) {
                continue;
            }
        }
    }
    
    public void addArgumentValue(int index, STObject value)  {
        mArgumentsValues.put(index, value);
    }
    
    public int getCountArguments() {
        return mArguments.size();
    }
    
    public STSymbol getArgument(int index) {
        return mArguments.getAndCast(index);
   }
    
    public void addTemporary(STSymbol name) throws DuplicateVariableException {
        if (mTemporaries.has(name)) {
            throw new DuplicateVariableException(name.toString());
        }

        mTemporaries.add(name);
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
            STObject value = mArgumentsValues.at(i);
            //System.out.println(varName.toString());
            scope.put(varName, value);
        }
        
        count = mTemporaries.size();
        for(int i = 0; i < count; i++) {
            STSymbol varName  = mTemporaries.getAndCast(i);
            scope.put(varName, ImageSuite.image().objects().NIL);
        }
    }

    public abstract Routine createRoutine();
    
    
    public void fillExecutable(STExecutableObject executable) throws DuplicateVariableException {
        int count = getCountArguments();
        for(int i = 0; i < count; i++){
            STSymbol varName = mArguments.getAndCast(i);
            executable.addArgument(varName);
        }
        
        count = mTemporaries.size();
        for(int i = 0; i < count; i++) {
            STSymbol varName  = mTemporaries.getAndCast(i);
            executable.addTemporary(varName);
        }
        
        count = mLiterals.size();
        for(int i = 0; i < count; i++) {
            STObject literal = mLiterals.at(i);
            executable.placeLiteral(literal);
        }
        
        executable.mBytecode = mBytecode;
        executable.mCompileInfo = mCompileInfo;
    }
}