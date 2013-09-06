package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.io.Serializable;
import java.util.Set;

/*
Array.cpp
BlockClosure.cpp
BlockContext.cpp
Boolean.cpp
ByteArray.cpp
Character.cpp
ClassObject.cpp
CompiledBlock.cpp
CompiledMethod.cpp
Context.cpp
Dictionary.cpp
Float.cpp
Globals.cpp
LargeInteger.cpp
MetaClass.cpp
MethodContext.cpp
NilObject.cpp
Object.cpp
Process.cpp
Semaphore.cpp
SmallInteger.cpp
String.cpp
Symbol.cpp
Symbols.cpp
VariableBinding.cpp*/

/*! List of commands of a bytecode in SyxBytecode::code */

public class STObject implements Serializable {
    private static final long serialVersionUID = 1L;
    
    protected STScope mScope = null;
    
    public STScope getScope() {
        return mScope;
    }
    
    public void setScope(STScope scope) {
        mScope = scope;
    }
    
    public STClass getSuperClass() {
        return mSuperClass;
    }
    
    public void setSuperClass(STClass _class) {
        mSuperClass = _class;
    }
    
    private STClass mSuperClass;
}
