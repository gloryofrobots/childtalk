package ua.ho.gloryofrobots.yellowtalk.bootstrap;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.yellowtalk.scheduler.ExceptionHandler;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.STArray;
import ua.ho.gloryofrobots.yellowtalk.stobject.STBlock;
import ua.ho.gloryofrobots.yellowtalk.stobject.STByteObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STCharacter;
import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STFloating;
import ua.ho.gloryofrobots.yellowtalk.stobject.STLargeInteger;
import ua.ho.gloryofrobots.yellowtalk.stobject.STMetaclass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STNumber;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSmallInteger;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STPrimitive;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;
import ua.ho.gloryofrobots.yellowtalk.stobject.STString;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

@SuppressWarnings("serial")
public class DefaultPrimitivesInitialiser {
    private static class NumberPrimitives {

        public STPrimitive add = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.add(first, second);
            }
        };

        public STPrimitive substract = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.substract(first, second);
            }
        };

        public STPrimitive multiply = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.multiply(first, second);
            }
        };

        public STPrimitive divide = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.divide(first, second);
            }
        };

        public final STPrimitive lessThen = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.lessThen(first, second);
            }
        };

        public STPrimitive greaterThen = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.greaterThen(first, second);
            }
        };

        public STPrimitive lessEqual = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.lessEqual(first, second);
            }
        };

        public STPrimitive greaterEqual = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.greaterEqual(first, second);
            }
        };

        public STPrimitive equal = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.equal(first, second);
            }
        };

        public STPrimitive notEqual = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.notEqual(first, second);
            }
        };

        public STPrimitive mod = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.mod(first, second);
            }
        };
    }

    public static <T extends STObject> T getStrictCastedObject(Routine routine,
            STObject obj, String typeName) {
        T result = obj.castToSubclass();
        if (result == null) {
            routine.signal(Universe.signals().PrimitiveError, String.format(
                    "Expected type : %s but got instance of %s", typeName, obj
                            .getSTClass().getName()));

        }

        return result;
    }
    
    public static void initialisePrimitives() {
        initialiseBehaviour();
        initialiseBlock();
        initialiseObject();
        initialiseByteArray();
        initialiseSmallInteger();
        initialiseLargeInteger();
        initialiseFloat();
        initialiseCharacter();
        initialiseString();
        
        initialiseDateTime();
        initialiseSmalltalk();
    }

    private static void initialiseSmalltalk() {
        /*STClass smallTalk = Universe.classes().Smalltalk;
        smallTalk.setPrimitive("Smalltalk_quit", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                throw new RuntimeException();
            }
        });
        
        smallTalk.setPrimitive("Smalltalk_environmentVariableAt", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject arg = routine.getArgument(0);
                
                String name = arg.toString();
                
                return STString.create(System.getenv(name)); 
            }
        });*/
    }

    private static void initialiseDateTime() {
        STClass dateTime = Universe.classes().DateTime;
        dateTime.setPrimitive("DateTime_gmTime", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                throw new RuntimeException();
            }
        });
    }

    private static void initialiseString() {
        STClass string = Universe.classes().String;
        string.setPrimitive("String_asSymbol", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STString str = receiver.castToSubclass();
                return STSymbol.unique(str.toString());
            }
        });
        
        string.setPrimitive("String_hash", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {

                return STSmallInteger.create(receiver.hashCode());
            }
        });
    }

    private static void initialiseObject() {
        STClass object = Universe.classes().Object;
        object.setPrimitive("Object_class", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STClass klass = receiver.getSTClass();
                return klass;
            }
        });

        object.setPrimitive("Object_identityEqual", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject argument = routine.getArgument(0);

                if (argument == receiver) {
                    return Universe.objects().TRUE;
                }

                return Universe.objects().FALSE;
            }
        });

        object.setPrimitive("Object_equal", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject argument = routine.getArgument(0);

                if (argument.equals(receiver) == true) {
                    return Universe.objects().TRUE;
                }

                return Universe.objects().FALSE;
            }
        });

        object.setPrimitive("Object_hash", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {

                return STSmallInteger.create(receiver.hashCode());
            }
        });

        object.setPrimitive("Object_copy", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                return receiver.shallowCopy();
            }
        });

        object.setPrimitive("Object_perform", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                // FIXME May be String here
                STSymbol selector = (STSymbol) routine.getArgument(0);
                routine.flushArgumentsToStack(stack);
                SchedulingSuite.callForSelector(routine, receiver, selector);

                return Universe.objects().NIL;
            }
        });
    }

    public static void initialiseBlock() {
        STClass block = Universe.classes().Block;
        block.setPrimitive("BlockClosure_execute", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                SchedulingSuite.callExecutable(routine, (STExecutableObject) receiver);
                return Universe.objects().NIL;
            }
        });

        block.setPrimitive("BlockClosure_on_do", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject exception = stack.pop();
                STBlock block = (STBlock) stack.pop();

                SchedulingSuite.callExecutableWithExceptionHandling(routine,
                        (STExecutableObject) receiver, exception,
                        (ExceptionHandler) block);

                return Universe.objects().NIL;
            }
        });

        block.setPrimitive("BlockClosure_newProcess", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                throw new RuntimeException();
            }
        });

    }

    private static void initialiseBehaviour() {
        STClass behaviour = Universe.classes().Behaviour;
        behaviour.setPrimitive("Behavior_new", new STPrimitive() {

            @SuppressWarnings("unchecked")
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {

                STObject object = null;

                STClass klass = (STClass) receiver;
                Class internalClass = klass.getInternalObjectClass();
                if (internalClass != null) {
                    object = STObject.newObject(klass, internalClass);
                } else {
                    object = STObject.createWithClass(klass);
                }

                return object;
            }
        });
        
        behaviour.setPrimitive("Behaviour_createSubclass", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STMetaclass.createClassInRuntime(this, routine, receiver, stack);
                return Universe.objects().NIL;
            }
        });
        
    }

    private static void initialiseByteArray() {
        STClass byteArray = Universe.classes().ByteArray;

        byteArray.setPrimitive("ByteArray_newColon", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger argument = (STSmallInteger) routine
                        .getArgument(0);
                STByteObject obj = STByteObject.create(argument.toInt());
                return obj;
            }
        });

        byteArray.setPrimitive("ByteArray_at", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger indexArgument = (STSmallInteger) routine
                        .getArgument(0);

                STByteObject obj = receiver.castToSubclass();

                if (indexArgument == null) {
                    primitiveError(routine, "Invalid ByteArray index %s",
                            routine.getArgument(0).toString());

                    return null;
                }

                int index = indexArgument.toInt();
                if (index > obj.size() || index < 0) {
                    primitiveError(routine, "Invalid ByteArray index %d ",
                            index);

                    return null;
                }

                byte value = obj.at(index);

                return STSmallInteger.create((int) value);
            }
        });

        byteArray.setPrimitive("ByteArray_at_put", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger indexArgument = routine.getArgument(0)
                        .castToSubclass();
                STSmallInteger valueArgument = routine.getArgument(1)
                        .castToSubclass();

                if (indexArgument == null) {
                    primitiveError(routine, "Invalid ByteArray index %s",
                            routine.getArgument(0).toString());

                    return null;
                }

                if (valueArgument == null) {
                    primitiveError(routine, "Invalid ByteArray value %s",
                            routine.getArgument(1).toString());

                    return null;
                }

                STByteObject obj = receiver.castToSubclass();

                int index = indexArgument.toInt();
                if (index > obj.size() || index < 0) {
                    primitiveError(routine, "Invalid ByteArray index %d ",
                            index);

                    return null;
                }

                int value = valueArgument.toInt();
                if (value < 0 || value > 255) {
                    primitiveError(routine, "Expected byte value, but got %d ",
                            value);

                    return null;
                }
                obj.put(index, (byte) value);

                return Universe.objects().NIL;
            }
        });
    }

    private static void initialiseCharacter() {
        STClass character = Universe.classes().Character;
        character.setPrimitive("Character_new", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger value = (STSmallInteger) routine.getArgument(0);
                if (value == null) {
                    primitiveError(
                            routine,
                            "Invalid Character data, expected SmallInteger, but got %s",
                            routine.getArgument(0).toString());

                    return null;
                }

                return STCharacter.create((char) value.toInt());
            }
        });

        character.setPrimitive("Character_value", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STCharacter character = receiver.castToSubclass();
                int value = (int) character.toChar();
                return STSmallInteger.create(value);
            }
        });
    }

    private static void initialiseFloat() {
     
        STClass floating = Universe.classes().Float;

        floating.setPrimitive("Float_plus", sNumberPrimitives.add);
        floating.setPrimitive("Float_minus",
                sNumberPrimitives.substract);
        floating.setPrimitive("Float_mul",
                sNumberPrimitives.multiply);
        floating.setPrimitive("Float_div", sNumberPrimitives.divide);
        floating
                .setPrimitive("Float_lt", sNumberPrimitives.lessThen);
        floating.setPrimitive("Float_gt",
                sNumberPrimitives.greaterThen);
        floating.setPrimitive("Float_le",
                sNumberPrimitives.lessEqual);
        floating.setPrimitive("Float_ge",
                sNumberPrimitives.greaterEqual);
        floating.setPrimitive("Float_eq", sNumberPrimitives.equal);
        floating
                .setPrimitive("Float_ne", sNumberPrimitives.notEqual);
        floating.setPrimitive("Float_mod", sNumberPrimitives.mod);
        floating.setPrimitive("Float_ceil", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STFloating first = receiver.castToSubclass();
                return first.ceil();
            }
        });
        
        floating.setPrimitive("Float_floor", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STFloating first = receiver.castToSubclass();
                return first.floor();
            }
        });
        
        floating.setPrimitive("Float_trunc", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STFloating first = receiver.castToSubclass();
                return first.truncate();
            }
        });
        
        floating.setPrimitive("Float_newColon", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber number = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");
                STNumber result = number.convert(STNumber.FLOAT_INTEGER_PRIORITY);
                if(result == null) {
                    primitiveError(routine, "Can`t create Float instance from %s ",
                            number.toString());
                }
                
                return result;
            }
        });
  
    }

    private static void initialiseLargeInteger() {
        // TODO Auto-generated method stub
        STClass largeInteger = Universe.classes().LargeInteger;

        largeInteger.setPrimitive("LargeInteger_plus", sNumberPrimitives.add);
        largeInteger.setPrimitive("LargeInteger_minus",
                sNumberPrimitives.substract);
        largeInteger.setPrimitive("LargeInteger_mul",
                sNumberPrimitives.multiply);
        largeInteger.setPrimitive("LargeInteger_div", sNumberPrimitives.divide);
        largeInteger
                .setPrimitive("LargeInteger_lt", sNumberPrimitives.lessThen);
        largeInteger.setPrimitive("LargeInteger_gt",
                sNumberPrimitives.greaterThen);
        largeInteger.setPrimitive("LargeInteger_le",
                sNumberPrimitives.lessEqual);
        largeInteger.setPrimitive("LargeInteger_ge",
                sNumberPrimitives.greaterEqual);
        largeInteger.setPrimitive("LargeInteger_eq", sNumberPrimitives.equal);
        largeInteger
                .setPrimitive("LargeInteger_ne", sNumberPrimitives.notEqual);
        largeInteger.setPrimitive("LargeInteger_mod", sNumberPrimitives.mod);

        largeInteger.setPrimitive("LargeInteger_bitAnd", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STLargeInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "LargeInteger");

                return first.bitAnd(second.toInt());
            }
        });

        largeInteger.setPrimitive("LargeInteger_bitOr", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STLargeInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "LargeInteger");

                return first.bitOr(second.toInt());
            }
        });

        largeInteger.setPrimitive("LargeInteger_bitXor", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STLargeInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "LargeInteger");

                return first.bitXor(second.toInt());
            }
        });

        largeInteger.setPrimitive("LargeInteger_bitShift", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STLargeInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "LargeInteger");

                return first.bitShift(second.toInt());
            }
        });

        largeInteger.setPrimitive("LargeInteger_asFloat", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STLargeInteger first = receiver.castToSubclass();

                return first.asFloat();
            }
        });

        largeInteger.setPrimitive("LargeInteger_clear", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STLargeInteger first = receiver.castToSubclass();

                first.clear();
                return Universe.objects().NIL;
            }
        });
        
        largeInteger.setPrimitive("LargeInteger_newColon", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber number = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");
                STNumber result = number.convert(STNumber.LARGE_INTEGER_PRIORITY);
                if(result == null) {
                    primitiveError(routine, "Can`t create LargeInteger instance from %s ",
                            number.toString());
                }
                
                return result;
            }
        });
    }

    private static NumberPrimitives sNumberPrimitives = new NumberPrimitives();

    private static void initialiseSmallInteger() {

        STClass smallInteger = Universe.classes().SmallInteger;

        smallInteger.setPrimitive("SmallInteger_plus", sNumberPrimitives.add);
        smallInteger.setPrimitive("SmallInteger_minus",
                sNumberPrimitives.substract);
        smallInteger.setPrimitive("SmallInteger_mul",
                sNumberPrimitives.multiply);
        smallInteger.setPrimitive("SmallInteger_div", sNumberPrimitives.divide);
        smallInteger
                .setPrimitive("SmallInteger_lt", sNumberPrimitives.lessThen);
        smallInteger.setPrimitive("SmallInteger_gt",
                sNumberPrimitives.greaterThen);
        smallInteger.setPrimitive("SmallInteger_le",
                sNumberPrimitives.lessEqual);
        smallInteger.setPrimitive("SmallInteger_ge",
                sNumberPrimitives.greaterEqual);
        smallInteger.setPrimitive("SmallInteger_eq", sNumberPrimitives.equal);
        smallInteger
                .setPrimitive("SmallInteger_ne", sNumberPrimitives.notEqual);
        smallInteger.setPrimitive("SmallInteger_mod", sNumberPrimitives.mod);

        smallInteger.setPrimitive("SmallInteger_bitAnd", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "SmallInteger");

                return first.bitAnd(second);
            }
        });

        smallInteger.setPrimitive("SmallInteger_bitOr", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "SmallInteger");

                return first.bitOr(second);
            }
        });

        smallInteger.setPrimitive("SmallInteger_bitXor", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "SmallInteger");

                return first.bitXor(second);
            }
        });

        smallInteger.setPrimitive("SmallInteger_bitShift", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "SmallInteger");

                return first.bitShift(second);
            }
        });

        smallInteger.setPrimitive("SmallInteger_asFloat", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger first = receiver.castToSubclass();

                return first.asFloat();
            }
        });

        smallInteger.setPrimitive("SmallInteger_asLargeInteger",
                new STPrimitive() {
                    @Override
                    protected STObject onExecute(Routine routine,
                            STObject receiver, STStack stack) {
                        STSmallInteger first = receiver.castToSubclass();

                        return first.asLargeInteger();
                    }
                });
        
        smallInteger.setPrimitive("SmallInteger_newColon", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber number = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");
                STNumber result = number.convert(STNumber.SMALL_INTEGER_PRIORITY);
                if(result == null) {
                    primitiveError(routine, "Can`t create SmallInteger instance from %s ",
                            number.toString());
                }
                
                return result;
            }
        });

    }
}

// SyxPrimitiveEntry _syx_primitive_entries[] = {
// { "Processor_yield", Processor_yield },
//
// /* Common for objects */

// !!!!!!!!!!!!!!!!!!!!!
// { "Object_at", Object_at },
// { "Object_at_put", Object_at_put },
// { "ArrayedCollection_replaceFromToWithStartingAt",
// ArrayedCollection_replaceFromToWithStartingAt },
// { "LargeInteger_intDiv", LargeInteger_intDiv },
// { "LargeInteger_quo", LargeInteger_quo },
// !!!!!!!!!!!!!!!!!!!!

//


//
// /* Contexts */
// { "ContextPart_parent", ContextPart_parent },
// { "ContextPart_receiver", ContextPart_receiver },
// { "BlockContext_outerContext", BlockContext_outerContext },
//
// /* Interpreter */
// { "Processor_enter", Processor_enter },
// { "Processor_swapWith", Processor_swapWith },
// { "Processor_leaveTo_andAnswer", Processor_leaveTo_andAnswer },
//
// { "Semaphore_signal", Semaphore_signal },
// { "Semaphore_wait", Semaphore_wait },
// { "Semaphore_waitFor", Semaphore_waitFor },
//

//
// /* File streams */
// { "StdIOStream_nextPut", StdIOStream_nextPut },
// { "StdIOStream_nextPutAll", StdIOStream_nextPutAll },
// { "FileStream_fileOp", FileStream_fileOp },
